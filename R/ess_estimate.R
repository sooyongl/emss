#' @include ess_data.R
NULL

# Estimation -------------------------------------------------------------------
#
# An analysis unit is one grade, one round and one item subset ("All" items,
# or one content domain). Within a unit, items are ordered by location (ties
# by Order_of_Difficulty, then input order); an item's position in that order
# is its page in the ordered item booklet.
#
# Levels are integer ranks everywhere in this file; labels are attached only
# when results are displayed.

# Build the analysis units --------------------------------------------------

ess_units <- function(data, location, by_domain = FALSE, domains = NULL, grades = NULL) {
  
  # data=d; 
  
  loc <- location
  items <- data$items
  if (!loc %in% names(items)) {
    num <- names(items)[vapply(items, is.numeric, logical(1))]
    num <- setdiff(num, c("order", "row"))
    stop("Location column `", loc, "` is not in the item data. Numeric columns available: ",
         .ess_show(num, 20), ".", call. = FALSE)
  }
  if (!is.numeric(items[[loc]])) {
    stop("Location column `", loc, "` must be numeric.", call. = FALSE)
  }

  all_grades <- data$grades$grade
  if (is.null(grades)) {
    use <- all_grades
  } else if (is.numeric(grades)) {
    use <- all_grades[grades]
  } else {
    use <- all_grades[all_grades %in% grades]
  }
  use <- use[use %in% data$ratings$grade]
  if (!length(use)) stop("No grades selected.", call. = FALSE)

  want_dom <- isTRUE(by_domain)
  if (want_dom && all(is.na(items$domain))) {
    stop("`by_domain = TRUE`, but the item data has no Domain column.", call. = FALSE)
  }

  units <- list()
  for (g in use) { # g = use[1]
    gi <- items[items$grade == g, , drop = FALSE]
    miss <- gi$item_id[is.na(gi[[loc]])]
    if (length(miss)) {
      stop("Location column `", loc, "` is missing for item(s): ", .ess_show(miss), ".",
           call. = FALSE)
    }
    subsets <- "All"
    if (want_dom) {
      doms <- unique(stats::na.omit(gi$domain))
      if (!is.null(domains)) doms <- doms[doms %in% domains]
      subsets <- c(subsets, sort(doms))
    }
    rounds <- unique(data$ratings$round[data$ratings$grade == g])
    for (rd in rounds) {    # rd = rounds[1]
      for (sb in subsets) { # sb = subsets[1]
        ui <- if (sb == "All") gi else gi[gi$domain %in% sb, , drop = FALSE]
        if (!nrow(ui)) next
        ui <- ui[order(ui[[loc]], ui$order, ui$row), , drop = FALSE]
        units[[length(units) + 1]] <- list(
          grade = g, subset = sb, round = rd,
          item_id  = ui$item_id,
          location = ui[[loc]],
          n_levels = sum(data$levels$grade == g),
          sd = data$grades$sd[data$grades$grade == g],
          ec = data$grades$ec[data$grades$grade == g]
        )
      }
    }
  }

  multi_round <- length(unique(vapply(units, `[[`, "", "round"))) > 1
  ids <- vapply(units, function(u) {
    id <- paste(u$grade, u$subset, sep = "-")
    if (multi_round) id <- paste0(id, " Round ", u$round)
    id
  }, character(1))
  names(units) <- ids
  for (i in seq_along(units)) units[[i]]$id <- ids[[i]]
  units
}

# Panelists' level vectors for one unit, aligned to the unit's item order.
# Panelists are sorted in C-locale order, which is also the order used to
# break ties between equally correlated panelists in the modal ALD.
unit_ratings <- function(data, unit) {
  r <- data$ratings
  r <- r[r$grade == unit$grade & r$round == unit$round & r$item_id %in% unit$item_id, , drop = FALSE]
  panelists <- sort(unique(r$panelist), method = "radix")
  lev <- vapply(panelists, function(p) {
    rp <- r[r$panelist == p, , drop = FALSE]
    rp$level[match(unit$item_id, rp$item_id)]
  }, numeric(length(unit$item_id)))
  lev <- matrix(lev, ncol = length(panelists), dimnames = list(unit$item_id, panelists))
  table <- vapply(panelists, function(p) r$table[r$panelist == p][1], character(1))
  list(levels = lev, panelist = panelists, table = unname(table))
}

# Fit one set of alignments -----------------------------------------------------

# Inconsistency profile for every cut level: one row per candidate cut (each
# item location, in item order), columns L2_C, L3_C, ..., L2_W, L3_W, ...
# Weights are divided by SD and stripped of floating-point noise.
ess_profile <- function(location, level, n_levels, sd = 1, candidates = location) {
  
  d <- data.frame(location = location, ALD = level)
  cuts <- seq.int(2, n_levels)
  m <- length(candidates)
  prof <- lapply(cuts, function(k) essCW(d, cut_level = k, candidates = candidates)$incon_default)
  counts  <- vapply(prof, function(p) as.numeric(p$counts),  numeric(m))
  weights <- vapply(prof, function(p) p$weights / sd, numeric(m))
  counts  <- matrix(counts,  ncol = length(cuts), dimnames = list(NULL, paste0("L", cuts, "_C")))
  weights <- matrix(weights, ncol = length(cuts), dimnames = list(NULL, paste0("L", cuts, "_W")))
  exact_profile(data.frame(counts, weights, check.names = FALSE))
}

# Pick the cut row for each level: the minimum of the chosen criterion
# (weight or count); ties go to the lower weight, then the lower count, then
# the earlier item.
select_cut_rows <- function(profile, method = c("weight", "count")) {
  method <- match.arg(method)
  ks <- sub("^L([0-9]+)_C$", "\\1", grep("_C$", names(profile), value = TRUE))
  vapply(ks, function(k) {
    cnt <- profile[[paste0("L", k, "_C")]]
    wt  <- profile[[paste0("L", k, "_W")]]
    crit <- if (method == "weight") wt else cnt
    cand <- which(crit == min(crit))
    cand[order(wt[cand], cnt[cand], cand)][1]
  }, integer(1))
}

# Empirical (operational) level of each item: 1 + the number of cut scores at
# or below its location, the same rule the estimation uses (an item at or
# above a level's cut belongs to that level). When two cuts coincide, the
# level between them is empty and items at the cut go to the higher level.
operational_levels <- function(location, cut_scores) {
  1L + findInterval(location, sort(cut_scores))
}

# GAM-smoothed minimum of one profile column: the smoothed curve is evaluated
# at 200 points across the location range; the cut score is its minimum and
# the cut row is the first item at or above it.
gam_minimum <- function(location, y) {
  n <- length(location)
  knots <- if (n > 20) 10 else round(n / 2, 0)
  d <- data.frame(y = y, x = location)
  fit <- mgcv::gam(y ~ s(x, k = knots), data = d, family = "gaussian")
  pred <- predict_gam(fit, length_out = 200)
  i <- which(pred$fit == min(pred$fit))[1]
  x <- pred$x[i]
  row <- which(location >= x)[1]
  row <- which(location == location[row])[1]
  list(score = round(x, 2), value = round(pred$fit[i], 2), row = row, fit = fit)
}

safe_cor <- function(x, y) {
  tryCatch(stats::cor(x, y), warning = function(w) 0, error = function(e) 0)
}

# Fit ESS to one level vector. Returns the profile, the cut for each level,
# each item's operational level, and the alignment-location correlation.
ess_fit <- function(location, level, n_levels, sd = 1,
                    method = c("weight", "count"), smooth = FALSE) {
  
  # location = u$location; level= ur$levels[, 1]; n_levels = u$n_levels; sd= u$sd
  
  method <- match.arg(method)
  profile <- ess_profile(location, level, n_levels, sd)
  ks <- seq.int(2, n_levels)

  if (!smooth) {
    rows <- select_cut_rows(profile, method)
    cut_score <- location[rows]
    weight <- vapply(seq_along(ks), function(j) profile[[paste0("L", ks[j], "_W")]][rows[j]], numeric(1))
    gam <- NULL
  } else {
    gam <- lapply(ks, function(k) {
      w <- gam_minimum(location, profile[[paste0("L", k, "_W")]])
      list(crit = if (method == "weight") w else gam_minimum(location, profile[[paste0("L", k, "_C")]]),
           weight = w)
    })
    rows <- vapply(gam, function(g) g$crit$row, integer(1))
    cut_score <- vapply(gam, function(g) g$crit$score, numeric(1))
    weight <- vapply(gam, function(g) g$weight$value, numeric(1))
  }
  count <- vapply(seq_along(ks), function(j) profile[[paste0("L", ks[j], "_C")]][rows[j]], numeric(1))

  list(
    profile = profile,
    cuts = data.frame(level = ks, page = unname(rows), cut_score = unname(cut_score),
                      count = count, weight = weight),
    operational = operational_levels(location, cut_score),
    correlation = safe_cor(level, location),
    gam = gam
  )
}

# Panelist and aggregate cuts ------------------------------------------------------

# Fit every panelist and every modal group (each table, and all tables) in
# every unit. Returns a list of tidy data frames.
ess_estimate <- function(data, units, method = "weight", smooth = FALSE) {
  
  # method = settings$method; smooth = settings$smooth
  
  panel_rows <- list(); modal_rows <- list(); modal_items <- list(); fits <- list()

  for (u in units) { # u = units[[1]]
    ur <- unit_ratings(data, u)
    p_fits <- lapply(seq_along(ur$panelist), function(j)
      ess_fit(u$location, ur$levels[, j], u$n_levels, u$sd, method, smooth))
    names(p_fits) <- ur$panelist
    cors <- vapply(p_fits, `[[`, numeric(1), "correlation")

    for (j in seq_along(ur$panelist)) {
      panel_rows[[length(panel_rows) + 1]] <- data.frame(
        unit = u$id, grade = u$grade, subset = u$subset, round = u$round,
        table = ur$table[j], panelist = ur$panelist[j],
        correlation = cors[[j]], p_fits[[j]]$cuts, stringsAsFactors = FALSE)
    }

    groups <- c(stats::setNames(as.list(unique(ur$table)), unique(ur$table)), list(All = NULL))
    for (gname in names(groups)) {
      sel <- if (gname == "All") seq_along(ur$panelist) else which(ur$table == gname)
      modal <- as.integer(row_modes(ur$levels[, sel, drop = FALSE], cors[sel]))
      fit <- ess_fit(u$location, modal, u$n_levels, u$sd, method, smooth)
      modal_rows[[length(modal_rows) + 1]] <- data.frame(
        unit = u$id, grade = u$grade, subset = u$subset, round = u$round,
        table = gname, correlation = fit$correlation, fit$cuts, stringsAsFactors = FALSE)
      if (gname == "All") {
        modal_items[[u$id]] <- data.frame(
          unit = u$id, item_id = u$item_id, page = seq_along(u$item_id),
          location = u$location, aligned = modal, operational = fit$operational,
          stringsAsFactors = FALSE)
        fits[[u$id]] <- fit
      }
    }
  }

  panelist <- do.call(rbind, panel_rows)
  list(
    panelist    = panelist,
    modal       = do.call(rbind, modal_rows),
    median      = aggregate_cuts(panelist, units, "median"),
    average     = aggregate_cuts(panelist, units, "average"),
    modal_items = do.call(rbind, modal_items),
    modal_fits  = fits
  )
}

# Median and average of panelist cuts, per table and for all tables.
#
# Median: the middle page (or the two middle pages, for an even number of
#   panelists); its cut score is that of the first panelist at that page,
#   averaged over the two middle pages; the page is rounded up.
# Average: the mean cut score (rounded to 2 decimals); the page is that of
#   the panelist whose cut score is closest to the mean.
# In both, a panelist's cut score is read from the first panelist with the
# same page, as in the original code (identical unless GAM smoothing is on).
aggregate_cuts <- function(panelist, units, how = c("median", "average")) {
  how <- match.arg(how)
  out <- list()
  for (uid in names(units)) {
    pu <- panelist[panelist$unit == uid, , drop = FALSE]
    groups <- c(stats::setNames(as.list(unique(pu$table)), unique(pu$table)), list(All = NULL))
    for (gname in names(groups)) {
      pg <- if (gname == "All") pu else pu[pu$table == gname, , drop = FALSE]
      for (k in sort(unique(pg$level))) {
        pk <- pg[pg$level == k, , drop = FALSE]
        first_at <- function(page) pk$cut_score[match(page, pk$page)]
        if (how == "median") {
          s <- sort(pk$page); n <- length(s); mid <- ceiling(n / 2)
          med <- unique(if (n %% 2) s[mid] else s[c(mid, mid + 1)])
          page <- ceiling(mean(med)); score <- mean(first_at(med))
        } else {
          sc <- first_at(pk$page)
          score <- round(mean(sc), 2)
          page <- pk$page[which.min(abs(pk$cut_score - mean(sc)))]
        }
        out[[length(out) + 1]] <- data.frame(
          unit = uid, grade = units[[uid]]$grade, subset = units[[uid]]$subset,
          round = units[[uid]]$round, table = gname, level = k,
          page = page, cut_score = score, stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}
