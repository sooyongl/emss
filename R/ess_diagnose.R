#' @include ess_estimate.R
NULL

# Diagnostics for the modal (all tables) result of each unit -------------------
#
# All functions take the output of ess_estimate() and return tidy data frames
# with integer levels.

# Signed distance of each item from the cut bounding its aligned level:
#   aligned above operational: location - cut of the aligned level
#   aligned below operational: location - (cut of the next level up - 1)
# The "- 1" treats scale scores as integers (kept from the original code;
# see the Stage 3 notes).
item_distance <- function(aligned, operational, location, cuts) {
  # cuts: cut score for levels 2..K, so cuts[k - 1] is where level k starts
  cut_at <- function(k) ifelse(k >= 2 & k <= length(cuts) + 1, cuts[pmax(k - 1, 1)], NA_real_)
  ifelse(aligned > operational, location - cut_at(aligned),
         ifelse(aligned < operational, location - (cut_at(aligned + 1) - 1), 0))
}

ess_item_review <- function(est, units) {
  out <- lapply(names(units), function(uid) {
    u <- units[[uid]]
    mi <- est$modal_items[est$modal_items$unit == uid, , drop = FALSE]
    cuts <- est$modal$cut_score[est$modal$unit == uid & est$modal$table == "All"]
    dist <- item_distance(mi$aligned, mi$operational, mi$location, cuts)
    std <- abs(round(dist / u$sd, 3))
    status <- ifelse(abs(dist) <= u$ec & abs(dist) > 0, "Essentially Consistent",
              ifelse(abs(dist) > u$ec, "Inconsistent",
              ifelse(abs(dist) == 0, "Consistent", NA_character_)))
    data.frame(unit = uid, item_id = mi$item_id, page = mi$page, aligned = mi$aligned,
               operational = mi$operational, level_diff = mi$aligned - mi$operational,
               location = mi$location, distance = dist, std_distance = std,
               status = status, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Agreement between aligned and operational levels. With EC > 0, items whose
# distance is below EC count as agreeing (they are essentially consistent).
ess_agreement <- function(est, units) {
  ct_rows <- list(); sum_rows <- list()
  for (uid in names(units)) {
    u <- units[[uid]]; K <- u$n_levels
    mi <- est$modal_items[est$modal_items$unit == uid, , drop = FALSE]
    cuts <- est$modal$cut_score[est$modal$unit == uid & est$modal$table == "All"]

    # Distance of each inconsistent item from the cut bounding its aligned level.
    off <- mi$aligned != mi$operational
    dist <- rep(NA_real_, nrow(mi))
    dist[off] <- abs(item_distance(mi$aligned, mi$operational, mi$location, cuts)[off])

    for (type in c("exact", "ec")) {
      aligned <- mi$aligned
      if (type == "ec" && u$ec > 0) {
        ess <- !is.na(dist) & dist < u$ec
        aligned[ess] <- mi$operational[ess]
      }
      ct <- table(factor(mi$operational, levels = seq_len(K)), factor(aligned, levels = seq_len(K)))
      ct <- matrix(as.numeric(ct), K, K, dimnames = list(seq_len(K), seq_len(K)))
      ct_rows[[length(ct_rows) + 1]] <- data.frame(
        unit = uid, type = type, operational = rep(seq_len(K), K), aligned = rep(seq_len(K), each = K),
        n = as.vector(ct))
      gap <- abs(row(ct) - col(ct))
      wk <- suppressWarnings(psych::cohen.kappa(as.data.frame(ct))$weighted.kappa)
      sum_rows[[length(sum_rows) + 1]] <- data.frame(
        unit = uid, type = type, n = sum(ct), agree = sum(ct[gap == 0]),
        disagree = sum(ct[gap > 0]), adjacent = sum(ct[gap == 1]),
        discrepant = sum(ct[gap > 1]), weighted_kappa = wk)
    }
  }
  list(crosstab = do.call(rbind, ct_rows), summary = do.call(rbind, sum_rows))
}

# Impact data: ordered item booklet pages per level and the percentage of
# examinees in, at or above, and below each level (NA without examinee data).
ess_impact <- function(est, units, data) {
  out <- lapply(names(units), function(uid) {
    u <- units[[uid]]; K <- u$n_levels; n <- length(u$item_id)
    m <- est$modal[est$modal$unit == uid & est$modal$table == "All", , drop = FALSE]
    m <- m[order(m$level), ]
    p <- ceiling(m$page); cs <- m$cut_score

    from <- c(1, p)
    to   <- c(if (p[1] == 1) 1 else p[1] - 1, if (K > 2) p[-1] - 1, n)

    ex <- data$examinees[data$examinees$grade == u$grade, , drop = FALSE]
    if (nrow(ex)) {
      sc <- rep(ex$score, ex$freq)
      pct <- function(x) round(x / length(sc), 3) * 100
      bounds <- c(-Inf, cs, Inf)
      pct_in <- vapply(seq_len(K), function(k) pct(sum(sc >= bounds[k] & sc < bounds[k + 1])), numeric(1))
      pct_in[1] <- pct(sum(sc < cs[1])); pct_in[K] <- pct(sum(sc >= cs[K - 1]))
      at_above <- c(100, vapply(cs, function(c) pct(sum(sc >= c)), numeric(1)))
      below <- c(NA, vapply(cs, function(c) pct(sum(sc < c)), numeric(1)))
    } else {
      pct_in <- at_above <- below <- rep(NA_real_, K)
      at_above[1] <- 100
    }
    data.frame(unit = uid, level = seq_len(K), page_from = from, page_to = to,
               cut_score = c(NA, cs), pct_in = pct_in, pct_at_above = at_above,
               pct_below = below)
  })
  do.call(rbind, out)
}
