#' @include ess_diagnose.R
NULL

#' Run Embedded Standard Setting Analysis
#'
#' `emstans()` estimates cut scores with Embedded Standard Setting (ESS) from
#' panelists' item-level alignments and item locations. Each grade and round
#' is analysed separately: cut scores are estimated for every panelist and
#' for the modal alignment across panelists (per table and for all tables),
#' and the median and average of the panelist cuts are reported. For the
#' modal result of each grade, the function also reports agreement between
#' aligned and empirical levels, an item review, and impact data.
#'
#' @param file Path to an emss Excel workbook (sheets Setup, Panelists,
#'   Ratings, ItemMetaData, ExamineeMetaData). Either `file` or `data` must be
#'   given.
#' @param data An object from [ess_data()] or [ess_read_excel()], or a list
#'   of data frames in the workbook's sheet order (setup, panelists, ratings,
#'   item data, examinee data).
#' @param grades Grades to analyse: `NULL` for all, a numeric vector of
#'   positions, or a character vector of grade names. Default `NULL`.
#' @param location Name of the item location column, such as `"RP67"` or
#'   `"RP50"`. Default `"RP67"`.
#' @param method `"weight"` for ESS-Weight (minimise the summed distance of
#'   inconsistent items) or `"count"` for ESS-Count (minimise their number).
#'   Default `"weight"`.
#' @param smooth If `TRUE`, cut scores are the minima of a GAM-smoothed
#'   inconsistency profile instead of the exact minima. Default `FALSE`.
#' @param by_domain If `TRUE`, each content domain (the item data's Domain
#'   column) is also analysed separately, in addition to all items of each
#'   grade. Default `FALSE`.
#' @param domains With `by_domain = TRUE`, the domains to analyse; `NULL` for
#'   all of them.
#' @param font_size Font size for plots. Default `14`.
#' @param digits Decimal places in printed tables. Default `3`.
#' @param ... Former argument names, still accepted with a warning. All
#'   arguments after `data` must be named in full. Former names:
#'   `filePath` (now `file`), `grade` (`grades`), `loc` (`location`), `WESS`
#'   (`method`), `gamest` (`smooth`), `domain` and `select_domain`
#'   (`by_domain`, `domains`), `targets` (use the `columns` argument of
#'   [ess_data()]) and `median` (no longer needed: modal, median and average
#'   cuts are always reported).
#'
#' @return An object of class `"ESS"`. Use [extract()] to get its tables,
#'   [report()] for an HTML report, [plot()] for plots, [update()] to set cut
#'   scores manually and [bootstrapping()] for bootstrap cut scores.
#'
#' @examples
#' \dontrun{
#' res <- emstans("study.xlsx", location = "RP67")
#' res
#' extract(res, "cutscore")
#' report(res)
#'
#' # ESS-Count, and each content domain separately
#' emstans("study.xlsx", method = "count", by_domain = TRUE)
#'
#' # CSV input with custom column names and level labels
#' d <- ess_data(ratings = "ratings.csv", items = "items.csv",
#'               levels = c("Basic", "Intermediate", "Advanced"))
#' res <- emstans(data = d, location = "RP50")
#' }
#' @export
emstans <- function(file = NULL,
                    data = NULL,
                    ...,
                    grades = NULL,
                    location = "RP67",
                    method = c("weight", "count"),
                    smooth = FALSE,
                    by_domain = FALSE,
                    domains = NULL,
                    font_size = 14,
                    digits = 3) {
  # Arguments after `...` must be named in full, so former names such as
  # `loc` or `domain` are never partially matched to `location` or `domains`.

  old <- list(...)
  columns <- NULL
  if (length(old)) {
    a <- emstans_old_args(old, method_missing = missing(method))
    if (!is.null(a$file)) file <- a$file
    if (!is.null(a$grades)) grades <- a$grades
    if (!is.null(a$location)) location <- a$location
    if (!is.null(a$method)) method <- a$method
    if (!is.null(a$smooth)) smooth <- a$smooth
    if (!is.null(a$by_domain)) by_domain <- a$by_domain
    if (!is.null(a$domains)) domains <- a$domains
    columns <- a$columns
  }
  method <- match.arg(method)

  if (is.null(file) && is.null(data)) {
    stop("Either `file` or `data` must be provided.", call. = FALSE)
  }

  d <- if (!is.null(data)) {
    if (inherits(data, "ess_data")) data else as_ess_data(data, columns = columns)
  } else {
    ess_read_excel(file, columns = columns)
  }

  settings <- list(location = location, method = method, smooth = isTRUE(smooth),
                   by_domain = isTRUE(by_domain), domains = domains,
                   font_size = font_size, digits = digits, source = file)

  units <- ess_units(d, location, by_domain = by_domain, domains = domains, grades = grades)
  ess_compute(d, units, settings)
}

# Translate former argument names, with one warning listing the changes.
emstans_old_args <- function(old, method_missing = TRUE) {
  known <- c("filePath", "grade", "loc", "WESS", "gamest", "domain", "select_domain",
             "targets", "median")
  unknown <- setdiff(names(old), known)
  if (is.null(names(old)) || any(names(old) == "") || length(unknown)) {
    stop("Unknown argument(s): ", paste(c(unknown, if (any(names(old) == "")) "<unnamed>"), collapse = ", "),
         call. = FALSE)
  }
  a <- list(); notes <- character(0)
  note <- function(from, to) notes <<- c(notes, paste0("`", from, "` -> ", to))
  if (!is.null(old$filePath)) { a$file <- old$filePath; note("filePath", "`file`") }
  if (!is.null(old$grade))    { a$grades <- old$grade; note("grade", "`grades`") }
  if (!is.null(old$loc))      { a$location <- old$loc; note("loc", "`location`") }
  if (!is.null(old$WESS)) {
    a$method <- if (isTRUE(old$WESS)) "weight" else "count"
    note("WESS", paste0('`method = "', a$method, '"`'))
  }
  if (!is.null(old$gamest))   { a$smooth <- old$gamest; note("gamest", "`smooth`") }
  if (!is.null(old$domain)) {
    a$by_domain <- any(.ess_norm(old$domain) == "DOMAIN")
    note("domain", paste0("`by_domain = ", a$by_domain, "`"))
  }
  if (!is.null(old$select_domain)) { a$domains <- old$select_domain; note("select_domain", "`domains`") }
  if (!is.null(old$targets)) {
    if (!identical(old$targets, "ALD")) a$columns <- list(ratings = list(level = old$targets))
    note("targets", "`ess_data(columns = list(ratings = list(level = ...)))`")
  }
  if (!is.null(old$median)) note("median", "not needed (all summaries are reported)")
  warning("emstans(): former argument names used; they still work but will be removed:\n  ",
          paste(notes, collapse = "\n  "), call. = FALSE)
  a
}

# Run every analysis step and assemble the ESS object.
ess_compute <- function(data, units, settings, manual = NULL) {
  est <- ess_estimate(data, units, settings$method, settings$smooth)
  if (!is.null(manual)) est <- apply_manual_cuts(est, units, manual)
  structure(list(
    data        = data,
    settings    = settings,
    units       = units,
    panelist    = est$panelist,
    modal       = est$modal,
    median      = est$median,
    average     = est$average,
    modal_items = est$modal_items,
    profiles    = lapply(est$modal_fits, `[[`, "profile"),
    agreement   = ess_agreement(est, units),
    review      = ess_item_review(est, units),
    impact      = ess_impact(est, units, data),
    manual      = manual
  ), class = "ESS")
}

# Replace the modal (all tables) cuts of some units with given cut scores.
# Each cut's page is the first item at or above the cut score; operational
# levels, count and weight follow from that page.
apply_manual_cuts <- function(est, units, manual) {
  for (uid in names(manual)) {
    u <- units[[uid]]; cs <- manual[[uid]]
    prof <- est$modal_fits[[uid]]$profile
    rows <- vapply(cs, function(x) {
      r <- which(u$location >= x)[1]
      if (is.na(r)) stop("Cut score ", x, " for `", uid, "` is above every item location.",
                         call. = FALSE)
      r
    }, integer(1))
    ks <- seq.int(2, u$n_levels)
    sel <- est$modal$unit == uid & est$modal$table == "All"
    m <- est$modal[sel, ]
    m <- m[order(m$level), ]
    m$page <- rows
    m$cut_score <- cs
    m$count <- vapply(seq_along(ks), function(j) prof[[paste0("L", ks[j], "_C")]][rows[j]], numeric(1))
    m$weight <- vapply(seq_along(ks), function(j) prof[[paste0("L", ks[j], "_W")]][rows[j]], numeric(1))
    est$modal[sel, ] <- m
    mi <- est$modal_items$unit == uid
    est$modal_items$operational[mi] <- operational_levels(u$location, cs)
  }
  est
}

# Level labels ------------------------------------------------------------------

# Label of `level` (integer) in `grade`, as an ordered factor over all labels
# so that tables and plots keep the level order.
level_label <- function(x, grade, level) {
  lv <- x$data$levels
  lab <- lv$label[match(paste(grade, level), paste(lv$grade, lv$level))]
  all_labels <- unique(lv$label[order(lv$level)])
  factor(lab, levels = all_labels)
}

# The unit's grade, for looking up labels from unit ids.
unit_grade <- function(x, unit) {
  vapply(unit, function(u) x$units[[u]]$grade, character(1))
}

# Tables for users ------------------------------------------------------------------
#
# All tables are long (one row per cut or item), with level labels.

cut_table <- function(x, df, who = c("table")) {
  out <- df
  out$level <- level_label(x, out$grade, out$level)
  names(out)[names(out) == "level"] <- "cut_level"
  out[order(match(out$unit, names(x$units)), out$cut_level), , drop = FALSE]
}

ess_tables <- function(x) {
  g <- function(unit) unit_grade(x, unit)
  panelist <- cut_table(x, x$panelist)
  modal    <- cut_table(x, x$modal)
  median   <- cut_table(x, x$median)
  average  <- cut_table(x, x$average)

  review <- x$review
  review$aligned <- level_label(x, g(review$unit), review$aligned)
  review$operational <- level_label(x, g(review$unit), review$operational)

  ag <- x$agreement
  ct <- ag$crosstab
  ct$operational <- level_label(x, g(ct$unit), ct$operational)
  ct$aligned <- level_label(x, g(ct$unit), ct$aligned)

  impact <- x$impact
  impact$level <- level_label(x, g(impact$unit), impact$level)

  list(panelist = panelist, modal = modal, median = median, average = average,
       agreement = ag$summary, crosstab = ct, review = review, impact = impact)
}

# Modal (all tables) cut scores per unit, named by the level each cut opens.
modal_cut_scores <- function(x) {
  m <- x$modal[x$modal$table == "All", ]
  out <- lapply(names(x$units), function(uid) {
    mu <- m[m$unit == uid, ]
    mu <- mu[order(mu$level), ]
    stats::setNames(mu$cut_score, as.character(level_label(x, x$units[[uid]]$grade, mu$level)))
  })
  stats::setNames(out, names(x$units))
}

# Profile of a unit's modal result with labelled columns, e.g. "M count".
labelled_profile <- function(x, unit) {
  u <- x$units[[unit]]
  prof <- x$profiles[[unit]]
  k <- as.integer(sub("^L([0-9]+)_[CW]$", "\\1", names(prof)))
  what <- ifelse(grepl("_C$", names(prof)), "count", "weight")
  names(prof) <- paste(as.character(level_label(x, u$grade, k)), what)
  cbind(data.frame(item_id = u$item_id, page = seq_along(u$item_id), location = u$location,
                   stringsAsFactors = FALSE), prof)
}
