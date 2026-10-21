#' @include ess_plots.R
NULL

#' @rdname report.ESS
#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' HTML report of an ESS result
#'
#' Builds an HTML page (shown in the RStudio viewer or a browser) with the
#' results of [emstans()]. Level labels from the input are used throughout.
#'
#' @param x An object of class `"ESS"`.
#' @param what Section(s) to show: `"all"`, `"individual"` (panelist and
#'   group cut scores), `"detailed"` (per-unit profile plot, crosstabs and
#'   agreement), `"summary"` (impact data), `"review"` (item review) or
#'   `"setup"` (input data overview). Default `"all"`.
#' @param ... Not used.
#' @return A browsable HTML object.
#' @export
report.ESS <- function(x, what = "all", ...) {
  what <- match.arg(what, c("all", "individual", "detailed", "summary", "review", "setup"))
  tb <- ess_tables(x)
  sections <- list(
    individual = function() report_individual(x, tb),
    detailed   = function() report_detailed(x, tb),
    summary    = function() report_summary(x, tb),
    review     = function() report_review(x, tb),
    setup      = function() report_setup(x)
  )
  titles <- c(individual = "Cut scores", detailed = "Detailed", summary = "CS Summary",
              review = "Item Review", setup = "Setup")
  body <- if (what == "all") {
    do.call(bslib::navset_tab, unname(lapply(names(sections), function(s)
      bslib::nav_panel(titles[[s]], sections[[s]]()))))
  } else {
    sections[[what]]()
  }
  htmltools::browsable(bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    htmltools::tags$h3("Embedded Standard Setting"),
    htmltools::tags$p(report_settings_line(x)),
    body
  ))
}

#' @export
report.bootESS <- function(x, ...) {
  htmltools::browsable(bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    htmltools::tags$h3(paste("Bootstrap cut scores:", x$unit)),
    bslib::card(bslib::card_body(plotly::ggplotly(plot_bootstrap(x)))),
    bslib::card(bslib::card_header("Summary"), bslib::card_body(report_dt(x$summary, x$ess$settings$digits))),
    bslib::card(bslib::card_header("Draws"), bslib::card_body(report_dt(x$draws, x$ess$settings$digits)))
  ))
}

# Pieces --------------------------------------------------------------------------

report_settings_line <- function(x) {
  s <- x$settings
  paste0("ESS-", if (s$method == "weight") "Weight" else "Count",
         if (s$smooth) " (GAM-smoothed)" else "", "; location: ", s$location, "; ",
         length(x$units), " unit(s)",
         if (length(x$manual)) paste0("; manual cut scores for ", paste(names(x$manual), collapse = ", ")) else "")
}

report_dt <- function(df, digits = 3, ...) {
  df <- as.data.frame(df)
  num <- vapply(df, is.double, logical(1))
  df[num] <- lapply(df[num], round, digits)
  DT::datatable(df, rownames = FALSE, filter = "top",
                options = list(pageLength = 15, scrollX = TRUE), ...)
}

report_card <- function(title, ...) {
  bslib::card(full_screen = TRUE, bslib::card_header(title), bslib::card_body(...))
}

drop_cols <- function(df, cols) df[, setdiff(names(df), cols), drop = FALSE]

report_individual <- function(x, tb) {
  d <- x$settings$digits
  hide <- c("grade", "subset")
  htmltools::tagList(
    report_card("Panelists", report_dt(drop_cols(tb$panelist, hide), d)),
    report_card("Modal alignment across panelists (by table and all tables)", report_dt(drop_cols(tb$modal, hide), d)),
    report_card("Median of panelist cuts", report_dt(drop_cols(tb$median, hide), d)),
    report_card("Average of panelist cuts", report_dt(drop_cols(tb$average, hide), d))
  )
}

crosstab_html <- function(ct) {
  lv <- levels(droplevels(ct$operational))
  m <- matrix(ct$n, length(lv), length(lv), dimnames = list(lv, lv))
  total <- sum(m)
  cell <- function(n) sprintf("%d<br><small>%.1f%%</small>", as.integer(n), 100 * n / total)
  rows <- lapply(seq_along(lv), function(i) htmltools::tags$tr(
    htmltools::tags$th(lv[i]),
    lapply(seq_along(lv), function(j) htmltools::tags$td(
      htmltools::HTML(cell(m[i, j])),
      style = if (i == j) "background:#e8f3e8;" else NULL)),
    htmltools::tags$td(htmltools::HTML(cell(sum(m[i, ]))))))
  htmltools::tags$table(
    class = "table table-sm table-bordered text-center",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Empirical \u2193 / Aligned \u2192"),
      lapply(lv, htmltools::tags$th), htmltools::tags$th("Total"))),
    htmltools::tags$tbody(rows, htmltools::tags$tr(
      htmltools::tags$th("Total"),
      lapply(seq_along(lv), function(j) htmltools::tags$td(htmltools::HTML(cell(sum(m[, j]))))),
      htmltools::tags$td(htmltools::HTML(cell(total))))))
}

agreement_html <- function(s) {
  pct <- function(v) sprintf("%.0f%%", 100 * v / s$n)
  htmltools::tags$table(
    class = "table table-sm",
    htmltools::tags$tbody(
      htmltools::tags$tr(htmltools::tags$td("Agree"), htmltools::tags$td(s$agree), htmltools::tags$td(pct(s$agree))),
      htmltools::tags$tr(htmltools::tags$td("Disagree"), htmltools::tags$td(s$disagree), htmltools::tags$td(pct(s$disagree))),
      htmltools::tags$tr(htmltools::tags$td("\u2003adjacent levels"), htmltools::tags$td(s$adjacent), htmltools::tags$td(pct(s$adjacent))),
      htmltools::tags$tr(htmltools::tags$td("\u2003non-adjacent"), htmltools::tags$td(s$discrepant), htmltools::tags$td(pct(s$discrepant))),
      htmltools::tags$tr(htmltools::tags$td("Weighted kappa"), htmltools::tags$td(round(s$weighted_kappa, 2)), htmltools::tags$td(""))))
}

report_detailed <- function(x, tb) {
  tabs <- lapply(names(x$units), function(uid) {
    u <- x$units[[uid]]
    ct <- tb$crosstab[tb$crosstab$unit == uid, ]
    ag <- tb$agreement[tb$agreement$unit == uid, ]
    prof <- labelled_profile(x, uid)
    bslib::nav_panel(uid,
      report_card("Inconsistency profile (modal alignment)", plotly::ggplotly(plot_profile(x, uid))),
      bslib::layout_columns(
        col_widths = c(6, 6),
        report_card("Aligned vs empirical level", crosstab_html(ct[ct$type == "exact", ]),
             agreement_html(ag[ag$type == "exact", ])),
        report_card(sprintf("Counting essentially consistent items as agreeing (EC = %s)", u$ec),
             crosstab_html(ct[ct$type == "ec", ]), agreement_html(ag[ag$type == "ec", ]))
      ),
      report_card("Inconsistency of each candidate cut", report_dt(prof, x$settings$digits))
    )
  })
  do.call(bslib::navset_pill, tabs)
}

impact_wide <- function(im) {
  data.frame(
    unit = im$unit, level = im$level,
    pages = paste0(im$page_from, "-", im$page_to),
    cut_score = im$cut_score,
    percent_in = im$pct_in, percent_at_or_above = im$pct_at_above, percent_below = im$pct_below)
}

report_summary <- function(x, tb) {
  subsets <- unique(vapply(x$units, `[[`, "", "subset"))
  plots <- lapply(subsets, function(s) {
    p <- plot_impact(x, s)
    htmltools::tagList(htmltools::tags$h5(s), lapply(p, plotly::ggplotly))
  })
  htmltools::tagList(
    report_card("Impact data (modal cut scores)", report_dt(impact_wide(tb$impact), x$settings$digits)),
    report_card("Across grades", plots)
  )
}

report_review <- function(x, tb) {
  report_card("Item review (modal alignment)", report_dt(tb$review, x$settings$digits))
}

report_setup <- function(x) {
  d <- x$data
  lv <- do.call(rbind, lapply(split(d$levels, d$levels$grade), function(l)
    data.frame(grade = l$grade[1], levels = paste(l$label, collapse = " < "))))
  grades <- merge(d$grades, lv, by = "grade", sort = FALSE)
  r <- d$ratings
  panel <- stats::aggregate(item_id ~ grade + round + table + panelist, data = r, FUN = length)
  names(panel)[names(panel) == "item_id"] <- "items_rated"
  items <- stats::aggregate(item_id ~ grade + domain, data = transform(d$items, domain = ifelse(is.na(domain), "-", domain)),
                            FUN = length)
  names(items)[names(items) == "item_id"] <- "items"
  htmltools::tagList(
    report_card("Grades", report_dt(grades)),
    report_card("Panelists", report_dt(panel)),
    report_card("Items", report_dt(items))
  )
}
