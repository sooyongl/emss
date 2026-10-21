#' @include ess_main.R
NULL

#' @export
print.ESS <- function(x, ...) {
  s <- x$settings
  cat("Embedded Standard Setting\n")
  cat(sprintf("  method: ESS-%s%s | location: %s | %d unit(s)%s\n",
              if (s$method == "weight") "Weight" else "Count",
              if (s$smooth) " (GAM-smoothed)" else "", s$location, length(x$units),
              if (length(x$manual)) paste0(" | manual cuts: ", paste(names(x$manual), collapse = ", ")) else ""))
  cat("\nModal cut scores (all tables):\n")
  cuts <- modal_cut_scores(x)
  for (uid in names(cuts)) {
    v <- formatC(cuts[[uid]], format = "f", digits = s$digits, width = s$digits + 5)
    cat(sprintf("  %-14s %s\n", uid, paste0(names(cuts[[uid]]), " ", v, collapse = "   ")))
  }
  cat("\nUse extract() for tables, report() for the full report.\n")
  invisible(x)
}

#' Summarise an ESS result
#'
#' One row per unit and cut level with the modal, median and average cut
#' scores (all tables), and the agreement between aligned and empirical
#' levels for the modal result.
#'
#' @param object An object of class `"ESS"`.
#' @param ... Not used.
#' @return A data frame, invisibly after printing.
#' @export
summary.ESS <- function(object, ...) {
  x <- object; tb <- ess_tables(x)
  pick <- function(df, nm) {
    df <- df[df$table == "All", c("unit", "cut_level", "cut_score")]
    names(df)[3] <- nm; df
  }
  out <- Reduce(function(a, b) merge(a, b, by = c("unit", "cut_level"), sort = FALSE),
                list(pick(tb$modal, "modal"), pick(tb$median, "median"), pick(tb$average, "average")))
  ag <- tb$agreement[tb$agreement$type == "exact", ]
  out$agreement <- round(ag$agree / ag$n, 3)[match(out$unit, ag$unit)]
  out$weighted_kappa <- round(ag$weighted_kappa, 3)[match(out$unit, ag$unit)]
  out <- out[order(match(out$unit, names(x$units)), out$cut_level), ]
  rownames(out) <- NULL
  print(out, digits = x$settings$digits + 2)
  invisible(out)
}

#' @rdname extract.ESS
#' @export
extract <- function(x, ...) {
  UseMethod("extract")
}

#' Extract tables from an ESS result
#'
#' @param x An object of class `"ESS"` returned by [emstans()].
#' @param what Which results: `"cutscore"` (modal cut scores per unit, named
#'   by the level each cut opens), `"individual"` (panelist, modal, median and
#'   average cuts), `"detailed"` (agreement and crosstabs), `"summary"`
#'   (impact data), `"review"` (item review), `"profile"` (inconsistency
#'   profiles of the modal results) or `"all"`. Default `"all"`.
#' @param ... Not used.
#' @return A data frame or a named list of data frames. Level columns hold
#'   the level labels as ordered factors.
#' @export
extract.ESS <- function(x, what = "all", ...) {
  what <- match.arg(what, c("all", "cutscore", "individual", "detailed", "summary",
                            "review", "profile"))
  tb <- ess_tables(x)
  individual <- tb[c("panelist", "modal", "median", "average")]
  detailed <- list(agreement = tb$agreement, crosstab = tb$crosstab)
  switch(what,
         cutscore   = modal_cut_scores(x),
         individual = individual,
         detailed   = detailed,
         summary    = tb$impact,
         review     = tb$review,
         profile    = stats::setNames(lapply(names(x$units), labelled_profile, x = x), names(x$units)),
         all        = list(cutscore = modal_cut_scores(x), individual = individual,
                           detailed = detailed, summary = tb$impact, review = tb$review))
}

#' Set cut scores manually
#'
#' Replaces the modal (all tables) cut scores of one or more units and
#' recomputes agreement, item review and impact for them. Each cut's page is
#' the first item at or above the cut score.
#'
#' @param object An object of class `"ESS"`.
#' @param ... Named numeric vectors of cut scores, one value per cut, named
#'   by unit as in `extract(object, "cutscore")`, for example
#'   `` update(res, `R1-All` = c(-0.9, 0.4)) ``.
#' @return An updated object of class `"ESS"`.
#' @export
update.ESS <- function(object, ...) {
  updates <- list(...)
  if (!length(updates)) stop("No manual cut scores were provided.", call. = FALSE)
  if (is.null(names(updates)) || any(names(updates) == "")) {
    stop("All manual cut scores must be named, for example `R1-All` = c(-0.9, 0.4).",
         call. = FALSE)
  }
  unknown <- setdiff(names(updates), names(object$units))
  if (length(unknown)) {
    stop("Unknown unit name(s): ", paste0("`", unknown, "`", collapse = ", "),
         ". Available: ", paste0("`", names(object$units), "`", collapse = ", "), ".",
         call. = FALSE)
  }
  for (nm in names(updates)) {
    n_cut <- object$units[[nm]]$n_levels - 1
    if (!is.numeric(updates[[nm]]) || length(updates[[nm]]) != n_cut) {
      stop("`", nm, "` needs ", n_cut, " numeric cut score(s), got ",
           length(updates[[nm]]), ".", call. = FALSE)
    }
  }
  manual <- object$manual
  manual[names(updates)] <- lapply(updates, unname)
  ess_compute(object$data, object$units, object$settings, manual = manual)
}
