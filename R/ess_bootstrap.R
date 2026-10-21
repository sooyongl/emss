#' @include ess_main.R
NULL

#' @rdname bootstrapping.ESS
#' @export
bootstrapping <- function(x, ...) {
  UseMethod("bootstrapping")
}

#' Bootstrap the modal cut scores of one unit
#'
#' Resamples the unit's items with replacement, keeping each item's location
#' and modal aligned level, and re-estimates the cut scores in every sample.
#'
#' @param x An object of class `"ESS"`.
#' @param unit The unit to bootstrap, as named in `extract(x, "cutscore")`.
#' @param n Number of bootstrap samples. Default `100`.
#' @param seed Optional random seed, for reproducible draws.
#' @param grid Candidate cuts in each sample:
#'   * `"items"` (default): the sampled items' locations, with the exact
#'     ESS minimum, the same estimator as [emstans()] (with `smooth = TRUE`
#'     in the original fit, the GAM-smoothed minimum).
#'   * `"jittered"`: an evenly spaced grid over the location range (step
#'     0.1 when the range is under 10, else 1), each point randomly moved by
#'     up to one step, shared by all cut levels; the cut is the minimum of a
#'     GAM-smoothed inconsistency curve over that grid. This is the earlier
#'     EmStanS bootstrap.
#' @param ... Former argument names, still accepted with a warning:
#'   `gca_name` (now `unit`) and `boot_num` (now `n`).
#' @return An object of class `"bootESS"`: use `print()`, `summary()`,
#'   `extract()`, `report()`.
#' @export
bootstrapping.ESS <- function(x, unit, n = 100, seed = NULL,
                              grid = c("items", "jittered"), ...) {
  old <- list(...)
  if (length(old)) {
    unknown <- setdiff(names(old), c("gca_name", "boot_num", "output"))
    if (length(unknown) || is.null(names(old))) {
      stop("Unknown argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    if (!is.null(old$gca_name)) unit <- old$gca_name
    if (!is.null(old$boot_num)) n <- old$boot_num
    warning("bootstrapping(): `gca_name` and `boot_num` are now `unit` and `n`.", call. = FALSE)
  }
  grid <- match.arg(grid)
  if (missing(unit) || length(unit) != 1 || !unit %in% names(x$units)) {
    stop("`unit` must be one of: ", paste0("`", names(x$units), "`", collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  u <- x$units[[unit]]
  mi <- x$modal_items[x$modal_items$unit == unit, ]
  method <- x$settings$method
  smooth <- isTRUE(x$settings$smooth)

  draws <- list(); failures <- character(0)
  for (b in seq_len(n)) {
    res <- tryCatch(
      boot_sample_cuts(mi$location, mi$aligned, u$n_levels, u$sd, method, grid, smooth),
      error = function(e) conditionMessage(e))
    if (is.character(res)) { failures <- c(failures, res); next }
    draws[[length(draws) + 1]] <- data.frame(rep = b, res)
  }
  if (!length(draws)) {
    stop("All bootstrap samples failed. First error: ", failures[1], call. = FALSE)
  }
  if (length(failures)) {
    warning(length(failures), " of ", n, " bootstrap samples failed and were dropped. First error: ",
            failures[1], call. = FALSE)
  }
  draws <- do.call(rbind, draws)
  draws$cut_level <- level_label(x, u$grade, draws$level)
  draws <- draws[c("rep", "cut_level", "cut_score", "inconsistency")]

  stat <- function(v) c(n = length(v), mean = mean(v), sd = stats::sd(v), median = stats::median(v),
                        min = min(v), max = max(v), q2.5 = unname(stats::quantile(v, .025)),
                        q97.5 = unname(stats::quantile(v, .975)))
  smry <- do.call(rbind, lapply(split(draws$cut_score, draws$cut_level, drop = TRUE), stat))
  smry <- data.frame(cut_level = rownames(smry), smry, row.names = NULL)
  smry$estimate <- unname(modal_cut_scores(x)[[unit]][smry$cut_level])

  structure(list(ess = x, unit = unit, grid = grid, n = n, draws = draws, summary = smry),
            class = "bootESS")
}

# Cut scores in one bootstrap sample: resample items, order them by location
# and estimate the cuts.
boot_sample_cuts <- function(location, level, n_levels, sd, method, grid, smooth = FALSE) {
  i <- sample.int(length(location), replace = TRUE)
  o <- order(location[i])
  loc <- location[i][o]; lev <- level[i][o]
  ks <- seq.int(2, n_levels)
  crit <- if (method == "weight") "_W" else "_C"

  if (grid == "items") {
    fit <- ess_fit(loc, lev, n_levels, sd, method, smooth)
    y <- if (method == "weight") fit$cuts$weight else fit$cuts$count
    return(data.frame(level = ks, cut_score = fit$cuts$cut_score, inconsistency = y))
  }

  step <- if (max(loc) - min(loc) < 10) 0.1 else 1
  cand <- seq(min(loc), max(loc), by = step)
  cand <- cand - stats::runif(length(cand), -step, step)
  prof <- ess_profile(loc, lev, n_levels, sd, candidates = cand)
  knots <- if (length(cand) > 20) 10 else round(length(cand) / 2, 0)
  out <- lapply(ks, function(k) {
    d <- data.frame(y = prof[[paste0("L", k, crit)]], x = cand)
    fit <- mgcv::gam(y ~ s(x, k = knots), data = d, family = "gaussian")
    pred <- predict_gam(fit, length_out = length(loc))
    j <- which.min(pred$fit)
    data.frame(level = k, cut_score = round(pred$x[j], 3), inconsistency = round(pred$fit[j], 3))
  })
  do.call(rbind, out)
}

#' @export
print.bootESS <- function(x, ...) {
  cat("Bootstrap of modal cut scores for", x$unit,
      sprintf("(%d samples, %s)\n", x$n,
              if (x$grid == "items") "item locations, exact minimum" else "jittered grid, smoothed minimum"))
  print(x$summary, digits = 4, row.names = FALSE)
  invisible(x)
}

#' @export
summary.bootESS <- function(object, ...) {
  print(object)
  invisible(object$summary)
}

#' @export
extract.bootESS <- function(x, what = "all", ...) {
  what <- match.arg(what, c("all", "draws", "summary", "plot"))
  switch(what,
         draws = x$draws, summary = x$summary, plot = plot_bootstrap(x),
         all = list(draws = x$draws, summary = x$summary, plot = plot_bootstrap(x)))
}
