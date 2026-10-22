#' Item locations at a response probability
#'
#' @description
#' Converts IRT item parameters into ESS item locations. The location of a
#' dichotomous item is the ability at which an examinee answers it correctly
#' with probability `rp`. A polytomous item with maximum score K gives K
#' locations ("pages"), one for each score point k = 1, ..., K: the ability at
#' which an examinee earns score k or higher with probability `rp`.
#'
#' @param pars A data frame of item parameters, one row per item.
#' @param model The IRT model: `"rasch"`, `"2pl"`, `"3pl"` (dichotomous) or
#'   `"pcm"`, `"gpcm"`, `"grm"` (polytomous). Either one value for every item
#'   or one value per row of `pars`, which allows mixed-format tests. If
#'   `NULL`, `pars` must have a column named `model`.
#' @param rp Response probability, between 0 and 1. The default 0.67 is the
#'   usual RP67; 0.5 returns the RP50 location (`b` for Rasch and 2PL items).
#' @param D Scaling constant: 1 for the logistic metric (mirt, flexMIRT,
#'   Winsteps), 1.702 (or 1.7) for the normal metric (PARSCALE, BILOG-MG and
#'   many operational programs).
#' @param item Column of `pars` holding the item ID, by name or position.
#' @param a Column holding the discrimination (slope). Not used for `"rasch"`
#'   and `"pcm"` items, where it is fixed at 1.
#' @param b Threshold columns. Either a prefix (the default `"b"` matches
#'   columns `b`, `b1`, `b2`, ...; a column `b` counts as `b1`) or a character
#'   vector naming the threshold columns in order. Dichotomous items use one
#'   threshold; polytomous items use thresholds 1 to K and `NA` after that, so
#'   items with different maximum scores can share one table. See Details for
#'   what the thresholds mean under each model.
#' @param c Column holding the lower asymptote (pseudo-guessing) of `"3pl"`
#'   items.
#' @param guessing How `rp` is applied to `"3pl"` items:
#'   * `"none"` (default): P(correct) = `rp`. Requires `rp > c`.
#'   * `"rescale"`: P(correct) = c + (1 - c) `rp`, i.e. `rp` of the way from
#'     the guessing floor to 1.
#'   * `"huynh"`: P(correct) = (2 + c) / 3 (Huynh, 1998, 2006). `rp` is not
#'     used for these items; with c = 0 this is 2/3.
#'
#'   Other models are not affected.
#' @param slope,intercept Linear transformation to a reporting scale:
#'   `location = intercept + slope * theta`. `slope` must be positive so that
#'   item order is kept.
#' @param sep Separator between item ID and score point in the `Item_ID` of a
#'   polytomous score point (`"Q12_2"`).
#'
#' @details
#' **Dichotomous items.** With P(theta) = c + (1 - c) / (1 + exp(-D a (theta - b))),
#' the location is
#' `theta = b + log((p - c) / (1 - p)) / (D a)`, where p is the target
#' probability (`rp`, or as set by `guessing`). Rasch items have a = 1 and
#' c = 0; 2PL items have c = 0. Under Rasch, changing `rp` moves every item by
#' the same amount, so item order does not change. Under 2PL and 3PL the shift
#' is `1 / (D a)` times larger for less discriminating items, so `rp` can
#' change item order.
#'
#' **Polytomous items.** The location of score point k solves
#' P(X >= k | theta) = `rp`. These locations are always increasing in k within
#' an item.
#' * `"grm"`: the thresholds are the category boundaries `b_k` of
#'   P(X >= k) = 1 / (1 + exp(-D a (theta - b_k))), so
#'   `theta_k = b_k + log(rp / (1 - rp)) / (D a)`. Boundaries must increase.
#' * `"pcm"`, `"gpcm"`: the thresholds are step difficulties `delta_v` of
#'   P(X = k) proportional to exp(sum over v <= k of D a (theta - delta_v)).
#'   They may be disordered. The location is found numerically.
#'
#' Parameterizations differ between programs; convert before calling. For
#' mirt, `"graded"` items give `b_k = -d_k / a`, and `"gpcm"` items (with the
#' default scoring 0, 1, ..., K) give `delta_k = -(d_k - d_(k-1)) / a`. Programs
#' that report an item location b with step deviations d_v use
#' `delta_v = b - d_v` (PARSCALE) or `b + d_v`, depending on sign convention.
#'
#' @return A data frame with one row per location: `Item_ID` (the item ID for
#'   dichotomous items, item ID, `sep` and score point for polytomous items),
#'   `item`, `score`, `max_score`, `model`, `p` (the probability the location
#'   was solved for), `theta` and `location` (`theta` on the reporting scale).
#'   Rows keep the order of `pars`; they are not sorted by location.
#'
#' @references
#' Huynh, H. (1998). On score locations of binary and partial credit items and
#' their applications to item mapping and criterion-referenced interpretation.
#' *Journal of Educational and Behavioral Statistics, 23*(1), 35-56.
#'
#' Huynh, H. (2006). A clarification on the response probability criterion
#' RP67 for standard settings based on bookmark and item mapping.
#' *Educational Measurement: Issues and Practice, 25*(2), 19-20.
#'
#' @examples
#' # Mixed-format test: 3PL multiple-choice items and a GPCM constructed-
#' # response item scored 0-3
#' pars <- data.frame(
#'   Item_ID = c("MC1", "MC2", "CR1"),
#'   model   = c("3pl", "3pl", "gpcm"),
#'   a  = c(1.2, 0.7, 0.9),
#'   b1 = c(-0.5, 0.3, -1.0),
#'   b2 = c(NA, NA, 0.2),
#'   b3 = c(NA, NA, 1.1),
#'   c  = c(0.20, 0.25, NA)
#' )
#' ess_locations(pars, D = 1.702)
#'
#' # RP50 and Huynh's guessing-adjusted RP for the 3PL items
#' ess_locations(pars, rp = 0.5, D = 1.702)
#' ess_locations(pars, D = 1.702, guessing = "huynh")
#'
#' # Locations on a reporting scale (scale score = 500 + 50 * theta)
#' ess_locations(pars, D = 1.702, slope = 50, intercept = 500)
#' @export
ess_locations <- function(pars, model = NULL, rp = 0.67, D = 1,
                          item = 1, a = "a", b = "b", c = "c",
                          guessing = c("none", "rescale", "huynh"),
                          slope = 1, intercept = 0, sep = "_") {

  guessing <- match.arg(guessing)
  if (!is.data.frame(pars)) {
    stop("`pars` must be a data frame of item parameters.", call. = FALSE)
  }
  pars <- as.data.frame(pars, stringsAsFactors = FALSE)
  n <- nrow(pars)
  if (n == 0) stop("`pars` has no rows.", call. = FALSE)

  loc_check_number(rp, "rp", lower = 0, upper = 1)
  loc_check_number(D, "D", lower = 0)
  loc_check_number(slope, "slope", lower = 0)
  loc_check_number(intercept, "intercept")

  # model ---------------------------------------------------------------
  dich_models <- c("rasch", "2pl", "3pl")
  poly_models <- c("pcm", "gpcm", "grm")
  if (is.null(model)) {
    if (!"model" %in% names(pars)) {
      stop("Give `model`, or include a column named `model` in `pars`.",
           call. = FALSE)
    }
    model <- pars[["model"]]
  }
  model <- tolower(trimws(as.character(model)))
  if (length(model) == 1) model <- rep(model, n)
  if (length(model) != n) {
    stop("`model` must have length 1 or one value per row of `pars` (",
         n, ").", call. = FALSE)
  }
  bad <- is.na(model) | !model %in% c(dich_models, poly_models)
  if (any(bad)) {
    stop("Unknown model: ", paste(unique(model[bad]), collapse = ", "),
         ". Use one of ", paste(c(dich_models, poly_models), collapse = ", "),
         ".", call. = FALSE)
  }
  is_poly <- model %in% poly_models

  # item IDs ------------------------------------------------------------
  ids <- trimws(as.character(pars[[loc_pick_column(pars, item, "item")]]))
  if (anyNA(ids) || any(ids == "")) {
    stop("Item IDs must not be missing.", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop("Duplicated item IDs: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "), ".",
         call. = FALSE)
  }

  # thresholds ----------------------------------------------------------
  thr <- loc_threshold_matrix(pars, b)
  n_thr <- rowSums(!is.na(thr))
  # thresholds must be contiguous from the first column: b1..bK, then NA
  gaps <- vapply(seq_len(n), function(i) {
    x <- !is.na(thr[i, ])
    n_thr[i] > 0 && !all(x[seq_len(n_thr[i])])
  }, logical(1))
  if (any(n_thr == 0)) {
    stop("No thresholds for item(s): ", loc_id_list(ids[n_thr == 0]), ".",
         call. = FALSE)
  }
  if (any(gaps)) {
    stop("Thresholds must fill columns from the first one, with NA only ",
         "after the last threshold. Check item(s): ", loc_id_list(ids[gaps]), ".",
         call. = FALSE)
  }
  multi <- !is_poly & n_thr > 1
  if (any(multi)) {
    stop("Dichotomous item(s) with more than one threshold: ",
         loc_id_list(ids[multi]), ". Use a polytomous model or remove the extra ",
         "thresholds.", call. = FALSE)
  }

  # slopes --------------------------------------------------------------
  needs_a <- model %in% c("2pl", "3pl", "gpcm", "grm")
  a_val <- rep(1, n)
  if (any(needs_a)) {
    a_col <- loc_pick_column(pars, a, "a",
                         why = "it is needed for 2PL, 3PL, GPCM and GRM items")
    a_val[needs_a] <- loc_numeric_column(pars, a_col)[needs_a]
    bad <- needs_a & (is.na(a_val) | !is.finite(a_val) | a_val <= 0)
    if (any(bad)) {
      stop("Discrimination `", a_col, "` must be positive for item(s): ",
           loc_id_list(ids[bad]), ".", call. = FALSE)
    }
  }

  # lower asymptotes ----------------------------------------------------
  is_3pl <- model == "3pl"
  c_val <- rep(0, n)
  if (any(is_3pl)) {
    c_col <- loc_pick_column(pars, c, "c", why = "it is needed for 3PL items")
    c_val[is_3pl] <- loc_numeric_column(pars, c_col)[is_3pl]
    bad <- is_3pl & (is.na(c_val) | c_val < 0 | c_val >= 1)
    if (any(bad)) {
      stop("Lower asymptote `", c_col, "` must be in [0, 1) for item(s): ",
           loc_id_list(ids[bad]), ".", call. = FALSE)
    }
  }

  # GRM boundaries must increase
  grm_bad <- vapply(seq_len(n), function(i) {
    model[i] == "grm" && n_thr[i] > 1 && any(diff(thr[i, seq_len(n_thr[i])]) <= 0)
  }, logical(1))
  if (any(grm_bad)) {
    stop("GRM boundaries must increase (b1 < b2 < ...) for item(s): ",
         loc_id_list(ids[grm_bad]), ".", call. = FALSE)
  }

  # target probabilities for dichotomous items ---------------------------
  p_dich <- rep(rp, n)
  if (any(is_3pl)) {
    p_dich[is_3pl] <- switch(guessing,
      none    = rp,
      rescale = c_val[is_3pl] + (1 - c_val[is_3pl]) * rp,
      huynh   = (2 + c_val[is_3pl]) / 3
    )
    bad <- is_3pl & p_dich <= c_val
    if (any(bad)) {
      stop("`rp` (", rp, ") must be above the lower asymptote c for 3PL ",
           "item(s): ", loc_id_list(ids[bad]), '. Use `guessing = "rescale"` ',
           'or `"huynh"`, or a larger `rp`.', call. = FALSE)
    }
  }

  # locations -----------------------------------------------------------
  out <- lapply(seq_len(n), function(i) {
    K <- n_thr[i]
    th <- thr[i, seq_len(K)]
    if (!is_poly[i]) {
      p <- p_dich[i]
      theta <- th + stats::qlogis((p - c_val[i]) / (1 - c_val[i])) / (D * a_val[i])
      return(data.frame(Item_ID = ids[i], item = ids[i], score = 1L,
                        max_score = 1L, model = model[i], p = p,
                        theta = theta, stringsAsFactors = FALSE))
    }
    theta <- if (model[i] == "grm") {
      th + stats::qlogis(rp) / (D * a_val[i])
    } else {
      vapply(seq_len(K), function(k) loc_pcm_location(th, k, a_val[i], D, rp),
             numeric(1))
    }
    data.frame(Item_ID = paste0(ids[i], sep, seq_len(K)), item = ids[i],
               score = seq_len(K), max_score = K, model = model[i], p = rp,
               theta = theta, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out$location <- intercept + slope * out$theta
  rownames(out) <- NULL

  if (anyDuplicated(out$Item_ID)) {
    dup <- unique(out$Item_ID[duplicated(out$Item_ID)])
    stop("Score-point IDs clash with other item IDs: ", loc_id_list(dup),
         ". Choose another `sep`.", call. = FALSE)
  }
  out
}

# Ability where P(X >= k) = rp under the (generalized) partial credit model.
# deltas: step difficulties delta_1..delta_K. P(X >= k) increases in theta.
loc_pcm_location <- function(deltas, k, a, D, rp) {
  f <- function(theta) loc_log_pcm_cum(theta, deltas, k, a, D) - log(rp)
  # P(X >= k) is at least P(X = K) and at most 1 - P(X = 0), so the root lies
  # between the smallest and largest step shifted by the logit of rp
  span <- (abs(stats::qlogis(rp)) + log(length(deltas)) + 1) / (D * a)
  lo <- min(deltas) - span
  hi <- max(deltas) + span
  stats::uniroot(f, c(lo, hi), extendInt = "upX", tol = 1e-12)$root
}

loc_log_pcm_cum <- function(theta, deltas, k, a, D) {
  z <- c(0, cumsum(D * a * (theta - deltas)))   # log numerators, scores 0..K
  loc_log_sum_exp(z[(k + 1):length(z)]) - loc_log_sum_exp(z)
}

loc_log_sum_exp <- function(z) {
  m <- max(z)
  m + log(sum(exp(z - m)))
}

# Threshold columns as a numeric matrix, one column per threshold.
loc_threshold_matrix <- function(pars, b) {
  if (is.numeric(b)) b <- names(pars)[b]
  if (length(b) > 1) {
    missing <- setdiff(b, names(pars))
    if (length(missing)) {
      stop("Threshold column(s) not found in `pars`: ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    cols <- b
  } else {
    nms <- names(pars)
    rest <- substring(nms, nchar(b) + 1)
    hit <- startsWith(nms, b) & grepl("^[0-9]*$", rest)
    cols <- nms[hit]
    if (!length(cols)) {
      stop("No threshold columns found: expected `", b, "` or `", b, "1`, `",
           b, "2`, ... in `pars`.", call. = FALSE)
    }
    num <- rest[hit]
    num[num == ""] <- "1"
    num <- as.integer(num)
    if (anyDuplicated(num)) {
      stop("Both `", b, "` and `", b, "1` are in `pars`; keep one of them.",
           call. = FALSE)
    }
    cols <- cols[order(num)]
    if (!identical(sort(num), seq_along(num))) {
      stop("Threshold columns must be numbered 1, 2, ... without gaps; found ",
           paste(cols, collapse = ", "), ".", call. = FALSE)
    }
  }
  m <- vapply(cols, function(col) loc_numeric_column(pars, col),
              numeric(nrow(pars)))
  matrix(m, nrow = nrow(pars), dimnames = list(NULL, cols))
}

loc_pick_column <- function(pars, spec, arg, why = NULL) {
  if (length(spec) != 1 || is.na(spec)) {
    stop("`", arg, "` must be one column name or position.", call. = FALSE)
  }
  if (is.numeric(spec)) {
    if (spec < 1 || spec > ncol(pars)) {
      stop("`", arg, "` = ", spec, " is not a column position of `pars`.",
           call. = FALSE)
    }
    return(names(pars)[spec])
  }
  if (!spec %in% names(pars)) {
    stop("Column `", spec, "` (argument `", arg, "`) not found in `pars`",
         if (!is.null(why)) paste0("; ", why), ".", call. = FALSE)
  }
  spec
}

loc_numeric_column <- function(pars, col) {
  x <- pars[[col]]
  if (is.numeric(x)) return(as.numeric(x))
  y <- suppressWarnings(as.numeric(as.character(x)))
  bad <- !is.na(x) & trimws(as.character(x)) != "" & is.na(y)
  if (any(bad)) {
    stop("Column `", col, "` has non-numeric values: ",
         paste(utils::head(unique(as.character(x[bad])), 5), collapse = ", "),
         ".", call. = FALSE)
  }
  y
}

loc_check_number <- function(x, arg, lower = -Inf, upper = Inf) {
  ok <- is.numeric(x) && length(x) == 1 && is.finite(x) &&
    x > lower && x < upper
  if (!ok) {
    range <- if (is.finite(lower) && is.finite(upper)) {
      paste0(" between ", lower, " and ", upper)
    } else if (is.finite(lower)) {
      paste0(" greater than ", lower)
    } else ""
    stop("`", arg, "` must be a single number", range, ".", call. = FALSE)
  }
}

loc_id_list <- function(x, max = 5) {
  more <- length(x) - max
  paste0(paste(utils::head(x, max), collapse = ", "),
         if (more > 0) paste0(" and ", more, " more"))
}
