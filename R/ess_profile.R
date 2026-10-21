#' @include ess_data.R
NULL

# ESS estimate for Count and Weight
#
# For cut level `cut_level` and candidate cut c:
#   items aligned at or above cut_level are inconsistent when location <  c,
#   items aligned below cut_level      are inconsistent when location >= c,
# and an inconsistent item's Distance is |location - c|. Count is the number
# of inconsistent items; Weight is the sum of their Distances.
#
# Each group's locations are sorted once with cumulative sums, so every
# candidate needs one binary search (findInterval):
#   count_up(c)  = #{up < c} = k_up,          weight_up(c) = c * k_up - sum(up < c)
#   count_lo(c)  = #{lo >= c} = n_lo - k_lo,  weight_lo(c) = sum(lo >= c) - c * count_lo
# This is O((n + m) log n) for n items and m candidates. The previous version
# expanded an n x m data.table and gave identical results (checked to 1e-12).
essCW <- function(new_data, d_alpha = 1, cut_level = 2, SD = 1, empirical = TRUE, EC = 0,
                  candidates = NULL) {

  if (!identical(d_alpha, 1)) {
    stop("essCW() supports d_alpha = 1 only.", call. = FALSE)
  }

  if (!is.null(candidates)) {

    cut_candi <- candidates

  } else if(empirical) {

    cut_candi <- new_data$location

  } else {

    if(max(new_data$location) - min(new_data$location) < 10) {
      seq_by = 0.1

      cut_candi <- seq(min(new_data$location), max(new_data$location),
                       by = seq_by)

      cut_candi <- cut_candi - runif(length(cut_candi), -0.1, 0.1)

    } else {

      seq_by = 1

      cut_candi <- seq(min(new_data$location), max(new_data$location),
                       by = seq_by)

      cut_candi <- cut_candi - runif(length(cut_candi), -1, 1)

    }
  }

  up <- sort(new_data$location[new_data$ALD >= cut_level])
  lo <- sort(new_data$location[new_data$ALD <  cut_level])
  n_lo <- length(lo)

  k_up <- findInterval(cut_candi, up, left.open = TRUE)  # number of up < c
  k_lo <- findInterval(cut_candi, lo, left.open = TRUE)  # number of lo < c

  cs_up <- c(0, cumsum(up))
  cs_lo <- c(0, cumsum(lo))

  count_lo  <- n_lo - k_lo
  weight_up <- cut_candi * k_up - cs_up[k_up + 1]
  weight_lo <- (cs_lo[n_lo + 1] - cs_lo[k_lo + 1]) - cut_candi * count_lo

  incon_default <- data.table(
    cut_score = cut_candi,
    counts    = k_up + count_lo,
    weights   = weight_up + weight_lo
  )

  return(list(incon_default = incon_default))
}

# Strip floating-point noise from an inconsistency profile before comparing
# values. The weight profile is piecewise linear, so neighbouring candidates
# are often exactly tied in theory but differ by ~1e-13 in floating point;
# rounding to 9 decimals restores those ties without merging any real
# difference (unlike rounding to 2 decimals, which merged real differences).
exact_profile <- function(profile, decimals = 9) {
  profile[] <- lapply(profile, round, decimals)
  profile
}

#' Modal ALD for every item at once
#'
#' Vectorized equivalent of `apply(ald, 1, get_mode, cors)`. For each row
#' (item) the most frequent ALD across columns (panelists) wins; when several
#' ALDs tie for most frequent, the one chosen by the panelist with the highest
#' correlation wins (stable order, so earlier panelists win equal
#' correlations), exactly as in `get_mode()`.
#'
#' @param ald items x panelists table of ALD labels.
#' @param cors panelist correlations, one per column of `ald`.
#' @return character vector of modal ALDs, one per row.
#' @keywords internal
row_modes <- function(ald, cors) {
  ald <- as.matrix(ald)
  n <- nrow(ald)
  if (n == 0) return(character(0))

  values <- sort(unique(as.vector(ald)))
  counts <- vapply(values, function(v) rowSums(ald == v, na.rm = TRUE), numeric(n))
  counts <- matrix(counts, nrow = n)
  max_count <- do.call(pmax, as.data.frame(counts))

  # Walk panelists from highest to lowest correlation; the first whose ALD
  # has the maximum count supplies the mode. With a unique maximum this is
  # simply that value; with ties it is the highest-correlation choice.
  result <- rep(NA_character_, n)
  for (p in order(cors, decreasing = TRUE, method = "radix")) {
    todo <- is.na(result)
    if (!any(todo)) break
    v <- ald[, p]
    hit <- todo & !is.na(v) & counts[cbind(seq_len(n), match(v, values))] == max_count
    hit[is.na(hit)] <- FALSE
    result[hit] <- v[hit]
  }
  result
}

# Predictions of a fitted GAM over a grid of its numeric terms.
predict_gam <- function(model, exclude_terms = NULL, length_out = 50, values = NULL, type = "link") {
  n_terms <- length(model[["var.summary"]])

  term_list <- list()

  for (term in 1:n_terms) {
    term_summary <- model[["var.summary"]][[term]]
    term_name <- names(model[["var.summary"]])[term]

    if (term_name %in% names(values)) {
      new_term <- values[[which(names(values) == term_name)]]

      if (is.null(new_term)) {
        new_term <- model[["var.summary"]][[term]][[1]]
      }

    } else {
      if (is.numeric(term_summary)) {

        min_value <- min(term_summary)
        max_value <- max(term_summary)

        new_term <- seq(min_value, max_value, length.out = length_out)

      } else if (is.factor(term_summary)) {

        new_term <- levels(term_summary)

      } else {
        stop("The terms are not numeric or factor.\n")
      }
    }

    term_list <- append(term_list, list(new_term))

    names(term_list)[term] <- term_name
  }

  new_data <- expand.grid(term_list)

  if (type == "link") {
    predicted <- as.data.frame(mgcv::predict.gam(model, new_data, exclude = exclude_terms, se.fit = TRUE))

    predictions <- cbind(new_data, predicted)

    predictions <- tibble::as_tibble(predictions)
  } else if (type == "terms") {
    if (!is.null(exclude_terms)) {
      message("Type is 'terms': exclude_terms is ignored.")
    }
    predicted <- mgcv::predict.gam(model, new_data, type = "terms", se.fit = TRUE)

    predicted_fit <- tibble::as_tibble(predicted$fit)
    predicted_se <- tibble::as_tibble(predicted$se.fit)
    colnames(predicted_se) <- paste0(colnames(predicted_se), "_se")

    predictions <- dplyr::bind_cols(predicted_fit, predicted_se)
  } else {
    stop("The argument `type` has to be 'link' or 'terms'.")
  }

  return(predictions)
}

#' Calculate the outcome of GAM function given x
#' @param data a gam class
#' @param par a numeric value for x-axis value
