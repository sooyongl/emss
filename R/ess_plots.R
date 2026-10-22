#' @include ess_methods.R
NULL

#' Plot an ESS result
#'
#' @param x An object of class `"ESS"`.
#' @param type `"profile"` for the inconsistency profile of a unit's modal
#'   result, or `"impact"` for cut scores and impact across grades.
#' @param unit For `type = "profile"`, the unit to plot (default: the first).
#'   For `type = "impact"`, the item subset (default `"All"`).
#' @param ... Not used.
#' @return A ggplot object (for `"impact"`, a list of ggplot objects).
#' @export
plot.ESS <- function(x, type = c("profile", "impact"), unit = NULL, ...) {
  type <- match.arg(type)
  if (type == "profile") plot_profile(x, if (is.null(unit)) names(x$units)[1] else unit)
  else plot_impact(x, if (is.null(unit)) "All" else unit)
}

# Inconsistency of every candidate cut (each item location) for each cut
# level, with the selected cut marked. `interactive = TRUE` (used by the
# HTML report) labels the cuts with plain text, which plotly can draw.
plot_profile <- function(x, unit, interactive = FALSE) {
  if (!unit %in% names(x$units)) stop("Unknown unit `", unit, "`.", call. = FALSE)
  u <- x$units[[unit]]; prof <- x$profiles[[unit]]
  suffix <- if (x$settings$method == "weight") "_W" else "_C"
  ks <- seq.int(2, u$n_levels)
  d <- do.call(rbind, lapply(ks, function(k) data.frame(
    location = u$location, page = seq_along(u$location), level = k,
    inconsistency = prof[[paste0("L", k, suffix)]])))
  d$cut_level <- level_label(x, u$grade, d$level)
  m <- x$modal[x$modal$unit == unit & x$modal$table == "All", ]
  m$cut_level <- level_label(x, u$grade, m$level)
  m$inconsistency <- if (x$settings$method == "weight") m$weight else m$count

  fs <- x$settings$font_size
  m$label <- paste0(m$cut_level, "\n", signif(m$cut_score, 4))
  # Labels sit above the curves, one per cut line, so they neither cover the
  # curves nor run off the panel.
  cut_labels <- if (interactive) {
      top <- max(d$inconsistency, na.rm = TRUE)
      geom_text(data = m, aes(x = .data$cut_score, y = top * 1.15, label = .data$label),
              size = fs * 5 / 14 * .75, show.legend = FALSE)
  } else {
      geom_label(data = m, aes(x = .data$cut_score, y = Inf, label = .data$label, hjust = .data$hjust), vjust = 1.1, size = fs * 5 / 14 * .75, label.size = 0, fill = "white", lineheight = .9, show.legend = FALSE)
  }
  ggplot(d, aes(x = .data$location, y = .data$inconsistency, colour = .data$cut_level)) +
    geom_line(alpha = .5) +
    geom_point(size = 1, alpha = .6) +
    geom_vline(data = m, aes(xintercept = .data$cut_score, colour = .data$cut_level),
               linetype = "dashed") +
    geom_point(data = m, aes(x = .data$cut_score, y = .data$inconsistency), size = 3, shape = 21,
               fill = "white", stroke = 1.5) +
    # geom_text(data = m, aes(x = .data$cut_score, y = .data$inconsistency,
    #                         label = paste0(.data$cut_level, ": ", round(.data$cut_score, 3),
    #                                        " (p. ", .data$page, ")")),
    #           vjust = -1, fontface = "bold", size = fs * 5 / 14 * .8, show.legend = FALSE) +
    cut_labels +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.22))) +
    labs(x = x$settings$location,
         y = if (x$settings$method == "weight") "Weighted inconsistency" else "Inconsistent items",
         colour = "Cut for", title = unit) +
    theme_bw(base_size = fs)
}

# Cut scores, percentage in level, and percentage at or above level, across
# grades for one item subset (and the first round with data).
plot_impact <- function(x, subset = "All") {
  im <- extract(x, "summary")
  units <- names(x$units)[vapply(x$units, function(u) u$subset == subset, logical(1))]
  if (!length(units)) stop("No units for subset `", subset, "`.", call. = FALSE)
  im <- im[im$unit %in% units, ]
  im$grade <- factor(unit_grade(x, im$unit), levels = unique(unit_grade(x, units)))
  fs <- x$settings$font_size
  base <- theme_bw(base_size = fs)

  cuts <- im[!is.na(im$cut_score), ]
  p_cut <- ggplot(cuts, aes(x = .data$grade, y = .data$cut_score, colour = .data$level,
                            group = .data$level)) +
    geom_line(linewidth = 1) + geom_point(size = 3, shape = 21, fill = "white", stroke = 1.5) +
    labs(x = "Grade", y = x$settings$location, colour = "Cut for", title = "Cut scores") + base
  out <- list(cut_scores = p_cut)

  if (any(!is.na(im$pct_in))) {
    out$percent_in <- ggplot(im[!is.na(im$pct_in), ],
                             aes(x = .data$grade, y = .data$pct_in, fill = .data$level)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      labs(x = "Grade", y = "Percent of examinees", fill = "Level", title = "Percent in level") + base
    out$percent_at_above <- ggplot(im[!is.na(im$pct_at_above) & !is.na(im$cut_score), ],
                                   aes(x = .data$grade, y = .data$pct_at_above, colour = .data$level,
                                       group = .data$level)) +
      geom_line(linewidth = 1) + geom_point(size = 3) +
      labs(x = "Grade", y = "Percent of examinees", colour = "Level",
           title = "Percent at or above level") + base
  }
  out
}

plot_bootstrap <- function(b) {
  est <- data.frame(cut_level = factor(b$summary$cut_level, levels = levels(b$draws$cut_level)),
                    estimate = b$summary$estimate, mean = b$summary$mean)
  ggplot(b$draws, aes(x = .data$cut_score, fill = .data$cut_level)) +
    geom_histogram(bins = 30, alpha = .7, position = "identity") +
    geom_vline(data = est, aes(xintercept = .data$estimate, colour = .data$cut_level), linewidth = 1) +
    geom_vline(data = est, aes(xintercept = .data$mean, colour = .data$cut_level), linetype = "dashed") +
    labs(x = b$ess$settings$location, y = "Bootstrap samples", fill = "Cut for", colour = "Cut for",
         title = paste("Bootstrap cut scores:", b$unit),
         subtitle = "Solid: estimate; dashed: bootstrap mean") +
    theme_bw(base_size = b$ess$settings$font_size)
}
