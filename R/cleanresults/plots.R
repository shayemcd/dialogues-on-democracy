# Figures ------------------------------------------------------------------------
#
# Every cr_plot_*() returns an ordinary ggplot, so `+ labs()`, `+ facet_wrap()`,
# `+ theme()` and any other layer work as usual. Means and brackets come from
# the same models (and the same covariance matrices and corrections) as the
# tables and text, or from the raw data when a data frame is passed. See
# theme_cr() and cr_palettes for styling.

#' Bar, dot, box, and raincloud plots of group means with significance brackets
#'
#' From a [clean_regress()] result, plots model-estimated marginal means
#' (averaged over other predictors, with the model's robust SEs) and labels
#' brackets with the same adjusted contrasts the tables report. From a data
#' frame, plots raw means with SE, SD, or CI and labels brackets with Welch
#' *t*-tests ([cr_ttest()]).
#'
#' @param x A `clean_results` object or a data frame.
#' @param dv Outcome.
#' @param variable Grouping variable on the x-axis.
#' @param by Optional second grouping variable: dodged groups in the lab
#'   palette. Brackets then compare `by` levels within each `variable` level.
#' @param facet Optional variable to facet by (brackets stay within panels).
#' @param type `"bar"`, `"dot"`, or, for raw data, `"raincloud"` (density,
#'   box, jittered points, mean and error bar) or `"box"`.
#' @param uncertainty Error bars: `"se"`, `"ci"` (default), `"sd"` (raw data
#'   only), or `"none"`.
#' @param p Bracket labels: `"stars"` (with "ns"), `"value"` (*p* = .031), or
#'   `"none"` (no brackets).
#' @param comparisons Which brackets: `"ref"` (each level vs. `ref`), `"all"`,
#'   `"sig"` (only significant pairs), a character vector such as
#'   `c("Loss vs. Control")`, or `"none"`. P-values always come from the full
#'   set of pairwise comparisons with the model's correction (or `adjust` for
#'   raw data), so they match the tables and text.
#' @param ref Reference level for `comparisons = "ref"` (default: first level).
#' @param show_values Print the means (above bars; beside dots). Default: on
#'   for bars.
#' @param points Raw data, dot plots: show the jittered observations behind
#'   the means.
#' @param ref_line Optional y value for a dashed reference line (e.g. a scale
#'   midpoint).
#' @param lines Connect dots across x within each `by` group (interaction
#'   style). Default: `TRUE` for dot plots with `by`.
#' @param palette Palette name or colors (see [cr_palette()]).
#' @param style Theme style passed to [theme_cr()].
#' @param digits Decimal places for printed means.
#' @param adjust Raw data only: correction across pairs.
#' @param conf_level Confidence level for `"ci"`.
#' @param caption Put the method note under the plot. It is always available
#'   as `attr(plot, "cr_note")` for a figure caption.
#' @param labels,level_labels Display labels (raw data); model results use
#'   their own labels.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' cr_plot_means(res, "trust", "condition")                    # model means, 95% CIs
#' cr_plot_means(res, "trust", "condition", by = "female", type = "dot")
#' cr_plot_means(d, "trust", "condition", type = "raincloud") + ggplot2::ylim(1, 7)
#' }
cr_plot_means <- function(x, dv, variable, by = NULL, facet = NULL,
                          type = c("bar", "dot", "raincloud", "box"),
                          uncertainty = c("ci", "se", "sd", "none"),
                          p = c("stars", "value", "none"), comparisons = "ref", ref = NULL,
                          show_values = NULL, points = FALSE, ref_line = NULL, lines = NULL,
                          palette = NULL, style = "nature", digits = 2, adjust = "holm",
                          conf_level = 0.95, caption = FALSE, labels = NULL,
                          level_labels = NULL) {
  type <- match.arg(type)
  uncertainty <- match.arg(uncertainty)
  p <- match.arg(p)
  model <- inherits(x, "clean_results")
  if (model && type %in% c("box", "raincloud")) {
    stop("Box and raincloud plots show raw data: pass the data frame.", call. = FALSE)
  }
  if (model && uncertainty == "sd") {
    stop("SDs describe raw data; model means come with SEs or CIs. Pass the data frame for SDs.",
         call. = FALSE)
  }
  vars <- c(variable, by, facet)
  compare <- if (is.null(by)) variable else by
  groups <- setdiff(vars, compare)

  info <- if (model) model_means(x, dv, vars, conf_level) else
    raw_means(x, dv, vars, conf_level)
  mm <- info$means
  lab <- info$labeller %||% make_labeller(labels, level_labels)
  comps <- NULL
  if (p != "none" && !identical(comparisons, "none")) {
    comps <- if (model) model_comparisons(x, dv, compare, groups) else
      raw_comparisons(x, dv, compare, groups, adjust)
    if (!is.null(comps)) {
      comps <- select_comparisons(comps, comparisons, ref %||% levels(mm[[compare]])[1], p)
    }
  }

  mm$err_lo <- switch(uncertainty, se = mm$mean - mm$se, sd = mm$mean - mm$sd,
                      ci = mm$lo, none = mm$mean)
  mm$err_hi <- switch(uncertainty, se = mm$mean + mm$se, sd = mm$mean + mm$sd,
                      ci = mm$hi, none = mm$mean)

  # positions: x levels at 1..k, `by` levels dodged within each
  xl <- levels(mm[[variable]])
  n_by <- if (is.null(by)) 1 else nlevels(mm[[by]])
  dodge <- switch(type, bar = if (is.null(by)) 0.55 else 0.8, dot = 0.35, 0.8)
  slot <- if (is.null(by)) 1 else as.integer(mm[[by]])
  mm$xpos <- as.integer(mm[[variable]]) + (slot - (n_by + 1) / 2) * dodge / n_by
  width <- dodge / n_by

  col_var <- by %||% variable
  pal <- cr_palette(nlevels(mm[[col_var]]), palette)
  names(pal) <- levels(mm[[col_var]])
  mm$.group <- mm[[col_var]]
  legend_name <- if (!is.null(by)) lab$var(by) else NULL
  guide <- if (is.null(by)) "none" else "legend"
  level_lab <- function(v) vapply(v, function(l) lab$level(col_var, l), "")

  g <- switch(type,
    bar = {
      g <- ggplot2::ggplot(mm, ggplot2::aes(x = .data$xpos, y = .data$mean, fill = .data$.group)) +
        ggplot2::geom_col(width = width * 0.9, alpha = 0.9, colour = NA)
      if (uncertainty != "none") {
        g <- g + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$err_lo, ymax = .data$err_hi),
                                        width = min(0.12, width * 0.3), colour = "#111111",
                                        linewidth = 0.6)
      }
      if (show_values %||% TRUE) {
        mm$.ylab <- ifelse(mm$mean >= 0, pmax(mm$mean, mm$err_hi), pmin(mm$mean, mm$err_lo))
        mm$.vj <- ifelse(mm$mean >= 0, -0.6, 1.6)
        g <- g + ggplot2::geom_text(
          data = mm, ggplot2::aes(y = .data$.ylab, label = fmt_value(.data$mean, digits, info$percent),
                                  vjust = .data$.vj),
          fontface = "bold", size = 3.1, colour = "#111111")
      }
      g + ggplot2::scale_fill_manual(values = pal, name = legend_name, guide = guide,
                                     labels = level_lab)
    },
    dot = {
      g <- ggplot2::ggplot(mm, ggplot2::aes(x = .data$xpos, y = .data$mean, colour = .data$.group))
      if (points && !model) {
        raw <- position_raw(info$raw, variable, by, n_by, dodge)
        g <- g + ggplot2::geom_point(data = raw, ggplot2::aes(y = .data[[dv]]),
                                     position = ggplot2::position_jitter(width = width * 0.25,
                                                                         height = 0, seed = 42),
                                     alpha = 0.3, size = 0.9)
      }
      if (lines %||% !is.null(by)) {
        g <- g + ggplot2::geom_line(ggplot2::aes(group = .data$.group), linewidth = 0.6,
                                    linetype = "dashed")
      }
      if (uncertainty != "none") {
        g <- g + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$err_lo, ymax = .data$err_hi),
                                        width = 0.12, linewidth = 0.8)
      }
      g <- g + ggplot2::geom_point(size = 3)
      if (isTRUE(show_values)) {
        g <- g + ggplot2::geom_text(ggplot2::aes(label = fmt_value(.data$mean, digits, info$percent)),
                                    nudge_x = 0.1, hjust = 0, size = 3, colour = .cr_ink[["text"]],
                                    show.legend = FALSE)
      }
      g + ggplot2::scale_colour_manual(values = pal, name = legend_name, guide = guide,
                                       labels = level_lab)
    },
    raincloud = ,
    box = {
      raw <- position_raw(info$raw, variable, by, n_by, dodge)
      g <- ggplot2::ggplot(raw, ggplot2::aes(x = .data$xpos, y = .data[[dv]], fill = .data$.group,
                                             colour = .data$.group))
      if (type == "raincloud") {
        cloud <- half_density(raw, dv, width * 0.55, offset = width * 0.12)
        g <- g + ggplot2::geom_polygon(data = cloud, ggplot2::aes(x = .data$x, y = .data$y,
                                                                  group = .data$id),
                                       alpha = 0.75, colour = NA)
      }
      g <- g +
        ggplot2::geom_boxplot(ggplot2::aes(group = .data$xpos),
                              width = width * if (type == "raincloud") 0.18 else 0.6,
                              outlier.shape = NA, alpha = 0.85, colour = "black", linewidth = 0.45) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = width * 0.08, height = 0,
                                                                seed = 42),
                            alpha = 0.22, size = 0.9, show.legend = FALSE)
      mm$.mx <- mm$xpos - width * 0.15
      if (uncertainty != "none") {
        g <- g + ggplot2::geom_errorbar(data = mm, ggplot2::aes(x = .data$.mx, ymin = .data$err_lo,
                                                                ymax = .data$err_hi),
                                        inherit.aes = FALSE, colour = "#111111", width = 0.06,
                                        linewidth = 0.8)
      }
      g + ggplot2::geom_point(data = mm, ggplot2::aes(x = .data$.mx, y = .data$mean),
                              inherit.aes = FALSE, shape = 21, size = 2.8, fill = "#111111",
                              colour = "white", stroke = 1.1) +
        ggplot2::scale_fill_manual(values = pal, name = legend_name, guide = guide,
                                   labels = level_lab) +
        ggplot2::scale_colour_manual(values = pal, guide = "none")
    }
  )
  if (!is.null(ref_line)) {
    g <- g + ggplot2::geom_hline(yintercept = ref_line, colour = .cr_ink[["ref"]],
                                 linewidth = 0.5, linetype = "dashed")
  }

  if (!is.null(comps) && nrow(comps)) {
    br <- bracket_layout(comps, mm, variable, by, facet, groups, compare, dodge, n_by,
                         raw = info$raw, dv = dv,
                         use_raw = type %in% c("box", "raincloud") || (points && !model),
                         label_pad = type == "bar" && (show_values %||% TRUE))
    g <- g +
      ggplot2::geom_segment(data = br$segments, ggplot2::aes(x = .data$x, xend = .data$xend,
                                                            y = .data$y, yend = .data$yend),
                            inherit.aes = FALSE, linewidth = 0.4, colour = .cr_ink[["text"]]) +
      ggplot2::geom_text(data = br$labels, ggplot2::aes(x = .data$x, y = .data$y,
                                                        label = .data$label),
                         inherit.aes = FALSE, size = 3, vjust = -0.3, colour = .cr_ink[["text"]]) +
      ggplot2::geom_blank(data = br$headroom, ggplot2::aes(x = .data$x, y = .data$y),
                          inherit.aes = FALSE)
  }

  x_labels <- vapply(xl, function(l) lab$level(variable, l), "")
  g <- g +
    ggplot2::scale_x_continuous(breaks = seq_along(xl), labels = x_labels,
                                expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(x = lab$var(variable), y = info$y_label %||% lab$var(dv)) +
    theme_cr(style) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (isTRUE(info$percent)) {
    g <- g + ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"),
                                         expand = ggplot2::expansion(mult = c(0, 0.05)))
  } else if (type == "bar") {
    g <- g + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
  }
  if (!is.null(facet)) {
    g <- g + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]),
                                 labeller = ggplot2::as_labeller(function(v) {
                                   vapply(v, function(l) lab$level(facet, l), "")
                                 }))
  }
  note <- plot_note(info, uncertainty, p, comps, model, x, dv, adjust, conf_level, type)
  if (caption) g <- g + ggplot2::labs(caption = paste(strwrap(note, 110), collapse = "\n"))
  attr(g, "cr_note") <- note
  g
}

fmt_value <- function(v, digits, percent = FALSE) {
  if (isTRUE(percent)) paste0(formatC(100 * v, format = "f", digits = 1), "%") else fmt_num(v, digits)
}

position_raw <- function(raw, variable, by, n_by, dodge) {
  slot <- if (is.null(by)) 1 else as.integer(raw[[by]])
  raw$xpos <- as.integer(raw[[variable]]) + (slot - (n_by + 1) / 2) * dodge / n_by
  raw$.group <- raw[[by %||% variable]]
  raw
}

# Right-facing half densities ("clouds") as polygons, one per x position.
half_density <- function(raw, dv, width, offset) {
  out <- lapply(split(raw, raw$xpos), function(dd) {
    y <- dd[[dv]]
    if (length(unique(y)) < 2) return(NULL)
    den <- stats::density(y, adjust = 0.8, n = 128, from = min(y), to = max(y))
    h <- den$y / max(den$y) * width
    x0 <- dd$xpos[1] + offset
    data.frame(x = c(x0, x0 + h, x0), y = c(min(den$x), den$x, max(den$x)),
               id = dd$xpos[1], .group = dd$.group[1])
  })
  do.call(rbind, out)
}

#' Interaction plots
#'
#' Means of `dv` across `x_var` for each level of `trace`, as dots joined by
#' dashed lines with error bars (categorical `x_var`), or as predicted lines
#' with confidence ribbons (continuous `x_var`). From a model, the means are
#' the model's predictions; from a data frame, raw means (or linear fits for
#' a continuous `x_var`).
#'
#' @inheritParams cr_plot_means
#' @param x_var Variable on the x-axis.
#' @param trace Variable whose levels get separate lines.
#' @param n_points For a continuous `x_var`: points along its range.
#' @param ... Passed to [cr_plot_means()] for a categorical `x_var`.
#' @return A ggplot object.
#' @export
cr_plot_interaction <- function(x, dv, x_var, trace, uncertainty = c("ci", "se", "sd", "none"),
                                palette = NULL, style = "nature", n_points = 50,
                                conf_level = 0.95, ...) {
  uncertainty <- match.arg(uncertainty)
  data <- if (inherits(x, "clean_results")) x$data[[dv]] else x
  if (!x_var %in% names(data)) stop(sprintf("'%s' not found.", x_var), call. = FALSE)
  if (!is.numeric(data[[x_var]])) {
    return(cr_plot_means(x, dv, x_var, by = trace, type = "dot", lines = TRUE,
                         uncertainty = uncertainty, palette = palette, style = style,
                         conf_level = conf_level, ...))
  }
  if (uncertainty == "sd") stop("Use \"se\" or \"ci\" for a continuous x.", call. = FALSE)
  rng <- range(data[[x_var]], na.rm = TRUE)
  grid <- seq(rng[1], rng[2], length.out = n_points)
  if (inherits(x, "clean_results")) {
    em <- model_emmeans(x, dv, c(x_var, trace), at = stats::setNames(list(grid), x_var))
    s <- as.data.frame(summary(em, type = "response", level = conf_level))
    V <- model_cr2(x, dv)
    if (!is.null(V)) s <- cr2_summary(s, em, x$models[[dv]], V, conf_level, response = TRUE)
    pl <- prediction_frame(s, c(x_var, trace))
    lab <- results_labeller(x)
    y_lab <- unname(x$settings$dv_labels[dv])
  } else {
    pl <- do.call(rbind, lapply(split(data, data[[trace]]), function(dd) {
      f <- stats::lm(stats::as.formula(sprintf("`%s` ~ `%s`", dv, x_var)), data = dd)
      nd <- stats::setNames(data.frame(grid), x_var)
      pr <- stats::predict(f, nd, se.fit = TRUE)
      q <- stats::qt(1 - (1 - conf_level) / 2, f$df.residual)
      data.frame(nd, trace = dd[[trace]][1], mean = pr$fit, se = pr$se.fit,
                 lo = pr$fit - q * pr$se.fit, hi = pr$fit + q * pr$se.fit)
    }))
    names(pl)[names(pl) == "trace"] <- trace
    lab <- make_labeller()
    y_lab <- lab$var(dv)
  }
  lv <- factor_levels(data[[trace]]) %||% unique(as.character(pl[[trace]]))
  pl[[trace]] <- factor(as.character(pl[[trace]]), levels = lv)
  pal <- cr_palette(nlevels(pl[[trace]]), palette)
  names(pal) <- levels(pl[[trace]])
  pl$band_lo <- if (uncertainty == "ci") pl$lo else pl$mean - pl$se
  pl$band_hi <- if (uncertainty == "ci") pl$hi else pl$mean + pl$se
  trace_labels <- function(v) vapply(v, function(l) lab$level(trace, l), "")
  g <- ggplot2::ggplot(pl, ggplot2::aes(x = .data[[x_var]], y = .data$mean,
                                        colour = .data[[trace]], fill = .data[[trace]]))
  if (uncertainty != "none") {
    g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$band_lo, ymax = .data$band_hi),
                                  alpha = 0.15, colour = NA)
  }
  g + ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = pal, name = lab$var(trace), labels = trace_labels) +
    ggplot2::scale_fill_manual(values = pal, name = lab$var(trace), labels = trace_labels) +
    ggplot2::labs(x = lab$var(x_var), y = y_lab) +
    theme_cr(style)
}

#' Coefficient (forest) plots
#'
#' Dot-and-whisker plot of the estimates in the results table: one row per
#' term, outcomes in the package palette, a dashed line at no effect, a thick
#' inner 90% CI and a thin outer CI at the table's level (the same interval
#' the table reports), and the estimate with stars (or the *p*-value) above
#' each point. Ratios (OR, IRR) use a log axis.
#'
#' @param x A `clean_results` object.
#' @param dv Outcomes to include (default: all).
#' @param terms Variables to include (default: all but the intercept).
#' @param contrasts Plot the pairwise contrasts instead of the coefficients.
#' @param inner Level of the inner (thick) interval, or `NULL` for one
#'   interval. Not drawn for contrasts whose CIs are multiplicity-adjusted.
#' @param p `"stars"`, `"value"`, or `"none"` for the point labels.
#' @param show_values Print the estimate above each point.
#' @param palette Palette name or colors.
#' @param style Theme style (default `"polsci"`, the forest style).
#' @param digits Decimal places.
#' @return A ggplot object.
#' @export
cr_plot_coef <- function(x, dv = NULL, terms = NULL, contrasts = FALSE, inner = 0.90,
                         p = c("stars", "value", "none"), show_values = TRUE, palette = NULL,
                         style = "polsci", digits = 2) {
  p <- match.arg(p)
  r <- cr_rows(x)
  r <- r[!r$is_ref & r$variable != "(Intercept)", , drop = FALSE]
  r <- r[r$section == if (contrasts) "contrast" else "fixed", , drop = FALSE]
  if (!is.null(dv)) r <- r[r$dv %in% dv | r$dv_label %in% dv, , drop = FALSE]
  if (!is.null(terms)) r <- r[r$variable %in% terms | r$variable_label %in% terms, , drop = FALSE]
  if (!nrow(r)) stop("Nothing to plot after filtering.", call. = FALSE)
  r$term_label <- ifelse(is.na(r$level_label), r$variable_label,
                         ifelse(r$section == "contrast", r$level_label,
                                paste0(r$variable_label, ": ", r$level_label)))
  if (any(!is.na(r$subgroup))) {
    r$term_label <- ifelse(is.na(r$subgroup), r$term_label,
                           paste0(r$term_label, " (", r$subgroup, ")"))
  }
  r$term_label <- factor(r$term_label, levels = rev(unique(r$term_label)))
  r$dv_label <- factor(r$dv_label, levels = unique(r$dv_label))
  k <- nlevels(r$dv_label)
  pal <- cr_palette(max(k, 2), palette)[seq_len(k)]
  if (k == 1 && is.null(palette)) pal <- cr_palettes$teal_terracotta[["navy"]]
  names(pal) <- levels(r$dv_label)
  ratio <- r$est_name %in% c("OR", "IRR")
  mixed_scales <- length(unique(ratio)) > 1
  # outcomes side by side within a term; faceted outcomes need no offset
  r$ypos <- as.integer(r$term_label) +
    if (mixed_scales) 0 else (as.integer(r$dv_label) - (k + 1) / 2) * 0.5 / k
  r$null <- ifelse(ratio, 1, 0)
  # inner interval: same SE, narrower level (log scale for ratios)
  r$in_lo <- r$in_hi <- NA_real_
  if (!is.null(inner)) {
    unadj <- is.na(r$adjustment) & r$interval %in% "CI" & is.finite(r$std.error)
    df <- ifelse(is.finite(r$df), r$df, Inf)
    q_in <- stats::qt(1 - (1 - inner) / 2, df)
    se_link <- r$std.error; est_link <- r$estimate
    se_link[ratio] <- r$std.error[ratio] / r$estimate[ratio]
    est_link[ratio] <- log(r$estimate[ratio])
    lo <- est_link - q_in * se_link; hi <- est_link + q_in * se_link
    lo[ratio] <- exp(lo[ratio]); hi[ratio] <- exp(hi[ratio])
    r$in_lo <- ifelse(unadj, lo, NA)
    r$in_hi <- ifelse(unadj, hi, NA)
  }
  r$lab <- paste0(if (show_values) fmt_num(r$estimate, digits) else "",
                  switch(p, stars = fmt_stars(r$p.value),
                         value = ifelse(is.na(r$p.value), "",
                                        paste0(if (show_values) ", " else "", "p ",
                                               ifelse(startsWith(fmt_p(r$p.value), "<"), "", "= "),
                                               fmt_p(r$p.value))),
                         none = ""))
  g <- ggplot2::ggplot(r, ggplot2::aes(x = .data$estimate, y = .data$ypos, colour = .data$dv_label)) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$null), colour = .cr_ink[["ref"]],
                        linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high),
                           orientation = "y", width = 0, linewidth = 0.6)
  if (!is.null(inner) && any(!is.na(r$in_lo))) {
    g <- g + ggplot2::geom_errorbar(data = r[!is.na(r$in_lo), , drop = FALSE],
                                    ggplot2::aes(xmin = .data$in_lo, xmax = .data$in_hi),
                                    orientation = "y", width = 0, linewidth = 1.3)
  }
  g <- g +
    ggplot2::geom_point(shape = 21, size = 3, fill = "white", stroke = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$lab), vjust = -1.1, size = 3,
                       colour = .cr_ink[["text"]], show.legend = FALSE) +
    ggplot2::scale_y_continuous(breaks = seq_len(nlevels(r$term_label)),
                                labels = levels(r$term_label),
                                expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_colour_manual(values = pal, name = NULL,
                                 guide = if (k > 1 && !mixed_scales) "legend" else "none") +
    ggplot2::labs(x = if (all(ratio)) unique(r$est_name)[1] else if (!any(ratio)) "Estimate" else NULL,
                  y = NULL) +
    theme_cr(style)
  if (mixed_scales) {
    g <- g + ggplot2::facet_wrap(ggplot2::vars(.data$dv_label), scales = "free_x")
  } else if (all(ratio)) {
    g <- g + ggplot2::scale_x_log10()
  }
  attr(g, "cr_note") <- sprintf(
    "Points show estimates; %sthin lines show the CIs reported in the table%s.",
    if (!is.null(inner)) sprintf("thick lines show %g%% CIs and ", 100 * inner) else "",
    if (contrasts) " (adjusted for multiple comparisons where noted)" else "")
  g
}

#' Histograms
#'
#' A histogram with a dashed line at the mean; with `by`, one panel per group
#' in the package palette (or overlaid).
#'
#' @param data A data frame.
#' @param var Variable to plot.
#' @param by Optional grouping variable.
#' @param layout With `by`: `"facet"` (one panel per group) or `"overlay"`.
#' @param bins,binwidth Passed to [ggplot2::geom_histogram()].
#' @param mean_line Draw the mean as a dashed line.
#' @param palette Palette name or colors.
#' @param style Theme style passed to [theme_cr()].
#' @param labels,level_labels Display labels.
#' @return A ggplot object.
#' @export
cr_plot_hist <- function(data, var, by = NULL, layout = c("facet", "overlay"), bins = 30,
                         binwidth = NULL, mean_line = TRUE, palette = NULL, style = "nature",
                         labels = NULL, level_labels = NULL) {
  layout <- match.arg(layout)
  check_cols(data, c(var, by))
  lab <- make_labeller(labels, level_labels)
  d <- data[!is.na(data[[var]]), , drop = FALSE]
  teal <- cr_palettes$teal_terracotta[["teal"]]
  terracotta <- cr_palettes$teal_terracotta[["terracotta"]]
  if (is.null(by)) {
    g <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[var]])) +
      ggplot2::geom_histogram(bins = bins, binwidth = binwidth, fill = teal, colour = "white",
                              alpha = 0.9)
    if (mean_line) {
      g <- g + ggplot2::geom_vline(xintercept = mean(d[[var]]), colour = terracotta,
                                   linetype = "dashed", linewidth = 0.75)
    }
  } else {
    d[[by]] <- factor(d[[by]], levels = factor_levels(d[[by]]))
    pal <- cr_palette(nlevels(d[[by]]), palette)
    names(pal) <- levels(d[[by]])
    means <- stats::aggregate(d[[var]], list(g = d[[by]]), mean)
    names(means) <- c(by, "m")
    by_labels <- function(v) vapply(v, function(l) lab$level(by, l), "")
    g <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[var]], fill = .data[[by]])) +
      ggplot2::geom_histogram(bins = bins, binwidth = binwidth, colour = "white",
                              alpha = if (layout == "overlay") 0.55 else 0.9,
                              position = "identity") +
      ggplot2::scale_fill_manual(values = pal, name = lab$var(by), labels = by_labels)
    if (mean_line) {
      g <- g + ggplot2::geom_vline(data = means, ggplot2::aes(xintercept = .data$m,
                                                              colour = .data[[by]]),
                                   linetype = "dashed", linewidth = 0.75, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = if (layout == "facet") {
          stats::setNames(rep(.cr_ink[["text"]], length(pal)), names(pal))
        } else pal)
    }
    if (layout == "facet") {
      g <- g + ggplot2::facet_wrap(ggplot2::vars(.data[[by]]),
                                   labeller = ggplot2::as_labeller(by_labels))
    }
  }
  g <- g + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = lab$var(var), y = "Count") +
    theme_cr(style)
  if (!is.null(by) && layout == "facet") g <- g + ggplot2::theme(legend.position = "none")
  g
}

# Internals ---------------------------------------------------------------------

model_emmeans <- function(x, dv, specs, by = NULL, at = NULL, mode = NULL) {
  m <- x$models[[dv]]
  type <- x$meta[[dv]]$type
  args <- list(object = m, specs = specs, data = x$data[[dv]])
  if (!is.null(by)) args$by <- by
  if (!is.null(at)) args$at <- at
  if (!is_bayes(type) && x$settings$se == "robust" && robust_available(type) &&
      !is.null(x$vcov[[dv]])) {
    args$vcov. <- as.matrix(x$vcov[[dv]])
  }
  if (type == "lmer") args$lmer.df <- "satterthwaite"
  if (!is.null(mode)) args$mode <- mode
  suppressMessages(do.call(emmeans::emmeans, args))
}

# The CR2 matrix for an outcome when its inference uses CR2 Satterthwaite df
# (so plotted CIs and brackets must too; see robust_df.R), otherwise NULL.
model_cr2 <- function(x, dv) {
  V <- x$vcov[[dv]]
  type <- x$meta[[dv]]$type
  if (!is_bayes(type) && x$settings$se == "robust" && robust_available(type) && uses_cr2(V)) V
  else NULL
}

prediction_frame <- function(s, vars) {
  est <- intersect(c("emmean", "prob", "rate", "response", "mean.class"), names(s))[1]
  lo <- grep("(lower\\.CL|LCL|lower\\.HPD)$", names(s), value = TRUE)[1]
  hi <- grep("(upper\\.CL|UCL|upper\\.HPD)$", names(s), value = TRUE)[1]
  out <- s[, vars, drop = FALSE]
  out$mean <- s[[est]]
  out$se <- if ("SE" %in% names(s)) s$SE else NA_real_
  out$lo <- s[[lo]]
  out$hi <- s[[hi]]
  out
}

model_means <- function(x, dv, vars, conf_level) {
  if (!dv %in% names(x$models)) stop(sprintf("Outcome '%s' not found.", dv), call. = FALSE)
  type <- x$meta[[dv]]$type
  if (type == "bayes_ordinal") {
    stop("Means plots are not available for Bayesian ordinal models; use cr_plot_coef().",
         call. = FALSE)
  }
  em <- model_emmeans(x, dv, vars, mode = if (type %in% c("ordinal", "ordinal_mixed")) "mean.class")
  s <- as.data.frame(summary(em, type = "response", level = conf_level))
  V <- model_cr2(x, dv)
  if (!is.null(V)) s <- cr2_summary(s, em, x$models[[dv]], V, conf_level, response = TRUE)
  mm <- prediction_frame(s, vars)
  for (v in vars) mm[[v]] <- factor(as.character(mm[[v]]),
                                    levels = factor_levels(x$data[[dv]][[v]]) %||%
                                      unique(as.character(mm[[v]])))
  y_label <- unname(x$settings$dv_labels[dv])
  if (is_logistic(type)) y_label <- paste(y_label, "(probability)")
  if (is_ordinal(type)) y_label <- paste(y_label, "(expected response)")
  list(means = mm, labeller = results_labeller(x), y_label = y_label,
       percent = is_logistic(type), source = "model", type = type)
}

# Labels for a results object: the cleaned names its table uses.
results_labeller <- function(x) {
  e <- x$estimates
  var_lab <- stats::setNames(e$label[e$row_type == "header"], e$variable[e$row_type == "header"])
  terms <- e[e$row_type == "term", , drop = FALSE]
  var_lab <- c(var_lab, stats::setNames(terms$label, terms$variable))
  lv <- e[e$row_type %in% c("level", "ref") & e$section == "fixed", , drop = FALSE]
  lv_lab <- stats::setNames(sub(" (ref.)", "", lv$label, fixed = TRUE),
                            paste(lv$variable, lv$level, sep = "\r"))
  list(
    var = function(v) if (v %in% names(var_lab)) unname(var_lab[[v]]) else
      if (v %in% names(x$settings$dv_labels)) unname(x$settings$dv_labels[[v]]) else clean_label(v),
    level = function(v, l) {
      k <- paste(v, l, sep = "\r")
      if (k %in% names(lv_lab)) unname(lv_lab[[k]]) else if (l %in% c("TRUE", "FALSE")) l else
        clean_label(l)
    }
  )
}

raw_means <- function(data, dv, vars, conf_level) {
  check_cols(data, c(dv, vars))
  d <- data[stats::complete.cases(data[, c(dv, vars)]), , drop = FALSE]
  for (v in vars) d[[v]] <- factor(as.character(d[[v]]),
                                   levels = factor_levels(data[[v]]) %||% sort(unique(d[[v]])))
  key <- interaction(d[vars], drop = TRUE, lex.order = TRUE)
  parts <- split(d, key, drop = TRUE)
  mm <- do.call(rbind, lapply(parts, function(dd) {
    y <- dd[[dv]]
    n <- length(y); s <- stats::sd(y); se <- s / sqrt(n)
    q <- stats::qt(1 - (1 - conf_level) / 2, max(n - 1, 1))
    out <- dd[1, vars, drop = FALSE]
    out$mean <- mean(y); out$sd <- s; out$se <- se; out$n <- n
    out$lo <- mean(y) - q * se; out$hi <- mean(y) + q * se
    out
  }))
  rownames(mm) <- NULL
  list(means = mm, raw = d, source = "raw")
}

model_comparisons <- function(x, dv, compare, groups) {
  type <- x$meta[[dv]]$type
  if (is_bayes(type)) return(NULL)
  em <- model_emmeans(x, dv, compare, by = if (length(groups)) groups else NULL)
  ct <- emmeans::contrast(em, method = "revpairwise", adjust = x$settings$contrast_adjust)
  s <- as.data.frame(summary(ct, infer = c(FALSE, TRUE)))
  V <- model_cr2(x, dv)
  if (!is.null(V)) {
    s <- cr2_summary(s, ct, x$models[[dv]], V, 0.95, adjust = x$settings$contrast_adjust)
  }
  pr <- match_pairs(as.character(s$contrast), as.character(em@levels[[compare]]))
  out <- data.frame(a = pr$a, b = pr$b, p = s$p.value, stringsAsFactors = FALSE)
  for (g in groups) out[[g]] <- as.character(s[[g]])
  out
}

raw_comparisons <- function(data, dv, compare, groups, adjust) {
  d <- data
  gcol <- NULL
  if (length(groups)) {
    gcol <- ".cr_group"
    d[[gcol]] <- do.call(paste, c(lapply(groups, function(g) as.character(d[[g]])), sep = "\r"))
  }
  tt <- cr_ttest(d, dv, compare, pairs = "all", by = gcol, adjust = adjust, clean_labels = FALSE)$rows
  pr <- strsplit(tt$level, " vs. ", fixed = TRUE)
  out <- data.frame(a = vapply(pr, `[`, "", 1), b = vapply(pr, `[`, "", 2), p = tt$p.value,
                    stringsAsFactors = FALSE)
  if (length(groups)) {
    parts <- strsplit(tt$subgroup, "\r", fixed = TRUE)
    for (i in seq_along(groups)) out[[groups[i]]] <- vapply(parts, `[`, "", i)
  }
  out
}

select_comparisons <- function(comps, comparisons, ref, p) {
  if (identical(comparisons, "all")) {
    keep <- rep(TRUE, nrow(comps))
  } else if (identical(comparisons, "ref")) {
    keep <- comps$a == ref | comps$b == ref
  } else if (identical(comparisons, "sig")) {
    keep <- comps$p < 0.05
  } else {
    want <- strsplit(comparisons, " vs. ", fixed = TRUE)
    keep <- vapply(seq_len(nrow(comps)), function(i) {
      any(vapply(want, function(w) setequal(w, c(comps$a[i], comps$b[i])), logical(1)))
    }, logical(1))
  }
  comps <- comps[keep, , drop = FALSE]
  comps$label <- if (p == "stars") {
    ifelse(comps$p < 0.05, fmt_stars(comps$p), "ns")
  } else {
    s <- fmt_p(comps$p)
    ifelse(startsWith(s, "<"), paste("p", s), paste("p =", s))
  }
  comps
}

bracket_layout <- function(comps, mm, variable, by, facet, groups, compare, dodge, n_by,
                           raw = NULL, dv = NULL, use_raw = FALSE, label_pad = FALSE) {
  use_raw <- use_raw && !is.null(raw)
  tops <- if (use_raw) raw[[dv]] else c(mm$err_hi, mm$mean)
  span <- max(tops, na.rm = TRUE) - min(0, if (use_raw) raw[[dv]] else c(mm$err_lo, mm$mean),
                                         na.rm = TRUE)
  step <- 0.09 * span
  tick <- 0.025 * span
  pos <- function(level, row) {
    hit <- mm[[compare]] == level
    for (g in groups) hit <- hit & as.character(mm[[g]]) == row[[g]]
    mm$xpos[hit][1]
  }
  top_of <- function(row) {
    src <- if (use_raw) raw else mm
    hit <- rep(TRUE, nrow(src))
    for (g in groups) hit <- hit & as.character(src[[g]]) == row[[g]]
    if (use_raw) max(src[[dv]][hit], na.rm = TRUE) else
      max(src$err_hi[hit], src$mean[hit], na.rm = TRUE)
  }
  comps$x1 <- vapply(seq_len(nrow(comps)), function(i) pos(comps$a[i], comps[i, ]), numeric(1))
  comps$x2 <- vapply(seq_len(nrow(comps)), function(i) pos(comps$b[i], comps[i, ]), numeric(1))
  comps$width <- abs(comps$x2 - comps$x1)
  comps$base <- vapply(seq_len(nrow(comps)), function(i) top_of(comps[i, ]), numeric(1)) +
    if (label_pad) 0.08 * span else 0.02 * span
  key <- if (length(groups)) do.call(paste, comps[groups]) else rep("", nrow(comps))
  comps <- comps[order(key, comps$width), , drop = FALSE]
  key <- if (length(groups)) do.call(paste, comps[groups]) else rep("", nrow(comps))
  comps$tier <- stats::ave(seq_len(nrow(comps)), key, FUN = seq_along)
  comps$y <- comps$base + step * comps$tier
  lo <- pmin(comps$x1, comps$x2); hi <- pmax(comps$x1, comps$x2)
  seg <- rbind(
    data.frame(x = lo, xend = hi, y = comps$y, yend = comps$y),
    data.frame(x = lo, xend = lo, y = comps$y, yend = comps$y - tick),
    data.frame(x = hi, xend = hi, y = comps$y, yend = comps$y - tick)
  )
  labs <- data.frame(x = (lo + hi) / 2, y = comps$y, label = comps$label)
  if (!is.null(facet)) {
    seg[[facet]] <- factor(rep(comps[[facet]], 3), levels = levels(mm[[facet]]))
    labs[[facet]] <- factor(comps[[facet]], levels = levels(mm[[facet]]))
  }
  head <- data.frame(x = labs$x, y = comps$y + step * 0.9)
  if (!is.null(facet)) head[[facet]] <- labs[[facet]]
  list(segments = seg, labels = labs, headroom = head)
}

plot_note <- function(info, uncertainty, p, comps, model, x, dv, adjust, conf_level, type) {
  what <- if (model) {
    sprintf("%s show model-estimated marginal means (%s; %s)",
            switch(type, bar = "Bars", dot = "Points"),
            .type_labels[[info$type]], x$meta[[dv]]$se_text)
  } else {
    switch(type, bar = "Bars show means", dot = "Points show means",
           box = "Boxes show medians and interquartile ranges, small points individual responses, and large points means",
           raincloud = paste("Clouds show the distribution, boxes medians and interquartile ranges,",
                             "small points individual responses, and large points means"))
  }
  err <- switch(uncertainty, se = "Error bars show \u00b11 SE.", sd = "Error bars show \u00b11 SD.",
                ci = sprintf("Error bars show %g%% %s.", 100 * conf_level,
                             if (isTRUE(model && x$meta[[dv]]$bayes)) "credible intervals" else
                               "confidence intervals"),
                none = "")
  br <- if (!is.null(comps) && nrow(comps)) {
    adj <- if (model) x$settings$contrast_adjust else adjust
    src <- if (model) "model contrasts" else "Welch t-tests"
    sprintf("Brackets show pairwise %s%s.%s", src,
            if (adj == "none") "" else sprintf(", %s-adjusted across all pairs", adjust_name(adj)),
            if (p == "stars") " * p < .05, ** p < .01, *** p < .001; ns = not significant." else "")
  } else ""
  trimws(paste0(what, ". ", err, " ", br))
}
