# Equivalence tests ------------------------------------------------------------

#' Equivalence tests (TOST) for any reported result
#'
#' Tests whether an effect is smaller than a smallest effect size of interest
#' (SESOI): two one-sided tests against the lower and upper bounds (Lakens,
#' 2017). The effect is declared equivalent when both are significant, which is
#' the same as the 90% CI (for alpha = .05) lying inside the bounds. Uses each
#' result's own estimate, SE, and df, so robust and cluster-robust SEs carry
#' over.
#'
#' @param x A `clean_results` or `cr_tests` object (t-tests and regression rows).
#' @param bounds Equivalence bounds: one number `b` for `c(-b, b)`, or
#'   `c(lower, upper)`. On the estimate's scale (raw units, or ratios such as
#'   `c(0.8, 1.25)` for odds ratios), or in *d* units with `scale = "d"`.
#' @param dv,variable,level Which result(s) to test; `NULL` tests every
#'   non-intercept estimate (or every comparison for simple tests).
#' @param scale `"raw"` or `"d"` (bounds in standardized units; needs a *d*).
#' @param alpha Alpha for each one-sided test.
#' @param subgroup For results with subgroups or moderator levels.
#' @return A `cr_tests` object with one row per tested result; it works with
#'   [cr_inline()], [cr_sentence()], [cr_report()], [cr_h()], and
#'   [cr_ground_truth()].
#' @export
#' @examples
#' \dontrun{
#' eq <- cr_equivalence(res, bounds = 0.2, dv = "trust", variable = "condition")
#' cr_report(eq)
#' }
cr_equivalence <- function(x, bounds, dv = NULL, variable = NULL, level = NULL,
                           scale = c("raw", "d"), alpha = 0.05, subgroup = NULL) {
  scale <- match.arg(scale)
  if (length(bounds) == 1) bounds <- c(-abs(bounds), abs(bounds))
  if (length(bounds) != 2 || bounds[1] >= bounds[2]) {
    stop("`bounds` must be one positive number or c(lower, upper).", call. = FALSE)
  }
  rows <- if (!is.null(dv) && !is.null(level)) {
    cr_find(x, dv, variable, level, subgroup)
  } else {
    r <- cr_rows(x)
    r <- r[!r$is_ref & r$variable != "(Intercept)" & !is.na(r$std.error), , drop = FALSE]
    if (!is.null(dv)) r <- r[r$dv %in% dv | r$dv_label %in% dv, , drop = FALSE]
    if (!is.null(variable)) {
      r <- r[r$variable %in% variable | r$variable_label %in% variable, , drop = FALSE]
    }
    if (!is.null(subgroup)) r <- r[r$subgroup %in% subgroup, , drop = FALSE]
    # contrasts already include each level vs. the reference; test those once
    has_ctr <- unique(paste(r$dv, r$variable)[r$section %in% "contrast"])
    r[!(r$section %in% "fixed" & !is.na(r$level) & paste(r$dv, r$variable) %in% has_ctr), ,
      drop = FALSE]
  }
  if (!nrow(rows)) stop("No results to test.", call. = FALSE)
  if (any(rows$interval %in% "CrI")) {
    stop("Use a ROPE for Bayesian models (bayestestR::rope()), not TOST.", call. = FALSE)
  }
  out <- lapply(seq_len(nrow(rows)), function(i) tost_row(rows[i, ], bounds, scale, alpha))
  res <- do.call(rbind, out)
  structure(list(rows = res[, row_cols], test = "equivalence (TOST)",
                 adjust_text = NULL, bounds = bounds, scale = scale), class = "cr_tests")
}

tost_row <- function(r, bounds, scale, alpha) {
  ratio <- r$est_name %in% c("OR", "IRR")
  est <- r$estimate; se <- r$std.error
  if (ratio) {               # test on the log scale; SE was delta-method on the ratio
    se <- se / est; est <- log(est)
  }
  b <- bounds
  if (scale == "d") {
    if (is.na(r$es) || r$es == 0) stop("scale = \"d\" needs a d for every tested result.",
                                       call. = FALSE)
    b <- bounds * (r$estimate / r$es)   # d units -> raw units
  } else if (ratio) {
    if (any(b <= 0)) stop("Bounds for ratios must be positive, e.g. c(0.8, 1.25).", call. = FALSE)
    b <- log(b)
  }
  df <- if (is.finite(r$df)) r$df else Inf
  t_lo <- (est - b[1]) / se
  t_hi <- (est - b[2]) / se
  p_lo <- stats::pt(t_lo, df, lower.tail = FALSE)
  p_hi <- stats::pt(t_hi, df)
  binding <- if (p_lo >= p_hi) "lower" else "upper"
  crit <- stats::qt(1 - alpha, df)
  ci <- est + c(-1, 1) * crit * se
  out <- r
  out$id <- paste0(r$id, "::equivalence")
  out$kind <- "equivalence"
  out$model_type <- "TOST"
  out$section <- "equivalence"
  out$statistic <- if (binding == "lower") t_lo else t_hi
  out$stat_name <- if (is.finite(df)) "t" else "z"
  out$df <- if (is.finite(df)) df else NA_real_
  out$p.value <- max(p_lo, p_hi)
  out$p.within <- out$p.value
  out$conf.low <- if (ratio) exp(ci[1]) else ci[1]
  out$conf.high <- if (ratio) exp(ci[2]) else ci[2]
  out$conf_level <- 1 - 2 * alpha
  out$es <- out$es.low <- out$es.high <- NA_real_
  out$es_name <- NA_character_
  out$adjustment <- sprintf("TOST bounds [%s, %s]%s", format(signif(bounds[1], 3)),
                            format(signif(bounds[2], 3)), if (scale == "d") " (d units)" else "")
  out$group1 <- r$group1; out$group2 <- r$group2
  out$m1 <- bounds[1]; out$m2 <- bounds[2]   # the bounds, for the sentence
  out$sd1 <- out$sd2 <- NA_real_
  out
}
