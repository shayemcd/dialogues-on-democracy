# Small-sample inference for emmeans grids under CR2 ---------------------------
#
# emmeans() accepts a user covariance matrix (`vcov.`) but not its degrees of
# freedom. With `vcov.` = CR2 it keeps the model's own df: Satterthwaite df
# from the *model-based* covariance for lmer, df.residual for lm, and z for
# glm. Those are much larger than the CR2 Satterthwaite df that the coefficient
# rows use (clubSandwich::coef_test), so contrast rows, plot brackets, and
# plotted CIs were too liberal when there are few clusters, and a contrast
# could disagree with the identical coefficient row of the same table.
#
# For any emmGrid (means or contrasts), cr2_grid_inference() recomputes each
# row's estimate, SE, and Satterthwaite df from the grid's linear functions and
# the CR2 matrix (clubSandwich::linear_contrast), then applies the family-wise
# adjustment itself, using the same p-value and CI rules emmeans uses.

uses_cr2 <- function(V) inherits(V, "clubSandwich")

# Align an emmeans linfct matrix with the model's estimated coefficients
# (rank-deficient fits drop or NA some columns). Rows that load on a dropped
# coefficient are not estimable.
align_linfct <- function(L, b) {
  out <- matrix(0, nrow(L), length(b), dimnames = list(NULL, names(b)))
  shared <- intersect(colnames(L), names(b))
  out[, shared] <- L[, shared, drop = FALSE]
  dropped <- setdiff(colnames(L), names(b))
  estimable <- if (length(dropped)) rowSums(abs(L[, dropped, drop = FALSE])) < 1e-10 else
    rep(TRUE, nrow(L))
  list(L = out, estimable = estimable)
}

# Estimate, SE, and CR2 Satterthwaite df for every row of an emmGrid.
cr2_grid_inference <- function(em, model, V) {
  b <- fixed_coefs(model)
  b <- b[!is.na(b)]
  al <- align_linfct(em@linfct, b)
  n <- nrow(al$L)
  out <- data.frame(estimate = rep(NA_real_, n), std.error = NA_real_, df = NA_real_)
  ok <- al$estimable
  if (any(ok)) {
    lc <- clubSandwich::linear_contrast(model, vcov = V, contrasts = al$L[ok, , drop = FALSE],
                                        test = "Satterthwaite")
    out$estimate[ok] <- lc$Est
    out$std.error[ok] <- lc$SE
    out$df[ok] <- lc$df
  }
  out
}

# Family-wise p-values and CI critical values for one family of k tests.
# Mirrors emmeans: p.adjust methods adjust the p-values and use Bonferroni
# CIs; "tukey" uses the studentized range over `n_means`; "scheffe" uses F
# with the family's rank; "sidak" the Sidak inequality. Unsupported methods
# fall back to Bonferroni with a warning.
adjusted_inference <- function(t, df, adjust, conf_level, n_means = NULL, rank = NULL) {
  k <- length(t)
  p_raw <- 2 * stats::pt(abs(t), df, lower.tail = FALSE)
  alpha <- 1 - conf_level
  if (identical(adjust, "none") || k <= 1) {
    return(list(p = p_raw, crit = stats::qt(1 - alpha / 2, df)))
  }
  if (adjust %in% c("holm", "hochberg", "hommel", "BH", "BY", "fdr")) {
    return(list(p = stats::p.adjust(p_raw, adjust), crit = stats::qt(1 - alpha / (2 * k), df)))
  }
  switch(adjust,
    bonferroni = list(p = pmin(1, p_raw * k), crit = stats::qt(1 - alpha / (2 * k), df)),
    sidak = list(p = 1 - (1 - p_raw)^k,
                 crit = stats::qt(1 - (1 - conf_level^(1 / k)) / 2, df)),
    tukey = {
      nm <- n_means %||% k
      list(p = stats::ptukey(abs(t) * sqrt(2), nm, df, lower.tail = FALSE),
           crit = stats::qtukey(conf_level, nm, df) / sqrt(2))
    },
    scheffe = {
      r <- rank %||% (k - 1)
      list(p = stats::pf(t^2 / r, r, df, lower.tail = FALSE),
           crit = sqrt(r * stats::qf(conf_level, r, df)))
    },
    {
      warning(sprintf(paste("Adjustment '%s' is not available with CR2 small-sample df;",
                            "using Bonferroni."), adjust), call. = FALSE)
      list(p = pmin(1, p_raw * k), crit = stats::qt(1 - alpha / (2 * k), df))
    }
  )
}

# Replace df, test statistic, p-value, and CI columns of an emmeans summary
# with CR2 Satterthwaite inference. `s` is as.data.frame(summary(em, ...)) on
# the link scale for contrasts or for means; `adjust` applies within each `by`
# group (the emmeans family). For means of a glm on the response scale, pass
# `response = TRUE`: CIs are built on the link scale and back-transformed.
cr2_summary <- function(s, em, model, V, conf_level, adjust = "none", response = FALSE) {
  inf <- cr2_grid_inference(em, model, V)
  t <- inf$estimate / inf$std.error
  by_vars <- em@misc$by.vars
  fam <- if (length(by_vars)) interaction(s[by_vars], drop = TRUE) else factor(rep(1, nrow(s)))
  n_means <- em@misc$famSize  # means per family, set by emmeans::contrast()
  p <- crit <- rep(NA_real_, nrow(s))
  for (g in levels(fam)) {
    ix <- which(fam == g & !is.na(t))
    if (!length(ix)) next
    rk <- qr(em@linfct[ix, , drop = FALSE])$rank
    a <- adjusted_inference(t[ix], inf$df[ix], adjust, conf_level, n_means, rk)
    p[ix] <- a$p
    crit[ix] <- a$crit
  }
  lo <- inf$estimate - crit * inf$std.error
  hi <- inf$estimate + crit * inf$std.error
  est_col <- intersect(c("estimate", "emmean", "prob", "rate", "response", "mean.class"),
                       names(s))[1]
  lcl <- grep("(lower\\.CL|LCL|asymp\\.LCL)$", names(s), value = TRUE)[1]
  ucl <- grep("(upper\\.CL|UCL|asymp\\.UCL)$", names(s), value = TRUE)[1]
  if (response && inherits(model, "glm") && !identical(stats::family(model)$link, "identity")) {
    fam_obj <- stats::family(model)
    s[[est_col]] <- fam_obj$linkinv(inf$estimate)
    s$SE <- inf$std.error * abs(fam_obj$mu.eta(inf$estimate))
    lo <- fam_obj$linkinv(lo)
    hi <- fam_obj$linkinv(hi)
  } else {
    s[[est_col]] <- inf$estimate
    s$SE <- inf$std.error
  }
  s$df <- inf$df
  if (!is.na(lcl)) s[[lcl]] <- lo
  if (!is.na(ucl)) s[[ucl]] <- hi
  stat_col <- intersect(c("t.ratio", "z.ratio"), names(s))[1]
  if (!is.na(stat_col)) {
    s[[stat_col]] <- t
    names(s)[names(s) == stat_col] <- "t.ratio"
  }
  if ("p.value" %in% names(s)) s$p.value <- p
  s
}
