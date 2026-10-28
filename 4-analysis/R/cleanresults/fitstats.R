# Model-level statistics -----------------------------------------------------
#
# Linear and logistic mixed models: random-effect variances, adjusted ICC, and
# Nakagawa marginal / conditional R2 (random-intercept models; slopes leave ICC
# and R2 blank). Logistic mixed models use the latent-scale residual variance
# pi^2 / 3. Other mixed models report their variance components and group
# counts. Bayesian models report the Bayes R2 (posterior median).

fit_stats <- function(model, type, lab_fun = make_labeller(clean = FALSE), parts = NULL) {
  out <- list(random = list(), fit = list())
  n <- if (!is.null(parts)) nrow(parts$mf) else stats::nobs(model)
  out$fit[["Observations"]] <- n

  if (type %in% c("lmer", "glmer")) {
    out <- merMod_stats(model, type, lab_fun, out)
  } else if (type %in% c("poisson_mixed", "negbin_mixed", "ordinal_mixed") || is_bayes(type)) {
    vc <- random_components(model, type)
    for (i in seq_len(nrow(vc))) {
      nm <- if (vc$term[i] == "(Intercept)") {
        sprintf("\u03c4\u2080\u2080 (%s)", lab_fun$var(vc$group[i]))
      } else {
        sprintf("\u03c4 (%s: %s)", lab_fun$var(vc$group[i]), lab_fun$var(vc$term[i]))
      }
      out$random[[nm]] <- vc$variance[i]
    }
    ng <- group_counts(model, type)
    for (g in names(ng)) out$random[[sprintf("N (%s)", lab_fun$var(g))]] <- unname(ng[[g]])
  } else if (type == "lm") {
    out$fit[["R\u00b2"]] <- summary(model)$r.squared
  } else if (type == "logit") {
    p <- stats::fitted(model)
    y <- stats::model.response(stats::model.frame(model))
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1])
    out$fit[["Tjur R\u00b2"]] <- mean(p[y == 1]) - mean(p[y == 0])
  }
  if (is_bayes(type)) {
    if (type %in% c("bayes_linear", "bayes_logistic", "bayes_poisson", "bayes_negbin")) {
      r2 <- tryCatch(brms::bayes_R2(model, robust = TRUE)[1, "Estimate"], error = function(e) NA)
      if (!is.na(r2)) out$fit[["Bayes R\u00b2"]] <- r2
    }
  } else {
    aic <- tryCatch(stats::AIC(model), error = function(e) NA_real_)
    if (is.finite(aic)) out$fit[["AIC"]] <- aic   # none for quasi-likelihood fits
  }
  out
}

merMod_stats <- function(model, type, lab_fun, out) {
  vc <- as.data.frame(lme4::VarCorr(model))
  vc <- vc[is.na(vc$var2) & vc$grp != "Residual", , drop = FALSE]
  sigma2 <- if (type == "lmer") stats::sigma(model)^2 else pi^2 / 3
  out$random[["\u03c3\u00b2"]] <- sigma2
  for (i in seq_len(nrow(vc))) {
    nm <- if (vc$var1[i] == "(Intercept)") {
      sprintf("\u03c4\u2080\u2080 (%s)", lab_fun$var(vc$grp[i]))
    } else {
      sprintf("\u03c4 (%s: %s)", lab_fun$var(vc$grp[i]), lab_fun$var(vc$var1[i]))
    }
    out$random[[nm]] <- vc$vcov[i]
  }
  intercept_only <- all(vc$var1 == "(Intercept)")
  tau <- sum(vc$vcov)
  if (intercept_only) out$random[["ICC"]] <- tau / (tau + sigma2)
  ng <- lme4::ngrps(model)
  for (g in names(ng)) out$random[[sprintf("N (%s)", lab_fun$var(g))]] <- unname(ng[[g]])
  if (intercept_only) {
    X <- lme4::getME(model, "X")
    var_f <- stats::var(as.vector(X %*% lme4::fixef(model)))
    total <- var_f + tau + sigma2
    out$fit[["Marginal R\u00b2"]] <- var_f / total
    out$fit[["Conditional R\u00b2"]] <- (var_f + tau) / total
  }
  out
}

# Variance components (no correlations) as group / term / variance.
random_components <- function(model, type) {
  empty <- data.frame(group = character(), term = character(), variance = numeric())
  vcl <- tryCatch({
    if (type %in% c("poisson_mixed")) {
      v <- as.data.frame(lme4::VarCorr(model))
      v <- v[is.na(v$var2), , drop = FALSE]
      return(data.frame(group = v$grp, term = v$var1, variance = v$vcov))
    }
    if (type == "negbin_mixed") glmmTMB::VarCorr(model)$cond
    else if (type == "ordinal_mixed") ordinal::VarCorr(model)
    else brms::VarCorr(model, summary = TRUE, robust = TRUE)
  }, error = function(e) NULL)
  if (is.null(vcl)) return(empty)
  if (is_bayes(type)) {
    rows <- lapply(setdiff(names(vcl), "residual__"), function(g) {
      sdv <- vcl[[g]]$sd
      if (is.null(sdv)) return(NULL)
      data.frame(group = g, term = sub("^Intercept$", "(Intercept)", rownames(sdv)),
                 variance = sdv[, "Estimate"]^2)
    })
    return(do.call(rbind, c(list(empty), rows)))
  }
  rows <- lapply(names(vcl), function(g) {
    m <- as.matrix(vcl[[g]])
    data.frame(group = g, term = rownames(m) %||% "(Intercept)", variance = diag(m))
  })
  do.call(rbind, c(list(empty), rows))
}

group_counts <- function(model, type) {
  tryCatch({
    if (type == "poisson_mixed") return(lme4::ngrps(model))
    if (type == "negbin_mixed") {
      return(vapply(model$modelInfo$reTrms$cond$flist, nlevels, numeric(1)))
    }
    if (type == "ordinal_mixed") return(model$dims$nlev.gf)
    unlist(brms::ngrps(model))
  }, error = function(e) numeric())
}
