# Coefficient inference -------------------------------------------------------
#
# Defaults (se = "robust"):
#   lm, logit, poisson, negbin, no cluster -> HC3 (sandwich); t (lm) or z
#   lm, logit, poisson, negbin, cluster    -> CR2 (clubSandwich), Satterthwaite df
#   lmer                                   -> CR2 clustered on `cluster` (default:
#                                             first grouping factor), Satterthwaite df
#   ordinal (clm)                          -> sandwich (HC0) or clustered (vcovCL)
#   glmer, poisson/negbin/ordinal mixed    -> model-based Wald z; no small-sample
#                                             cluster-robust estimator exists for
#                                             them, so the table notes the fallback
#   bayes_*                                -> posterior median, MAD, equal-tailed
#                                             credible interval, probability of direction
# se = "model" uses each model's own SEs (Satterthwaite df for lmer).

robust_available <- function(type) type %in% c("lm", "logit", "poisson", "negbin", "lmer",
                                                "ordinal")

model_vcov <- function(model, type, se, cluster_vec, vcov_type = "HC1") {
  if (is_bayes(type)) return(NULL)
  if (se == "robust" && robust_available(type)) {
    if (type == "ordinal") {
      if (is.null(cluster_vec)) return(sandwich::sandwich(model))
      return(sandwich::vcovCL(model, cluster = cluster_vec, type = vcov_type))
    }
    if (is.null(cluster_vec) && type != "lmer") {
      return(sandwich::vcovHC(model, type = vcov_type))
    }
    return(clubSandwich::vcovCR(model, cluster = cluster_vec, type = "CR2"))
  }
  if (inherits(model, "glmmTMB")) return(as.matrix(stats::vcov(model)$cond))
  as.matrix(stats::vcov(model))
}

fixed_coefs <- function(model) {
  if (inherits(model, "merMod")) return(lme4::fixef(model))
  if (inherits(model, "glmmTMB")) return(glmmTMB::fixef(model)$cond)
  if (inherits(model, c("clm", "clmm"))) return(model$beta)
  stats::coef(model)
}

infer_coefs <- function(model, type, se, V, conf_level) {
  if (is_bayes(type)) return(infer_bayes(model, conf_level))
  est <- fixed_coefs(model)
  est <- est[!is.na(est)]
  use_robust <- se == "robust" && robust_available(type)

  if (use_robust && inherits(V, "clubSandwich")) {
    ct <- clubSandwich::coef_test(model, vcov = V, test = "Satterthwaite")
    out <- data.frame(
      term = as.character(ct$Coef), estimate = ct$beta, std.error = ct$SE,
      statistic = ct$tstat, df = ct$df_Satt, p.value = ct$p_Satt,
      stringsAsFactors = FALSE
    )
  } else {
    se_vec <- sqrt(diag(as.matrix(V))[names(est)])
    stat <- est / se_vec
    df <- switch(type,
      lm    = rep(stats::df.residual(model), length(est)),
      lmer  = if (use_robust) rep(Inf, length(est)) else lmer_satt_df(model)[names(est)],
      rep(Inf, length(est))
    )
    p <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
    out <- data.frame(
      term = names(est), estimate = unname(est), std.error = unname(se_vec),
      statistic = unname(stat), df = unname(df), p.value = unname(p),
      stringsAsFactors = FALSE
    )
  }
  crit <- stats::qt(1 - (1 - conf_level) / 2, df = out$df)
  out$conf.low <- out$estimate - crit * out$std.error
  out$conf.high <- out$estimate + crit * out$std.error
  out$pd <- NA_real_
  out
}

# Posterior summaries: median, MAD as the SE analogue, equal-tailed interval,
# and the probability of direction (share of the posterior with the median's sign).
infer_bayes <- function(model, conf_level) {
  need_pkg("bayestestR")
  dp <- as.data.frame(bayestestR::describe_posterior(
    model, effects = "fixed", component = "conditional", centrality = "median",
    dispersion = TRUE, ci = conf_level, ci_method = "eti", test = "p_direction"))
  dp <- dp[!grepl("^(sigma|shape|disc|Intercept\\[)", dp$Parameter), , drop = FALSE]
  term <- sub("^b_", "", dp$Parameter)
  term[term == "Intercept"] <- "(Intercept)"
  data.frame(term = term, estimate = dp$Median, std.error = dp$MAD,
             statistic = NA_real_, df = NA_real_, p.value = NA_real_,
             conf.low = dp$CI_low, conf.high = dp$CI_high, pd = dp$pd,
             stringsAsFactors = FALSE)
}

lmer_satt_df <- function(model) {
  cf <- summary(model)$coefficients
  if ("df" %in% colnames(cf)) stats::setNames(cf[, "df"], rownames(cf))
  else stats::setNames(rep(Inf, nrow(cf)), rownames(cf))
}

# Exponentiate estimate and CI; SE becomes the delta-method SE of the ratio.
exponentiate_rows <- function(df) {
  df$std.error <- exp(df$estimate) * df$std.error
  df$estimate <- exp(df$estimate)
  df$conf.low <- exp(df$conf.low)
  df$conf.high <- exp(df$conf.high)
  df
}

se_method_text <- function(type, se, cluster_name, vcov_type = "HC1") {
  if (is_bayes(type)) {
    return("posterior medians with equal-tailed credible intervals (brms)")
  }
  if (se != "robust") {
    return(if (type == "lmer") "model-based SEs with Satterthwaite df" else "model-based SEs")
  }
  if (!robust_available(type)) return("model-based (Wald) SEs")
  if (type == "ordinal") {
    return(if (is.null(cluster_name)) "robust (sandwich) SEs" else
      sprintf("cluster-robust SEs clustered by %s", cluster_name))
  }
  if (type != "lmer" && is.null(cluster_name)) return(paste0(vcov_type, " robust SEs"))
  sprintf("CR2 cluster-robust SEs clustered by %s with Satterthwaite df", cluster_name)
}
