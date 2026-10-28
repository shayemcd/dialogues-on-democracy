#' Fit regressions for one or more outcomes and collect publication-ready results
#'
#' Fits one model per dependent variable (linear, logistic, Poisson, negative
#' binomial, or ordinal; each also as a mixed model or a Bayesian brms model),
#' computes robust or model-based inference, keeps reference
#' categories as explicit rows, optionally adds pairwise contrasts, and applies
#' multiple-comparison corrections within factors and/or across outcomes.
#' Render the result with [cr_table()] (or just print it in a Quarto chunk),
#' save it with [cr_save()], and cite numbers in text with [cr_inline()].
#'
#' @param data A data frame.
#' @param dvs Named character vector mapping each outcome to a model type, e.g.
#'   `c(trust = "linear", donated = "logistic", liking = "ordinal")`. Types:
#'   `"linear"`, `"logistic"`, `"poisson"` (counts, or Poisson quasi-ML for
#'   skewed non-negative outcomes: ratio of means), `"negbin"` (overdispersed
#'   counts), `"ordinal"` (cumulative logit via `ordinal::clm`; `"polr"` is an
#'   alias, same model). Each becomes its mixed version when `random` is given
#'   (`lmer`, `glmer`, `glmmTMB` negative binomial, `ordinal::clmm`). Bayesian
#'   versions via brms: `"bayes_linear"`, `"bayes_logistic"`, `"bayes_ordinal"`,
#'   `"bayes_poisson"`, `"bayes_negbin"` (with or without `random`; pass priors,
#'   chains, iter, etc. in `fit_args`). An unnamed vector of outcome names is
#'   allowed when `type` is supplied.
#' @param predictors One-sided formula (`~ condition + age`) or character
#'   vector of predictor terms shared by all outcomes.
#' @param random Random-effects term(s) for mixed models, e.g. `~ (1 | id)`.
#' @param formulas Optional named list of full formulas that override
#'   `predictors`/`random` for specific outcomes.
#' @param type Model type used for every outcome when `dvs` is unnamed.
#' @param cluster Column name to cluster robust SEs on. Defaults to the first
#'   grouping factor for mixed models and to none (HC3) for lm/logit.
#' @param se `"robust"` (default; HC3, or CR2 when clustered or mixed) or `"model"`.
#' @param contrasts Pairwise contrasts: `NULL` (none), `TRUE` (every factor
#'   predictor with 3+ levels), or a character vector of factor names.
#' @param contrast_adjust Correction across the pairwise contrasts of each factor
#'   (any `emmeans` adjust method: `"holm"`, `"tukey"`, `"bonferroni"`,
#'   `"fdr"`, `"none"`, ...).
#' @param dv_adjust Correction across outcomes, applied to each row separately
#'   (the same coefficient or contrast across all outcomes is one family). Any
#'   [stats::p.adjust()] method; `"none"` by default.
#' @param dv_adjust_terms Variables whose rows get the across-outcome
#'   correction. `NULL` means every non-intercept row.
#' @param exponentiate Report ratios: odds ratios (logistic), cumulative odds
#'   ratios (ordinal), incidence rate ratios (Poisson, negative binomial).
#' @param conf_level Confidence level.
#' @param labels Named vector of display labels for predictors, e.g.
#'   `c(condition = "Framing condition")`. Unlisted names are cleaned
#'   automatically (see `clean_labels`).
#' @param level_labels Display labels for factor levels: a named vector
#'   applied to every factor (`c(GainFraming = "Gain frame")`) or a list per
#'   factor (`list(condition = c(GainFraming = "Gain frame"))`).
#' @param dv_labels Named vector of display labels for outcomes.
#' @param clean_labels Tidy names that have no explicit label:
#'   `GainFraming` -> "Gain framing", `age_years` -> "Age years". See
#'   [clean_label()].
#' @param label_case Case for cleaned labels: `"sentence"`, `"title"`, or `"asis"`.
#' @param intercept Show the intercept row.
#' @param baseline Pre-treatment measure of each outcome, added as a covariate
#'   centered on the analysis sample (ANCOVA): a named vector
#'   `c(trust = "trust_pre")`, or one column for every outcome.
#' @param lin Treatment variable(s) to interact with the centered baseline (the
#'   Lin, 2013 estimator); treatment rows then estimate the effect at the mean
#'   baseline.
#' @param contrast_by A moderator: pairwise contrasts are computed within each
#'   of its levels (simple effects), adjusted within each level.
#' @param fit_args List of extra arguments passed to every model fitter, e.g.
#'   `list(control = lme4::glmerControl(optimizer = "bobyqa"))`.
#'
#' @return An object of class `clean_results`.
#' @export
#' @examples
#' \dontrun{
#' res <- clean_regress(
#'   d,
#'   dvs = c(trust = "linear", donated = "logistic"),
#'   predictors = ~ condition + age,
#'   random = ~ (1 | id),
#'   contrasts = "condition",
#'   dv_adjust = "holm"
#' )
#' res
#' cr_save(res, "tables/table2.docx")
#' }
clean_regress <- function(data, dvs, predictors = NULL, random = NULL, formulas = NULL,
                          type = NULL, cluster = NULL, se = c("robust", "model"),
                          vcov_type = c("HC1", "HC3", "HC0", "HC2", "HC4"),
                          contrasts = NULL, contrast_adjust = "tukey",
                          dv_adjust = "none", dv_adjust_terms = NULL,
                          exponentiate = TRUE, conf_level = 0.95,
                          labels = NULL, level_labels = NULL, dv_labels = NULL,
                          clean_labels = TRUE, label_case = "sentence", intercept = TRUE,
                          baseline = NULL, lin = NULL, contrast_by = NULL,
                          fit_args = list()) {
  se <- match.arg(se)
  vcov_type <- match.arg(vcov_type)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  dv_adjust <- match.arg(dv_adjust, stats::p.adjust.methods)
  dvs <- normalize_dvs(dvs, type)
  if (is.null(predictors) && is.null(formulas)) {
    stop("Supply `predictors` (or per-outcome `formulas`).", call. = FALSE)
  }
  if (!is.null(cluster) && !cluster %in% names(data)) {
    stop(sprintf("Cluster column '%s' not found in `data`.", cluster), call. = FALSE)
  }

  if (length(lin)) {
    if (is.null(baseline)) stop("`lin` needs a `baseline` to interact with.", call. = FALSE)
    if (!is.null(predictors)) {
      pv <- all.vars(stats::as.formula(paste("~", rhs_text(predictors))))
      if (!all(lin %in% pv)) {
        stop(sprintf("`lin` variable(s) not among the predictors: %s",
                     paste(setdiff(lin, pv), collapse = ", ")), call. = FALSE)
      }
    }
  }
  if (!is.null(baseline)) {
    if (is.null(names(baseline))) baseline <- stats::setNames(rep(baseline, length(dvs)), names(dvs))
    if (!".baseline" %in% names(labels)) labels <- c(labels, .baseline = "Baseline (centered)")
  }
  lab_fun <- make_labeller(labels, level_labels, clean_labels, label_case)
  models <- list(); stats_list <- list(); est_list <- list(); meta <- list()
  vcovs <- list(); datas <- list()
  for (dv in names(dvs)) {
    base_col <- if (!is.null(baseline)) unname(baseline[dv]) else NA_character_
    has_base <- !is.na(base_col)
    extra <- if (has_base) c(".baseline", if (length(lin)) paste0(lin, ":.baseline")) else NULL
    f <- if (!is.null(formulas[[dv]])) {
      if (has_base) stats::update(formulas[[dv]], stats::as.formula(
        paste(". ~ . +", paste(extra, collapse = " + ")))) else formulas[[dv]]
    } else build_formula(dv, predictors, random, extra)
    has_random <- any(grepl("|", deparse(f, width.cutoff = 500L), fixed = TRUE))
    mtype <- resolve_type(dvs[[dv]], has_random)

    vars <- unique(c(setdiff(all.vars(f), ".baseline"), cluster, if (has_base) base_col))
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars)) {
      stop(sprintf("Variables not found in `data`: %s", paste(missing_vars, collapse = ", ")),
           call. = FALSE)
    }
    ok <- stats::complete.cases(data[vars])
    if (is.numeric(data[[dv]])) ok <- ok & is.finite(data[[dv]])
    d_i <- data[ok, , drop = FALSE]
    # Baseline centered in each model's own analysis sample, so the treatment
    # coefficients are effects at the mean baseline (ANCOVA; Lin, 2013 with `lin`)
    if (has_base) d_i$.baseline <- d_i[[base_col]] - mean(d_i[[base_col]])
    m <- fit_one(mtype, f, d_i, fit_args)
    parts <- model_parts(f, d_i)

    cluster_name <- cluster
    if (is.null(cluster_name) && is_mixed(mtype)) cluster_name <- grouping_factors(m)[1]
    cluster_vec <- if (!is.null(cluster_name)) d_i[[cluster_name]] else NULL

    V <- model_vcov(m, mtype, se, cluster_vec, vcov_type)
    coefs <- infer_coefs(m, mtype, se, V, conf_level)
    map <- term_map(parts, lab_fun = lab_fun, intercept = intercept && !is_ordinal(mtype))
    fixed <- merge(map, coefs, by = "term", all.x = TRUE, sort = FALSE)
    fixed <- fixed[match(map$key, fixed$key), , drop = FALSE]
    fixed$section <- "fixed"
    fixed$by_level <- NA_character_
    exp_i <- exponentiate && !is.null(ratio_name(mtype))
    if (exp_i) fixed <- exponentiate_rows(fixed)

    model_vars <- attr(parts$terms, "term.labels")
    if (!is.null(contrast_by) && !contrast_by %in% model_vars) {
      stop(sprintf("`contrast_by` = '%s' must be a predictor in the model for '%s'.",
                   contrast_by, dv), call. = FALSE)
    }
    if (length(lin) && !all(lin %in% model_vars)) {
      stop(sprintf("`lin` variable(s) not among the predictors: %s",
                   paste(setdiff(lin, model_vars), collapse = ", ")), call. = FALSE)
    }
    cvars <- contrast_vars(contrasts, parts)
    ctr <- pairwise_rows(m, mtype, d_i, cvars, V, se, contrast_adjust, conf_level,
                         exp_i, lab_fun, mf = parts$mf, by = contrast_by)
    est <- rbind(fixed[, names(ctr %||% fixed)], ctr)
    est$log_scale <- exp_i
    est$ci_level <- ci_levels(est, conf_level)
    est <- add_std_diff(est, m, mtype)
    est$dv <- dv
    est$p.within <- est$p.value
    est_list[[dv]] <- est

    models[[dv]] <- m
    vcovs[dv] <- list(V)
    datas[[dv]] <- d_i
    stats_list[[dv]] <- fit_stats(m, mtype, lab_fun, parts)
    meta[[dv]] <- list(
      type = mtype, exponentiate = exp_i, est_name = if (exp_i) ratio_name(mtype) else "b",
      bayes = is_bayes(mtype), baseline = if (has_base) base_col else NULL,
      cluster = if (robust_available(mtype) && se == "robust") cluster_name else NULL,
      se_text = se_method_text(mtype, se,
                               if (!is.null(cluster_name)) lab_fun$var(cluster_name),
                               vcov_type),
      robust_fallback = se == "robust" && !robust_available(mtype) && !is_bayes(mtype),
      contrast_vars = cvars, contrast_by = contrast_by, n = nrow(parts$mf)
    )
  }

  estimates <- do.call(rbind, est_list)
  rownames(estimates) <- NULL
  estimates <- adjust_across_dvs(estimates, dv_adjust, dv_adjust_terms, length(dvs))

  dv_lab <- stats::setNames(vapply(names(dvs), lab_fun$var, ""), names(dvs))
  if (!is.null(dv_labels)) dv_lab[intersect(names(dv_labels), names(dvs))] <-
    dv_labels[intersect(names(dv_labels), names(dvs))]

  structure(list(
    estimates = estimates,
    models = models,
    vcov = vcovs,
    data = datas,
    stats = stats_list,
    meta = meta,
    settings = list(
      dvs = names(dvs), dv_labels = dv_lab, se = se, conf_level = conf_level,
      contrast_adjust = contrast_adjust, dv_adjust = dv_adjust,
      dv_adjust_terms = dv_adjust_terms, lin = lin,
      var_labels = vapply(unique(c(dv_adjust_terms, character())), lab_fun$var, "")
    )
  ), class = "clean_results")
}

normalize_dvs <- function(dvs, type) {
  if (is.list(dvs)) dvs <- unlist(dvs)
  if (is.null(names(dvs)) || any(names(dvs) == "")) {
    if (is.null(type)) {
      stop("Name each outcome with its model type, e.g. dvs = c(trust = \"linear\"), ",
           "or supply `type`.", call. = FALSE)
    }
    dvs <- stats::setNames(rep(type, length(dvs)), dvs)
  }
  dvs
}

contrast_vars <- function(contrasts, parts) {
  if (is.null(contrasts) || isFALSE(contrasts)) return(character())
  mf <- parts$mf
  labs <- attr(parts$terms, "term.labels")
  if (isTRUE(contrasts)) {
    return(labs[vapply(labs, function(l) {
      l %in% names(mf) && length(factor_levels(mf[[l]])) >= 3
    }, logical(1))])
  }
  bad <- setdiff(contrasts, labs)
  if (length(bad)) {
    warning(sprintf("Contrast variable(s) not in model, skipped: %s", paste(bad, collapse = ", ")),
            call. = FALSE)
  }
  intersect(contrasts, labs)
}

# Each displayed row (same key and section) across outcomes is one family.
adjust_across_dvs <- function(est, method, terms, n_dv) {
  est$p.value <- est$p.within
  est$dv_adjusted <- FALSE
  if (method == "none" || n_dv < 2) return(est)
  eligible <- !is.na(est$p.within) & est$key != "(Intercept)"
  if (!is.null(terms)) eligible <- eligible & est$variable %in% terms
  grp <- paste(est$section, est$key, sep = "|")
  for (g in unique(grp[eligible])) {
    idx <- which(grp == g & eligible)
    est$p.value[idx] <- stats::p.adjust(est$p.within[idx], method = method)
  }
  est$dv_adjusted <- eligible
  # widen the CIs of adjusted rows to match: Bonferroni across the outcomes in
  # each family, on top of any within-factor adjustment already in the CI
  for (g in unique(grp[eligible])) {
    idx <- which(grp == g & eligible)
    if (length(idx) < 2) next
    new_level <- 1 - (1 - est$ci_level[idx]) / length(idx)
    est[idx, ] <- rescale_ci(est[idx, , drop = FALSE], new_level)
  }
  est
}

# The confidence level each row's CI actually has. Coefficient rows use
# `conf_level`; contrast CIs come from emmeans already adjusted within the
# factor (Bonferroni for Holm, Tukey's studentized range for Tukey), so their
# per-comparison level is read back from the interval itself.
ci_levels <- function(est, conf_level) {
  lev <- rep(conf_level, nrow(est))
  ctr <- which(est$section == "contrast" & est$row_type == "level" & is.na(est$pd))
  if (length(ctr)) {
    q <- ci_q(est[ctr, , drop = FALSE])
    df <- ifelse(is.finite(est$df[ctr]), est$df[ctr], Inf)
    lev[ctr] <- ifelse(is.na(q), conf_level, 2 * stats::pt(q, df) - 1)
  }
  lev
}

# Critical value implied by a row's CI: half-width / SE, on the log scale for
# odds ratios (where std.error is the delta-method SE of the ratio).
ci_q <- function(r) {
  # computed per branch: ifelse() would evaluate log() on the linear rows too,
  # where a negative estimate gives a spurious "NaNs produced" warning
  log_scale <- r$log_scale %in% TRUE
  q <- rep(NA_real_, nrow(r))
  q[log_scale] <- log(r$conf.high[log_scale] / r$estimate[log_scale]) /
    (r$std.error[log_scale] / r$estimate[log_scale])
  q[!log_scale] <- (r$conf.high[!log_scale] - r$estimate[!log_scale]) /
    r$std.error[!log_scale]
  q
}

rescale_ci <- function(r, new_level) {
  q_old <- ci_q(r)
  df <- ifelse(is.finite(r$df), r$df, Inf)
  q_new <- stats::qt(1 - (1 - new_level) / 2, df)
  k <- q_new / q_old
  ok <- is.finite(k)
  log_scale <- r$log_scale %in% TRUE
  lo <- ifelse(log_scale, r$estimate * (r$conf.low / r$estimate)^k,
               r$estimate - (r$estimate - r$conf.low) * k)
  hi <- ifelse(log_scale, r$estimate * (r$conf.high / r$estimate)^k,
               r$estimate + (r$conf.high - r$estimate) * k)
  if ("es" %in% names(r)) {
    has_es <- ok & !is.na(r$es) & r$estimate != 0
    sd_ratio <- r$es / r$estimate
    r$es.low[has_es] <- (lo * sd_ratio)[has_es]
    r$es.high[has_es] <- (hi * sd_ratio)[has_es]
  }
  r$conf.low[ok] <- lo[ok]
  r$conf.high[ok] <- hi[ok]
  r$ci_level[ok] <- new_level[ok]
  r
}

# Standardized mean differences (Cohen's d) for linear models.
# lm: the difference divided by the pooled within-group SD of the raw outcome
#   across that factor's levels, so covariates don't inflate d (the residual SD
#   shrinks as covariates explain variance; Lakens, 2013).
# lmer: divided by the total SD of one observation: sqrt(residual variance +
#   the variance the random effects add, slopes included) (Westfall, Kenny, &
#   Judd, 2014; Brysbaert & Stevens, 2018).
std_sd <- function(model, type, variable = NULL) {
  if (type == "lm") {
    if (is.null(variable)) return(NA_real_)
    mf <- stats::model.frame(model)
    if (!variable %in% names(mf)) return(NA_real_)
    y <- stats::model.response(mf)
    g <- as.character(mf[[variable]])
    ok <- !is.na(y) & !is.na(g)
    y <- y[ok]; g <- g[ok]
    n_g <- tapply(y, g, length)
    v_g <- tapply(y, g, stats::var)
    keep <- n_g > 1
    return(sqrt(sum((n_g[keep] - 1) * v_g[keep]) / (sum(n_g[keep]) - sum(keep))))
  }
  if (type == "lmer") return(sqrt(stats::sigma(model)^2 + random_variance(model)))
  NA_real_
}

# Average variance the random effects add to one observation: mean over rows
# of z' Sigma z for each grouping term, so random slopes (and their
# covariances) count in proportion to the predictor values in the data
# (Johnson, 2014). With random intercepts only this is the sum of the
# intercept variances.
random_variance <- function(model) {
  vc <- lme4::VarCorr(model)
  mm <- lme4::getME(model, "mmList")
  total <- 0
  for (k in seq_along(vc)) {
    Sigma <- as.matrix(vc[[k]])
    Z <- as.matrix(mm[[k]])
    Z <- Z[, colnames(Sigma), drop = FALSE]
    total <- total + mean(rowSums((Z %*% Sigma) * Z))
  }
  total
}

add_std_diff <- function(est, model, type) {
  est$es <- est$es.low <- est$es.high <- NA_real_
  if (!type %in% c("lm", "lmer")) return(est)
  ok <- which(est$row_type == "level" & !est$is_ref)
  for (i in ok) {
    sdv <- std_sd(model, type, est$variable[i])
    if (is.na(sdv) || sdv <= 0) next
    est$es[i] <- est$estimate[i] / sdv
    est$es.low[i] <- est$conf.low[i] / sdv
    est$es.high[i] <- est$conf.high[i] / sdv
  }
  est
}

`%||%` <- function(a, b) if (is.null(a)) b else a
