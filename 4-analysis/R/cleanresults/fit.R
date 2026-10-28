# Model fitting ---------------------------------------------------------------
#
# Each internal type is one estimator. "linear", "logistic", "poisson",
# "negbin", and "ordinal" resolve to their mixed version when a random term is
# given; "bayes_*" types fit the same families in brms with or without random
# terms.

.type_aliases <- c(
  lm = "lm", ols = "lm",
  logit = "logit", glm = "logit", binomial = "logit",
  lmer = "lmer", lmm = "lmer", mixed_linear = "lmer",
  glmer = "glmer", glmm = "glmer", mixed_logistic = "glmer", mixed_logit = "glmer",
  poisson_glm = "poisson", glmer_poisson = "poisson_mixed",
  glm.nb = "negbin", nb = "negbin", glmmtmb = "negbin_mixed",
  clm = "ordinal", polr = "ordinal", clmm = "ordinal_mixed",
  bayes_linear = "bayes_linear", bayes_gaussian = "bayes_linear",
  bayes_logistic = "bayes_logistic", bayes_logit = "bayes_logistic",
  bayes_ordinal = "bayes_ordinal", bayes_poisson = "bayes_poisson",
  bayes_negbin = "bayes_negbin"
)

# type: c(fixed version, mixed version)
.type_pairs <- list(
  linear = c("lm", "lmer"), gaussian = c("lm", "lmer"),
  logistic = c("logit", "glmer"),
  poisson = c("poisson", "poisson_mixed"), count = c("poisson", "poisson_mixed"),
  negbin = c("negbin", "negbin_mixed"), negative_binomial = c("negbin", "negbin_mixed"),
  ordinal = c("ordinal", "ordinal_mixed")
)

.type_labels <- c(
  lm = "Linear", logit = "Logistic",
  lmer = "Linear mixed", glmer = "Logistic mixed",
  poisson = "Poisson", poisson_mixed = "Poisson mixed",
  negbin = "Negative binomial", negbin_mixed = "Negative binomial mixed",
  ordinal = "Ordinal (cumulative logit)", ordinal_mixed = "Ordinal mixed",
  bayes_linear = "Bayesian linear", bayes_logistic = "Bayesian logistic",
  bayes_ordinal = "Bayesian ordinal", bayes_poisson = "Bayesian Poisson",
  bayes_negbin = "Bayesian negative binomial"
)

resolve_type <- function(type, has_random) {
  t <- tolower(type)
  if (t %in% names(.type_pairs)) return(.type_pairs[[t]][if (has_random) 2 else 1])
  out <- unname(.type_aliases[t])
  if (is.na(out)) {
    stop(sprintf(paste0(
      "Unknown model type '%s'. Use one of: linear, logistic, poisson, negbin, ordinal ",
      "(mixed automatically with `random`), or bayes_linear, bayes_logistic, ",
      "bayes_ordinal, bayes_poisson, bayes_negbin."), type), call. = FALSE)
  }
  if (is_mixed(out) && !is_bayes(out) && !has_random) {
    stop(sprintf("Model type '%s' needs a `random` term, e.g. random = ~ (1 | id).", type),
         call. = FALSE)
  }
  if (!is_mixed(out) && !is_bayes(out) && has_random) {
    fixed_twin <- names(.type_pairs)[vapply(.type_pairs, function(p) p[1] == out, logical(1))]
    if (length(fixed_twin)) out <- .type_pairs[[fixed_twin[1]]][2]
  }
  out
}

is_bayes <- function(type) startsWith(type, "bayes_")
is_mixed <- function(type) type %in% c("lmer", "glmer", "poisson_mixed", "negbin_mixed",
                                       "ordinal_mixed")
is_logistic <- function(type) type %in% c("logit", "glmer", "bayes_logistic")
is_count <- function(type) type %in% c("poisson", "poisson_mixed", "negbin", "negbin_mixed",
                                       "bayes_poisson", "bayes_negbin")
is_ordinal <- function(type) type %in% c("ordinal", "ordinal_mixed", "bayes_ordinal")
is_linear <- function(type) type %in% c("lm", "lmer", "bayes_linear")

# Coefficients on a log scale that are reported exponentiated, and their name.
ratio_name <- function(type) {
  if (is_logistic(type) || is_ordinal(type)) "OR" else if (is_count(type)) "IRR" else NULL
}

rhs_text <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "formula")) {
    rhs <- x[[length(x)]]
    return(paste(deparse(rhs, width.cutoff = 500L), collapse = " "))
  }
  if (is.character(x)) return(paste(x, collapse = " + "))
  stop("`predictors` and `random` must be a one-sided formula or a character vector.",
       call. = FALSE)
}

build_formula <- function(dv, predictors, random, extra = NULL) {
  rhs <- paste(c(rhs_text(predictors), extra), collapse = " + ")
  rnd <- rhs_text(random)
  if (!is.null(rnd)) rhs <- paste(rhs, "+", rnd)
  stats::as.formula(paste(sprintf("`%s`", dv), "~", rhs), env = globalenv())
}

fit_one <- function(type, formula, data, fit_args = list()) {
  if (type %in% c("ordinal", "ordinal_mixed")) {
    data[[response_name(formula)]] <- as_ordered(data[[response_name(formula)]])
  }
  if (!type %in% c("lm", "logit", "poisson", "negbin", "lmer", "glmer", "poisson_mixed",
                   "negbin_mixed", "ordinal", "ordinal_mixed")) {
    return(fit_bayes(type, c(list(formula = formula, data = data), fit_args)))
  }
  # Build the call with the fitter's name and a `data` symbol evaluated in a small
  # environment, instead of do.call() with the data frame itself: do.call() inlines the
  # whole data set into the model's stored call, so summary()/print() deparse every row
  # (hundreds of MB for a large sample) and the call is unreadable. The formula gets the
  # same environment so tools that recover the data later (emmeans, update) find it.
  env <- new.env(parent = environment(formula) %||% globalenv())
  env$data <- data
  environment(formula) <- env
  fitter <- switch(type,
    lm = quote(stats::lm), logit = quote(stats::glm), poisson = quote(stats::glm),
    negbin = quote(MASS::glm.nb), lmer = quote(lmerTest::lmer), glmer = quote(lme4::glmer),
    poisson_mixed = quote(lme4::glmer), negbin_mixed = quote(glmmTMB::glmmTMB),
    ordinal = quote(ordinal::clm), ordinal_mixed = quote(ordinal::clmm))
  family <- switch(type,
    logit = , glmer = quote(stats::binomial()), poisson = , poisson_mixed = quote(stats::poisson()),
    negbin_mixed = quote(glmmTMB::nbinom2()), NULL)
  switch(type,
    negbin = need_pkg("MASS"), lmer = { need_pkg("lme4"); need_pkg("lmerTest") },
    glmer = , poisson_mixed = need_pkg("lme4"), negbin_mixed = need_pkg("glmmTMB"),
    ordinal = , ordinal_mixed = need_pkg("ordinal"), NULL)
  cl <- as.call(c(list(fitter, formula = formula, data = quote(data)),
                  if (!is.null(family)) list(family = family), fit_args))
  # Poisson quasi-ML also suits non-integer skewed outcomes (ratio of means)
  if (type == "poisson") suppressWarnings(eval(cl, env)) else eval(cl, env)
}

fit_bayes <- function(type, args) {
  need_pkg("brms")
  fam <- switch(type,
    bayes_linear = brms::brmsfamily("gaussian"),
    bayes_logistic = brms::brmsfamily("bernoulli"),
    bayes_ordinal = brms::brmsfamily("cumulative", "logit"),
    bayes_poisson = brms::brmsfamily("poisson"),
    bayes_negbin = brms::brmsfamily("negbinomial"))
  if (type == "bayes_ordinal") {
    y <- response_name(args$formula)
    args$data[[y]] <- as_ordered(args$data[[y]])
  }
  defaults <- list(family = fam, refresh = 0, silent = 2, seed = 2026)
  args <- c(args, defaults[setdiff(names(defaults), names(args))])
  do.call(brms::brm, args)
}

response_name <- function(formula) all.vars(formula[[2]])[1]

as_ordered <- function(y) {
  if (is.ordered(y)) return(y)
  if (is.factor(y)) return(factor(y, levels = levels(y), ordered = TRUE))
  factor(y, levels = sort(unique(y)), ordered = TRUE)
}

need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for this model type. install.packages('%s')",
                 pkg, pkg), call. = FALSE)
  }
}

grouping_factors <- function(model) {
  if (inherits(model, "merMod")) return(names(lme4::getME(model, "flist")))
  if (inherits(model, "clmm")) return(names(model$dims$nlev.gf))
  if (inherits(model, "brmsfit")) return(names(brms::ngrps(model)))
  if (inherits(model, "glmmTMB")) return(names(model$modelInfo$reTrms$cond$flist))
  character()
}

# Fixed-effects design pieces built from the formula and the analysis data, so
# every model class (lm to brms) is labelled the same way.
model_parts <- function(formula, data) {
  # nobars() moved from lme4 to reformulas; lme4's copy now warns on every call
  fixed <- if (requireNamespace("reformulas", quietly = TRUE)) {
    reformulas::nobars(formula)
  } else if (requireNamespace("lme4", quietly = TRUE)) {
    lme4::nobars(formula)
  } else {
    formula
  }
  mf <- stats::model.frame(fixed, data)
  tt <- stats::terms(mf)
  X <- stats::model.matrix(tt, mf)
  list(X = X, terms = tt, mf = mf)
}
