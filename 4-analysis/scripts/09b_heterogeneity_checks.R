# 09b. Causal-forest heterogeneity checks
#
# Companion to 09_heterogeneity_forests.Rmd. For each of the 8 outcome x study x treatment-arm contrasts it reports:
#   - the average treatment effect (ATE) and its SE                        (grf::average_treatment_effect)
#   - the spread of the forest's individual-level predictions (tau-hat)
#   - grf's calibration test: mean.forest.prediction (should be ~1 if the ATE is recovered) and
#     differential.forest.prediction (the omnibus heterogeneity test; > 0 would indicate real heterogeneity;
#     grf reports a ONE-SIDED p, so p ~ 1 means the coefficient is <= 0)
#   - RATE / AUTOC: does ranking held-out participants by the forest's predicted benefit sort their realized
#     effects? (train on a random half, evaluate on the other half)
#   - the number of covariates significant in the best linear projection (BLP)
#   - simple pre-specified moderator tests (HC1): treatment x baseline score and treatment x die-hard
#     (baseline claim certainty / 2020 denialism == 100)
# Results are written to ../results/cf_heterogeneity_checks.rds (and .csv).

suppressMessages({library(tidyverse); library(grf); library(labelled); library(sandwich); library(lmtest)})

local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/09b_heterogeneity_checks.R")
})

analysis_dir <- here::here()
load(file.path(analysis_dir, "data/clean_dfs.RData"))
# NB: df_both is deliberately left as loaded (a rowwise_df), exactly as in 09_heterogeneity_forests.Rmd. Ungrouping it changes the
# factor-level order and therefore the forest's column order, which shifts the estimates slightly (ATE by <= 0.09, omnibus
# p by up to .13) and reshuffles which BLP covariates cross p < .05 - the null conclusions are the same, but this script
# reproduces the numbers reported in the manuscript only when it mirrors 09.

# Reuse 09_heterogeneity_forests.Rmd's predictor lists and helper functions (everything before run_cf_shap()), so the
# covariate set is identical to the SHAP figure's.
L <- readLines(file.path(analysis_dir, "scripts/09_heterogeneity_forests.Rmd"))
chunk <- function(h) { s <- grep(h, L, fixed = TRUE)[1]; e <- s + which(L[(s + 1):length(L)] == "```")[1]; L[(s + 1):(e - 1)] }
eval(parse(text = chunk("```{r labels")))
cf <- chunk("```{r causal forest")
eval(parse(text = cf[1:(grep("^run_cf_shap <- function", cf)[1] - 1)]))

prep <- function(data, outcome, treat, control, predictors) {   # mirrors run_cf_shap()'s data preparation
  d <- data %>% filter(ConditionFeedback %in% c(treat, control)) %>%
    select(all_of(c(outcome, "ConditionFeedback", predictors))) %>% filter(!is.na(.data[[outcome]]))
  y <- d[[outcome]]; if (any(grepl("labelled", class(y)))) y <- labelled::unlabelled(y, user_na_to_na = TRUE)
  y <- if (is.factor(y)) as.numeric(as.character(y)) else as.numeric(y)
  x <- d %>% select(-all_of(outcome)) %>% mutate(across(everything(), coerce_labelled)) %>%
    mutate(across(where(is.character), ~ tidyr::replace_na(.x, "Missing"))) %>% mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), ~ forcats::fct_na_value_to_level(.x, level = "Missing")))
  keep <- !is.na(y) & is.finite(y); y <- y[keep]; x <- x[keep, , drop = FALSE]
  for (col in names(x)[vapply(x, is.numeric, logical(1))]) if (!all(is.na(x[[col]]))) x[[col]][is.na(x[[col]])] <- median(x[[col]], na.rm = TRUE)
  pc <- setdiff(names(x), "ConditionFeedback"); const <- pc[vapply(pc, function(c) length(unique(na.omit(x[[c]]))) < 2, logical(1))]
  if (length(const)) x <- x %>% select(-all_of(const))
  W <- as.integer(x$ConditionFeedback == treat)
  X <- model.matrix(~ 0 + ., data = x %>% select(-ConditionFeedback)); storage.mode(X) <- "double"
  list(X = X, Y = y, W = W)
}

combos <- tribble(~study, ~treat, ~control,
  1, "Belief-Tailored", "Adversarial Dialogue",       1, "Belief & Values-Tailored", "Adversarial Dialogue",
  2, "Belief-Tailored", "Control",                2, "Adversarial Dialogue",     "Control")
outs <- c(ER0Norm_Post = "2020 Denialism", Sureness_Post_1 = "Claim Certainty")
pre  <- c(ER0Norm_Post = "ER0Norm_Pre_center", Sureness_Post_1 = "Sureness_Pre_center")
pre_raw <- c(ER0Norm_Post = "ER0Norm_Pre", Sureness_Post_1 = "Sureness_Pre_1")   # die-hard = raw baseline == 100

rows <- list()
for (o in names(outs)) for (i in seq_len(nrow(combos))) {
  cb <- combos[i, ]; dat <- filter(df_both, Study == cb$study)
  preds <- if (cb$study == 1) cf_predictors_s1 else cf_predictors_s2
  p <- prep(dat, o, cb$treat, cb$control, preds); n <- length(p$Y)
  m <- causal_forest(p$X, p$Y, p$W, tune.parameters = "none", num.trees = 2000, seed = 1234)
  ate <- average_treatment_effect(m); tau <- predict(m)$predictions; cal <- test_calibration(m)
  blp <- try(best_linear_projection(m, A = p$X), silent = TRUE)
  pv <- if (inherits(blp, "try-error")) numeric(0) else blp[setdiff(rownames(blp), "(Intercept)"), "Pr(>|t|)"]
  set.seed(1234); idx <- sample(n, floor(n / 2))                                # RATE: train half -> rank held-out half
  mt <- causal_forest(p$X[idx, ], p$Y[idx], p$W[idx], tune.parameters = "none", num.trees = 2000, seed = 1234)
  me <- causal_forest(p$X[-idx, ], p$Y[-idx], p$W[-idx], tune.parameters = "none", num.trees = 2000, seed = 1234)
  rt <- rank_average_treatment_effect(me, predict(mt, p$X[-idx, ])$predictions, target = "AUTOC")
  dd <- dat %>% filter(ConditionFeedback %in% c(cb$treat, cb$control), !is.na(.data[[o]]))
  dd$W <- as.integer(dd$ConditionFeedback == cb$treat); dd$Y <- as.numeric(dd[[o]]); dd$B <- as.numeric(dd[[pre[[o]]]])
  dd$DH <- as.integer(as.numeric(dd[[pre_raw[[o]]]]) >= 100)
  c1 <- coeftest(lm(Y ~ W * B, dd), vcov. = vcovHC(lm(Y ~ W * B, dd), "HC1"))["W:B", ]
  f2 <- lm(Y ~ W * DH + B, dd); c2 <- coeftest(f2, vcov. = vcovHC(f2, "HC1"))["W:DH", ]
  rows[[length(rows) + 1]] <- tibble(
    outcome = outs[[o]], study = cb$study, contrast = paste(cb$treat, "vs", cb$control), n = n, n_covariates = ncol(p$X),
    ATE = ate[["estimate"]], ATE_se = ate[["std.err"]], tau_sd = sd(tau), tau_min = min(tau), tau_max = max(tau), pct_tau_neg = mean(tau < 0) * 100,
    cal_mean = cal["mean.forest.prediction", "Estimate"], cal_diff = cal["differential.forest.prediction", "Estimate"], cal_diff_p_onesided = cal["differential.forest.prediction", "Pr(>t)"],
    RATE_autoc = rt$estimate, RATE_se = rt$std.err, RATE_p = 2 * pnorm(-abs(rt$estimate / rt$std.err)),
    n_blp_sig = sum(pv < .05, na.rm = TRUE), blp_sig = paste(names(pv)[which(pv < .05)], collapse = "; "),
    int_baseline_b = c1[1], int_baseline_p = c1[4], int_diehard_b = c2[1], int_diehard_se = c2[2], int_diehard_p = c2[4], pct_diehard = mean(dd$DH) * 100)
  message("done: ", outs[[o]], " | ", cb$treat, " vs ", cb$control, " (study ", cb$study, ")")
}
res <- bind_rows(rows)
dir.create(file.path(analysis_dir, "results"), showWarnings = FALSE)
saveRDS(res, file.path(analysis_dir, "results/cf_heterogeneity_checks.rds"))
write_csv(res, file.path(analysis_dir, "results/cf_heterogeneity_checks.csv"))
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = 20, width = 250)
