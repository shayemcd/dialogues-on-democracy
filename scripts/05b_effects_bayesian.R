# 05b_effects_bayesian.R
# Bayesian censored (Tobit) regressions (weakly informative priors) of the two primary outcomes
# on condition, estimated separately for Study 1 and Study 2.
# Accounts for left-censoring at 0 and right-censoring at 100 on the bounded outcome scales.
# Saves posterior summaries to results/bayes_models.rds (read by 5-disseminate/supplement.qmd, Table D5).

suppressMessages({
  library(tidyverse)
  library(brms)
  library(bayestestR)
  library(sjPlot)
})

local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/05b_effects_bayesian.R")
})

analysis_dir <- if (file.exists(file.path(here::here(), "data", "clean_dfs.RData"))) here::here() else file.path(here::here(), "4-analysis")
load(file.path(analysis_dir, "data/clean_dfs.RData"))

# Prepare Study 1 analytic sample
df_s1 <- df_consp %>%
  ungroup() %>%
  mutate(
    cens_er0  = case_when(ER0Norm_Post <= 0 ~ "left", ER0Norm_Post >= 100 ~ "right", TRUE ~ "none"),
    cens_sure = case_when(Sureness_Post_1 <= 0 ~ "left", Sureness_Post_1 >= 100 ~ "right", TRUE ~ "none")
  )
df_s1$ConditionFeedback <- relevel(factor(as.character(df_s1$ConditionFeedback),
  levels = c("Adversarial Dialogue", "Belief & Values-Tailored", "Belief-Tailored")), ref = "Adversarial Dialogue")
df_s1$ConditionChat <- relevel(factor(as.character(df_s1$ConditionChat),
  levels = c("Chat", "Qualtrics")), ref = "Chat")

# Prepare Study 2 analytic sample
df_s2 <- df_consp_2 %>%
  ungroup() %>%
  mutate(
    cens_er0  = case_when(ER0Norm_Post <= 0 ~ "left", ER0Norm_Post >= 100 ~ "right", TRUE ~ "none"),
    cens_sure = case_when(Sureness_Post_1 <= 0 ~ "left", Sureness_Post_1 >= 100 ~ "right", TRUE ~ "none")
  )
df_s2$ConditionFeedback <- relevel(factor(as.character(df_s2$ConditionFeedback),
  levels = c("Control", "Adversarial Dialogue", "Belief-Tailored")), ref = "Control")

# Priors & MCMC settings
# Weakly informative priors calibrated to the 0-100 outcome scale: Intercept centered near sample baseline (~80), weakly regularizing normal(0, 10) on condition slopes, and half-Student-t on residual SD covering empirical SD (~16-20).
priors <- c(
  set_prior("student_t(3, 80, 25)", class = "Intercept"),
  set_prior("normal(0, 10)", class = "b"),
  set_prior("student_t(3, 0, 25)", class = "sigma")
)

settings <- list(chains = 4, iter = 2000, warmup = 1000, seed = 2025, adapt_delta = 0.95)

# Initialization function to prevent log(0) underflow in normal_lccdf on censored boundary observations
init_fun <- function() {
  list(
    Intercept = runif(1, 75, 85),
    sigma = runif(1, 15, 25)
  )
}

fit_cens <- function(formula, data) {
  brm(
    formula,
    data = data,
    family = gaussian(),
    prior = priors,
    init = init_fun,
    chains = settings$chains,
    iter = settings$iter,
    warmup = settings$warmup,
    cores = min(4, parallel::detectCores()),
    seed = settings$seed,
    control = list(adapt_delta = settings$adapt_delta),
    silent = 2,
    refresh = 0
  )
}


# --- Fit Study 1 Models ---
cat("Fitting Study 1: 2020 Denialism...\n")
fit_er0_s1  <- fit_cens(ER0Norm_Post | cens(cens_er0) ~ ER0Norm_Pre_center + ConditionFeedback + ConditionChat, df_s1)
fit_er0_s1

cat("Fitting Study 1: Claim Certainty...\n")
fit_sure_s1 <- fit_cens(Sureness_Post_1 | cens(cens_sure) ~ Sureness_Pre_center + ConditionFeedback + ConditionChat, df_s1)
fit_sure_s1


# --- Fit Study 2 Models ---
cat("Fitting Study 2: 2020 Denialism...\n")
fit_er0_s2  <- fit_cens(ER0Norm_Post | cens(cens_er0) ~ ER0Norm_Pre_center + ConditionFeedback, df_s2)
fit_er0_s2

cat("Fitting Study 2: Claim Certainty...\n")
fit_sure_s2 <- fit_cens(Sureness_Post_1 | cens(cens_sure) ~ Sureness_Pre_center + ConditionFeedback, df_s2)
fit_sure_s2


tab <- function(fit, nm, st) {
  brms::posterior_summary(fit, probs = c(0.025, 0.975)) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(grepl("^b_", term)) %>%
    mutate(study = st, model = nm, n = nobs(fit))
}

coef_all <- bind_rows(
  tab(fit_er0_s1,  "2020 Denialism", "Study 1"),
  tab(fit_sure_s1, "Claim Certainty", "Study 1"),
  tab(fit_er0_s2,  "2020 Denialism", "Study 2"),
  tab(fit_sure_s2, "Claim Certainty", "Study 2"))
coef_all



all_rhats <- c(rhat(fit_er0_s1), rhat(fit_sure_s1), rhat(fit_er0_s2), rhat(fit_sure_s2))
max_rhat <- max(all_rhats, na.rm = TRUE)

dir.create(file.path(analysis_dir, "results"), showWarnings = FALSE)

saveRDS(list(
  coef = coef_all,
  max_rhat = max_rhat,
  settings = settings
), file.path(analysis_dir, "results/bayes_models.rds"))

cat("Saved results/bayes_models.rds; max Rhat:", max_rhat, "\n")
