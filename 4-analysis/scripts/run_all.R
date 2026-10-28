# Runs the analysis scripts in manuscript order, each in its own R session. From R opened in 4-analysis/ or 4-analysis/scripts/:
#   source("scripts/run_all.R")        # or source("run_all.R") from scripts/
#
# Steps are grouped. By default only the group "" runs (about 3 minutes); add groups to `include` for the rest:
#   "slow"  Bayesian models, causal forests and mediation (the mediation bootstrap alone takes over an hour)
#   "llm"   GPT coding of themes, strategies and nuance (needs OPENAI_API_KEY; the coded output is already in data/)
#   "raw"   cleaning (runs 01_cleaning.Rmd using shareable raw data; recreates data/clean_dfs.RData)
# The BERTopic script (02d_bertopic.py) is run separately with Python.
include <- c("")

steps <- c(
  "01_cleaning.Rmd"                = "raw",   # Method: sample
  "02a_llm_theme_coding.R"         = "llm",   # derived measures
  "02b_llm_strategy_coding.R"      = "llm",
  "02c_llm_nuance_coding.R"        = "llm",
  "03a_method_descriptives.R"      = "",      # Method: participants, attrition, conversation length (results/descriptives.rds)
  "03b_method_checks.Rmd"          = "",      # Method: attrition tests, speeders, evidence counts, power
  "04_measures.Rmd"                = "",      # Appendix B: factor analysis, correlations
  "05_results_effects.Rmd"         = "",      # Results 1: effects by condition (fig-denialism-changes)
  "05b_effects_bayesian.R"         = "slow",  # Appendix D: Bayesian censored models
  "06_results_themes.Rmd"          = "",      # Results 2: themes and sentiment (fig-themes)
  "07_results_belief_level.Rmd"    = "",      # Results 3: baseline conviction (fig-baseline-conviction)
  "08_results_strategies.Rmd"      = "",      # Results 4: persuasion strategies (fig-strategy-quality-significant)
  "09_heterogeneity_forests.Rmd"   = "slow",  # Discussion: causal forests
  "09b_heterogeneity_checks.R"     = "slow",
  "10_mediation.Rmd"               = "slow",  # Discussion: mediation
  "99_check_manuscript_numbers.R"  = ""       # recomputes every number typed in the manuscript
)

scripts_dir <- local({
  d <- getwd()
  if (file.exists(file.path(d, "scripts", "run_all.R"))) file.path(d, "scripts") else d
})
stopifnot(file.exists(file.path(scripts_dir, "run_all.R")))
owd <- setwd(scripts_dir)
tryCatch({
  for (f in names(steps)[steps %in% include]) {
    message("== ", f, " (", format(Sys.time(), "%H:%M:%S"), ")")
    expr <- if (grepl("\\.Rmd$", f)) sprintf("rmarkdown::render('%s', quiet = TRUE)", f) else sprintf("source('%s')", f)
    status <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(expr)), stdout = FALSE)
    if (status != 0) stop(f, " failed (exit status ", status, ")")
  }
  message("Done.")
}, finally = setwd(owd))
