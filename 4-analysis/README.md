# 4-analysis: Dialogues on Democracy

Computational artifacts for the paper: cleaning, models, causal forests, mediation, conversation coding, figures, and the
saved results that the supplement reads. See the repository `README.md` for the run order and a description of each script.

## Directory structure

```
4-analysis/
├── scripts/            # analysis scripts in manuscript order (01-10, 99), run_all.R, GPT-coding scripts, shiny-app/
├── R/setup.R           # packages, data, reference levels, colours and outcome vectors shared by the scripts
├── data/               # de-identified analysis data and shareable stage-0 raw data (data/README.md)
├── results/            # saved model output (below)
├── figures/            # one file per figure, named by its Quarto label, and supplement/ (FIGURE_MANIFEST.md)
└── .here               # empty marker that anchors here::here() to this folder
```

## From scripts to the supplement

The supplement does not fit models. Each script saves what it computed into `results/`, and `supplement.qmd` reads those
files and formats them with helper functions (`5-disseminate/R/table_helpers.R`, not included in this repository). Re-running a script and re-rendering the supplement
refreshes the tables.

| File in `results/` | Written by | Contents |
|---|---|---|
| `models_effects.rds` | `05_results_effects.Rmd` ("save models for supplement") | Fitted `lm` objects for the primary, preregistered-sample, ITT, standardized, speeder-sensitivity, baseline-Claim-Certainty-sensitivity and interaction models; estimated marginal means; the voting model; outcome labels and sample sizes. Robust (HC1) SEs are applied when a table is built. |
| `supplement_full_regressions.html` | `05_results_effects.Rmd` ("supplement mega table") | Every regression behind the supplement with all coefficients, one indexed HTML file (`sjPlot::tab_model`). |
| `bayes_models.rds` | `05b_effects_bayesian.R` | Posterior summaries for Study 1 and Study 2 Bayesian censored (Tobit) models. |
| `models_belief_level.rds` | `07_results_belief_level.Rmd` | Condition x baseline interactions, GAM summaries, die-hard and non-die-hard models. |
| `models_ceiling.rds` | `07_results_belief_level.Rmd` | Tobit vs. OLS estimates and ceiling-escape models (tidy results, not model objects). |
| `strategy_quality_models.rds` | `08_results_strategies.Rmd` | Each coded conversation feature net of the other 20 (both outcomes), the Argument Quality x Tone interaction and simple slopes, Argument Quality slopes by condition, and the joint test. |
| `cf_heterogeneity_checks.{rds,csv}` | `09b_heterogeneity_checks.R` | Causal-forest calibration, RATE and interaction diagnostics. |
| `mediation_params.rds` | `10_mediation.Rmd` | lavaan path estimates with bootstrap CIs. |
| `descriptives.rds` | `03a_method_descriptives.R` | Table bodies for the sample descriptives and attrition tables, plus the attrition tests, cell sizes and conversation length quoted in the Method. |
| `condition_raw_means_sds.csv` | Generated from `data/clean_dfs.RData` | Raw unadjusted pre-mean, pre-SD, post-mean, post-SD, change score mean/SD, and cell counts ($n$) by condition for all 8 outcomes in Study 1 and Study 2. |
| `standardized_effect_sizes.csv` | `05_results_effects.Rmd` (`models_effects.rds`) | Standardized effect sizes (standardized $\beta$ / Cohen's $d$), robust HC1 standard errors, 95% CIs, and $p$-values for all outcomes across Study 1 and Study 2. |

The main manuscript does not read these files; its numbers are typed into the text. `scripts/99_check_manuscript_numbers.R`
recomputes each of them from the data and the saved results and flags any that disagree; re-run it whenever a script or the text changes.

## Notes on reproducibility
- `scripts/run_all.R` runs the analysis in manuscript order (see the repository `README.md`).
- `data/df_convosfin.xlsx` holds the GPT-coded themes and evidence counts for all 1,655 completed conversations across Study 1 and Study 2. Regenerating or updating it uses the OpenAI API via `02a_llm_theme_coding.R` (configured via root `.env`), which runs incrementally with checkpointing.
- Supplement figures `fig-effects` and `fig-effects-adjusted` are static images with no saving code; `fig-interfaces-chat` and `fig-interfaces-qualtrics` (the two-panel interface figure `@fig-interfaces`) and `fig-mediation-diagram` are external screenshots/diagrams (see `FIGURE_MANIFEST.md`).
