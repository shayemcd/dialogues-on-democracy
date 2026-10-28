# Dialogues on Democracy: Belief-Tailored AI Conversations Reduce Inaccurate Election Denial Beliefs

Replication code, de-identified analysis data, and survey instruments for **"Dialogues on Democracy: Belief-Tailored AI Conversations Reduce Inaccurate Election Denial Beliefs"**.

- **Authors:** Shaye-Ann M. Hopkins, Thomas H. Costello, Gordon Pennycook, David G. Rand
- **Preprint:** Hopkins, S., Costello, T., Pennycook, G., & Rand, D. (2026). *Dialogues on Democracy: Belief-Tailored AI Conversations Reduce Inaccurate Election Denial Beliefs*. Research Square. https://doi.org/10.21203/rs.3.rs-8663921/v1
- **Preregistrations:**
  - Study 1 preregistration: [osf.io/rq3fm](https://osf.io/rq3fm/overview)
  - Study 2 preregistration: [osf.io/pgdzw](https://osf.io/pgdzw/overview)
- **Interactive conversation explorer:** [Shiny app](https://shaye-hopkins.shinyapps.io/ai-election-convos/)

---

## License

Code is released under the MIT License. Research data, survey instruments,
documentation, and generated figures are released under CC BY 4.0. Third-party
content, raw participant data, and materials subject to separate permissions
are excluded. See [LICENSE](LICENSE) for the complete terms.

---

## 1. What is in this repository

```
ElectionConspiracies/
├── 2-design/instrument/          # Study1_Survey.qsf, Study2_Survey.qsf (Qualtrics) and *_Preview.docx (printed copies)
└── 4-analysis/
    ├── scripts/                  # analysis scripts in manuscript order (below), run_all.R, Shiny app
    ├── R/setup.R                 # packages, data, reference levels, colours and outcome vectors shared by the scripts
    ├── data/                     # analysis-ready, de-identified data (see data/README.md)
    ├── results/                  # saved model output read by the supplement (see 4-analysis/README.md)
    └── figures/                  # one file per figure, named by its Quarto label (see FIGURE_MANIFEST.md)
```

---

## 2. Reproducing the analysis

### Requirements

- **R** 4.5 (the analyses were run on 4.5.1). The scripts load their packages through `pacman::p_load()` (the shared list is in
  `4-analysis/R/setup.R`); the main ones are `tidyverse`, `sandwich`, `lmtest`, `emmeans`, `sjPlot`, `mgcv`, `grf`, `lavaan`,
  `brms`, `quantreg`, `survival`, `gtsummary`.
- **Python** 3.9+ for the BERTopic script (`02d_bertopic.py`).
- An `OPENAI_API_KEY` environment variable only if you re-run the LLM coding scripts (`02a`, `02b`, `02c`). The coded output
  they produce is included in `4-analysis/data/`.

### Start from `data/clean_dfs.RData` or run from raw

The shareable stage-0 raw dataset `4-analysis/data/raw/original_dfs_shareable.RData` is included in this repository (with all identifiers, network metadata, and unreviewed text stripped), allowing `01_cleaning.Rmd` to be reproduced from scratch.

By default, analyses consume the de-identified, processed `4-analysis/data/clean_dfs.RData`. Open R in `4-analysis/` and run
`source("scripts/run_all.R")` to run the analysis scripts in order (about 3 minutes; the Bayesian models, causal forests and
mediation, which together take well over an hour, are switched off by default, as are the raw-data and OpenAI steps; see the
top of `run_all.R`). Or run any script on its own.

The scripts follow the manuscript. Figure files are named by their Quarto label, so the order of the Results can change
without renaming anything.

| Script                                                           | Manuscript section               | What it does                                                                                                             | Writes                                                                                                                                                                                              |
| ---------------------------------------------------------------- | -------------------------------- | ------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `01_cleaning.Rmd`                                              | Method: sample                   | Cleans the Qualtrics exports, applies exclusions, builds analysis variables, removes quasi-identifiers (needs raw data)  | `data/clean_dfs.RData`                                                                                                                                                                            |
| `02a_llm_theme_coding.R`                                       | derived measures                 | GPT coding of claim themes and evidence counts (OpenAI API; incremental & resumable)                                     | `data/df_convosfin.xlsx`                                                                                                                                                                          |
| `02b_llm_strategy_coding.R`, `02c_llm_nuance_coding.R`       | derived measures                 | GPT coding of conversation turns (OpenAI API; resumable checkpoints)                                                     | `data/df_*_checkpoint.rds`                                                                                                                                                                        |
| `02d_bertopic.py`                                              | derived measures                 | BERTopic model of participants' initial statements                                                                       | `scripts/bertopic_userResponse_1_top10.*`                                                                                                                                                         |
| `03a_method_descriptives.R`                                    | Method: participants; Appendix A | Sample descriptives, attrition rates and tests, cell sizes and conversation length                                       | `results/descriptives.rds`                                                                                                                                                                        |
| `03b_method_checks.Rmd`                                        | Method: procedure; Appendix A    | Demographics, differential attrition, speeder exclusion by condition, evidence counts, post hoc power                    | printed output                                                                                                                                                                                      |
| `04_measures.Rmd`                                              | Appendix B                       | Factor analysis, correlations and reliability of the denialism measures                                                  | `figures/supplement/fig-denialism-correlations.png`, `fig-denialism-efa.png`                                                                                                                    |
| `05_results_effects.Rmd`                                       | Results 1; Appendix C            | Primary regressions (HC1) and sensitivity models, estimated marginal means, effect sizes, voting, Lee bounds             | `results/models_effects.rds`, `results/supplement_full_regressions.html`, `figures/fig-denialism-changes.png`                                                                                 |
| `05b_effects_bayesian.R`                                       | Appendix D                       | Bayesian censored (Tobit) regressions by study (brms)                                                                    | `results/bayes_models.rds`                                                                                                                                                                        |
| `06_results_themes.Rmd`                                        | Results 2                        | Claim themes and effects by theme; sentiment across conversation rounds                                                  | `figures/fig-themes.png`, `figures/supplement/fig-sentiment.png`                                                                                                                                |
| `07_results_belief_level.Rmd`                                  | Results 3; Appendix D            | Condition x baseline interactions, GAMs, quantile regression, die-hard models, Tobit and ceiling escape, random forests  | `results/models_belief_level.rds`, `results/models_ceiling.rds`, `figures/fig-baseline-conviction.png`, `figures/supplement/fig-belief-change.png`, `fig-rf-importance-2020denialism.png` |
| `08_results_strategies.Rmd`                                    | Results 4; Appendix E            | Persuasion strategies and argument quality from the coded conversations, net of one another                              | `results/strategy_quality_models.rds`, `data/df_strategy_coded.RData`, `figures/fig-strategy-quality-significant.png`                                                                         |
| `09_heterogeneity_forests.Rmd`, `09b_heterogeneity_checks.R` | Discussion; Appendix D           | Causal forests, SHAP, calibration and heterogeneity checks                                                               | `figures/supplement/fig-causal-forest.png`, `results/cf_heterogeneity_checks.{rds,csv}`                                                                                                         |
| `10_mediation.Rmd`                                             | Discussion; Appendix F           | Mediation models (lavaan, 10,000 bootstrap draws; takes over an hour)                                                    | `results/mediation_params.rds`                                                                                                                                                                    |
| `99_check_manuscript_numbers.R`                                | all                              | Recomputes every number typed into the manuscript and supplement text and flags mismatches (re-run after editing either) | `results/manuscript_number_check.csv` (not tracked)                                                                                                                                               |
| `run_all.R`                                                    |                                  | Runs the scripts above in order, each in its own R session                                                               |                                                                                                                                                                                                     |
| `shiny-app/2.3.app.R`                                          |                                  | Conversation explorer app                                                                                                |                                                                                                                                                                                                     |

Random seeds are set where results are stochastic (`set.seed(42)` in `04`, `07` and `10`; fixed seeds in `05` (Lee bounds),
`05b`, `09` and `09b`).

---

## 3. Sample and key variables

**Samples** (`data/clean_dfs.RData`): `df_all` (all Study 1 and 2 survey records), `df_trim` / `df_trim_2` (passed screening and the
baseline exclusions), and the analytic samples `df_consp` (Study 1, *n* = 1,215), `df_consp_2` (Study 2, *n* = 633) and
`df_both` (*N* = 1,848), which add the NLP coding of the open-ended claim as an election conspiracy. The screening and
exclusion rules are in `01_cleaning.Rmd` (chunk "diff attrition indicators"). Beyond the preregistered baseline 2020 Denialism
threshold (> 25), the analytic sample excludes participants whose baseline Claim Certainty was below 50 (*n* = 35; not
preregistered). The preregistered 10-minute completion-time exclusion is not applied. Sensitivity models that apply it, and
that drop the Claim Certainty exclusion, are in `05_results_effects.Rmd`.

**Outcomes**

- 2020 Election Denialism: `ER0Norm_Pre`, `ER0Norm_Post`, `ER0_Diff` (0 to 100).
- Claim Certainty (confidence in the participant's own stated claim): `Sureness_Pre_1`, `Sureness_Post_1`, `Sureness_Diff` (0 to 100).
- Other denialism and affective polarization outcomes: `ER1_*`, `ER2_*`, `ER2020_*`, `ER2024_*`, `ER_*`, `AffPol_*`.
- `*_Pre_center`: baseline score centered within study; `*_std`: z-scored with the mean and SD of the analytic sample (`df_both`);
  `*_itt`: post score with baseline carried forward.

**Conditions**

- `ConditionFeedback`: `Belief-Tailored`, `Belief & Values-Tailored` (Study 1 only), `Adversarial Dialogue`, `Control` (cats and dogs conversation; Study 2 only).
- `ConditionChat`: `Chat` or `Qualtrics` interface (Qualtrics interface in Study 1 only).

**Die-hards**: `CCDieHard` and `Den2020DieHard` flag participants at the scale maximum (100) at baseline (33.06% of the analytic sample for claim certainty).
