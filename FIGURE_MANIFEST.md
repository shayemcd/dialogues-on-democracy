# Figure manifest

Where each figure cited in the manuscript and supplement comes from. Files are named by their Quarto label, so a change in
the order of the paper does not rename anything. "Regenerates" means the figure was rebuilt from `data/clean_dfs.RData` by
the script named and matched the file in `figures/`.

## Main text

| File | Cited as | Produced by | Status |
|---|---|---|---|
| `figures/fig-study-flow.png` (copied to `images/` for the manuscript) | `@fig-study-flow` | LaTeX/TikZ source `figures/fig-study-flow.tex` (compiled to `fig-study-flow.pdf` and the PNG) | Hand-built from the study design; not produced by R |
| `figures/fig-denialism-changes.png` | `@fig-denialism-changes` | `scripts/05_results_effects.Rmd`, chunk "1. final figs" | Regenerates (byte-identical) |
| `figures/fig-themes.png` | `@fig-themes` | `scripts/06_results_themes.Rmd`, chunk "merged themes fig" | Regenerates from `df_convosfin.xlsx` (full sample $N = 1,655$); condition labels follow `df_convosfin.xlsx` |
| `figures/fig-baseline-conviction.png` | `@fig-baseline-conviction` | `scripts/07_results_belief_level.Rmd`, chunk "2b. final gam graphs" | Regenerates (byte-identical); panel c quantile-regression bands use bootstrap SEs (seeded in the script) |
| `figures/fig-strategy-quality-significant.png` | `@fig-strategy-quality-significant` | `scripts/08_results_strategies.Rmd`, chunk "fig-strategy-quality-significant" | Regenerates; net-of-other-items estimates only |

## Supplement (`figures/supplement/`)

| File | Cited as | Produced by | Status |
|---|---|---|---|
| `fig-denialism-correlations.png` (alias: `fig_b1.png`) | `@fig-denialism-correlations` | `scripts/04_measures.Rmd`, `psych::pairs.panels()` | Regenerates (values match the earlier static image) |
| `fig-denialism-efa.png` (alias: `fig_b2.png`) | `@fig-denialism-efa` | `scripts/04_measures.Rmd`, chunk "factor analysis" | Regenerates (loadings match the earlier static image; rows are labelled by variable name) |
| `fig-interfaces-chat.png`, `fig-interfaces-qualtrics.png` | `@fig-interfaces` (`@fig-interfaces-chat`, `@fig-interfaces-qualtrics`) | Screenshots of conversational chat vs. Qualtrics survey conditions | External images (two-panel layout in supplement) |
| `fig-effects.png` (alias: `fig_c1.png`), `fig-effects-adjusted.png` (alias: `fig_c2.png`) | `@fig-effects`, `@fig-effects-adjusted` | Produced from treatment effect regression models | Static images |
| `fig-sentiment.png` | `@fig-sentiment` | `scripts/06_results_themes.Rmd`, chunk "sentiment plot" | Regenerated from full $N = 1,655$ sample |
| `fig-belief-change.png` | `@fig-belief-change` | `scripts/07_results_belief_level.Rmd`, chunk "fig d1 - quantile regression" | Regenerates (byte-identical) |

Files in `figures/supplement/` that are not cited: `fig-causal-forest.png` (`09_heterogeneity_forests.Rmd`; causal forest results reported via `@tbl-cf-heterogeneity`), `fig-mediation-diagram.png` (a static mediation diagram with no saving code; the mediation estimates are in `results/mediation_params.rds`) and `fig-rf-importance-2020denialism.png` (`07_results_belief_level.Rmd`; byte-identical on regeneration).
