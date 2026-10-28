---
id: 1
slug: tab-model-linked-tables-figure-provenance
status: draft
branch:
created: 2026-09-27T13:23:49+02:00
concluded:
pr:
---

# Link full regression tables to tab_model output and trace supplement figures to source

## Plan

Two related but independently shippable workstreams in `5-disseminate/supplement.qmd` and
its upstream analysis scripts. Related to [[000-finalize-election-ai-manuscript]] (which
flags figure-numbering/cross-reference problems in the main manuscript) but scoped
separately: this plan owns the underlying provenance/tooling, not the manuscript prose pass.

### A. Full regression tables: stop hand-reimplementing tab_model, link to the real thing

**Current state.** `5-disseminate/R/table_helpers.R`'s `full_reg_table()`/`full_reg_two_study()`
manually rebuilds tab_model's exact column layout (Estimate, SE, 95% CI, *t*, *p*, *N*, *R*²)
coefficient-by-coefficient from `modelsummary::get_estimates()`, then `md_table()` renders it
as a pandoc long-table (`apa-longtable: true`) so it survives the apaquarto PDF. This is a
second, hand-maintained implementation of what `sjPlot::tab_model()` already does, and it's
what produces the long-form look in the appendix.

Separately, `4-analysis/scripts/02_main_analysis.Rmd:1746-1837` ("supplement mega table" chunk)
already calls real `tab_model()` per model set and writes a single indexed, anchored HTML file
to `4-analysis/results/supplement_full_regressions.html` (TOC + `<h2 id="...">` per section:
`interaction`, `prereg`, `primary`, `primary-std`, `itt`, `itt-std`, `vote`, `speed`). Nothing
currently links to this file — it's orphaned (confirmed via grep; no reference to
`supplement_full_regressions` anywhere in `supplement.qmd` or `dialogues-manuscript.qmd`).

`table_helpers.R` also already defines `reg_two_study()` (lines 79-87) — a more compact table
(Study | Outcome | Term | Estimate (SE) | Std. β (SE) | *p*) — but it is **currently unused**
(dead code; grep confirms only `full_reg_two_study` is called from `supplement.qmd`).

**Correction (found 2026-09-27 by rendering both against the real saved models — see
`4-analysis/results/models_02_main.rds`):** `reg_two_study()` is *not* condition-effects-only.
It still keeps the intercept and baseline-covariate rows for every outcome; it only drops the
interface (`ConditionChat`) term and the *N*/*R*² footer rows that `full_reg_table()` adds. For
`tbl-primary-ols` (16 outcome×study blocks) that's 64 rows vs. 122 — a real reduction, but not
"just the treatment effects." Update the pointer sentence in each swapped chunk accordingly
(full table has the interface term, *N*, and *R*² that the compact one omits, not "every other
coefficient").

**Bug found in the same test, worth fixing as part of this work:** `full_reg_table()`
deliberately blanks the "Std. β (SE)" cell for the intercept and baseline-covariate rows
(`not_effect <- ...` at table_helpers.R:128, comment: "not standardized effects"). `reg_long()`
(called by `reg_two_study()`) has no equivalent filter — it populates Std. β for every kept row,
including intercept and baseline. Never visible before since `reg_two_study()` was dead code.
Fix: add the same `not_effect` blanking to `reg_long()` before this ships, so the two tables
agree on what counts as a standardized effect.

**Decision (confirmed with Shaye 2026-09-27):** compact `reg_two_study()` table inline in the
PDF/docx/HTML supplement + a real link to the tab_model HTML for the full coefficient list.
Keep the mega-export as ONE indexed/anchored file (not split into per-table files) — it
already has a TOC and named anchors that map cleanly onto the `@tbl-*` labels below; splitting
would be more files to keep in sync for no real reader benefit.

**Table-to-anchor mapping** (5 chunks in `supplement.qmd` change; `#apx-effects` section):

| `@tbl-*` label | `supplement.qmd` lines | Current call | New call | mega-export anchor(s) |
|---|---|---|---|---|
| `tbl-interface-ols` | 342-348 | `full_reg_two_study(m02$interaction_s1, NULL, ...)` | `reg_two_study(m02$interaction_s1, NULL, ...)` | `#interaction` |
| `tbl-prereg-ols` | 354-360 | `full_reg_two_study(m02$prereg_s1, m02$prereg_s2, ...)` | `reg_two_study(m02$prereg_s1, m02$prereg_s2, ...)` | `#prereg` |
| `tbl-primary-ols` | 366-373 | `full_reg_two_study(..., m02$primary_std_s1, m02$primary_std_s2)` | `reg_two_study(..., m02$primary_std_s1, m02$primary_std_s2)` | `#primary`, `#primary-std` |
| `tbl-itt-ols` | 419-425 | `full_reg_two_study(m02$itt_s1, m02$itt_s2, ..., m02$itt_std_s1, m02$itt_std_s2)` | `reg_two_study(...)` (same args) | `#itt`, `#itt-std` |
| `tbl-speed-full` | 454-460 | `full_reg_two_study(m02$speed_s1, m02$speed_s2, ...)` | `reg_two_study(m02$speed_s1, m02$speed_s2, ...)` | `#speed` |

Leave `tbl-gam-s1`/`tbl-gam-s2` (`gam_table()`, mgcv smooth terms — not lm objects, tab_model
support for `gam` smooth terms is not equivalent) and `tbl-baseline-interaction`/`tbl-diehard`
(already interaction-only summaries, not full coefficient dumps) untouched — out of scope.

**Steps:**

1. In each of the 5 chunks above: swap `full_reg_two_study` → `reg_two_study` (same
   arguments — signature is compatible). Output is already markdown via `md_table()`, so the
   `apa-longtable` plumbing stays; the table just gets much shorter (condition/interface rows
   only, no intercept/baseline/N/R² blocks).
2. After each swapped table, add a sentence pointing to the full table, e.g.: *"Full
   regression output for every coefficient (including the intercept, baseline covariate, and
   model *N*/ *R*²) is available at [link]."* — see step 4 for what `[link]` resolves to.
3. Update the `#apx-effects` intro paragraph (`supplement.qmd:340`, currently "*Every
   regression table in this appendix reports all coefficients...*") — that claim becomes false
   once these 5 tables are condition-effects-only; rewrite to describe the compact-table +
   full-tab_model-link split.
4. **Resolve the link target** (needs a decision from Shaye, not just a file move):
   - The HTML build (`apaquarto-html` is already a real target in this file's YAML) can use a
     relative path: `../4-analysis/results/supplement_full_regressions.html#primary`.
   - The PDF/docx builds travel outside the repo (submission, OSF, reviewers) — a repo-relative
     path won't resolve for those readers. The existing convention in this supplement is OSF
     links for data/code/materials (`data-sharing` block, `#apx-demographics` intro). Recommend:
     upload `supplement_full_regressions.html` to the same OSF project
     (`https://osf.io/2cfkv/`) as a supplementary file, then hardcode that URL for PDF/docx and
     use the relative path only for the HTML build (or just use the OSF URL everywhere, for one
     link that works in every format — simpler, slight cost of not being anchor-linkable if OSF
     serves it as a download rather than a viewable page). I don't have OSF upload access —
     this step needs Shaye to upload and hand back the resulting URL.
   - Until that URL exists, use a placeholder (e.g. `[OSF LINK PENDING]`) styled the same way
     other in-progress items are flagged in this doc (see the red-styled sentinel at
     `supplement.qmd:411`), so it's visibly incomplete rather than silently wrong.
5. Once the 5 chunks are confirmed working (render both `apaquarto-html` and `apaquarto-pdf`
   locally), delete `full_reg_table()`/`full_reg_two_study()` from `table_helpers.R` — nothing
   else calls them. Leave `md_table()` (still used) and `reg_two_study()` (now used).
6. Give the mega-export chunk (`02_main_analysis.Rmd:1746-1837`) a one-line comment noting it
   is now linked from `supplement.qmd` (removing the "supplement.qmd does not render it" claim
   in the existing comment at line 1750, which will be stale).

### B. Supplement figures: trace to source, add real save calls, extend the manifest

`4-analysis/FIGURE_MANIFEST.md` already does this exact job for main-text figures (with mixed
results — 3 of 5 flagged as mismatched). Its supplement table already flags that 6 of 8
supplement figures have **no traceable `ggsave()`/save-to-file producer at all** — confirmed by
direct grep across `4-analysis/scripts/*.Rmd|*.R`:

| Figure | Cited as | Traced source | Problem |
|---|---|---|---|
| `fig_b1.png` | `@fig-denialism-correlations` | `04_robustness_and_supplement.Rmd:324`, `psych::pairs.panels()` | Base-R plot, renders inline only during knit; no `png()`/`ggsave()` capture anywhere |
| `fig_b2.png` | `@fig-denialism-efa` | `04_robustness_and_supplement.Rmd:367`, `psych::fa.diagram()` | Same — inline-only, no file capture |
| `fig_c1.png` | `@fig-effects` | **none found** | No matching object/save call anywhere in `scripts/` |
| `fig_c2.png` | `@fig-effects-adjusted` | **none found** | Same |
| `fig_c3.png` | `@fig-sentiment` | **none found** | Same |
| `fig_e1.png` | `@fig-mediation` | `06_mediation.Rmd:717-784` | Chunk has ~10 different exploratory rendering attempts (`grid.arrange`, `ggarrange`, `plot_grid`, `as_ggplot`, ...) for `p.meddet.2020`/`p.medpers.2020`/etc.; only concrete file write is `png("p.meddet.2020.png", ...)` at line 784, to the working-directory root, not `figures/supplement/fig_e1.png`, and not the same object as the on-disk `fig_e1.png` |

`fig_d1.png` (`@fig-belief-change`) is already fine — `03_baseline_gams_quantile.Rmd:857-875`,
matches. `fig_b3.png` (`@fig-interfaces`) is a manual UI screenshot, correctly flagged
not-reproducible in the manifest — leave as is.

**This is a content decision, not a pure file-move**, per the manifest's own framing of the
main-text mismatches. Before writing any `ggsave()` calls, need Shaye's confirmation on:

- `fig_b1`/`fig_b2`: are the on-disk PNGs still the correct/current correlation matrix and EFA
  diagram (data hasn't changed since), or do they need regenerating? `pairs.panels()`/
  `fa.diagram()` are base graphics — capturing them needs `png(...); print(...); dev.off()`
  (the commented-out `#dev.off()` in `04_robustness_and_supplement.Rmd` near line 367 suggests
  someone started this and stopped) rather than `ggsave()`.
- `fig_c1`/`fig_c2`/`fig_c3`: no candidate object found under any name close to "effects" or
  "sentiment" outside `fig2`/`fig4`/`fig5` (already claimed by main-text figures per the
  manifest). These may need to be rebuilt from scratch from the underlying model objects
  (`m02$primary_s1`/`m02$primary_s2` for the treatment-effect plots; `df_long`/
  `sentiment_scores` in `02_main_analysis.Rmd`'s "Convo Themes & Sentiment" section, ~line
  1839+, for the sentiment trajectory plot) rather than traced, since no prior code path exists.
- `fig_e1`: which of the ~10 mediation-plot variants in `06_mediation.Rmd:717-784` is the
  canonical one meant to ship as `@fig-mediation`? That chunk reads as leftover exploration
  (many alternate rendering libraries tried, most just `plot()`-ed inline, never saved) rather
  than a settled figure — needs Shaye to pick one before it's worth cleaning up and adding a
  real save call.

**Steps (once the above are resolved):**

1. For each figure, add an explicit save call at its actual creation point
   (`ggsave("figures/supplement/fig_XX.png", ...)` for ggplot objects; `png(...); print(...);
   dev.off()` for the base-R `pairs.panels`/`fa.diagram` plots), matching the existing
   `fig_d1.png` pattern (`03_baseline_gams_quantile.Rmd:875`) for dimensions/dpi consistency
   (`width = 8, height = 5, dpi = 600`).
2. Re-run the producing scripts and diff the regenerated PNGs against what's on disk /
   currently cited in `supplement.qmd`, to catch any drift before replacing the files.
3. Update `FIGURE_MANIFEST.md`'s supplement table: flip each row's Status from "Not
   code-reproducible" to the real script/line, matching the main-text table's format.
4. Mirror the `# written by 4-analysis/scripts/...` comment convention already used in
   `supplement.qmd`'s table-setup chunk (lines 139-145) at each supplement figure block (e.g.
   above `::: {#fig-denialism-correlations}` at line 250) — one line naming the producing
   script, so the source is visible at the point of use, not only in the separate manifest
   file.

### Out of scope

- Fixing the main-text figure mismatches (`fig3.png`/`fig4.png`/`fig5.png` identity chain) —
  already tracked as a flagged-but-undecided issue in `FIGURE_MANIFEST.md` and touches
  [[000-finalize-election-ai-manuscript]]'s figure-numbering item; a separate content decision.
- Switching table generation to `modelsummary::modelsummary()` or any other cross-format
  table library — considered and declined; Shaye wants literal `tab_model()` output, not an
  equivalent-looking reimplementation.
- Rendering tab_model HTML tables to a snapshot PDF/image for inline PDF embedding —
  considered and declined in favor of the link-out approach.

### Implementation order

1. Part A, steps 1-3 (swap the 5 table calls, add pointer sentences, fix the intro paragraph) —
   independent of the OSF-link decision, can render and review with a placeholder link.
2. Get Shaye's answers on the Part B open questions (fig_b1/b2 regenerate-or-not, fig_c1-3
   rebuild source, fig_e1 canonical variant).
3. Part B steps 1-4.
4. Part A steps 4-6 (once the OSF URL exists) — can trail behind B since it only needs the
   placeholder swapped for a real link.


## Log

- 2026-10-26: Part A has moved on. `supplement.qmd` now builds the five regression tables (`tbl-interface-ols`, `tbl-prereg-ols`,
  `tbl-primary-ols`, `tbl-itt-ols`, `tbl-speed-full`) with `reg_table_live()` (`sjPlot::tab_model()` for HTML, `modelsummary` for
  other formats) from `results/models_02_main.rds`, so the compact `reg_two_study()` route in this plan was not taken. During the
  pre-publication audit `reg_long()`, `reg_two_study()`, `full_reg_table()` and `full_reg_two_study()` were deleted from
  `table_helpers.R` as dead code, and the legacy non-`tab_model` regression tables were removed from `02_main_analysis.Rmd`
  (its HC1 tables for Figure 2 now come from the same model lists via `hc1_tidy()`). The link from the supplement to
  `results/supplement_full_regressions.html` (steps 4 and 6) is still open and waits on the OSF URL. The "standardized (ITT)" panels
  were found to duplicate the primary standardized models and were rebuilt on the imputed samples. Part B is unchanged;
  `FIGURE_MANIFEST.md` was rewritten to the current state.
