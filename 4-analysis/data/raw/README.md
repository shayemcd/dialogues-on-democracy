# Raw Data: Dialogues on Democracy

This directory contains the shareable, de-identified stage-0 survey data used to reproduce the entire data cleaning and variable derivation pipeline from scratch.

---

## 1. Overview & Contents

The primary file in this directory is:

- **`original_dfs_shareable.RData`** (approx. 13 MB):
  The stage-0 de-identified Qualtrics survey exports for Study 1 and Study 2. Loading this file loads four data frames into the R environment:

| Data Frame | Study | Records ($N$) | Variables | Description |
|---|---|---|---|---|
| `df` | Study 1 | 5,879 | 185 | Raw numeric survey responses, including screenouts, bot flags, and incomplete sessions. |
| `df_s2` | Study 2 | 3,237 | 143 | Raw numeric survey responses for Study 2 replication. |
| `df_text` | Study 1 | 5,879 | 41 | Raw response choice-text labels (used in `01_cleaning.Rmd` to map categorical demographics). |
| `df_text_s2` | Study 2 | 3,237 | 36 | Raw response choice-text labels for Study 2. |

Together, these four data frames represent all 8,981 survey respondents who initiated the surveys across both studies.

---

## 2. De-identification & Privacy Protocol

To ensure full compliance with IRB protocols and participant privacy while maximizing computational reproducibility, `original_dfs_shareable.RData` has undergone rigorous anonymization:

1. **Network & Geographic Metadata Scrubbed**:
   - All IP addresses (`IPAddress`) have been completely set to `NA`.
   - Geographic coordinates (`LocationLatitude`, `LocationLongitude`) and 5-digit ZIP codes have been removed.
2. **Participant Tracking Identifiers Removed**:
   - Third-party panel tracking tokens (`rid`, `participantId`, `ExternalReference`), Qualtrics distribution IDs, and survey invite hashes were removed.
3. **Hardware & Browser Fingerprints Purged**:
   - Operating system, browser version, screen resolution, and keystroke/paste telemetry (`alias_data`, `Q2`, `MetaInfo_*`) were removed.
4. **Text Anonymization**:
   - Free-text fields were screened for personal identifiers, with identified personal names or sensitive private information masked as `[REDACTED]`.

> [!NOTE]
> Un-anonymized raw Qualtrics files containing network logs (`original_dfs.RData` and the original `.xlsx` files) are permanently `.gitignore`d and not shared publicly.

---

## 3. How to Reproduce Data Cleaning from Scratch

Downstream analyses consume the processed dataset `4-analysis/data/clean_dfs.RData`. If you wish to reproduce `clean_dfs.RData` directly from this raw file:

### Option A: Render `01_cleaning.Rmd` directly
From R (with the working directory set to `4-analysis/` or `4-analysis/scripts/`):
```r
rmarkdown::render("scripts/01_cleaning.Rmd")
```

### Option B: Via `run_all.R`
In `scripts/run_all.R`, include `"raw"` in the `include` vector:
```r
include <- c("", "raw")
source("scripts/run_all.R")
```

### What `01_cleaning.Rmd` does:
1. Loads `original_dfs_shareable.RData`.
2. Merges numeric ratings and choice-text demographic labels.
3. Screens participants for bot checks, speeders, and attention criteria.
4. Computes normalized outcome scales (0–100) and unstandardized change scores (`Post - Pre`).
5. Imputes intention-to-treat (ITT) variables where baseline scores are carried forward for attriters.
6. Integrates initial claim NLP classifications from `data/threat_all_statement_nlp.xlsx`.
7. Outputs the final de-identified analytic dataset: `4-analysis/data/clean_dfs.RData` (containing `df_consp` [$n = 1,215$], `df_consp_2` [$n = 633$], `df_both` [$N = 1,848$], `df_all` [$N = 8,981$], `df_trim`, and `df_trim_2`).


