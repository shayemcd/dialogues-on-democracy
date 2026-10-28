# Shared setup for the analysis scripts that work on the Study 1 + 2 data
# (03b, 04, 05, 06, 07). Each script anchors here::here() to the repository root and then calls
#   source(here::here("R", "setup.R"))
# so the packages, data, reference levels, colours and outcome vectors are defined once.
# 01_cleaning.Rmd builds the data this file loads; 08, 09 and 10 are self-contained.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

pacman::p_load(
  tidyverse,
  patchwork,ggpubr,
  corrplot,psych,corrr,
  gtsummary,data.table,kableExtra,
  sandwich, lmtest,
  emmeans, sjPlot, ggeffects,
  ggstatsplot, ggthemes, ggdist, paletteer,
  report, rempsyc,
  writexl,openxlsx,readxl,
  brms,forcats,randomForest,
  labelled,nlme,mgcv,gratia,broom,
  quantreg,jtools,sentimentr,
  grf,fastshap,AER,survival)


# ---- data ----
# df_all = every Study 1 + 2 survey record; df_trim / df_trim_2 = after screening and baseline exclusions;
# df_consp / df_consp_2 = the analytic samples per study (preregistered exclusions + NLP-coded election
# conspiracy); df_both = the two analytic samples stacked (N = 1,848)
load(here::here("data", "clean_dfs.RData"))

# centered column = raw column, as built in 01_cleaning.Rmd
pre_center_map <- c(ER0Norm_Pre_center = "ER0Norm_Pre", Sureness_Pre_center = "Sureness_Pre_1", ER1_Pre_center = "ER1_Pre",
                    ER2_Pre_center = "ER2_Pre", ER2024_Pre_center = "ER2024_Pre", ER2020_Pre_center = "ER2020_Pre",
                    ERNorm_Pre_center = "ER_Pre", AffPol_Pre_center = "AffPol_Pre")

# Reference levels for the treatment contrasts (Study 1 reference = Adversarial Dialogue; Study 2 reference =
# Control), so every script gives the same reference group without relying on another script having run first.
df_trim$ConditionFeedback    <- relevel(df_trim$ConditionFeedback,    ref = "Adversarial Dialogue")
df_trim$ConditionChat        <- relevel(factor(df_trim$ConditionChat), ref = "Qualtrics")
df_trim$ConditionAdvers      <- relevel(factor(df_trim$ConditionAdvers), ref = "Adversarial Dialogue")
df_trim$messageType          <- relevel(factor(df_trim$messageType),   ref = "Qualtrics  + Adversarial Dialogue")
df_trim_2$ConditionFeedback  <- relevel(df_trim_2$ConditionFeedback,  ref = "Control")
df_trim_2$messageType        <- relevel(df_trim_2$messageType,        ref = "Chat + Control")
df_consp$ConditionFeedback   <- relevel(df_consp$ConditionFeedback,   ref = "Adversarial Dialogue")
df_consp$ConditionChat       <- relevel(factor(df_consp$ConditionChat), ref = "Chat")
df_consp$ConditionAdvers     <- relevel(factor(df_consp$ConditionAdvers), ref = "Adversarial Dialogue")
df_consp$messageType         <- relevel(factor(df_consp$messageType),  ref = "Qualtrics  + Adversarial Dialogue")
df_consp_2$ConditionFeedback <- relevel(df_consp_2$ConditionFeedback, ref = "Control")
df_consp_2$messageType       <- relevel(df_consp_2$messageType,       ref = "Chat + Control")

# GPT-coded claim themes and evidence counts for 1,655 conversations (02a_llm_theme_coding.R)
df_convos <- read_excel(here::here("data", "df_convosfin.xlsx"))

# everyone who passed screening and whose open-ended claim was coded as an election conspiracy, before the
# baseline exclusions (differential-attrition checks)
da.s1 <- df_all %>% filter(Study == 1, Conspiracy == "Conspiracy",
                           ScreenedDummy == 0)
da.s2 <- df_all %>% filter(Study == 2, Conspiracy == "Conspiracy",
                           ScreenedDummy == 0)

da.both <- df_all %>% filter(Study == 1 | Study == 2, Conspiracy == "Conspiracy",
                           ScreenedDummy == 0)


# ---- colours ----
condition_colors <- c(
  "Belief-Tailored"   = "#4158A6",
  "Belief & Values-Tailored"        = "#6A8E7F",
  "Adversarial Dialogue"  = "#A64521",
  "Control"            = "lightgray")  # matches the actual ConditionFeedback control level


# 2
colpal.2 <- c("#0B1B66", "#20A49C")

# 3
colpal.3     <- c("#4158A6", "#6A8E7F", "#A64521")        # navy, clear blue, green-teal
colpal.3.s2  <- c("#4158A6", "#A64521", "lightgray")      # ctl, lim, det

# 3 reversed
colpal.3rev    <- c("#A64521", "#6A8E7F", "#4158A6")
colpal.3rev.s2 <- c("lightgray", "#A64521", "#4158A6")

colpal.4 <- c("lightgray", "#A64521", "#6A8E7F", "#4158A6")

colpal.6 <- c("lightgray", "darkgrey", "#2F7EC1", "#4158A6", "#9EDFD8", "#0A6F6B")

colpal.7 <- c("gray", "#E0F7F5", "#97D9CC", "#3FB5AC", "#20A49C", "#2F7EC1", "#4158A6")


# ---- outcomes ----
dvs_post <- c("ER0Norm_Post","Sureness_Post_1",
             "ER1_Post","ER2_Post","ER2020_Post","ER2024_Post",
             "ER_Post","AffPol_Post")


dvsnorm_post <- c("ER0Norm_Post","Sureness_Post_1", #sureness already on 0-100 scale
             "ER1_Post","ER2_Post","ER2020_Post","ER2024_Post",
             "ER_Post","AffPol_Post")

dvs_post_itt <- c("ER0Norm_Post_itt","Sureness_Post_1_itt",
             "ER1_Post_itt","ER2_Post_itt","ER2020_Post_itt","ER2024_Post_itt",
             "ER_Post_itt","AffPol_Post_itt")

dvs_pre <- c("ER0Norm_Pre","Sureness_Pre_1",
              "ER1_Pre","ER2_Pre","ER2020_Pre","ER2024_Pre",
             "ER_Pre","AffPol_Pre")

dvs_precenter <- c("ER0Norm_Pre_center","Sureness_Pre_center",
              "ER1_Pre_center","ER2_Pre_center","ER2020_Pre_center","ER2024_Pre_center",
             "ERNorm_Pre_center","AffPol_Pre_center")

dvsnorm_pre <- c("ER0Norm_Pre","Sureness_Pre_1",
              "ER1Norm_Pre","ER2Norm_Pre","ER2020_Pre","ER2024_Pre",
             "ER_Pre","AffPol_Pre")

dvs_diff <- c("ER0_Diff","Sureness_Diff",
              "ER1_Diff","ER2_Diff","ER2020_Diff","ER2024_Diff",
              "ER_Diff","AffPol_Diff")


dvs_perc <- c("ER0_PercChange","Sureness_PercChange",
             "ER1_PercChange","ER2_PercChange","ER2020_PercChange","ER2024_PercChange",
             "ER_PercChange","AffPol_PercChange")

# Scale percent change DVs by 100 so all regressions report percentage points
scale_perc <- function(df) {
  cols <- intersect(names(df), dvs_perc)
  df[cols] <- lapply(df[cols], function(x) x * 100)
  df
}
df_trim    <- scale_perc(df_trim)
df_trim_2  <- scale_perc(df_trim_2)
df_consp   <- scale_perc(df_consp)
df_consp_2 <- scale_perc(df_consp_2)
df_both    <- scale_perc(df_both)
df_all     <- scale_perc(df_all)

dvs_pre_std <- paste0(dvs_pre,"_std")
dvs_post_std <- paste0(dvs_post,"_std")
dvs_diff_std <- paste0(dvs_diff,"_std")


# Create a named vector mapping old outcome names to new, cleaner names
new_labels <- c("ER0Norm_Post"    = "2020 Denialism (Main)",
                "Sureness_Post_1" = "Claim Certainty",
                "ER_Post"         = "Election Denialism (Aggregate)",
                "ER1_Post"        = "General Denialism",
                "ER2_Post"        = "Specific Denialism",
                "ER2020_Post"     = "2020 Denialism (Aggregate)",
                "ER2024_Post"     = "2024 Denialism",
                "AffPol_Post"     = "Affective Polarization")

new_labels_itt <- c("ER0Norm_Post_itt"    = "2020 Denialism (Main)",
                "Sureness_Post_1_itt" = "Claim Certainty",
                "ER_Post_itt"         = "Election Denialism (Aggregate)",
                "ER1_Post_itt"        = "General Denialism",
                "ER2_Post_itt"        = "Specific Denialism",
                "ER2020_Post_itt"     = "2020 Denialism (Aggregate)",
                "ER2024_Post_itt"     = "2024 Denialism",
                "AffPol_Post_itt"     = "Affective Polarization")

new_labels2 <- c("ER0Norm_Post_std"    = "2020 Denialism (Main)",
                "Sureness_Post_1_std" = "Claim Certainty",
                "ER_Post_std"         = "Election Denialism (Aggregate)",
                "ER1_Post_std"        = "General Denialism",
                "ER2_Post_std"        = "Specific Denialism",
                "ER2020_Post_std"     = "2020 Denialism (Aggregate)",
                "ER2024_Post_std"     = "2024 Denialism",
                "AffPol_Post_std"     = "Affective Polarization")

new_labelsperc <- c("ER0_PercChange"    = "2020 Denialism (Main)",
                "Sureness_PercChange" = "Claim Certainty",
                "ER_PercChange"         = "Election Denialism (Aggregate)",
                "ER1_PercChange"        = "General Denialism",
                "ER2_PercChange"        = "Specific Denialism",
                "ER2020_PercChange"     = "2020 Denialism (Aggregate)",
                "ER2024_PercChange"     = "2024 Denialism",
                "AffPol_PercChange"     = "Affective Polarization")

Hmisc::label(df_consp$messageType) <- "Condition"
Hmisc::label(df_consp_2$messageType) <- "Condition"
