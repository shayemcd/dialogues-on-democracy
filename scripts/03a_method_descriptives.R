# 03a_method_descriptives.R
# Supplement Tables A1 (sample characteristics by study), A2 (by endorsed election-conspiracy status) and
# A3 (differential attrition by condition), plus the sample, attrition and conversation numbers quoted in the Method
# section. Saves light-weight table bodies (not the gtsummary objects, which embed the data) to
# results/descriptives.rds, which 5-disseminate/supplement.qmd reads.

suppressMessages({library(tidyverse); library(gtsummary)})
local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/03a_method_descriptives.R")
})

analysis_dir <- here::here()
load(file.path(analysis_dir, "data/clean_dfs.RData"))

make_summary <- function(d, by) {
  d %>% ungroup() %>% droplevels() %>%
    tbl_summary(include = c(Age, Race, EducationCategory, GenderClean, religion,
                            AffPol_Pre, AffPol_Post, DemRep_C, EconomicConservatism, SocialConservatism,
                            VoteFor2020.clean, VoteMethod.text, Vote_1, Vote_Post_1, lean_bidentrump_Post_4),
                by = all_of(by),
                type = list(all_continuous() ~ "continuous2",
                            c(DemRep_C, religion, EconomicConservatism, SocialConservatism) ~ "continuous2"),
                statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} [{min},{max}]"),
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = list(all_continuous() ~ 2, all_categorical() ~ 2)) %>%
    add_n() %>% add_overall() %>%
    # Fisher's exact test for the many-level vote variable needs a larger workspace than R's default
    add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2),
          test.args = if (by == "Conspiracy") list(VoteFor2020.clean ~ list(workspace = 2e7)) else NULL)
}
slim <- function(tb, d, by) {
  d <- d %>% ungroup() %>% filter(!is.na(.data[[by]]))
  list(body = tb$table_body %>% select(any_of(c("variable", "var_label", "row_type", "label", "n", "stat_0", "stat_1", "stat_2", "p.value"))),
       overall_n = nrow(d), by_level = names(table(d[[by]])), by_n = as.integer(table(d[[by]])))
}

# A3: attrition (dropped out during the conversation / before the post-intervention measures) among participants in
# the analytic sample (df_both) who were randomized to a condition
df_rand <- df_both %>% ungroup() %>% filter(!is.na(ConditionChat), !is.na(ConditionFeedback))
attrition <- df_rand %>%
  group_by(Study, ConditionChat, ConditionFeedback) %>%
  summarise(n = n(), n_attrit = sum(AttritDurExp == 1, na.rm = TRUE), attrition_pct = mean(AttritDurExp == 1, na.rm = TRUE) * 100,
            .groups = "drop")

# Chi-squared tests of attrition by assignment (Method, "Attrition Rates"). Study 1: the six message x interface cells,
# then interface and message separately; Study 2: message condition.
chi <- function(x, y) { t <- suppressWarnings(chisq.test(table(x, y))); c(chisq = unname(t$statistic), df = unname(t$parameter), p = t$p.value) }
rand1 <- df_rand %>% filter(Study == 1) %>% droplevels()
rand2 <- df_rand %>% filter(Study == 2) %>% droplevels()
attrition_tests <- rbind(
  s1_cells     = chi(rand1$AttritDurExp, interaction(rand1$ConditionChat, rand1$ConditionFeedback)),
  s1_interface = chi(rand1$AttritDurExp, rand1$ConditionChat),
  s1_message   = chi(rand1$AttritDurExp, rand1$ConditionFeedback),
  s2_message   = chi(rand2$AttritDurExp, rand2$ConditionFeedback))

# Sample sizes by message condition and by message x interface cell (Method, "Participants")
cell_n <- df_both %>% ungroup() %>% count(Study, ConditionFeedback, ConditionChat)

# Conversation length and length of the turns (Method, "LLM Conversation Intervention"). Length in minutes is per
# participant (ConvoTime, built in 01_cleaning.Rmd); words are per non-empty turn, GPT and participant turns of
# rounds 1-4, and the condition rows pool both speakers.
turns <- df_both %>% ungroup() %>%
  pivot_longer(cols = starts_with("userResponse") | starts_with("GPTResponse"), names_to = c("Speaker", "Round"),
               names_pattern = "(user|GPT)Response_(\\d)", values_to = "Text") %>%
  filter(as.numeric(Round) < 5, !is.na(Text), Text != "") %>%
  mutate(words = str_count(Text, "\\w+"))
conversation <- list(
  minutes_overall = mean(df_both$ConvoTime, na.rm = TRUE),
  minutes_by_condition = df_both %>% ungroup() %>% filter(!is.na(ConditionFeedback)) %>%
    group_by(ConditionFeedback) %>% summarise(minutes = mean(ConvoTime, na.rm = TRUE), .groups = "drop"),
  words_by_speaker = turns %>% group_by(Speaker) %>% summarise(words = mean(words), .groups = "drop"),
  words_by_condition = turns %>% filter(!is.na(ConditionFeedback)) %>%
    group_by(ConditionFeedback) %>% summarise(words = mean(words), .groups = "drop"))

dir.create(file.path(analysis_dir, "results"), showWarnings = FALSE)
saveRDS(list(
  a1 = slim(make_summary(df_both, "Study"), df_both, "Study"),
  a2 = slim(make_summary(df_all, "Conspiracy"), df_all, "Conspiracy"),
  a3 = attrition,
  attrition_tests = attrition_tests, cell_n = cell_n, conversation = conversation
), file.path(analysis_dir, "results/descriptives.rds"))
cat("saved results/descriptives.rds\n")
