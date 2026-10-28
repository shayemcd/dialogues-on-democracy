#!/usr/bin/env Rscript
# ==============================================================================
# PARALLEL BATCH CODING: 16-Strategy Battery & Argument Quality
# ==============================================================================
# Model: gpt-4o-2024-08-06
# Concurrency: future/furrr parallelized (default 6 workers)
# Checkpointing: saves incrementally every 25 conversations to:
#   4-analysis/data/df_strategy_coded_checkpoint.rds
#
# USAGE (from terminal with OPENAI_API_KEY exported):
#   caffeinate -i Rscript "4-analysis/scripts/02b_llm_strategy_coding.R"
# ==============================================================================

cat("\n====================================================================\n")
cat("Starting 16-Strategy & Argument Quality Parallel Coding Pass\n")
cat("====================================================================\n")
flush.console()

suppressMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) try(install.packages("pacman", repos = "https://cloud.r-project.org"), silent = TRUE)
  pacman::p_load(tidyverse, openai, jsonlite, future, furrr)
})

# Path configuration
local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/02b_llm_strategy_coding.R")
})

data_dir <- here::here("data")
checkpoint_path <- file.path(data_dir, "df_strategy_coded_checkpoint.rds")
clean_data_path <- file.path(data_dir, "clean_dfs.RData")

N_WORKERS <- 6
CHECKPOINT_EVERY <- 25

api_key <- Sys.getenv("OPENAI_API_KEY")
if (api_key == "") {
  cat("\n[ERROR] OPENAI_API_KEY is not set!\n")
  cat("Please export it in your shell: export OPENAI_API_KEY='sk-...'\n")
  cat("Or in R before running: Sys.setenv(OPENAI_API_KEY = 'sk-...')\n\n")
  stop("Missing OPENAI_API_KEY")
} else {
  cat("✓ OpenAI API key detected\n")
  flush.console()
}

if (!file.exists(clean_data_path)) {
  stop("clean_dfs.RData not found at: ", clean_data_path)
}

cat("✓ Loading clean_dfs.RData... ")
flush.console()
load(clean_data_path)  # loads df_both
cat("done.\n")
flush.console()

# --- 1. Build conversation texts ---
build_convo_text <- function(row) {
  parts <- c()
  for (r in 1:4) {
    gpt_txt <- row[[paste0("GPTResponse_", r)]]
    usr_txt <- row[[paste0("userResponse_", r)]]
    if (!is.na(gpt_txt) && nzchar(trimws(gpt_txt))) {
      parts <- c(parts, sprintf("Persuader, Response %d: %s", r, gpt_txt))
    }
    if (!is.na(usr_txt) && nzchar(trimws(usr_txt))) {
      parts <- c(parts, sprintf("Respondent, Response %d: %s", r, usr_txt))
    }
  }
  paste(parts, collapse = "\n\n")
}

df_to_code <- df_both %>%
  filter(!is.na(GPTResponse_1) & nzchar(trimws(GPTResponse_1))) %>%
  distinct(ResponseId, .keep_all = TRUE)

cat("✓ Building conversation texts for", nrow(df_to_code), "conversations... ")
flush.console()
df_to_code$ConvoText <- purrr::map_chr(seq_len(nrow(df_to_code)), function(i) build_convo_text(df_to_code[i, ]))
cat("done.\n")
flush.console()

# --- 2. System prompt and parsing helpers ---
coding_system_prompt <- r"(You are about to be shown the text of a written conversation. One person (the Persuader) is trying to change the other person's (the Respondent's) view on a specific topic. In most conversations the topic is a conspiracy theory about U.S. elections, and the Persuader is trying to convince the Respondent that their belief is mistaken. In a smaller number of comparison conversations, the topic is instead an unrelated, lighter debate (for example, whether cats or dogs make better pets) used as a persuasion-task control condition - the Persuader is still genuinely trying to change the Respondent's mind, just on that different topic. Rate every conversation on the exact same criteria below, regardless of topic: the goal is to measure how well the Persuader argues, not what they argue about. The conversation has up to 4 rounds, but not every conversation reaches all 4 rounds.

Your job is to process the conversation and return a rating of each of the PERSUADER'S responses only. Use the Respondent's responses only for context.

** Persuasion Strategy List **

Rate each of the following on a 0-10 scale (integers only): 0 = not used at all, 10 = used centrally and extensively throughout the response.

Rapport: Establishes a respectful and understanding relationship with the Respondent.
Critical_Thinking: Encourages the Respondent to question and analyze the logic, evidence, or reasoning behind their current position.
Alternative_Explanations: Provides plausible, evidence-based alternative explanations or perspectives that run counter to the Respondent's stated position.
Credible_Sources: Introduces information from reliable, respected, neutral sources.
Harm: Discusses the personal or practical downsides of the Respondent's current position.
Stories_Examples: Shares stories, anecdotes, or real-world examples.
Encourage_Empathy: Helps the Respondent consider other perspectives, or the impact of their position on others.
Socratic_Questioning: Uses questions that lead the Respondent to reflect on and examine their own position.
Conflicting_Evidence: Introduces facts, data, or arguments that directly contradict the Respondent's stated position.
Expert_Consensus: Emphasizes agreement among experts or credible authorities.
Common_Ground: Identifies and builds on beliefs or values shared with the Respondent.
Psychological_Needs: Addresses emotional or psychological factors that may underlie the Respondent's attachment to their position (e.g., desire for control, identity).
Inconsistencies_Fallacies: Identifies logical inconsistencies or fallacies in the Respondent's argument for their position.
Analogies_Metaphors: Uses analogies or metaphors to simplify complex information.
Maintaining_Patience: Demonstrates patience, avoiding pressure for immediate agreement.
Offer_Resources: Provides resources for further learning (articles, books, websites).

** Additional Ratings **

Rate each of the following on a 0-10 scale (integers only), except EvidenceCount which is a raw count with no upper limit.

ArgumentQuality (0-10): how strong, logically coherent, and well-supported the Persuader's argument is in this round, on whatever topic the conversation is actually about. 0 = no real argument is made at all (e.g., a bare assertion with no support), 10 = a rigorous, well-evidenced, logically airtight argument.
Nuance (0-10): the degree to which the response engages thoughtfully with the Respondent's specific position rather than dismissively or simplistically. 0 = curt, dismissive, or black-and-white, 10 = highly nuanced, acknowledges complexity/uncertainty, steelmans the Respondent's position before countering it.
Specificity (0-10): how directly the response is tailored to the Respondent's own stated reasons/evidence (from their prior turns) rather than a generic, one-size-fits-all rebuttal. 0 = entirely generic, ignores what the Respondent actually said, 10 = precisely tailored to what this specific Respondent said.
Tone (0-10): how respectful, warm, and non-condescending the response is, based on the actual language used. 0 = confrontational/patronizing/dismissive, 10 = warm and respectful throughout.
EvidenceCount: an integer count (0 or more) of distinct factual claims, statistics, studies, or named sources cited in this round in support of the Persuader's position. 0 if none.
# Note on EvidenceCount vs. 02a EvidenceFreq:
# EvidenceCount here is coded per individual turn using a strict factual claims / sources criterion.
# When summed across rounds per participant, Adversarial Dialogue has a mean EvidenceCount of 0.16
# (87.32% zero; 0.23 in S1, 0.01 in S2), while treatment conditions average 4.96 to 5.53.
# This differs from the conversation-level EvidenceFreq in 02a_llm_theme_coding.R (mean 0.91; 84.71% zero),
# which evaluated the entire concatenated transcript at once with a broader prompt definition.
# The manuscript Table 1 (@tbl-conditions) reports these turn-level metrics (Option B),
# describing treatment evidence as "5-6 pieces of evidence within the entire conversation on average"
# (4.96 in Belief & Values-Tailored, 5.53 in Belief-Tailored).

All of these ratings apply in exactly the same way regardless of the conversation's topic (election conspiracy or the comparison topic) - rate the quality of the persuasive attempt itself, not whether the topic happens to be about elections.

** Output format **

Return ONLY a JSON object of this exact shape, with one array element per Persuader round actually present in the conversation (skip rounds where the Persuader did not respond):

{
  "rounds": [
    {
      "round": 1,
      "Rapport": 0,
      "Critical_Thinking": 0,
      "Alternative_Explanations": 0,
      "Credible_Sources": 0,
      "Harm": 0,
      "Stories_Examples": 0,
      "Encourage_Empathy": 0,
      "Socratic_Questioning": 0,
      "Conflicting_Evidence": 0,
      "Expert_Consensus": 0,
      "Common_Ground": 0,
      "Psychological_Needs": 0,
      "Inconsistencies_Fallacies": 0,
      "Analogies_Metaphors": 0,
      "Maintaining_Patience": 0,
      "Offer_Resources": 0,
      "ArgumentQuality": 0,
      "Nuance": 0,
      "Specificity": 0,
      "Tone": 0,
      "EvidenceCount": 0
    }
  ]
}

Do not include any text outside the JSON object.)"

safe_parse_rounds <- function(json_str, response_id) {
  tryCatch({
    json_str <- gsub("```json\\n|```\\n|```", "", json_str)
    parsed <- jsonlite::fromJSON(json_str)$rounds
    parsed <- as_tibble(parsed) %>% mutate(ResponseId = response_id, .before = 1)
    parsed
  }, error = function(e) {
    tibble(ResponseId = response_id, round = NA_integer_, parse_error = TRUE)
  })
}

code_one_conversation <- function(response_id, convo_text, model = "gpt-4o-2024-08-06", retry_limit = 5) {
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    result <- tryCatch({
      completion <- openai::create_chat_completion(
        model = model,
        temperature = 0,
        messages = list(
          list(role = "system", content = coding_system_prompt),
          list(role = "user", content = convo_text)
        )
      )
      completion$choices$message.content
    }, error = function(e) {
      message(sprintf("ResponseId %s attempt %d failed: %s", response_id, attempt, conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) return(safe_parse_rounds(result, response_id))
    if (attempt >= retry_limit) {
      message(sprintf("Max retries reached for ResponseId %s - coding as NA", response_id))
      return(tibble(ResponseId = response_id, round = NA_integer_, parse_error = TRUE))
    }
    Sys.sleep(2 * attempt)
  }
}

# --- 3. Parallel batch execution ---
already_coded <- if (file.exists(checkpoint_path)) readRDS(checkpoint_path) else tibble(ResponseId = character())
remaining_ids <- setdiff(df_to_code$ResponseId, unique(already_coded$ResponseId))
df_remaining <- df_to_code %>% filter(ResponseId %in% remaining_ids)

if (nrow(df_remaining) == 0) {
  cat(sprintf("\nAll %d conversations in df_to_code are already coded in checkpoint. Nothing to do!\n",
              nrow(df_to_code)))
  flush.console()
} else {
  cat(sprintf("\n=== STARTING STRATEGY & QUALITY CODING ===\n"))
  cat(sprintf("Already coded in checkpoint: %d\n", length(unique(already_coded$ResponseId))))
  cat(sprintf("Remaining to code: %d\n", nrow(df_remaining)))
  cat(sprintf("Workers: %d | Batch size: %d | Model: gpt-4o-2024-08-06\n\n", N_WORKERS, CHECKPOINT_EVERY))
  flush.console()

  plan(multisession, workers = N_WORKERS)
  batch_indices <- split(seq_len(nrow(df_remaining)), ceiling(seq_len(nrow(df_remaining)) / CHECKPOINT_EVERY))
  start_time <- proc.time()[["elapsed"]]
  n_done <- 0

  for (b_idx in seq_along(batch_indices)) {
    batch <- batch_indices[[b_idx]]
    df_batch <- df_remaining[batch, ]
    cat(sprintf("-> Processing batch %d of %d (%d conversations)... ", b_idx, length(batch_indices), nrow(df_batch)))
    flush.console()

    batch_results <- future_map2(
      df_batch$ResponseId, df_batch$ConvoText, code_one_conversation,
      .options = furrr_options(seed = TRUE)
    )
    new_rows <- bind_rows(batch_results)
    already_coded <- bind_rows(already_coded, new_rows)
    saveRDS(already_coded, checkpoint_path)

    n_done <- n_done + nrow(df_batch)
    elapsed_min <- (proc.time()[["elapsed"]] - start_time) / 60
    rate <- n_done / elapsed_min
    eta_min <- (nrow(df_remaining) - n_done) / rate
    cat(sprintf("done! [%d/%d total, %.1f%% | %.1f min elapsed | ETA: ~%.1f min]\n",
                n_done, nrow(df_remaining), 100 * (n_done / nrow(df_remaining)), elapsed_min, eta_min))
    flush.console()
  }
  plan(sequential)

  cat(sprintf("\n=== STRATEGY CODING COMPLETE ===\n"))
  cat(sprintf("Total conversations coded: %d\n", n_distinct(already_coded$ResponseId)))
  cat(sprintf("Total rows in checkpoint: %d\n", nrow(already_coded)))
  cat(sprintf("Saved to: %s\n\n", checkpoint_path))
  flush.console()
}
