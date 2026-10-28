#!/usr/bin/env Rscript
# ==============================================================================
# PARALLEL BATCH CODING: Targeted Nuance ("Clear-Claim" Definition)
# ==============================================================================
# Model: gpt-4o-2024-08-06
# Concurrency: future/furrr parallelized (default 6 workers)
# Checkpointing: saves incrementally every 25 conversations to:
#   4-analysis/data/df_nuance_clearclaim_checkpoint.rds
#
# USAGE (from terminal with OPENAI_API_KEY exported):
#   caffeinate -i Rscript "4-analysis/scripts/02c_llm_nuance_coding.R"
# ==============================================================================

cat("\n====================================================================\n")
cat("Starting Targeted Nuance (Clear-Claim) Parallel Coding Pass\n")
cat("====================================================================\n")
flush.console()

suppressMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) try(install.packages("pacman", repos = "https://cloud.r-project.org"), silent = TRUE)
  pacman::p_load(tidyverse, openai, jsonlite, future, furrr)
})

# Path configuration
# Locate 4-analysis/ regardless of R's working directory. here::here() walks up from getwd() to the
# .here marker in 4-analysis/, so if R was started elsewhere (e.g. your home folder) first move into
# this file's folder (works from RStudio and VS Code; needs the rstudioapi package).
local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/02c_llm_nuance_coding.R")
})

data_dir <- here::here("data")
checkpoint_path <- file.path(data_dir, "df_nuance_clearclaim_checkpoint.rds")
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
nuance_system_prompt <- r"(You are about to be shown the text of a written conversation. One person (the Persuader) is trying to change the other person's (the Respondent's) view on a specific topic. In most conversations the topic is a conspiracy theory about U.S. elections; in a smaller number of comparison conversations the topic is an unrelated, lighter debate (for example, whether cats or dogs make better pets) used as a persuasion-task control condition - the Persuader is still genuinely trying to change the Respondent's mind, just on that different topic. Rate every conversation on the same criterion below regardless of topic. The conversation has up to 4 rounds, but not every conversation reaches all 4 rounds.

Your job is to rate each of the PERSUADER'S responses only, on the construct below. Use the Respondent's responses only for context.

Nuance (0-10, integer): the degree to which the response makes a clear, defensible claim WHILE ALSO acknowledging relevant qualifications, competing considerations, boundary conditions, or uncertainty - rather than presenting the issue as simply one-sided. A response is NOT nuanced merely because it hedges, avoids taking a position, or sounds tentative - to score high it must combine a clear, defensible claim with honest acknowledgment of that claim's limits or of legitimate competing considerations. A confident response with zero acknowledgment of complexity is low-Nuance. A response that only hedges and never lands a clear claim is ALSO low-Nuance, not high-Nuance.

0 = presents the issue as simply one-sided, with no acknowledgment of any qualification, competing consideration, boundary condition, or uncertainty (regardless of whether a clear claim is made).
5 = makes a claim but only partially or superficially acknowledges qualifications/competing considerations, OR hedges so much that no clear, defensible claim is actually made.
10 = makes a clear, defensible claim AND substantively acknowledges relevant qualifications, competing considerations, boundary conditions, or uncertainty.

** Output format **

Return ONLY a JSON object of this exact shape, with one array element per Persuader round actually present in the conversation (skip rounds where the Persuader did not respond):

{
  "rounds": [
    { "round": 1, "Nuance": 0 }
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
          list(role = "system", content = nuance_system_prompt),
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
  cat(sprintf("\n=== STARTING TARGETED NUANCE (CLEAR-CLAIM) CODING ===\n"))
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

  cat(sprintf("\n=== NUANCE CODING COMPLETE ===\n"))
  cat(sprintf("Total conversations coded: %d\n", n_distinct(already_coded$ResponseId)))
  cat(sprintf("Total rows in checkpoint: %d\n", nrow(already_coded)))
  cat(sprintf("Saved to: %s\n\n", checkpoint_path))
  flush.console()
}
