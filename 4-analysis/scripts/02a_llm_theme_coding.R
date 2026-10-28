#!/usr/bin/env Rscript
# ==============================================================================
# Script 02a: LLM Theme & Topic Coding (Incremental & Resumable)
# ==============================================================================
# Model: gpt-4o-2024-08-06 (temperature = 0)
#
# Tasks:
#   1. Claim Theme & Subtheme Coding on userResponse_1 (initial conspiracy claim)
#   2. Conversation-Level Evidence Frequency Coding on GPTResponses
#
# Input:
#   - `4-analysis/data/clean_dfs.RData` (`df_both`, full analytic sample)
#   - `4-analysis/data/df_convosfin.xlsx` (existing coded conversations)
#
# Output:
#   - `4-analysis/data/df_convosfin.xlsx`: Slimmed dataset containing only
#     conversation text and NLP response data (no raw survey clutter).
#
# Checkpointing:
#   - Incremental progress is saved to `4-analysis/data/df_convos_checkpoint.rds`
#     every 10 conversations so no work or API cost is lost if interrupted.
#
# USAGE:
#   1. Add your OpenAI API key to .env at the root of the project:
#        OPENAI_API_KEY="sk-..."
#   2. Run from terminal or RStudio:
#        Rscript "4-analysis/scripts/02a_llm_theme_coding.R"
# ==============================================================================

cat("\n====================================================================\n")
cat("Starting LLM Theme & Evidence Coding Pass (02a)\n")
cat("====================================================================\n\n")
flush.console()

# --- 1. Path & Environment Setup ---
local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/02a_llm_theme_coding.R")
})

# Automatic .env loader (checks repo root, parent dirs, and here() root)
load_env <- function() {
  candidates <- c(
    ".env",
    "../.env",
    "../../.env",
    file.path(here::here(), ".env"),
    file.path(here::here(), "..", ".env")
  )
  for (p in unique(candidates)) {
    if (file.exists(p) && !dir.exists(p)) {
      lines <- readLines(p, warn = FALSE)
      for (l in lines) {
        l <- trimws(l)
        if (nzchar(l) && !startsWith(l, "#") && grepl("=", l, fixed = TRUE)) {
          parts <- strsplit(l, "=", fixed = TRUE)[[1]]
          k <- trimws(parts[1])
          v <- trimws(paste(parts[-1], collapse = "="))
          v <- gsub("^['\"]|['\"]$", "", v)
          if (nzchar(k) && !nzchar(Sys.getenv(k))) {
            args <- list(v)
            names(args) <- k
            do.call(Sys.setenv, args)
          }
        }
      }
      return(p)
    }
  }
  return(NULL)
}

env_file <- load_env()
if (!is.null(env_file)) {
  cat(sprintf("✓ Loaded environment from: %s\n", env_file))
}

api_key <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(api_key) || api_key == "your_openai_api_key_here") {
  cat("\n[ERROR] OPENAI_API_KEY is not set or contains a placeholder!\n")
  cat("Please add your actual OpenAI API key to the .env file at the project root:\n")
  cat("  OPENAI_API_KEY=\"sk-...\"\n\n")
  stop("Missing OPENAI_API_KEY")
} else {
  cat("✓ OpenAI API key detected\n\n")
  flush.console()
}

suppressMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) try(install.packages("pacman", repos = "https://cloud.r-project.org"), silent = TRUE)
  pacman::p_load(tidyverse, readxl, openxlsx, httr, jsonlite)
})

data_dir <- here::here("data")
convosfin_path <- file.path(data_dir, "df_convosfin.xlsx")
checkpoint_path <- file.path(data_dir, "df_convos_checkpoint.rds")
clean_data_path <- file.path(data_dir, "clean_dfs.RData")

MODEL <- "gpt-4o-2024-08-06"
CHECKPOINT_EVERY <- 10

# Helper to strip haven/labelled attributes and classes to prevent vctrs incompatibility
strip_labels <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], "labelled")) {
      attr(df[[col]], "label") <- NULL
      attr(df[[col]], "labels") <- NULL
      class(df[[col]]) <- setdiff(class(df[[col]]), c("labelled", "haven_labelled"))
    }
  }
  df
}

# Clean output columns to retain (only identifiers, outcomes, text, and NLP outputs)
clean_convos_cols <- c(
  "ResponseId", "Study", "ConditionFeedback", "ConditionChat", "messageType",
  "ER0Norm_Pre", "ER0Norm_Post", "ER0_Diff",
  "Sureness_Pre_1", "Sureness_Post_1", "Sureness_Diff",
  "WritingScreener", "conRestatement",
  "userResponse_1", "userResponse_2", "userResponse_3", "userResponse_4",
  "GPTResponse_1", "GPTResponse_2", "GPTResponse_3", "GPTResponse_4",
  "GPTResponses", "ConvoTime",
  "ConspiracyTheme", "ConspiracyTheme_Main", "ConspiracyTheme_Sub",
  "Theme_Confidence", "Theme_Reasoning",
  "EvidenceFreq", "EvidFreq_Confidence"
)

# --- 2. Prompts ---
theme_system_prompt <- "You are an expert in analyzing conspiracy theories related to elections. Classify the following statement into both a **high-level category** and a **subcategory** based on its content. Pay careful attention to the themes, accusations, and specific references to political figures or election-related issues. If the statement does not fit clearly into any of the categories, classify it as 'Other.'

The **high-level categories** are:
1. **Election Integrity** – Claims that suggest the election process was manipulated, rigged, or fraudulent, or generally cast doubt on the integrity of the election.
2. **Political Influence** – Claims about political figures or parties manipulating or influencing the election process.
3. **Media Bias** – Claims related to biased media coverage, misinformation, or unfair reporting.
4. **Voter Process and Legal Manipulation** – Claims about voter eligibility, voter fraud, or procedural manipulation.
5. **Political Party Allegations** – Claims about specific political parties engaging in unethical or illegal actions to influence the election.
6. **No Specific Claim** – Claims that express general doubt or belief in election fraud without providing any specific details or reasoning behind the belief (e.g., 'The election was rigged, I just know it').
7. **Gibberish or Irrelevant** – Claims that do not address the topic of election fraud or conspiracies, or are incoherent or nonsensical (e.g., random or irrelevant statements).

Each **high-level category** has the following **subcategories**:
1. **Election Integrity**:
   - **Fraud and Manipulation**: Claims suggesting widespread fraud or manipulation of the election.
   - **Voter Fraud**: Claims about illegal votes or manipulation of voter counts.
2. **Political Influence**:
   - **Political Figures’ Manipulation**: Claims that political figures manipulated the election process.
   - **Political Party Influence**: Claims that political parties rigged or influenced the election outcome.
3. **Media Bias**:
   - **Media Manipulation**: Claims that the media was biased or manipulated the election process.
   - **Censorship and Misinformation**: Claims that social media or news outlets censored certain voices or spread misinformation.
4. **Voter Process and Legal Manipulation**:
   - **Voter Fraud**: Claims about people voting illegally or voters casting multiple ballots.
   - **Voter Suppression**: Claims about voter suppression or disenfranchisement of certain groups.
   - **Mail-in Ballots**: Claims about fraudulent mail-in ballots, absentee voting, or mishandling of ballots.
5. **Political Party Allegations**:
   - **Manipulation by Political Parties**: Claims that political parties, especially the Democratic or Republican party, engaged in unethical actions.
   - **Election Interference**: Claims about political interference with the election process (e.g., through lobbying, legal actions, or strategic maneuvers).
6. **No Specific Claim**:
   - **General Election Fraud Belief**: Claims expressing belief in election fraud without specific reasons or details (e.g., 'I just think the election was rigged').
7. **Gibberish or Irrelevant**:
   - Any statement that is incoherent, irrelevant, or not directly addressing election-related topics.

Please categorize the statement below into one **high-level category** and one **subcategory**. Return only the **high-level category** and **subcategory** names, separated by a comma, with no additional explanations or extra text."

evidence_system_prompt <- "You are an expert in analyzing persuasive responses. I want to understand the frequency of use of rationality, counterevidence, critical thinking, or reason-based persuasion strategies. Specifically, **facts**, **alternative explanations**, or any form of evidence such as studies, reports, data, or logical arguments and the extent to which the user was persuaded based on **facts or reason**.

Now, please analyze the following response:
1. Determine if the response adhered to the condition above: **Did the response attempt to persuade the user by using rationality, counterevidence, or facts?**
2. If evidence is **used** in the response (e.g., counterevidence, references to studies, articles, data points), count how many distinct pieces of evidence were mentioned and return that number. If **no evidence** was provided, return `0`.

**Only return** an integer representing how many pieces of evidence were provided. If no evidence was provided, return `0`. Do not provide any additional explanation or text. Only return the integer in the specified format."

# --- 3. API Execution Function (with Retries and Rate-Limiting) ---
call_openai <- function(system_prompt, user_text, model = MODEL, retry_limit = 5) {
  if (is.na(user_text) || !nzchar(trimws(user_text))) {
    return(NA_character_)
  }
  
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    res <- tryCatch({
      resp <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          "Authorization" = paste("Bearer", api_key),
          "Content-Type" = "application/json"
        ),
        body = jsonlite::toJSON(list(
          model = model,
          temperature = 0,
          messages = list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = user_text)
          )
        ), auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(60)
      )
      
      if (httr::status_code(resp) == 200) {
        parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
        trimws(parsed$choices$message$content[1])
      } else {
        err_msg <- httr::content(resp, as = "text", encoding = "UTF-8")
        message(sprintf("API HTTP error %d: %s", httr::status_code(resp), err_msg))
        NULL
      }
    }, error = function(e) {
      message(sprintf("Attempt %d failed: %s", attempt, conditionMessage(e)))
      NULL
    })
    
    if (!is.null(res)) return(res)
    if (attempt >= retry_limit) {
      warning(sprintf("Max retries reached on attempt %d. Returning NA.", attempt))
      return(NA_character_)
    }
    Sys.sleep(1.5 * attempt)
  }
}

# --- 4. Identify Eligible vs. Already Coded Conversations ---
load(clean_data_path)

# Eligible: Analytic sample (df_both) participants who have conversation text
eligible_df <- df_both %>%
  filter(!is.na(userResponse_1), !is.na(userResponse_3)) %>%
  mutate(GPTResponses = paste(
    replace_na(GPTResponse_1, ""),
    replace_na(GPTResponse_2, ""),
    replace_na(GPTResponse_3, ""),
    replace_na(GPTResponse_4, ""),
    sep = " "
  ) %>% trimws()) %>%
  strip_labels() %>%
  select(any_of(clean_convos_cols))

# Existing coded records from df_convosfin.xlsx
existing_cv <- if (file.exists(convosfin_path)) {
  suppressMessages(read_excel(convosfin_path)) %>% strip_labels()
} else {
  tibble(ResponseId = character())
}

# Checkpoint records (if resuming an interrupted run)
checkpoint_df <- if (file.exists(checkpoint_path)) {
  cat(sprintf("✓ Found existing checkpoint: %s\n", checkpoint_path))
  readRDS(checkpoint_path) %>% strip_labels()
} else {
  NULL
}

already_coded_ids <- unique(c(
  existing_cv$ResponseId,
  if (!is.null(checkpoint_df)) checkpoint_df$ResponseId else character()
))
to_code_ids <- setdiff(eligible_df$ResponseId, already_coded_ids)
df_to_code <- eligible_df %>% filter(ResponseId %in% to_code_ids)

cat("====================================================================\n")
cat(sprintf("Total eligible conversations in df_both: %d\n", nrow(eligible_df)))
cat(sprintf("Already coded: %d\n", length(already_coded_ids)))
cat(sprintf("Remaining to code: %d\n", nrow(df_to_code)))
cat("====================================================================\n\n")
flush.console()

# --- 5. Batch Coding on Remaining Conversations ---
if (nrow(df_to_code) > 0) {
  new_results <- vector("list", nrow(df_to_code))
  
  for (i in seq_len(nrow(df_to_code))) {
    row <- df_to_code[i, ]
    rid <- row$ResponseId
    
    cat(sprintf("[%d/%d] Coding ResponseId: %s ... ", i, nrow(df_to_code), rid))
    
    # Pass 1: Theme coding on userResponse_1
    theme_raw <- call_openai(theme_system_prompt, row$userResponse_1)
    
    # Parse theme into main and sub
    parts <- strsplit(theme_raw, ",\\s*")[[1]]
    theme_main <- if (length(parts) >= 1) parts[1] else theme_raw
    theme_sub  <- if (length(parts) >= 2) parts[2] else NA_character_
    
    # Pass 2: Evidence frequency on GPTResponses
    evid_raw <- call_openai(evidence_system_prompt, row$GPTResponses)
    evid_num <- as.numeric(gsub("[^0-9]", "", evid_raw))
    if (is.na(evid_num)) evid_num <- 0
    
    cat(sprintf("Theme: '%s' | Evidence: %s\n", theme_main, as.character(evid_num)))
    flush.console()
    
    # Record row
    new_results[[i]] <- row %>%
      mutate(
        ConspiracyTheme      = theme_raw,
        ConspiracyTheme_Main = theme_main,
        ConspiracyTheme_Sub  = theme_sub,
        Theme_Confidence     = 1.0,
        Theme_Reasoning      = NA_character_,
        EvidenceFreq         = evid_num,
        EvidFreq_Confidence  = 1.0
      )
    
    # Save checkpoint incrementally
    if (i %% CHECKPOINT_EVERY == 0 || i == nrow(df_to_code)) {
      current_batch <- bind_rows(new_results[1:i])
      combined_chk <- if (is.null(checkpoint_df) || nrow(checkpoint_df) == 0) {
        current_batch
      } else {
        bind_rows(checkpoint_df, current_batch)
      }
      saveRDS(combined_chk, checkpoint_path)
      cat(sprintf("  --> Checkpoint saved (%d coded)\n", nrow(combined_chk)))
      flush.console()
    }
    
    # Gentle rate-limit pause
    Sys.sleep(0.15)
  }
  
  newly_coded_df <- bind_rows(new_results)
  checkpoint_df <- if (is.null(checkpoint_df) || nrow(checkpoint_df) == 0) {
    newly_coded_df
  } else {
    bind_rows(checkpoint_df, newly_coded_df)
  }
}

# --- 6. Combine and Slim Down Columns ---
cat("\nAssembling final clean dataset...\n")

# Combine existing df_convos records with any newly coded records
all_coded <- if (is.null(checkpoint_df) || nrow(checkpoint_df) == 0) {
  existing_cv
} else {
  bind_rows(existing_cv, checkpoint_df)
} %>%
  distinct(ResponseId, .keep_all = TRUE)

# Ensure all clean columns are present and select ONLY clean columns
for (col in clean_convos_cols) {
  if (!col %in% names(all_coded)) {
    if (col %in% names(eligible_df)) {
      match_idx <- match(all_coded$ResponseId, eligible_df$ResponseId)
      all_coded[[col]] <- eligible_df[[col]][match_idx]
    } else {
      all_coded[[col]] <- NA
    }
  }
}

final_df_convos <- all_coded %>%
  select(all_of(clean_convos_cols))

cat(sprintf("Final df_convos rows: %d | cols: %d (slimmed from raw)\n", nrow(final_df_convos), ncol(final_df_convos)))

# Save final Excel file
write.xlsx(final_df_convos, convosfin_path, overwrite = TRUE)
cat(sprintf("✓ Saved clean output to: %s\n", convosfin_path))

# Clean up checkpoint file on successful completion
if (file.exists(checkpoint_path) && nrow(df_to_code) > 0) {
  unlink(checkpoint_path)
  cat("✓ Removed temporary checkpoint file\n")
}

cat("\n====================================================================\n")
cat("Theme & Evidence Coding Pass Completed Successfully\n")
cat("====================================================================\n\n")
