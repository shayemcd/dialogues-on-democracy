#' Categorize strings using an LLM
#'
#' This function takes a column of strings from a tibble and returns category ratings
#' based on a custom system prompt using the ellmer package.
#'
#' @param data A tibble or data frame
#' @param column The name or index of the column containing strings to categorize
#' @param system_prompt A custom system prompt that explains the categorization task
#' @param categories A character vector of possible categories (optional)
#' @param custom_instruction Additional instruction to add to each request (optional)
#' @param provider The chat provider to use, defaults to "openai" 
#' @param model The model to use, if NULL uses the default for the provider
#' @param parallel Logical, whether to process in parallel (faster but more expensive)
#' @param max_parallel Maximum number of parallel requests if parallel=TRUE
#' @param batch_size Number of items to process per batch if not parallel
#' @param echo How to display progress: "none", "text", or "all"
#'
#' @return A tibble with the original data plus a new column containing the categorizations
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ellmer)
#' 
#' # Simple example with customer feedback
#' feedback <- tibble(
#'   id = 1:3,
#'   text = c(
#'     "The product arrived damaged and customer service was unhelpful.",
#'     "Great experience, will definitely buy again!",
#'     "Shipping was slow but the product quality is excellent."
#'   )
#' )
#' 
#' # Categorize sentiment
#' result <- categorize_strings(
#'   data = feedback,
#'   column = "text",
#'   system_prompt = "You are a sentiment analysis expert. Categorize each text as positive, negative, or mixed.",
#'   categories = c("positive", "negative", "mixed")
#' )
#' }
#' @export
categorize_strings <- function(data, 
                               column,
                               system_prompt,
                               categories = NULL,
                               custom_instruction = NULL,
                               provider = "openai",
                               model = NULL,
                               parallel = FALSE,
                               max_parallel = 10,
                               batch_size = 20,
                               echo = "text") {
  
  # Input validation
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for this function")
  }
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble")
  }
  
  # Handle column input as name or index
  if (is.character(column)) {
    if (!column %in% names(data)) {
      stop(paste0("Column '", column, "' not found in data"))
    }
    col_name <- column
  } else if (is.numeric(column)) {
    if (column > ncol(data)) {
      stop(paste0("Column index ", column, " exceeds number of columns in data"))
    }
    col_name <- names(data)[column]
  } else {
    stop("'column' must be a column name or index")
  }
  
  # Extract the column to categorize
  texts <- data[[col_name]]
  
  # Choose the provider function based on input
  provider_fn <- switch(
    provider,
    "openai" = ellmer::chat_openai,
    "claude" = ellmer::chat_claude,
    "gemini" = ellmer::chat_gemini,
    "ollama" = ellmer::chat_ollama,
    "bedrock" = ellmer::chat_bedrock,
    "azure" = ellmer::chat_azure,
    "snowflake" = ellmer::chat_snowflake,
    stop(paste0("Unsupported provider: ", provider))
  )
  
  # Create the chat object with appropriate model and system prompt
  chat_args <- list(
    system_prompt = system_prompt,
    echo = echo
  )
  
  # Add model if specified
  if (!is.null(model)) {
    chat_args$model <- model
  }
  
  chat <- do.call(provider_fn, chat_args)
  
  # Build the prompt template
  prompt_template <- "Please analyze the following statement:\n\n"
  prompt_template <- paste0(prompt_template, "\"{text}\"\n\n")
  
  if (!is.null(categories)) {
    cat_str <- paste(categories, collapse = ", ")
    prompt_template <- paste0(prompt_template, 
                              "Categorize this statement as: ", cat_str, "\n\n")
  }
  
  if (!is.null(custom_instruction)) {
    prompt_template <- paste0(prompt_template, custom_instruction, "\n\n")
  }
  
  prompt_template <- paste0(prompt_template, 
                            "Return ONLY the category name without any explanation or additional text.")
  
  # Function to process one text item
  process_item <- function(text) {
    prompt <- gsub("\\{text\\}", text, prompt_template)
    result <- chat$chat(prompt)
    return(trimws(result))
  }
  
  # Process items based on parallel setting
  if (parallel) {
    # Create all prompts first
    prompts <- lapply(texts, function(text) {
      gsub("\\{text\\}", text, prompt_template)
    })
    
    if (echo == "text" || echo == "all") {
      message(sprintf("Processing %d items in parallel mode with max_active=%d", 
                      length(prompts), max_parallel))
    }
    
    # Use chat_parallel to process in parallel with rate limiting
    # Note: ellmer internally handles the parallel_requests function you mentioned
    responses <- chat$chat_parallel(
      prompts = prompts, 
      max_active = max_parallel, 
      rpm = 500  # Default to a reasonable RPM, could be made configurable
    )
    
    # Extract the text from each response
    categories_result <- sapply(responses, function(r) {
      # Make sure to handle any potential nulls in the responses
      if (is.null(r) || is.null(r$last_turn())) {
        return(NA_character_)
      }
      return(trimws(r$last_turn()@text))
    })
    
    # Check if we have any failures that need to be retried
    failed_indices <- which(is.na(categories_result))
    if (length(failed_indices) > 0 && echo != "none") {
      message(sprintf("Retrying %d failed requests individually", length(failed_indices)))
      
      # Retry failed requests individually
      for (idx in failed_indices) {
        tryCatch({
          text <- texts[idx]
          prompt <- gsub("\\{text\\}", text, prompt_template)
          result <- chat$chat(prompt)
          categories_result[idx] <- trimws(result)
          if (echo != "none") message(sprintf("  Retry successful for item %d", idx))
        }, error = function(e) {
          if (echo != "none") message(sprintf("  Retry failed for item %d: %s", idx, e$message))
        })
      }
    }
  } else {
    # Process in batches
    n <- length(texts)
    categories_result <- character(n)
    
    for (i in seq_len(ceiling(n / batch_size))) {
      start_idx <- (i - 1) * batch_size + 1
      end_idx <- min(i * batch_size, n)
      batch_texts <- texts[start_idx:end_idx]
      
      if (echo == "text" || echo == "all") {
        message(sprintf("Processing batch %d of %d (%d items)", 
                        i, ceiling(n / batch_size), length(batch_texts)))
      }
      
      # Process each item in the batch
      for (j in seq_along(batch_texts)) {
        idx <- start_idx + j - 1
        categories_result[idx] <- process_item(batch_texts[j])
        
        if (echo == "text" || echo == "all") {
          message(sprintf("  Processed item %d of %d", idx, n))
        }
      }
    }
  }
  
  # Add the results to the original data
  data$category <- categories_result
  
  return(data)
}
