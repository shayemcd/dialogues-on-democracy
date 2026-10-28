#' Extract detailed categories from strings using an LLM
#'
#' This function takes a column of strings from a tibble and returns structured
#' category data with confidence scores using ellmer's extract_data capability.
#'
#' @param data A tibble or data frame
#' @param column The name or index of the column containing strings to categorize
#' @param system_prompt A custom system prompt that explains the categorization task
#' @param categories A character vector of possible categories
#' @param include_reasoning Logical, whether to include reasoning for each categorization
#' @param include_confidence Logical, whether to include confidence scores (0-1)
#' @param provider The chat provider to use, defaults to "openai" 
#' @param model The model to use, if NULL uses the default for the provider
#' @param parallel Logical, whether to process in parallel (faster but more expensive)
#' @param max_parallel Maximum number of parallel requests if parallel=TRUE
#' @param batch_size Number of items to process per batch if not parallel
#' @param echo How to display progress: "none", "text", or "all"
#'
#' @return A tibble with the original data plus new columns containing the structured categorizations
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ellmer)
#' 
#' # Example with customer feedback
#' feedback <- tibble(
#'   id = 1:3,
#'   text = c(
#'     "The product arrived damaged and customer service was unhelpful.",
#'     "Great experience, will definitely buy again!",
#'     "Shipping was slow but the product quality is excellent."
#'   )
#' )
#' 
#' # Extract detailed sentiment data
#' result <- extract_categories(
#'   data = feedback,
#'   column = "text",
#'   system_prompt = "You are a sentiment analysis expert. Categorize each text by sentiment.",
#'   categories = c("positive", "negative", "mixed", "neutral"),
#'   include_reasoning = TRUE,
#'   include_confidence = TRUE
#' )
#' }
#' @export
extract_categories <- function(data,
                               column,
                               system_prompt,
                               categories,
                               include_reasoning = TRUE,
                               include_confidence = TRUE,
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
  
  # Set up the type schema for structured data extraction
  if (include_reasoning && include_confidence) {
    type <- ellmer::type_object(
      category = ellmer::type_string(
        description = paste0("The category for the text, one of: ", paste(categories, collapse = ", "))
      ),
      confidence = ellmer::type_number(
        description = "Confidence score between 0 and 1, with 1 being most confident",
      ),
      reasoning = ellmer::type_string(
        description = "Brief explanation of why this category was chosen"
      )
    )
  } else if (include_reasoning) {
    type <- ellmer::type_object(
      category = ellmer::type_string(
        description = paste0("The category for the text, one of: ", paste(categories, collapse = ", "))
      ),
      reasoning = ellmer::type_string(
        description = "Brief explanation of why this category was chosen"
      )
    )
  } else if (include_confidence) {
    type <- ellmer::type_object(
      category = ellmer::type_string(
        description = paste0("The category for the text, one of: ", paste(categories, collapse = ", "))
      ),
      confidence = ellmer::type_number(
        description = "Confidence score between 0 and 1, with 1 being most confident",
      )
    )
  } else {
    type <- ellmer::type_object(
      category = ellmer::type_string(
        description = paste0("The category for the text, one of: ", paste(categories, collapse = ", "))
      )
    )
  }
  
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
  
  # Function to process one text item
  process_item <- function(text) {
    # Build a prompt that aligns with the system prompt role
    prompt <- paste0("Please analyze the following statement:\n\n",
                     "\"", text, "\"\n\n",
                     "Categorize this statement as: ", 
                     paste(categories, collapse = ", "), ".")
    
    if (include_reasoning) {
      prompt <- paste0(prompt, " Include your reasoning for this categorization.")
    }
    
    if (include_confidence) {
      prompt <- paste0(prompt, " Also provide your confidence level (0-1) in this categorization.")
    }
    
    result <- chat$extract_data(prompt, type = type)
    return(result)
  }
  
  # Process items based on parallel setting
  if (parallel) {
    # Create all prompts first
    # Create all prompts first
    prompts <- lapply(texts, function(text) {
      # Build a prompt that aligns with the system prompt role
      prompt <- paste0("Please analyze the following statement:\n\n",
                       "\"", text, "\"\n\n",
                       "Categorize this statement as: ", 
                       paste(categories, collapse = ", "), ".")
      
      if (include_reasoning) {
        prompt <- paste0(prompt, " Include your reasoning for this categorization.")
      }
      
      if (include_confidence) {
        prompt <- paste0(prompt, " Also provide your confidence level (0-1) in this categorization.")
      }
      
      return(prompt)
    })
    
    if (echo == "text" || echo == "all") {
      message(sprintf("Processing %d items in parallel with structured data extraction", 
                      length(prompts)))
    }
    
    # Use extract_data_parallel to process in parallel with rate limiting
    results <- chat$extract_data_parallel(
      prompts = prompts, 
      type = type, 
      max_active = max_parallel,
      rpm = 500  # Default RPM, could be configurable
    )
    
    # Handle potential failures by checking each result
    failed_indices <- integer(0)
    for (i in seq_along(results)) {
      if (is.null(results[[i]]) || 
          !is.list(results[[i]]) || 
          is.null(results[[i]]$category) || 
          results[[i]]$category == "") {
        failed_indices <- c(failed_indices, i)
      }
    }
    
    # Retry failed requests individually if any
    if (length(failed_indices) > 0) {
      if (echo != "none") {
        message(sprintf("Retrying %d failed structured data extractions individually", 
                        length(failed_indices)))
      }
      
      for (idx in failed_indices) {
        tryCatch({
          text <- texts[idx]
          results[[idx]] <- process_item(text)
          if (echo != "none") message(sprintf("  Retry successful for item %d", idx))
        }, error = function(e) {
          if (echo != "none") message(sprintf("  Retry failed for item %d: %s", idx, e$message))
          # Create an empty result with appropriate structure to prevent errors later
          empty_result <- list(category = NA_character_)
          if (include_confidence) empty_result$confidence <- NA_real_
          if (include_reasoning) empty_result$reasoning <- NA_character_
          results[[idx]] <- empty_result
        })
      }
    }
  } else {
    # Process in batches
    n <- length(texts)
    results <- vector("list", n)
    
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
        results[[idx]] <- process_item(batch_texts[j])
        
        if (echo == "text" || echo == "all") {
          message(sprintf("  Processed item %d of %d", idx, n))
        }
      }
    }
  }
  
  # Extract and add results to the original data
  data$category <- sapply(results, function(r) r$category)
  
  if (include_confidence) {
    data$confidence <- sapply(results, function(r) r$confidence)
  }
  
  if (include_reasoning) {
    data$reasoning <- sapply(results, function(r) r$reasoning)
  }
  
  return(data)
}
