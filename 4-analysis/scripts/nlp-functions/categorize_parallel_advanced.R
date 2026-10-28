#' Advanced parallel categorization with custom parallelization
#'
#' This function uses the future and furrr packages to provide more flexible
#' parallelization options, including parallel processing across multiple cores
#' or even multiple machines.
#'
#' @param data A tibble or data frame
#' @param column The name or index of the column containing strings to categorize
#' @param system_prompt A custom system prompt that explains the categorization task
#' @param categories A character vector of possible categories
#' @param include_reasoning Logical, whether to include reasoning for each categorization
#' @param include_confidence Logical, whether to include confidence scores (0-1)
#' @param provider The chat provider to use, defaults to "openai"
#' @param model The model to use, if NULL uses the default for the provider
#' @param parallel_strategy String indicating the parallelization strategy:
#'   "multisession" (multiple cores), "multicore" (forked processes, not on Windows),
#'   or "cluster" (multiple machines)
#' @param workers Number of parallel workers to use
#' @param chunk_size Size of chunks for processing
#' @param rate_limit_per_minute Maximum API calls per minute (rate limiting)
#' @param echo How to display progress: "none", "text", or "all"
#' @param show_progress Whether to display a progress bar (TRUE) or just logging (FALSE)
#' @param progress_update_interval How often to update the progress bar in seconds
#'
#' @return A tibble with the original data plus new columns containing the structured categorizations
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' library(future)
#' library(furrr)
#' library(tibble)
#' 
#' # Example with larger dataset
#' reviews <- tibble(
#'   id = 1:100,
#'   text = rep(c(
#'     "Great product, highly recommend!",
#'     "Terrible quality, avoid at all costs.",
#'     "Average product, nothing special."
#'   ), length.out = 100)
#' )
#' 
#' result <- categorize_parallel_advanced(
#'   data = reviews,
#'   column = "text",
#'   system_prompt = "You are a sentiment analyst. Categorize by sentiment.",
#'   categories = c("positive", "negative", "neutral"),
#'   parallel_strategy = "multisession",
#'   workers = 4,
#'   chunk_size = 10
#' )
#' }
#' @export
categorize_parallel_advanced <- function(data,
                                         column,
                                         system_prompt,
                                         categories,
                                         include_reasoning = TRUE,
                                         include_confidence = TRUE,
                                         provider = "openai",
                                         model = NULL,
                                         parallel_strategy = "multisession",
                                         workers = 4,
                                         chunk_size = 10,
                                         rate_limit_per_minute = 500,
                                         echo = "text",
                                         show_progress = TRUE,
                                         progress_update_interval = 0.5) {
  
  # Check for required packages
  for (pkg in c("future", "furrr", "progressr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required for advanced parallelization. ",
                  "Please install it with install.packages('", pkg, "')"))
    }
  }
  
  # Input validation (similar to previous functions)
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble")
  }
  
  # Handle column input
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
  n <- length(texts)
  
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
  
  # Initialize the parallelization strategy
  future::plan(parallel_strategy, workers = workers)
  
  # Configure progress reporting
  if (echo != "none") {
    if (show_progress) {
      # Create a progress bar
      progressr::handlers(progressr::handler_progress(
        format = "[:bar] :percent | Items: :current/:total | Elapsed: :elapsed | ETA: :eta",
        clear = FALSE,
        width = 80,
        # Update more frequently for smoother progress
        update_interval = progress_update_interval
      ))
    } else {
      # Just use logging without a progress bar
      progressr::handlers(progressr::handler_logger())
    }
  }
  
  # Split texts into chunks for processing
  chunks <- split(seq_along(texts), ceiling(seq_along(texts) / chunk_size))
  
  # Define the worker function with better progress tracking
  process_chunk <- function(indices, p) {
    # Set up a new chat instance
    provider_fn <- eval(parse(text = paste0("ellmer::chat_", provider)))
    chat_args <- list(system_prompt = system_prompt, echo = "none")
    if (!is.null(model)) chat_args$model <- model
    chat <- do.call(provider_fn, chat_args)
    
    # Process each item in the chunk
    results <- vector("list", length(indices))
    
    # Create a throttle mechanism for rate limiting
    last_call_time <- Sys.time() - 1  # Start with ability to make a call
    min_interval <- 60 / rate_limit_per_minute
    
    # Start time for this chunk (for logging)
    chunk_start_time <- Sys.time()
    
    for (i in seq_along(indices)) {
      idx <- indices[i]
      
      # Rate limiting with adaptive wait
      if (i > 1 && rate_limit_per_minute < 600) {
        elapsed <- as.numeric(difftime(Sys.time(), last_call_time, units = "secs"))
        if (elapsed < min_interval) {
          Sys.sleep(min_interval - elapsed)
        }
      }
      last_call_time <- Sys.time()
      
      # Update progress for each item (not chunk)
      if (echo != "none") {
        # Signal exactly one step of progress
        p(amount = 1, message = sprintf("Item %d", idx))
      }
      
      # Create prompt and extract data
      text <- texts[idx]
      
      # Build a prompt that aligns better with the system prompt
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
      
      # Handle potential errors
      tryCatch({
        results[[i]] <- chat$extract_data(prompt, type = type)
      }, error = function(e) {
        # Create an empty result with appropriate structure
        empty_result <- list(category = NA_character_)
        if (include_confidence) empty_result$confidence <- NA_real_
        if (include_reasoning) empty_result$reasoning <- NA_character_
        results[[i]] <- empty_result
      })
    }
    
    return(results)
  }
  
  # Run the parallel processing with improved progress reporting
  progressr::with_progress({
    # Create a progressor that tracks overall items rather than chunks
    p <- progressr::progressor(steps = n)
    
    # Use future_map to process chunks in parallel
    results_by_chunk <- furrr::future_map(
      chunks, 
      function(indices) {
        # Create a special function that updates global progress counter
        # rather than just chunk-level progress
        result <- process_chunk(indices, p)
        return(result)
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  }, handlers = if(echo != "none") {
    # Use a more accurate progress handler
    progressr::handler_progress(
      format = "[:bar] :percent | Items: :current/:total | Elapsed: :elapsed | ETA: :eta",
      clear = FALSE,
      width = 80
    )
  })
  
  # Flatten the results
  results <- unlist(results_by_chunk, recursive = FALSE)
  
  # Make sure results are in the correct order
  results <- results[order(unlist(chunks))]
  
  # Add the results to the original data
  data$category <- sapply(results, function(r) r$category)
  
  if (include_confidence) {
    data$confidence <- sapply(results, function(r) r$confidence)
  }
  
  if (include_reasoning) {
    data$reasoning <- sapply(results, function(r) r$reasoning)
  }
  
  # Return to sequential mode
  future::plan(future::sequential)
  
  return(data)
}
