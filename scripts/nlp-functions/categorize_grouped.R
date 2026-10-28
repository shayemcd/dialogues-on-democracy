#' Categorize groups of data with different system prompts
#'
#' This function allows you to apply different categorization approaches to
#' different subsets of your data, using filtering expressions and group-specific
#' system prompts.
#'
#' @param data A tibble or data frame
#' @param column The name or index of the column containing strings to categorize
#' @param groups A list of group specifications, where each group is a list with:
#'   - filter: Either a logical vector or a string with an R expression that evaluates to logical
#'   - system_prompt: System prompt specific to this group
#'   - categories: (Optional) Categories for this group
#'   - name: (Optional) Name for this group, used in the output
#' @param default_system_prompt System prompt to use for rows not matching any filter
#' @param default_categories Categories to use for rows not matching any filter
#' @param include_reasoning Logical, whether to include reasoning
#' @param include_confidence Logical, whether to include confidence scores
#' @param include_group_id Logical, whether to include a group identifier column
#' @param provider The chat provider to use, defaults to "openai"
#' @param model The model to use, if NULL uses the default for the provider
#' @param parallel Logical, whether to process in parallel
#' @param workers Number of parallel workers to use (if using advanced parallelization)
#' @param use_advanced_parallel Logical, whether to use the advanced parallel function
#'
#' @return A tibble with the original data plus categorization columns
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' library(dplyr)
#' library(tibble)
#' 
#' # Sample data with different types of statements
#' statements <- tibble(
#'   id = 1:6,
#'   text = c(
#'     "The government is hiding aliens at Area 51.",
#'     "Studies show vaccines are safe and effective.",
#'     "I think that conspiracy theories are harmful.",
#'     "Multiple insiders confirm a secret space program.",
#'     "Scientific evidence contradicts flat earth claims.",
#'     "I enjoy reading about unexplained phenomena."
#'   ),
#'   type = c("conspiracy", "scientific", "opinion", 
#'            "conspiracy", "scientific", "opinion")
#' )
#' 
#' # Define different approaches for different types
#' result <- categorize_grouped(
#'   data = statements,
#'   column = "text",
#'   groups = list(
#'     list(
#'       filter = "type == 'conspiracy'",
#'       system_prompt = "You expertly determine if a statement affirms or denies a conspiracy claim",
#'       categories = c("affirms", "denies", "unclear"),
#'       name = "conspiracy_analysis"
#'     ),
#'     list(
#'       filter = "type == 'scientific'",
#'       system_prompt = "You evaluate if a statement is supported by scientific consensus",
#'       categories = c("supported", "contested", "unclear"),
#'       name = "science_analysis"
#'     )
#'   ),
#'   default_system_prompt = "Categorize the sentiment of this opinion statement",
#'   default_categories = c("positive", "negative", "neutral"),
#'   include_reasoning = TRUE
#' )
#' }
#' @export
categorize_grouped <- function(data,
                               column,
                               groups,
                               default_system_prompt = NULL,
                               default_categories = NULL,
                               include_reasoning = FALSE,
                               include_confidence = FALSE,
                               include_group_id = TRUE,
                               provider = "openai",
                               model = NULL,
                               parallel = FALSE,
                               workers = 4,
                               use_advanced_parallel = FALSE) {
  
  # Input validation
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function")
  }
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble")
  }
  
  if (!is.list(groups)) {
    stop("'groups' must be a list of group specifications")
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
  
  # Add row IDs to original data for tracking
  data$.row_id <- seq_len(nrow(data))
  
  # Initialize results list and ID column
  results_list <- list()
  processed_indices <- integer(0)
  
  # Process each group
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    
    # Validate group structure
    if (!is.list(group)) {
      stop(paste0("Group ", i, " is not a list"))
    }
    
    if (is.null(group$filter)) {
      stop(paste0("Group ", i, " is missing a filter expression"))
    }
    
    if (is.null(group$system_prompt)) {
      stop(paste0("Group ", i, " is missing a system_prompt"))
    }
    
    # Get group name
    group_name <- if (!is.null(group$name)) group$name else paste0("group_", i)
    
    # Apply filter
    filtered_indices <- numeric(0)
    
    # Handle different filter types
    if (is.character(group$filter)) {
      # Filter is a string expression to evaluate
      tryCatch({
        # Create an environment with the data columns
        filter_env <- list2env(as.list(data), parent = parent.frame())
        # Evaluate the filter expression
        filter_result <- eval(parse(text = group$filter), envir = filter_env)
        if (length(filter_result) != nrow(data)) {
          stop("Filter expression did not return a vector of correct length")
        }
        if (!is.logical(filter_result)) {
          stop("Filter expression did not return a logical vector")
        }
        filtered_indices <- which(filter_result & !(seq_len(nrow(data)) %in% processed_indices))
      }, error = function(e) {
        stop(paste0("Error evaluating filter for group ", group_name, ": ", e$message))
      })
    } else if (is.logical(group$filter)) {
      # Filter is a logical vector
      if (length(group$filter) != nrow(data)) {
        stop(paste0("Logical filter for group ", group_name, " has incorrect length"))
      }
      filtered_indices <- which(group$filter & !(seq_len(nrow(data)) %in% processed_indices))
    } else {
      stop(paste0("Filter for group ", group_name, " must be a character string or logical vector"))
    }
    
    # Skip if no rows match
    if (length(filtered_indices) == 0) {
      message(paste0("No rows matched filter for group: ", group_name))
      next
    }
    
    # Create filtered dataset
    filtered_data <- data[filtered_indices, , drop = FALSE]
    
    message(paste0("Processing group: ", group_name, " (", nrow(filtered_data), " rows)"))
    
    # Get categories for this group
    categories <- if (!is.null(group$categories)) group$categories else default_categories
    
    # Process this group's data
    if (use_advanced_parallel && nrow(filtered_data) > 10) {
      # Check for required packages
      for (pkg in c("future", "furrr", "progressr")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          stop(paste0("Package '", pkg, "' is required for advanced parallelization. ",
                      "Please install it with install.packages('", pkg, "')"))
        }
      }
      
      group_result <- categorize_parallel_advanced(
        data = filtered_data,
        column = col_name,
        system_prompt = group$system_prompt,
        categories = categories,
        include_reasoning = include_reasoning,
        include_confidence = include_confidence,
        provider = provider,
        model = model,
        workers = workers,
        parallel_strategy = "multisession",
        echo = "text"
      )
    } else {
      # Use the simpler function
      group_result <- extract_categories(
        data = filtered_data,
        column = col_name,
        system_prompt = group$system_prompt,
        categories = categories,
        include_reasoning = include_reasoning,
        include_confidence = include_confidence,
        provider = provider,
        model = model,
        parallel = parallel,
        echo = "text"
      )
    }
    
    # Add group ID if requested
    if (include_group_id) {
      group_result$.group_id <- rep(group_name, nrow(group_result))
    }
    
    # Store the results
    results_list[[i]] <- group_result
    
    # Track processed indices
    processed_indices <- c(processed_indices, filtered_indices)
  }
  
  # Process default group if specified
  if (!is.null(default_system_prompt)) {
    # Find rows not processed by any group
    remaining_indices <- setdiff(seq_len(nrow(data)), processed_indices)
    
    if (length(remaining_indices) > 0) {
      remaining_data <- data[remaining_indices, , drop = FALSE]
      
      message(paste0("Processing default group (", nrow(remaining_data), " rows)"))
      
      # Process remaining data with default settings
      if (use_advanced_parallel && nrow(remaining_data) > 10) {
        default_result <- categorize_parallel_advanced(
          data = remaining_data,
          column = col_name,
          system_prompt = default_system_prompt,
          categories = default_categories,
          include_reasoning = include_reasoning,
          include_confidence = include_confidence,
          provider = provider,
          model = model,
          workers = workers,
          parallel_strategy = "multisession",
          echo = "text"
        )
      } else {
        default_result <- extract_categories(
          data = remaining_data,
          column = col_name,
          system_prompt = default_system_prompt,
          categories = default_categories,
          include_reasoning = include_reasoning,
          include_confidence = include_confidence,
          provider = provider,
          model = model,
          parallel = parallel,
          echo = "text"
        )
      }
      
      # Add group ID if requested
      if (include_group_id) {
        default_result$.group_id <- rep("default", nrow(default_result))
      }
      
      # Add to results list
      results_list[[length(results_list) + 1]] <- default_result
    }
  }
  
  # Combine all results
  if (length(results_list) == 0) {
    stop("No data was processed. Check your filter expressions.")
  }
  
  # Bind all results and restore original order
  combined_results <- dplyr::bind_rows(results_list)
  combined_results <- combined_results[order(combined_results$.row_id), ]
  
  # Remove row ID column used for ordering
  combined_results$.row_id <- NULL
  
  return(combined_results)
}
