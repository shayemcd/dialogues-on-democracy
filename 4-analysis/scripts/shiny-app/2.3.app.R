#install.packages(c("shiny", "ggplot2", "dplyr", "ggthemes", "ggpubr","jtools"))

library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(jtools)

# Load data ----
# Check if pre-cached df_app.RData exists for instant startup
app_cache_file <- file.path("df_app.RData")
if (!file.exists(app_cache_file)) {
  candidate_caches <- c(
    file.path("scripts", "shiny-app", "df_app.RData"),
    file.path("4-analysis", "scripts", "shiny-app", "df_app.RData")
  )
  found_cache <- candidate_caches[file.exists(candidate_caches)]
  if (length(found_cache) > 0) app_cache_file <- found_cache[1]
}

if (file.exists(app_cache_file)) {
  message("Loading cached app data from: ", app_cache_file)
  load(app_cache_file)
} else {
  data_dirs <- c(
    file.path("..", "..", "data"),
    file.path("..", "..", "4-analysis", "data"),
    "data",
    file.path("4-analysis", "data"),
    "."
  )

  data_files <- unique(unlist(lapply(data_dirs[dir.exists(data_dirs)], function(d) {
    list.files(d, pattern = "df_long.*\\.RData$", full.names = TRUE)
  })))

  if (length(data_files) == 0) {
    fallback_files <- c(
      file.path("..", "..", "data", "df_long.RData"),
      file.path("data", "df_long.RData"),
      "df_long.RData"
    )
    data_files <- fallback_files[file.exists(fallback_files)]
  }

  if (length(data_files) == 0) {
    stop("Could not find df_long.RData in the data folder or candidate directories.")
  }

  file_info <- file.info(data_files)
  data_file <- rownames(file_info)[order(file_info$mtime, decreasing = TRUE)][1]
  message("Loading df_long from: ", data_file)
  load(data_file)

  participant_map <- df_long %>%
    distinct(ResponseId) %>%
    arrange(ResponseId) %>%
    mutate(IncrementalResponseId = row_number())

  df <- df_long %>%
    left_join(participant_map, by = "ResponseId") %>%
    select(
      ResponseId,
      IncrementalResponseId,
      Speaker,
      Round,
      ResponseText,
      Sureness_Diff,
      ER0_Diff,
      ConditionFeedback
    ) %>%
    mutate(
      ConditionFeedback = if_else(is.na(ConditionFeedback), "Unassigned", as.character(ConditionFeedback)),
      ConditionFeedback = factor(ConditionFeedback)
    )

  rm(df_long)
  gc()
}

# Condition colors (used for plots & convo container accent) ----
condition_levels <- levels(df$ConditionFeedback)

condition_colors <- c(
  "Belief-Tailored"          = "#4158A6",
  "Belief & Values-Tailored" = "#6A8E7F",
  "Adversarial Dialogue"     = "#A64521",
  "CtlCatsDogs"              = "#78909C",
  "Control"                  = "#78909C",
  "Unassigned"               = "#BDBDBD"
)

missing_cols <- setdiff(condition_levels, names(condition_colors))
if (length(missing_cols) > 0) {
  extra_cols <- colorRampPalette(c("#586F7C", "#C06C84", "#355C7D"))(length(missing_cols))
  names(extra_cols) <- missing_cols
  condition_colors <- c(condition_colors, extra_cols)
}

# Icons for speakers ----
speaker_icons <- c(
  "user" = "🧑‍💬",
  "gpt"  = "🤖"
)

# UI ----
ui <- fluidPage(
  titlePanel("Election Conspiracies Conversation Overview"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "condition",
        "Filter by Condition:",
        choices  = condition_levels,
        selected = NULL,
        multiple = TRUE
      ),
      sliderInput(
        "surenessSlider",
        "Filter by Claim Certainty Change:",
        min   = min(df$Sureness_Diff, na.rm = TRUE),
        max   = max(df$Sureness_Diff, na.rm = TRUE),
        value = c(min(df$Sureness_Diff, na.rm = TRUE),
                  max(df$Sureness_Diff, na.rm = TRUE))
      ),
      checkboxInput(
        "include_incomplete",
        "Include Incomplete Sessions (Missing Change Score)",
        value = TRUE
      ),
      selectizeInput(
        "responseId",
        "Select Conversation:",
        choices = NULL,
        options = list(
          maxItems    = 1,
          placeholder = "Type to search ID...",
          loadOnFocus = TRUE
        )
      ),
      textInput("search", "Search Conversations:", value = ""),
      actionButton("clearFilters", "Clear All Filters"),
      hr(),
      h3("Change Scores for Selected Conversation"),
      tableOutput("changeScore"),
      hr(),
      textOutput("responseCount")
    ),
    
    mainPanel(
      h3("Claim Certainty Change Distribution by Experimental Condition"),
      plotOutput("summaryPlot"),
      hr(),
      tableOutput("summaryTable"),
      hr(),
      h3("AI Conversation Overview"),
      uiOutput("conversationChat")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Debounce search term so typing does not trigger recalculation on every keystroke
  search_query <- reactive({
    input$search
  }) %>% debounce(350)
  
  # Reactive: filter based on search term ----
  filtered_data_by_search <- reactive({
    sq <- search_query()
    if (is.null(sq) || trimws(sq) == "") return(df)
    matching_ids <- df %>% 
      dplyr::filter(!is.na(ResponseText), grepl(sq, ResponseText, ignore.case = TRUE)) %>%
      dplyr::pull(ResponseId) %>%
      unique()
    df %>% dplyr::filter(ResponseId %in% matching_ids)
  })
  
  # Reactive: filter based on condition ----
  filtered_data_by_condition <- reactive({
    if (is.null(input$condition) || length(input$condition) == 0) {
      filtered_data_by_search()
    } else {
      filtered_data_by_search() %>%
        dplyr::filter(ConditionFeedback %in% input$condition)
    }
  })
  
  # Reactive: filter based on Sureness_Diff slider ----
  filtered_data_by_sureness <- reactive({
    data <- filtered_data_by_condition()
    req(input$surenessSlider)
    if (isTRUE(input$include_incomplete)) {
      data %>%
        dplyr::filter(
          is.na(Sureness_Diff) | (
            Sureness_Diff >= input$surenessSlider[1] &
            Sureness_Diff <= input$surenessSlider[2]
          )
        )
    } else {
      data %>%
        dplyr::filter(
          !is.na(Sureness_Diff),
          Sureness_Diff >= input$surenessSlider[1],
          Sureness_Diff <= input$surenessSlider[2]
        )
    }
  })
  
  # Fast server-side update of selectize dropdown ----
  observe({
    fd <- filtered_data_by_sureness()
    if (nrow(fd) == 0) {
      updateSelectizeInput(session, "responseId", choices = character(0), selected = character(0), server = TRUE)
      return()
    }
    
    response_choices <- fd %>%
      distinct(IncrementalResponseId, .keep_all = TRUE) %>%
      dplyr::mutate(
        Label = if_else(
          is.na(Sureness_Diff),
          paste0("ID ", IncrementalResponseId, "  (Incomplete / No Δ Score)"),
          paste0("ID ", IncrementalResponseId, "  (Δ Certainty: ", round(Sureness_Diff, 2), ")")
        )
      )
    
    curr <- isolate(input$responseId)
    new_selected <- if (!is.null(curr) && curr %in% response_choices$IncrementalResponseId) {
      curr
    } else {
      response_choices$IncrementalResponseId[1]
    }
    
    updateSelectizeInput(
      session,
      "responseId",
      choices  = setNames(response_choices$IncrementalResponseId, response_choices$Label),
      selected = new_selected,
      server   = TRUE
    )
  })
  
  # Reactive: data for selected conversation ----
  filtered_data <- reactive({
    req(input$responseId)
    df %>% dplyr::filter(IncrementalResponseId == input$responseId)
  })
  
  # Text: count of filtered conversations ----
  output$responseCount <- renderText({
    total_responses    <- n_distinct(df$ResponseId)
    filtered_responses <- n_distinct(filtered_data_by_sureness()$ResponseId)
    percentage <- round((filtered_responses / total_responses) * 100, 1)
    paste0(
      "Filtered participants: ",
      filtered_responses, " of ", total_responses,
      " (", percentage, "%)"
    )
  })
  
  # Plot: distribution of Sureness_Diff by ConditionFeedback (1 per person) ----
  output$summaryPlot <- renderPlot({
    fd <- filtered_data_by_sureness() %>%
      distinct(ResponseId, .keep_all = TRUE) %>%
      filter(!is.na(Sureness_Diff))
      
    if (nrow(fd) == 0) return(NULL)
    
    gghistogram(
      fd,
      x       = "Sureness_Diff",
      add     = "mean",
      bins    = 100,
      rug     = TRUE,
      color   = "ConditionFeedback",
      fill    = "ConditionFeedback",
      palette = condition_colors
    ) +
      labs(
        x = "Claim Certainty Change (Post – Pre)",
        y = "Frequency (Participants)",
        title = "Distribution of Claim Certainty Change by Condition (1 per Participant)"
      ) +
      theme_apa()
  })
  
  # Table: summary stats by ConditionFeedback (1 per person) ----
  output$summaryTable <- renderTable({
    fd <- filtered_data_by_sureness() %>%
      distinct(ResponseId, .keep_all = TRUE)
      
    if (nrow(fd) == 0) return(NULL)
    
    fd %>%
      group_by(ConditionFeedback) %>%
      summarise(
        `Avg Claim Certainty Change` = mean(Sureness_Diff, na.rm = TRUE),
        SD = sd(Sureness_Diff, na.rm = TRUE),
        `Completed n` = sum(!is.na(Sureness_Diff)),
        `Total n` = n(),
        .groups = "drop"
      )
  }, na = "—")
  
  # Table: change scores for selected conversation ----
  output$changeScore <- renderTable({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    data %>%
      dplyr::select(Sureness_Diff, ER0_Diff) %>%
      distinct() %>%
      dplyr::rename(
        `2020 Denialism Change`   = ER0_Diff,
        `Claim Certainty Change`  = Sureness_Diff
      )
  }, na = "Incomplete / NA")
  
  # Chat UI: conversation viewer with rounded containers, icons, condition tag ----
  output$conversationChat <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    cond <- unique(data$ConditionFeedback)
    cond_color <- condition_colors[as.character(cond)]
    if (is.na(cond_color) || length(cond_color) == 0) cond_color <- "#444444"
    
    # Get change scores for tag
    change_tbl <- data %>%
      dplyr::select(Sureness_Diff, ER0_Diff) %>%
      distinct()
    
    sureness_change <- if_else(is.na(change_tbl$Sureness_Diff[1]), "N/A", as.character(round(change_tbl$Sureness_Diff[1], 2)))
    denial_change   <- if_else(is.na(change_tbl$ER0_Diff[1]), "N/A", as.character(round(change_tbl$ER0_Diff[1], 2)))
    
    # Filter only turns with actual text to render bubbles
    dialogue_data <- data %>%
      filter(!is.na(ResponseText), nzchar(trimws(ResponseText))) %>%
      arrange(Round, Speaker)
    
    if (nrow(dialogue_data) == 0) {
      return(
        div(
          style = paste(
            "background:white;",
            "padding:30px;",
            "border-radius:15px;",
            "max-width:850px;",
            "margin:auto;",
            "border-left:6px solid", cond_color, ";",
            "box-shadow:0 2px 6px rgba(0,0,0,0.12);",
            "text-align:center;",
            "color:#777;",
            "font-style:italic;"
          ),
          h3(
            paste0("Conversation (Condition: ", cond, ")"),
            style = paste("color:", cond_color, "; font-weight:700; margin-bottom:10px; font-style:normal;")
          ),
          p("No conversation messages recorded for this participant (incomplete session or control condition).")
        )
      )
    }
    
    # Build chat bubbles
    chat_blocks <- lapply(seq_len(nrow(dialogue_data)), function(i) {
      row <- dialogue_data[i, ]
      speaker_raw <- as.character(row$Speaker)
      speaker_type <- ifelse(tolower(speaker_raw) == "user", "user", "gpt")
      
      resp <- row$ResponseText
      
      bubble_align <- ifelse(speaker_type == "user", "flex-start", "flex-end")
      bg_col <- ifelse(speaker_type == "user", "#F0F7F5", "#E8ECF8")
      
      div(
        style = paste(
          "display:flex;",
          "justify-content:", bubble_align, ";",
          "margin-bottom:12px;"
        ),
        div(
          style = paste(
            "max-width:70%;",
            "background-color:", bg_col, ";",
            "border-radius:15px;",
            "padding:12px 18px;",
            "box-shadow:0 1px 4px rgba(0,0,0,0.1);",
            "font-size:15px;"
          ),
          div(
            style="font-weight:600; margin-bottom:6px; display:flex; align-items:center; gap:6px;",
            span(speaker_icons[speaker_type]),
            span(toupper(speaker_type))
          ),
          div(resp)
        )
      )
    })
    
    # Wrap everything in a centered card-like container
    div(
      style = paste(
        "background:white;",
        "padding:20px;",
        "border-radius:15px;",
        "max-width:850px;",
        "margin:auto;",
        "border-left:6px solid", cond_color, ";",
        "box-shadow:0 2px 6px rgba(0,0,0,0.12);"
      ),
      
      h3(
        paste0("Conversation (Condition: ", cond, ")"),
        style = paste(
          "color:", cond_color, ";",
          "font-weight:700;",
          "margin-bottom:5px;"
        )
      ),
      
      div(
        style="margin-bottom:18px; font-size:14px; color:#555;",
        paste0(
          "Belief Change — Claim Certainty: ", sureness_change,
          " | 2020 Denialism: ", denial_change
        )
      ),
      
      do.call(tagList, chat_blocks)
    )
  })
  
  # Clear filters ----
  observeEvent(input$clearFilters, {
    updateTextInput(session, "search", value = "")
    updateSelectInput(session, "condition", selected = NULL)
    updateCheckboxInput(session, "include_incomplete", value = TRUE)
    updateSliderInput(
      session,
      "surenessSlider",
      value = c(
        min(df$Sureness_Diff, na.rm = TRUE),
        max(df$Sureness_Diff, na.rm = TRUE)
      )
    )
    updateSelectizeInput(session, "responseId", selected = NULL)
  })
}

# Run app ----
shinyApp(ui = ui, server = server)
