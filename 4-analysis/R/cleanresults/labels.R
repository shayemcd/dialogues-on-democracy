#' Clean a variable or level name for display
#'
#' Splits camelCase (`GainFraming` -> "Gain framing"), turns underscores, dots,
#' and word hyphens into spaces (`low-income` -> "Low income"), keeps numeric
#' ranges as en dashes (`18-24` -> "18-24" with an en dash), and applies a case.
#' Acronyms written in capitals (`SES`, `GPA`) are kept, and `id` becomes "ID".
#'
#' @param x Character vector.
#' @param case `"sentence"` (default), `"title"`, or `"asis"`.
#' @return Character vector.
#' @export
#' @examples
#' clean_label(c("GainFraming", "age_years", "low-income", "18-24", "SES_score"))
clean_label <- function(x, case = c("sentence", "title", "asis")) {
  case <- match.arg(case)
  x <- as.character(x)
  out <- x
  out <- gsub("([0-9])\\s*-\\s*([0-9])", "\\1\u2013\\2", out)      # 18-24 -> en dash
  out <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", out)                # camelCase
  out <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", out)             # HTMLPage
  out <- gsub("[_.]+", " ", out)
  out <- gsub("(^|\\s)-+|-+(\\s|$)", "\\1\\2", out)               # stray dashes
  out <- gsub("([[:alpha:]])-+([[:alpha:]])", "\\1 \\2", out)     # word-word
  out <- gsub("([[:alpha:]])-+([[:alpha:]])", "\\1 \\2", out)     # a-b-c needs two passes
  out <- gsub("\\s+", " ", trimws(out))
  if (case == "asis") return(out)
  vapply(out, function(s) {
    if (!nzchar(s)) return(s)
    w <- strsplit(s, " ", fixed = TRUE)[[1]]
    acronym <- grepl("^[A-Z0-9]{2,}$", w)
    if (case == "title") {
      w[!acronym] <- paste0(toupper(substring(w[!acronym], 1, 1)), substring(w[!acronym], 2))
    } else {
      w[!acronym] <- tolower(w[!acronym])
      if (!acronym[1]) w[1] <- paste0(toupper(substring(w[1], 1, 1)), substring(w[1], 2))
    }
    w[tolower(w) == "id"] <- "ID"
    paste(w, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# Builds the label functions used everywhere a name is displayed.
make_labeller <- function(labels = NULL, level_labels = NULL, clean = TRUE,
                          case = "sentence") {
  tidy <- function(s) if (clean) clean_label(s, case) else s
  list(
    var = function(v) {
      if (!is.null(labels) && v %in% names(labels)) return(unname(labels[[v]]))
      tidy(v)
    },
    level = function(v, lv) {
      if (is.list(level_labels) && v %in% names(level_labels) &&
          lv %in% names(level_labels[[v]])) {
        return(unname(level_labels[[v]][[lv]]))
      }
      if (is.character(level_labels) && lv %in% names(level_labels)) {
        return(unname(level_labels[[lv]]))
      }
      if (lv %in% c("TRUE", "FALSE")) return(lv)
      tidy(lv)
    }
  )
}
