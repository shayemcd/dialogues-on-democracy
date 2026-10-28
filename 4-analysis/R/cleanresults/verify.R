# Check numbers typed in prose against the results ----------------------------------

#' Check every number in a manuscript against the results
#'
#' Pulls each number out of a text (a string, or a `.qmd`, `.Rmd`, `.md`,
#' `.tex`, `.txt`, `.html`, or `.docx` file) and looks for a matching value in
#' the results, allowing for legitimate rounding. Numbers produced by inline R
#' code in a `.qmd`/`.Rmd` are skipped because they come from the results by
#' construction: what is left are the numbers someone typed. Run it on a
#' `.docx` downloaded from Google Docs to catch edits made after export.
#'
#' A match means the number exists somewhere in the results, not that the
#' sentence attaches it to the right result. Checking that the prose says the
#' right thing about the right comparison is a reading task: hand the
#' [cr_ground_truth()] export and the text to a reviewer or an LLM for that.
#'
#' @param text A character string or a file path.
#' @param ... Sources: `clean_results`, `cr_tests`, `cr_hypotheses`, or
#'   `cr_ground_truth` objects.
#' @param strict If `TRUE`, stop with an error when any number is unmatched or
#'   matches only with the opposite sign (use in a render to block drift).
#' @param ignore Extra numbers to skip, e.g. sample sizes stated in Methods.
#' @return A `cr_verify` data frame: each number, its context, and the
#'   matching result(s).
#' @export
cr_verify <- function(text, ..., strict = FALSE, ignore = NULL) {
  src <- list(...)
  if (!length(src)) stop("Supply the results to check against.", call. = FALSE)
  txt <- read_text(text)
  cand <- candidate_values(src)
  toks <- extract_numbers(txt)
  if (!nrow(toks)) {
    out <- data.frame(value = character(), context = character(), status = character(),
                      matches = character())
    class(out) <- c("cr_verify", "data.frame")
    return(out)
  }
  toks$status <- "unmatched"
  toks$matches <- ""
  for (i in seq_len(nrow(toks))) {
    v <- toks$norm[i]
    if (toks$label[i] != "") { toks$status[i] <- toks$label[i]; next }
    if (v %in% ignore || toks$value[i] %in% ignore) { toks$status[i] <- "ignored"; next }
    hit <- cand$where[cand$text == v]
    if (length(hit)) {
      toks$status[i] <- "matched"
      toks$matches[i] <- summarize_hits(hit)
      next
    }
    hit <- cand$where[cand$text == flip_sign(v)]
    if (length(hit)) {
      toks$status[i] <- "SIGN DIFFERS"
      toks$matches[i] <- summarize_hits(hit)
    }
  }
  out <- toks[, c("value", "context", "status", "matches")]
  class(out) <- c("cr_verify", "data.frame")
  bad <- out$status %in% c("unmatched", "SIGN DIFFERS")
  if (strict && any(bad)) {
    print(out[bad, , drop = FALSE])
    stop(sprintf("cr_verify: %d number(s) in the text do not match the results.", sum(bad)),
         call. = FALSE)
  }
  out
}

summarize_hits <- function(hit) {
  hit <- unique(hit)
  s <- paste(utils::head(hit, 3), collapse = " | ")
  if (length(hit) > 3) s <- sprintf("%s | +%d more", s, length(hit) - 3)
  s
}

flip_sign <- function(v) if (startsWith(v, "-")) substring(v, 2) else paste0("-", v)

read_text <- function(text) {
  if (length(text) == 1 && !grepl("\n", text) && file.exists(text)) {
    ext <- tolower(tools::file_ext(text))
    if (ext == "docx") {
      s <- officer::docx_summary(officer::read_docx(text))
      return(paste(s$text[!is.na(s$text)], collapse = "\n"))
    }
    txt <- paste(readLines(text, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (ext %in% c("qmd", "rmd")) {
      txt <- gsub("(?s)```\\s*\\{[^}]*\\}.*?```", " ", txt, perl = TRUE)  # code chunks
      txt <- gsub("`r [^`]*`", " ", txt)                                 # inline R
      txt <- gsub("(?s)^---.*?---", " ", txt, perl = TRUE)                # YAML
    }
    if (ext %in% c("html", "htm")) {
      txt <- gsub("(?is)<(script|style|head)\\b.*?</\\1>", " ", txt, perl = TRUE)
      txt <- gsub("<[^>]+>", " ", txt)
      txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
    }
    if (ext == "tex") txt <- gsub("(?m)%.*$", "", txt, perl = TRUE)
    return(txt)
  }
  paste(text, collapse = "\n")
}

extract_numbers <- function(txt) {
  txt <- gsub("[\u2212\u2013\u2014]", "-", txt)   # unicode minus and dashes
  pat <- "(?<![[:alnum:]_.])-?(?:\\d{1,3}(?:,\\d{3})+|\\d+)?(?:\\.\\d+)?(?![[:alnum:]])"
  m <- gregexpr(pat, txt, perl = TRUE)[[1]]
  if (m[1] == -1) return(data.frame())
  len <- attr(m, "match.length")
  keep <- len > 0
  m <- m[keep]; len <- len[keep]
  vals <- substring(txt, m, m + len - 1)
  ok <- grepl("\\d", vals)
  m <- m[ok]; len <- len[ok]; vals <- vals[ok]
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  nc <- length(chars)
  sub_chars <- function(from, to) {
    vapply(seq_along(from), function(i) {
      a <- max(1, from[i]); b <- min(nc, to[i])
      if (b < a) "" else paste(chars[a:b], collapse = "")
    }, "")
  }
  ctx <- gsub("\\s+", " ", sub_chars(m - 35, m + len + 15))
  before <- sub_chars(m - 12, m - 1)
  after <- sub_chars(m + len, m + len + 6)
  label <- ifelse(grepl("(H|Hypothesis|Study|Experiment|Table|Figure|Fig\\.|Model|Wave|RQ|Section|Appendix)\\s*$",
                        before, ignore.case = TRUE), "label",
           ifelse(grepl("^\\s*(%|\\\\%)\\s*(CI|confidence|credible|CrI)", after) &
                    vals %in% c("90", "95", "99"), "CI level",
           ifelse(grepl("^(19|20)\\d\\d$", vals), "year?", "")))
  norm <- gsub(",", "", vals)
  data.frame(value = vals, norm = norm, context = ctx, label = label, stringsAsFactors = FALSE)
}

# Every value a source can legitimately be reported as, at 0 to 3 decimals,
# with and without the leading zero.
candidate_values <- function(src) {
  rows <- list()
  for (s in src) {
    if (inherits(s, "cr_hypotheses")) {
      for (t in hyp_tests(s)) rows <- c(rows, list(t$row), lapply(t$robustness, `[[`, "row"))
    } else if (inherits(s, "cr_ground_truth")) {
      rows <- c(rows, list(s$terms))
      if (!is.null(s$models)) rows <- c(rows, list(models_as_rows(s$models)))
    } else {
      rows <- c(rows, list(cr_rows(s)))
      if (inherits(s, "clean_results")) {
        rows <- c(rows, list(models_as_rows(model_rows(s, "results"))))
      }
    }
  }
  cols <- c("estimate", "std.error", "statistic", "df", "df2", "conf.low", "conf.high",
            "p.value", "p.within", "es", "es.low", "es.high", "n", "m1", "sd1", "n1", "m2",
            "sd2", "n2", "pct1", "pct2")
  out <- list()
  for (r in rows) {
    r <- as.data.frame(r)
    if (!is.null(r$m1) && identical(unique(stats::na.omit(r$kind)), "chisq")) {
      r$pct1 <- 100 * r$m1; r$pct2 <- 100 * r$m2
    }
    for (i in seq_len(nrow(r))) {
      who <- sprintf("%s%s", r$dv_label[i] %||% r$model[i] %||% "",
                     if (!is.null(r$level_label) && !is.na(r$level_label[i])) paste0(" [", r$level_label[i], "]")
                     else if (!is.null(r$variable_label) && !is.na(r$variable_label[i])) paste0(" ~ ", r$variable_label[i])
                     else "")
      for (cl in intersect(cols, names(r))) {
        v <- suppressWarnings(as.numeric(r[[cl]][i]))
        if (!is.finite(v)) next
        txt <- unique(c(num_variants(v), if (cl %in% c("p.value", "p.within")) p_variants(v)))
        out[[length(out) + 1]] <- data.frame(text = txt, where = paste0(who, ": ", cl),
                                             stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}

models_as_rows <- function(m) {
  data.frame(dv_label = m$model, kind = "model", n = m$n,
             estimate = NA_real_, r2 = m$r2, stringsAsFactors = FALSE)[, c("dv_label", "kind", "n")]
}

num_variants <- function(v) {
  s <- vapply(0:3, function(d) fmt_num(v, d), "")
  s <- c(s, sub("^(-?)0\\.", "\\1.", s))
  if (abs(v - round(v)) < 1e-8) s <- c(s, format(round(v), scientific = FALSE))
  unique(s)
}

# "p < .001" is written as the threshold; match the threshold itself.
p_variants <- function(p) {
  out <- character()
  for (d in 1:3) {
    th <- 10^-d
    if (p < th) out <- c(out, fmt_num(th, d), sub("^0", "", fmt_num(th, d)))
  }
  out
}

#' @export
print.cr_verify <- function(x, ...) {
  tab <- table(factor(x$status, c("matched", "unmatched", "SIGN DIFFERS", "label", "CI level",
                                  "year?", "ignored")))
  tab <- tab[tab > 0]
  cat(sprintf("%d number(s) checked: %s\n", nrow(x),
              paste(sprintf("%s %s", tab, names(tab)), collapse = ", ")))
  bad <- x$status %in% c("unmatched", "SIGN DIFFERS")
  if (any(bad)) {
    cat("\nNeeds a look:\n")
    print(as.data.frame(x)[bad, c("value", "status", "context", "matches")], row.names = FALSE,
          right = FALSE)
  } else {
    cat("Every number matches a value in the results.\n")
  }
  invisible(x)
}
