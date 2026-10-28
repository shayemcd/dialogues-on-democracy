# Ground truth export and change detection ----------------------------------------
#
# The ground truth is the one place every reported number lives. It feeds an
# LLM drafting or checking prose (e.g. a results-writer skill), and comparing
# two exports shows exactly which results changed between runs.

#' Export every reportable number, model, and hypothesis link
#'
#' Produces three tables: `terms` (one row per estimate, with the exact
#' reported string), `models` (one row per model: N, fit, SE method,
#' corrections), and `hypotheses` (which result answers which hypothesis, and
#' its robustness checks). Save it as JSON for an LLM or for [cr_diff()], or as
#' Markdown to paste into a chat.
#'
#' @param ... Sources: `clean_results`, `cr_tests`, or `cr_hypotheses`
#'   objects. Name them (`main = res`) to control how they are labelled; a
#'   `cr_hypotheses` object also contributes every source it points to.
#' @param path Optional file to write: `.json`, `.md`, or `.csv` (terms only).
#' @param compare When `path` already exists as JSON, compare against it and
#'   report what changed before overwriting (see [cr_diff()]).
#' @param style,digits,p_digits Used for the `reported` strings.
#' @return A `cr_ground_truth` object (invisibly when `path` is given).
#' @export
cr_ground_truth <- function(..., path = NULL, compare = TRUE, style = "apa", digits = 2,
                            p_digits = 3) {
  args <- list(...)
  nms <- names(args) %||% rep("", length(args))
  exprs <- vapply(as.list(substitute(list(...)))[-1], function(e) paste(deparse(e), collapse = ""), "")
  nms[!nzchar(nms)] <- exprs[!nzchar(nms)]
  if (!length(args)) stop("Supply at least one results object.", call. = FALSE)

  terms <- list(); models <- list(); hyps <- NULL
  add_source <- function(obj, name) {
    r <- cr_rows(obj)
    r <- r[!r$is_ref, , drop = FALSE]
    r$source <- name
    terms[[name]] <<- r
    models[[name]] <<- model_rows(obj, name)
  }
  for (i in seq_along(args)) {
    a <- args[[i]]
    if (inherits(a, "cr_hypotheses")) {
      hyps <- rbind(hyps, hyp_rows(a))
    } else if (inherits(a, c("clean_results", "cr_tests"))) {
      add_source(a, nms[i])
    } else {
      stop("Sources must be clean_results, cr_tests, or cr_hypotheses objects.", call. = FALSE)
    }
  }
  # rows referenced by hypotheses whose source was not passed directly
  if (!is.null(hyps)) {
    for (a in args[vapply(args, inherits, logical(1), "cr_hypotheses")]) {
      for (t in hyp_tests(a)) {
        for (row in c(list(t$row), lapply(t$robustness, `[[`, "row"))) {
          if (is.null(terms[[row$source]]) || !row$id %in% terms[[row$source]]$id) {
            terms[[paste0(row$source, "#", row$id)]] <- row[, c(row_cols, "source")]
          }
        }
      }
    }
  }
  terms <- do.call(rbind, lapply(terms, function(t) t[, c("source", row_cols)]))
  rownames(terms) <- NULL
  terms$stars <- fmt_stars(terms$p.value)
  terms$reported <- vapply(seq_len(nrow(terms)), function(i) {
    stats_text(terms[i, ], NULL, "plain", style, digits, p_digits)
  }, "")
  models <- do.call(rbind, models)
  rownames(models) <- NULL

  gt <- structure(list(
    meta = list(package = "cleanresults", version = as.character(utils::packageVersion("cleanresults")),
                created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                note = paste("Every number in the results prose must match a cell in `terms`;",
                             "`reported` is the exact reporting string.")),
    terms = terms, models = models, hypotheses = hyps
  ), class = "cr_ground_truth")

  if (!is.null(path)) {
    if (compare && file.exists(path) && tolower(tools::file_ext(path)) == "json") {
      d <- cr_diff(path, gt)
      if (nrow(d)) {
        message("Results changed since the last export (", path, "):\n",
                paste(utils::capture.output(print(d)), collapse = "\n"))
      }
    }
    dir <- dirname(path)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    write_ground_truth(gt, path)
    return(invisible(gt))
  }
  gt
}

model_rows <- function(obj, source) {
  if (inherits(obj, "cr_tests")) {
    return(data.frame(source = source, model = unique(obj$rows$dv), model_type = obj$test,
                      n = NA_real_, groups = NA_character_, r2 = NA_real_, marginal_r2 = NA_real_,
                      conditional_r2 = NA_real_, tjur_r2 = NA_real_, icc = NA_real_, aic = NA_real_,
                      se_method = NA_character_, cluster = NA_character_,
                      corrections = obj$adjust_text %||% NA_character_, stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(obj$settings$dvs, function(dv) {
    st <- obj$stats[[dv]]; m <- obj$meta[[dv]]
    get <- function(part, nm) { v <- st[[part]][[nm]]; if (is.null(v)) NA_real_ else v }
    grp <- grep("^N \\(", names(st$random), value = TRUE)
    corr <- c(if (length(m$contrast_vars) && obj$settings$contrast_adjust != "none") {
                paste(adjust_name(obj$settings$contrast_adjust), "within factor")
              },
              if (obj$settings$dv_adjust != "none") {
                paste(adjust_name(obj$settings$dv_adjust), "across outcomes")
              })
    data.frame(
      source = source, model = dv, model_type = m$type, n = m$n,
      groups = if (length(grp)) paste(sprintf("%s = %s", sub("^N \\((.*)\\)$", "\\1", grp),
                                              unlist(st$random[grp])), collapse = "; ")
               else NA_character_,
      r2 = get("fit", "R\u00b2"), marginal_r2 = get("fit", "Marginal R\u00b2"),
      conditional_r2 = get("fit", "Conditional R\u00b2"), tjur_r2 = get("fit", "Tjur R\u00b2"),
      icc = get("random", "ICC"), aic = get("fit", "AIC"),
      se_method = m$se_text, cluster = m$cluster %||% NA_character_,
      corrections = if (length(corr)) paste(corr, collapse = "; ") else NA_character_,
      stringsAsFactors = FALSE)
  }))
}

hyp_rows <- function(x) {
  out <- list()
  for (h in names(x$tests)) {
    for (k in seq_along(x$tests[[h]])) {
      t <- x$tests[[h]][[k]]
      base <- data.frame(hypothesis = h, statement = x$labels[[h]] %||% NA_character_,
                         test = k, role = "primary", source = t$row$source, id = t$row$id,
                         describe = t$describe, expect = t$expect, status = t$status,
                         stringsAsFactors = FALSE)
      rob <- lapply(t$robustness, function(rb) {
        data.frame(hypothesis = h, statement = x$labels[[h]] %||% NA_character_, test = k,
                   role = paste("robustness:", rb$name), source = rb$row$source, id = rb$row$id,
                   describe = describe_row(rb$row), expect = NA_character_, status = NA_character_,
                   stringsAsFactors = FALSE)
      })
      out <- c(out, list(base), rob)
    }
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

write_ground_truth <- function(gt, path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    json = jsonlite::write_json(unclass(gt), path, pretty = TRUE, digits = NA, na = "null",
                                auto_unbox = TRUE, dataframe = "rows"),
    md = writeLines(enc2utf8(gt_markdown(gt)), path, useBytes = TRUE),
    csv = utils::write.csv(gt$terms, path, row.names = FALSE, fileEncoding = "UTF-8"),
    stop(sprintf("Unsupported extension '.%s'. Use .json, .md, or .csv.", ext), call. = FALSE))
}

gt_markdown <- function(gt) {
  md_table <- function(df) {
    if (is.null(df) || !nrow(df)) return("(none)")
    df[] <- lapply(df, function(v) {
      if (is.numeric(v)) ifelse(is.na(v), "", format(signif(v, 6), trim = TRUE, scientific = FALSE))
      else ifelse(is.na(v), "", gsub("|", "\\|", as.character(v), fixed = TRUE))
    })
    c(paste0("| ", paste(names(df), collapse = " | "), " |"),
      paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
      apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
  }
  t <- gt$terms
  keep <- c("source", "id", "dv_label", "variable_label", "level_label", "est_name", "estimate",
            "std.error", "stat_name", "statistic", "df", "df2", "conf.low", "conf.high",
            "p.value", "p.within", "adjustment", "es_name", "es", "es.low", "es.high", "n",
            "m1", "sd1", "m2", "sd2", "stars", "reported")
  keep <- keep[vapply(keep, function(k) any(!is.na(t[[k]])), logical(1))]
  c("# Results ground truth", "",
    sprintf("Exported by cleanresults %s on %s. %s", gt$meta$version, gt$meta$created, gt$meta$note),
    "", "## Hypotheses", "", md_table(gt$hypotheses),
    "", "## Estimates", "", md_table(t[, keep]),
    "", "## Models", "", md_table(gt$models))
}

#' @export
print.cr_ground_truth <- function(x, ...) {
  cat(sprintf("Ground truth: %d estimates from %d source(s), %d model row(s)%s.\n",
              nrow(x$terms), length(unique(x$terms$source)), NROW(x$models),
              if (!is.null(x$hypotheses)) sprintf(", %d hypothesis link(s)", nrow(x$hypotheses)) else ""))
  print(utils::head(x$terms[, c("source", "id", "reported")], 20), row.names = FALSE)
  if (nrow(x$terms) > 20) cat("...\n")
  invisible(x)
}

#' What changed between two runs?
#'
#' Compares two ground-truth exports and lists every estimate whose reported
#' value changed, flagging the changes that make existing prose wrong: a sign
#' flip or a crossing of the significance threshold. Run it after changing
#' code or data to see which sentences need re-reading.
#'
#' @param old,new `cr_ground_truth` objects or paths to `.json` exports.
#' @param alpha Significance threshold.
#' @return A data frame with one row per changed, added, or removed estimate.
#' @export
cr_diff <- function(old, new, alpha = 0.05) {
  load <- function(x) {
    if (inherits(x, "cr_ground_truth")) return(x$terms)
    j <- jsonlite::read_json(x, simplifyVector = TRUE)
    j$terms
  }
  a <- load(old); b <- load(new)
  ka <- paste(a$source, a$id); kb <- paste(b$source, b$id)
  keys <- union(ka, kb)
  rows <- lapply(keys, function(k) {
    ra <- a[match(k, ka), ]; rb <- b[match(k, kb), ]
    if (is.na(match(k, ka))) return(data.frame(key = k, old = NA, new = rb$reported, change = "added"))
    if (is.na(match(k, kb))) return(data.frame(key = k, old = ra$reported, new = NA, change = "removed"))
    if (identical(ra$reported, rb$reported)) return(NULL)
    va <- if (!is.na(ra$estimate)) ra$estimate else ra$statistic
    vb <- if (!is.na(rb$estimate)) rb$estimate else rb$statistic
    null <- if (identical(rb$est_name, "OR")) 1 else 0
    flags <- c(
      if (!is.na(va) && !is.na(vb) && sign(va - null) != sign(vb - null)) "SIGN FLIP",
      if (!is.na(ra$p.value) && !is.na(rb$p.value) &&
          (ra$p.value < alpha) != (rb$p.value < alpha)) {
        if (rb$p.value < alpha) "NOW SIGNIFICANT" else "NO LONGER SIGNIFICANT"
      })
    data.frame(key = k, old = ra$reported, new = rb$reported,
               change = if (length(flags)) paste(flags, collapse = "; ") else "value changed")
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- data.frame(key = character(), old = character(), new = character(),
                                      change = character())
  out <- out[order(!grepl("[A-Z]{3}", out$change)), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("cr_diff", "data.frame")
  out
}

#' @export
print.cr_diff <- function(x, ...) {
  if (!nrow(x)) {
    cat("No reported values changed.\n")
    return(invisible(x))
  }
  serious <- sum(grepl("[A-Z]{3}", x$change))
  cat(sprintf("%d reported value(s) changed; %d change direction or significance.\n",
              nrow(x), serious))
  for (i in seq_len(nrow(x))) {
    cat(sprintf("\n[%s] %s\n  old: %s\n  new: %s\n", x$change[i], x$key[i],
                ifelse(is.na(x$old[i]), "-", x$old[i]), ifelse(is.na(x$new[i]), "-", x$new[i])))
  }
  invisible(x)
}
