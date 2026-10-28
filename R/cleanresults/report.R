# Sentences and results paragraphs ---------------------------------------------
#
# Mechanical reporting only: direction, significance, and statistics. No
# interpretive language ("strong", "robust", "supports"), so generated text
# never claims more than the numbers do.

lc_first <- function(s) {
  if (is.na(s) || !nzchar(s)) return(s)
  first <- strsplit(s, " ", fixed = TRUE)[[1]][1]
  if (grepl("^[A-Z0-9]{2,}$", first)) return(s)
  paste0(tolower(substring(s, 1, 1)), substring(s, 2))
}
uc_first <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))

#' Write one result as a sentence
#'
#' Templates state direction and significance and then the statistics, e.g.
#' "Compared with Control, Gain was associated with higher trust, *b* = 0.46,
#' 95% CI \\[0.04, 0.89\\], *p* = .032." Non-significant results get "did not
#' differ significantly" wording. Words come from the data, so the sentence
#' stays true when results change.
#'
#' @inheritParams cr_inline
#' @param alpha Threshold for the significance wording.
#' @return A character string.
#' @export
cr_sentence <- function(x, dv, variable = NULL, level = NULL, stats = NULL,
                        format = c("markdown", "latex", "html", "plain"),
                        style = c("apa", "econ"), alpha = 0.05, subgroup = NULL,
                        digits = 2, p_digits = 3) {
  r <- cr_find(x, dv, variable, level, subgroup)
  row_sentence(r, stats, match.arg(format), match.arg(style), alpha, digits, p_digits)
}

row_sentence <- function(r, stats = NULL, format = "markdown", style = "apa", alpha = 0.05,
                         digits = 2, p_digits = 3) {
  f <- text_fmt(format, style)
  e <- f$esc
  st <- stats_text(r, stats, format, style, digits, p_digits)
  sig <- row_sig(r, alpha)
  s <- row_sign(r)
  hl <- if (s > 0) "higher" else "lower"
  dv <- e(lc_first(r$dv_label))
  g1 <- e(r$group1 %||% NA); g2 <- e(r$group2 %||% NA)
  sg <- if (!is.na(r$subgroup)) sprintf(" (%s)", e(r$subgroup)) else ""
  # "odds of" wording only for binary outcomes; ordinal and count ratios read
  # as higher/lower values of the outcome itself
  odds <- identical(r$est_name, "OR") && !is_ordinal(r$model_type %||% "")
  body <- switch(r$kind,
    regression = {
      if (!is.na(r$level)) {
        if (r$section == "contrast") {
          if (sig) {
            if (odds) sprintf("The odds of %s were %s for %s than for %s", dv, hl, g1, g2)
            else sprintf("%s was %s for %s than for %s", uc_first(dv), hl, g1, g2)
          } else {
            sprintf("%s did not differ significantly between %s and %s",
                    if (odds) paste("The odds of", dv) else uc_first(dv), g1, g2)
          }
        } else {
          if (sig) {
            sprintf("Compared with %s, %s was associated with %s %s%s", g2, g1, hl,
                    if (odds) "odds of " else "", dv)
          } else {
            sprintf("%s did not differ significantly between %s and %s",
                    if (odds) paste("The odds of", dv) else uc_first(dv), g1, g2)
          }
        }
      } else {
        v <- e(r$variable_label)
        if (sig) {
          if (odds) sprintf("%s was associated with %s odds of %s", uc_first(v), hl, dv)
          else sprintf("%s was %s associated with %s", uc_first(v),
                       if (s > 0) "positively" else "negatively", dv)
        } else {
          sprintf("%s was not significantly associated with %s%s", uc_first(v),
                  if (odds) "the odds of " else "", dv)
        }
      }
    },
    ttest = {
      desc <- function(m, sd) {
        if (is.na(m)) return("")
        sprintf(" (%s = %s, %s = %s)", f$it("M"), fmt_num(m, digits), f$it("SD"),
                fmt_num(sd, digits))
      }
      a <- paste0(g1, desc(r$m1, r$sd1)); b <- paste0(g2, desc(r$m2, r$sd2))
      if (sig) sprintf("%s%s was %s in %s than in %s", uc_first(dv), sg, hl, a, b)
      else sprintf("%s%s did not differ significantly between %s and %s", uc_first(dv), sg, a, b)
    },
    chisq = {
      if (!is.na(r$level)) {
        pct <- function(p) if (is.na(p)) "" else sprintf(" (%s%s)", fmt_num(100 * p, 1), f$pct)
        a <- paste0(g1, pct(r$m1)); b <- paste0(g2, pct(r$m2))
        if (sig) sprintf("%s%s was %s common in %s than in %s", uc_first(dv), sg,
                         if (s > 0) "more" else "less", a, b)
        else sprintf("%s%s did not differ significantly between %s and %s", uc_first(dv), sg, a, b)
      } else {
        sprintf("%s%s %s across %s", uc_first(dv), sg,
                if (sig) "differed significantly" else "did not differ significantly",
                e(lc_first(r$variable_label)))
      }
    },
    equivalence = {
      what <- if (!is.na(r$level)) {
        sprintf("The difference in %s between %s and %s", dv, g1, g2)
      } else {
        sprintf("The association between %s and %s", e(lc_first(r$variable_label)), dv)
      }
      b <- if (isTRUE(all.equal(-r$m1, r$m2))) {
        sprintf("\u00b1%s", fmt_num(r$m2, digits))
      } else sprintf("[%s, %s]", fmt_num(r$m1, digits), fmt_num(r$m2, digits))
      if (grepl("d units", r$adjustment %||% "", fixed = TRUE)) b <- paste(b, "(in d units)")
      sprintf("%s%s %s within equivalence bounds of %s (TOST)", what, sg,
              if (sig) "was statistically equivalent to no effect" else
                "could not be shown to be equivalent to no effect",
              b)
    },
    anova = sprintf("%s%s %s across %s", uc_first(dv), sg,
                    if (sig) "differed significantly" else "did not differ significantly",
                    e(lc_first(r$variable_label))),
    cor = {
      v <- e(lc_first(r$variable_label))
      if (sig) sprintf("%s and %s were %s correlated%s", uc_first(dv), v,
                       if (s > 0) "positively" else "negatively", sg)
      else sprintf("%s and %s were not significantly correlated%s", uc_first(dv), v, sg)
    },
    stop("Unknown result kind.", call. = FALSE)
  )
  if (r$kind == "regression" && !is.na(r$subgroup)) body <- paste0(body, sg)
  if (identical(r$interval, "CrI")) {
    body <- sub("did not differ significantly", "did not credibly differ", body, fixed = TRUE)
    body <- sub("was not significantly associated", "was not credibly associated", body, fixed = TRUE)
  }
  if (nzchar(st)) paste0(body, ", ", st, ".") else paste0(body, ".")
}

#' Write results paragraphs that rebuild on every render
#'
#' With a [cr_hypotheses()] map, writes one paragraph per hypothesis: each
#' primary test, then each robustness check attached to it. With a results
#' object, writes a sentence for every reported estimate (optionally filtered).
#' Put it in a chunk or inline code in Quarto/R Markdown; save it for Overleaf
#' or Google Docs with [cr_save()].
#'
#' @param x A `cr_hypotheses`, `clean_results`, or `cr_tests` object.
#' @param dv,variables Optional filters for a results object.
#' @param contrasts Include pairwise contrasts (results objects).
#' @param intercept Include the intercept (results objects).
#' @param status Add each hypothesis's mechanical status (expectation met or
#'   not) after its paragraph. Off by default: that call is yours to make.
#' @inheritParams cr_sentence
#' @return A `cr_text` object (a character string that prints as text and
#'   renders as-is in documents).
#' @export
cr_report <- function(x, dv = NULL, variables = NULL, contrasts = TRUE, intercept = FALSE,
                      status = FALSE, stats = NULL,
                      format = c("markdown", "latex", "html", "plain"),
                      style = c("apa", "econ"), alpha = 0.05, digits = 2, p_digits = 3) {
  format <- match.arg(format)
  style <- match.arg(style)
  args <- list(x = x, dv = dv, variables = variables, contrasts = contrasts,
               intercept = intercept, status = status, stats = stats, style = style,
               alpha = alpha, digits = digits, p_digits = p_digits)
  render <- function(format) {
    a <- args; a$format <- format
    do.call(report_text, a)
  }
  structure(render(format), class = c("cr_text", "character"), render = render,
            format = format)
}

report_text <- function(x, dv, variables, contrasts, intercept, status, stats, format, style,
                        alpha, digits, p_digits) {
  f <- text_fmt(format, style)
  sent <- function(r) row_sentence(r, stats, format, style, alpha, digits, p_digits)
  paras <- if (inherits(x, "cr_hypotheses")) {
    vapply(names(x$tests), function(h) {
      lab <- x$labels[[h]]
      lead <- f$bold(if (!is.null(lab) && nzchar(lab)) sprintf("%s: %s.", h, f$esc(lab))
                     else paste0(h, "."))
      body <- character()
      for (t in x$tests[[h]]) {
        body <- c(body, sent(t$row))
        for (k in seq_along(t$robustness)) {
          rb <- t$robustness[[k]]
          body <- c(body, sprintf("In the robustness check (%s), the corresponding estimate was %s.",
                                  f$esc(rb$name), stats_text(rb$row, stats, format, style, digits,
                                                             p_digits)))
        }
      }
      if (status) {
        st <- vapply(x$tests[[h]], function(t) t$status, "")
        body <- c(body, sprintf("[Status: %s.]", paste(unique(st), collapse = "; ")))
      }
      paste(c(lead, body), collapse = " ")
    }, "")
  } else {
    r <- cr_rows(x)
    r <- r[!r$is_ref, , drop = FALSE]
    if (!is.null(dv)) r <- r[r$dv %in% dv | r$dv_label %in% dv, , drop = FALSE]
    if (!is.null(variables)) {
      r <- r[r$variable %in% variables | r$variable_label %in% variables, , drop = FALSE]
    }
    if (r$kind[1] %in% "regression") {
      if (!contrasts) {
        r <- r[r$section != "contrast", , drop = FALSE]
      } else {
        # contrasts already include each level vs. the reference; don't say it twice
        has_ctr <- unique(paste(r$dv, r$variable)[r$section == "contrast"])
        r <- r[!(r$section == "fixed" & !is.na(r$level) & paste(r$dv, r$variable) %in% has_ctr), ,
               drop = FALSE]
      }
      if (!intercept) r <- r[r$variable != "(Intercept)", , drop = FALSE]
    }
    if (!nrow(r)) stop("Nothing to report after filtering.", call. = FALSE)
    para <- if (all(r$kind == "cor")) rep("all", nrow(r)) else r$dv
    vapply(split(seq_len(nrow(r)), factor(para, unique(para))), function(ix) {
      paste(vapply(ix, function(i) sent(r[i, ]), ""), collapse = " ")
    }, "")
  }
  sep <- if (format == "html") "" else "\n\n"
  if (format == "html") paras <- paste0("<p>", paras, "</p>")
  paste(paras, collapse = sep)
}

#' @export
print.cr_text <- function(x, ...) {
  cat(strwrap(unclass(x), width = getOption("width", 80)), sep = "\n")
  invisible(x)
}

#' @exportS3Method knitr::knit_print
knit_print.cr_text <- function(x, ...) {
  knitr::asis_output(as.character(unclass(x)))
}

#' @export
as.character.cr_text <- function(x, ...) {
  out <- unclass(x)
  attributes(out) <- NULL
  out
}

#' Re-render report text in another format
#'
#' @param x A `cr_text` object from [cr_report()].
#' @param format `"markdown"`, `"latex"`, `"html"`, or `"plain"`.
#' @return A `cr_text` object.
#' @export
cr_text_as <- function(x, format = c("markdown", "latex", "html", "plain")) {
  format <- match.arg(format)
  render <- attr(x, "render")
  structure(render(format), class = c("cr_text", "character"), render = render, format = format)
}

save_text <- function(x, path) {
  ext <- tolower(tools::file_ext(path))
  fmt <- switch(ext, tex = "latex", md = , qmd = "markdown", html = , htm = "html",
                txt = "plain",
                stop(sprintf("Unsupported extension '.%s' for text. Use .tex, .md, .html, or .txt.",
                             ext), call. = FALSE))
  txt <- as.character(cr_text_as(x, fmt))
  if (fmt == "html") {
    txt <- paste0("<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"></head>",
                  "<body style=\"font-family: 'Times New Roman', serif; font-size: 12pt;\">\n",
                  txt, "\n</body></html>")
  }
  if (fmt == "latex") txt <- paste("% Generated by cleanresults; re-run your script to update.", txt,
                                   sep = "\n")
  writeLines(enc2utf8(txt), path, useBytes = TRUE)
}
