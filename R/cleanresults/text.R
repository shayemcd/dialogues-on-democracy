# Text output ----------------------------------------------------------------
#
# Every number in generated text comes from cr_rows(), so prose rebuilt on each
# render cannot drift from the tables. Direction and significance words are
# computed too, so they change when the results change.

text_fmt <- function(format = "markdown", style = "apa") {
  format <- match.arg(format, c("markdown", "latex", "html", "plain"))
  style <- match.arg(style, c("apa", "econ"))
  it <- switch(format,
    markdown = function(s) paste0("*", s, "*"),
    latex = function(s) paste0("\\textit{", s, "}"),
    html = function(s) paste0("<i>", s, "</i>"),
    plain = identity)
  if (style == "econ") it <- identity
  sub <- switch(format,
    markdown = function(s, x) paste0(s, "~", x, "~"),
    latex = function(s, x) paste0(s, "\\textsubscript{", x, "}"),
    html = function(s, x) paste0(s, "<sub>", x, "</sub>"),
    plain = function(s, x) paste0(s, x))
  bold <- switch(format,
    markdown = function(s) paste0("**", s, "**"),
    latex = function(s) paste0("\\textbf{", s, "}"),
    html = function(s) paste0("<b>", s, "</b>"),
    plain = identity)
  greek <- function(sym) {
    if (format == "latex") return(switch(sym, chi2 = "$\\chi^2$", eta2 = "$\\eta^2$",
                                          omega2 = "$\\omega^2$"))
    switch(sym, chi2 = "\u03c7\u00b2", eta2 = "\u03b7\u00b2", omega2 = "\u03c9\u00b2")
  }
  esc <- if (format == "latex") latex_text else if (format == "html") html_text else identity
  list(format = format, style = style, it = it, sub = sub, bold = bold, greek = greek,
       esc = esc, pct = if (format == "latex") "\\%" else "%",
       lt = if (format == "html") "&lt;" else if (format == "latex") "$<$" else "<")
}

html_text <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

# Statistics bounded by 1 lose the leading zero in APA style.
bounded_stats <- c("r", "r_s", "V", "eta2", "omega2", "p")

sym_text <- function(name, f) {
  switch(name,
    r_s = f$sub(f$it("r"), "s"),
    Diff. = f$sub(f$it("M"), "diff"),
    chi2 = , eta2 = , omega2 = f$greek(name),
    `Fisher's exact` = "Fisher's exact test",
    f$it(name))
}

num_text <- function(x, name, f, digits) {
  s <- fmt_num(x, digits)
  if (f$style == "apa" && name %in% bounded_stats) s <- sub("^(-?)0\\.", "\\1.", s)
  s
}

df_text <- function(df) {
  if (!is.finite(df)) return("")
  if (abs(df - round(df)) < 1e-8) format(round(df)) else fmt_num(df, 2)
}

p_text <- function(p, f, p_digits) {
  s <- fmt_p(p, p_digits, apa = f$style == "apa")
  if (startsWith(s, "<")) paste(f$it("p"), f$lt, sub("^< ", "", s)) else paste(f$it("p"), "=", s)
}

default_stats <- function(kind, style) {
  if (kind == "regression") {
    return(if (style == "econ") c("est", "se", "p") else c("est", "ci", "p"))
  }
  switch(kind, cor = c("est", "ci", "p"), ttest = c("test", "p", "es"),
         equivalence = c("est", "ci", "test", "p"),
         c("test", "p", "es"))
}

# Format the statistics for one row, e.g. "*b* = 0.46, 95% CI [0.04, 0.89], *p* = .065".
stats_text <- function(r, stats = NULL, format = "markdown", style = "apa", digits = 2,
                       p_digits = 3) {
  f <- text_fmt(format, style)
  stats <- stats %||% default_stats(r$kind, f$style)
  ci_lab <- sprintf("%s%s %s", format(round(100 * r$conf_level, 1)), f$pct,
                    if (identical(r$interval, "CrI")) "CrI" else "CI")
  est_name <- r$est_name %||% NA
  piece <- function(s) {
    switch(s,
      est = {
        if (is.na(r$estimate)) return(NULL)
        lhs <- sym_text(est_name, f)
        if (r$kind == "cor" && is.finite(r$df)) lhs <- sprintf("%s(%s)", lhs, df_text(r$df))
        sprintf("%s = %s", lhs, num_text(r$estimate, est_name, f, digits))
      },
      se = if (!is.na(r$std.error)) sprintf("%s = %s", f$it("SE"), fmt_num(r$std.error, digits)),
      ci = if (!is.na(r$conf.low)) {
        sprintf("%s [%s, %s]", ci_lab, num_text(r$conf.low, est_name, f, digits),
                num_text(r$conf.high, est_name, f, digits))
      },
      test = {
        if (is.na(r$stat_name)) return(NULL)
        if (r$stat_name == "Fisher's exact") return("Fisher's exact test")
        if (is.na(r$statistic)) return(NULL)
        dfs <- switch(r$stat_name,
          F = sprintf("(%s, %s)", df_text(r$df), df_text(r$df2)),
          chi2 = sprintf("(%s, %s = %s)", df_text(r$df), f$it("N"), format(r$n, big.mark = ",")),
          z = "",
          if (is.finite(r$df)) sprintf("(%s)", df_text(r$df)) else "")
        sprintf("%s%s = %s", sym_text(r$stat_name, f), dfs, fmt_num(r$statistic, digits))
      },
      p = if (!is.na(r$p.value)) p_text(r$p.value, f, p_digits) else if (!is.na(r$pd %||% NA)) {
        sprintf("%s = %s%s", f$it("pd"), formatC(100 * r$pd, format = "f", digits = 1), f$pct)
      },
      es = {
        if (is.na(r$es)) return(NULL)
        s <- sprintf("%s = %s", sym_text(r$es_name, f), num_text(r$es, r$es_name, f, digits))
        if (!is.na(r$es.low)) {
          s <- sprintf("%s, %s [%s, %s]", s, ci_lab, num_text(r$es.low, r$es_name, f, digits),
                       num_text(r$es.high, r$es_name, f, digits))
        }
        s
      },
      stop(sprintf("Unknown statistic '%s'. Use est, se, ci, test, p, es.", s), call. = FALSE)
    )
  }
  parts <- unlist(lapply(stats, piece))
  if (!length(parts)) return("")
  if (f$style == "econ" && stats[1] == "est" && length(parts) > 1 && !is.na(r$estimate)) {
    return(sprintf("%s (%s)", parts[1], paste(parts[-1], collapse = ", ")))
  }
  paste(parts, collapse = ", ")
}

#' Report one result in text
#'
#' Use inside Quarto/R Markdown inline code, e.g.
#' `` `r cr_inline(res, "trust", "condition", "Loss")` `` gives
#' "*b* = -0.46, 95% CI \\[-0.83, -0.09\\], *p* = .017". Works for
#' [clean_regress()] results and for simple tests such as [cr_ttest()].
#'
#' @param x A `clean_results` or `cr_tests` object.
#' @param dv Outcome name (raw or displayed label).
#' @param variable Predictor or grouping variable, or a coefficient name such
#'   as `"age"`. For correlations, the other variable.
#' @param level Factor level as in the data (`"LossFraming"`) or as displayed
#'   (`"Loss framing"`); for contrasts, `"Loss vs. Gain"` in either form.
#' @param stats Which pieces to include, in order: any of `"est"`, `"se"`,
#'   `"ci"`, `"test"`, `"p"`, `"es"` (effect size). `NULL` uses the style's default.
#' @param format `"markdown"` (Quarto/R Markdown), `"latex"`, `"html"`, or
#'   `"plain"` (for pasting into Google Docs or Word).
#' @param style `"apa"` (italic symbols, no leading zero for *p*, *r*) or
#'   `"econ"` (estimate with SE and *p* in parentheses, leading zeros).
#' @param subgroup For tests run with `by`, which subgroup.
#' @param digits,p_digits Decimal places.
#' @return A character string.
#' @export
cr_inline <- function(x, dv, variable = NULL, level = NULL, stats = NULL,
                      format = c("markdown", "latex", "html", "plain"), style = c("apa", "econ"),
                      subgroup = NULL, digits = 2, p_digits = 3) {
  format <- match.arg(format)
  style <- match.arg(style)
  r <- cr_find(x, dv, variable, level, subgroup)
  stats_text(r, stats, format, style, digits, p_digits)
}

# Direction and significance --------------------------------------------------

# Significant: p < alpha, or for Bayesian rows a credible interval that
# excludes the null (0, or 1 for ratios).
row_sig <- function(r, alpha = 0.05) {
  if (identical(r$interval, "CrI")) {
    null <- if (r$est_name %in% c("OR", "IRR")) 1 else 0
    return(!is.na(r$conf.low) && (r$conf.low > null || r$conf.high < null))
  }
  !is.na(r$p.value) && r$p.value < alpha
}

row_sign <- function(r) {
  null <- if (r$est_name %in% c("OR", "IRR")) 1 else 0
  v <- if (!is.na(r$estimate)) r$estimate else if (!is.na(r$es)) r$es + null else r$statistic + null
  if (is.na(v) || v == null) 0L else if (v > null) 1L else -1L
}

#' Direction and significance words that update with the results
#'
#' Write your own sentence and let these fill in the words that depend on the
#' data, so the text stays correct when results change:
#' "Gain framing `` `r cr_direction(res, "trust", "condition", "Gain")` ``
#' trust" gives "increased" or "decreased".
#'
#' @inheritParams cr_inline
#' @param words For `cr_direction()`, the words for a positive and a negative
#'   estimate (odds ratios above and below 1). For `cr_sig()`, the words for a
#'   significant and a non-significant result.
#' @param alpha Significance threshold applied to the reported (adjusted) *p*.
#' @return A character string.
#' @export
cr_direction <- function(x, dv, variable = NULL, level = NULL,
                         words = c("higher", "lower"), subgroup = NULL) {
  r <- cr_find(x, dv, variable, level, subgroup)
  s <- row_sign(r)
  if (s == 0) return("no different")
  if (s > 0) words[1] else words[2]
}

#' @rdname cr_direction
#' @export
cr_sig <- function(x, dv, variable = NULL, level = NULL, words = c("was", "was not"),
                   alpha = 0.05, subgroup = NULL) {
  r <- cr_find(x, dv, variable, level, subgroup)
  if (row_sig(r, alpha)) words[1] else words[2]
}

#' Stop rendering when a claim in your text stops holding
#'
#' Put `cr_expect()` calls in a hidden chunk next to hand-written claims (in
#' the abstract or discussion, say). If a re-run changes the sign or the
#' significance of a result, rendering fails with a message naming the claim,
#' instead of the old sentence silently surviving.
#'
#' @inheritParams cr_inline
#' @param x A `clean_results`/`cr_tests` object, or a `cr_hypotheses` object
#'   (then every test with an `expect` is checked).
#' @param sign `"+"`, `"-"`, or `NULL` (don't check).
#' @param sig `TRUE` (must be significant), `FALSE` (must not be), or `NULL`.
#' @param alpha Significance threshold.
#' @param action `"error"` (stop rendering) or `"warning"`.
#' @param claim Optional text of the claim, shown in the message.
#' @return `TRUE` invisibly when every check passes.
#' @export
cr_expect <- function(x, dv = NULL, variable = NULL, level = NULL, sign = NULL, sig = NULL,
                      alpha = 0.05, action = c("error", "warning"), claim = NULL,
                      subgroup = NULL) {
  action <- match.arg(action)
  fails <- if (inherits(x, "cr_hypotheses")) {
    unlist(lapply(hyp_tests(x), function(t) {
      if (t$expect == "any") return(NULL)
      chk <- expect_check(t$row, t$expect, alpha)
      if (is.null(chk)) NULL else sprintf("%s (%s): %s", t$hypothesis, t$describe, chk)
    }))
  } else {
    r <- cr_find(x, dv, variable, level, subgroup)
    chk <- check_parts(r, sign, sig, alpha)
    if (is.null(chk)) NULL else sprintf("%s%s: %s", if (!is.null(claim)) paste0("\"", claim, "\" ") else "",
                                        describe_row(r), chk)
  }
  if (length(fails)) {
    msg <- paste(c("cr_expect: a claim no longer matches the results.", paste("-", fails)),
                 collapse = "\n")
    if (action == "error") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

check_parts <- function(r, sign, sig, alpha) {
  got <- sprintf("got %s", stats_text(r, c("est", "p"), "plain"))
  if (is.na(r$estimate)) got <- sprintf("got %s", stats_text(r, c("test", "p", "es"), "plain"))
  s <- row_sign(r)
  is_sig <- row_sig(r, alpha)
  bad <- c(
    if (!is.null(sign) && sign == "+" && s <= 0) "expected a positive effect",
    if (!is.null(sign) && sign == "-" && s >= 0) "expected a negative effect",
    if (isTRUE(sig) && !is_sig) {
      if (identical(r$interval, "CrI")) "expected the CrI to exclude the null" else sprintf("expected p < %s", format(alpha))
    },
    if (isFALSE(sig) && is_sig) {
      if (identical(r$interval, "CrI")) "expected the CrI to include the null" else sprintf("expected p >= %s", format(alpha))
    }
  )
  if (!length(bad)) NULL else paste0(paste(bad, collapse = " and "), "; ", got)
}

# expect: "+" / "-" mean signed and significant; "ns" means not significant.
expect_check <- function(r, expect, alpha) {
  switch(expect,
    `+` = check_parts(r, "+", TRUE, alpha),
    `-` = check_parts(r, "-", TRUE, alpha),
    ns = check_parts(r, NULL, FALSE, alpha),
    equiv = {
      if (!identical(r$kind, "equivalence")) {
        "expect = \"equiv\" needs a cr_equivalence() result"
      } else check_parts(r, NULL, TRUE, alpha)
    },
    NULL)
}

describe_row <- function(r) {
  lv <- if (!is.na(r$level_label)) paste0(" [", r$level_label, "]") else ""
  sg <- if (!is.na(r$subgroup)) paste0(" {", r$subgroup, "}") else ""
  sprintf("%s ~ %s%s%s", r$dv_label, r$variable_label, lv, sg)
}
