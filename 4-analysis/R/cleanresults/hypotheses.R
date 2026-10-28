# Hypothesis map ---------------------------------------------------------------
#
# Declares, in code, which result answers which hypothesis and which robustness
# checks qualify it. The map drives cr_report() paragraphs, cr_expect() checks,
# and the hypotheses table in cr_ground_truth().

#' Point a hypothesis at one result
#'
#' @param source A `clean_results` or `cr_tests` object.
#' @inheritParams cr_inline
#' @param expect The preregistered expectation: `"+"` (positive and
#'   significant), `"-"` (negative and significant), `"ns"` (not significant),
#'   or `"any"` (no expectation; nothing is checked).
#' @param robustness Robustness checks for this result: a named list whose
#'   elements are either another source with the same outcome, variable, and
#'   level (e.g. the model refit without outliers) or a `cr_h()` pointing
#'   somewhere else. Names describe each check ("excluding outliers").
#' @param alpha Significance threshold for `expect`.
#' @return A `cr_h` object.
#' @export
cr_h <- function(source, dv, variable = NULL, level = NULL,
                 expect = c("any", "+", "-", "ns", "equiv"),
                 robustness = NULL, subgroup = NULL, alpha = 0.05) {
  expect <- match.arg(expect)
  src_name <- paste(deparse(substitute(source)), collapse = "")
  row <- cr_find(source, dv, variable, level, subgroup)
  row$source <- src_name
  rob <- list()
  if (!is.null(robustness)) {
    if (!is.list(robustness) || inherits(robustness, c("clean_results", "cr_tests", "cr_h"))) {
      robustness <- list(robustness)
    }
    nms <- names(robustness) %||% rep("", length(robustness))
    for (k in seq_along(robustness)) {
      rb <- robustness[[k]]
      rrow <- if (inherits(rb, "cr_h")) rb$row else cr_find(rb, dv, variable, level, subgroup)
      if (!inherits(rb, "cr_h")) rrow$source <- sprintf("%s$robustness[[%d]]", src_name, k)
      rob[[k]] <- list(name = if (nzchar(nms[k])) nms[k] else sprintf("check %d", k), row = rrow)
    }
  }
  structure(list(row = row, expect = expect, alpha = alpha, robustness = rob,
                 describe = describe_row(row)), class = "cr_h")
}

#' Map hypotheses to results
#'
#' @param ... Named hypotheses, each a [cr_h()] or a list of them (when one
#'   hypothesis is tested several ways), e.g.
#'   `H1 = cr_h(res, "trust", "condition", "Gain", expect = "+")`.
#' @param labels Optional named vector of hypothesis statements, e.g.
#'   `c(H1 = "Gain framing increases trust")`, used as paragraph leads.
#' @return A `cr_hypotheses` object. Print it to see each test, its
#'   robustness checks, and whether the expectation is met.
#' @export
#' @examples
#' \dontrun{
#' hyp <- cr_hypotheses(
#'   H1 = cr_h(res, "trust", "condition", "Gain", expect = "+",
#'             robustness = list("excluding speeders" = res_strict)),
#'   H2 = list(cr_h(res, "shared", "condition", "Loss", expect = "+"),
#'             cr_h(res, "shared", "condition", "Loss vs. Gain", expect = "+")),
#'   labels = c(H1 = "Gain framing increases trust",
#'              H2 = "Loss framing increases sharing")
#' )
#' hyp
#' cr_report(hyp)
#' cr_expect(hyp)
#' }
cr_hypotheses <- function(..., labels = NULL) {
  tests <- list(...)
  if (!length(tests) || is.null(names(tests)) || any(!nzchar(names(tests)))) {
    stop("Name every hypothesis, e.g. cr_hypotheses(H1 = cr_h(...)).", call. = FALSE)
  }
  tests <- lapply(names(tests), function(h) {
    t <- tests[[h]]
    if (inherits(t, "cr_h")) t <- list(t)
    if (!all(vapply(t, inherits, logical(1), "cr_h"))) {
      stop(sprintf("Hypothesis %s must be a cr_h() or a list of them.", h), call. = FALSE)
    }
    lapply(t, function(ti) {
      ti$hypothesis <- h
      ti$status <- hyp_status(ti)
      ti
    })
  })
  names(tests) <- names(list(...))
  structure(list(tests = tests, labels = as.list(labels)), class = "cr_hypotheses")
}

hyp_status <- function(t) {
  if (t$expect == "any") return("no expectation set")
  if (is.null(expect_check(t$row, t$expect, t$alpha))) "expectation met" else "expectation not met"
}

hyp_tests <- function(x) unlist(x$tests, recursive = FALSE)

#' @export
print.cr_hypotheses <- function(x, ...) {
  for (h in names(x$tests)) {
    lab <- x$labels[[h]]
    cat(h, if (!is.null(lab)) paste0(": ", lab), "\n", sep = "")
    for (t in x$tests[[h]]) {
      cat(sprintf("  %s\n    %s\n    expect: %s -> %s\n", t$describe,
                  stats_text(t$row, NULL, "plain"), t$expect, t$status))
      for (rb in t$robustness) {
        cat(sprintf("    robustness (%s): %s\n", rb$name, stats_text(rb$row, NULL, "plain")))
      }
    }
  }
  invisible(x)
}
