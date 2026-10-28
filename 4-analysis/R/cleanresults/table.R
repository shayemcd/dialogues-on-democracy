#' Build a formatted results table
#'
#' Creates a table object that prints in the console, renders natively in
#' Quarto/R Markdown (HTML, Word, and PDF), and can be passed to [cr_save()].
#' Printing a `clean_results` object directly uses the defaults below.
#'
#' Start from a `style` preset and override any piece:
#'
#' | style | uncertainty | p | layout |
#' |---|---|---|---|
#' | `"apa"` (default) | `"ci"` | `"value"` | `"columns"` |
#' | `"stargazer"` | `"se"` | `"stars"` | `"stacked"` |
#' | `"compact"` | `"ci"` | `"stars"` | `"inline"` |
#'
#' @param x A `clean_results` object from [clean_regress()].
#' @param style Preset: `"apa"`, `"stargazer"`, or `"compact"` (see table).
#'   `"ci"` and `"se"` are accepted as aliases for `"apa"` and `"stargazer"`.
#' @param uncertainty `"ci"`, `"se"`, or `"none"`.
#' @param p `"value"` (a p column), `"stars"` (stars on the estimate, no p
#'   column), `"both"`, or `"none"`.
#' @param layout How the estimate and its uncertainty share space:
#'   `"columns"` (separate columns), `"stacked"` (uncertainty on the line
#'   below the estimate), or `"inline"` (one cell: `0.46 (0.21)` or
#'   `0.46 [0.04, 0.89]`).
#' @param contrasts `"include"` (contrasts as a section of this table),
#'   `"exclude"` (leave them out, e.g. to report them in a separate table), or
#'   `"only"` (a contrasts-only table; see [cr_contrast_table()]).
#' @param digits Decimal places for estimates, CIs, SEs, and fit statistics.
#' @param p_digits Decimal places for p-values (APA: no leading zero).
#' @param fit,random Show the model-fit and random-effects blocks.
#' @param show_model_type Show the model type under each outcome name.
#' @param note Extra note lines appended under the automatic note.
#' @param auto_note Include the automatic methods note.
#' @param effect_size Add a Cohen's *d* column for linear models (factor levels and
#'   contrasts).
#' @param omit Regular expression matched against model term labels (the
#'   `variable` column of [cr_tidy()], e.g. `"Topic"`, `"Month"`,
#'   `"age:condition"`): matching rows are left out of the table, typically
#'   nuisance controls. They stay in the fitted models and in [cr_tidy()],
#'   [cr_report()], and [cr_ground_truth()]. With `auto_note = TRUE` the note
#'   lists them ("Also controls for: ...").
#' @return An object of class `cr_table`.
#' @export
#' @examples
#' \dontrun{
#' cr_table(res)                                   # APA: b, 95% CI, p
#' cr_table(res, uncertainty = "se")               # b, SE, p
#' cr_table(res, p = "stars")                      # stars instead of a p column
#' cr_table(res, uncertainty = "se", layout = "inline", p = "stars")  # 0.46* (0.21)
#' cr_table(res, style = "stargazer")              # stacked SEs with stars
#' cr_table(res, contrasts = "exclude")            # main table without contrasts
#' cr_contrast_table(res)                          # contrasts in their own table
#' cr_table(res, omit = "^(region|wave)$")         # hide nuisance controls
#' }
cr_table <- function(x, style = "apa", uncertainty = NULL, p = NULL, layout = NULL,
                     contrasts = c("include", "exclude", "only"), digits = 2, p_digits = 3,
                     fit = TRUE, random = TRUE, show_model_type = TRUE, note = NULL,
                     auto_note = TRUE, effect_size = FALSE, omit = NULL) {
  stopifnot(inherits(x, "clean_results"))
  contrasts <- match.arg(contrasts)
  spec <- table_spec(style, uncertainty, p, layout, contrasts, effect_size)
  shown <- omit_rows(x, omit)
  if (auto_note && length(shown$omitted)) {
    note <- c(sprintf("Also controls for: %s.", paste(shown$omitted, collapse = ", ")), note)
  }
  if (auto_note) note <- c(low_df_note(shown$x), note)
  g <- build_grid(shown$x, spec = spec, digits = digits, p_digits = p_digits,
                  fit = fit, random = random, show_model_type = show_model_type,
                  note = note)
  if (!auto_note) g$notes <- note
  structure(list(grid = g, results = x), class = "cr_table")
}

#' Pairwise contrasts as a separate table
#'
#' Shortcut for `cr_table(x, contrasts = "only", ...)`. Pair it with
#' `cr_table(x, contrasts = "exclude")` for the main table.
#'
#' @inheritParams cr_table
#' @param ... Passed to [cr_table()] (`style`, `uncertainty`, `p`, `layout`, ...).
#' @return An object of class `cr_table`.
#' @export
cr_contrast_table <- function(x, ...) {
  cr_table(x, contrasts = "only", ...)
}

# CR2 Satterthwaite df below 4 mean the robust test rests on very few
# effective clusters; Tipton (2015) shows its p-values and CIs are then
# unreliable. Flag it in the note rather than let a tiny SE pass silently.
low_df_note <- function(x, threshold = 4) {
  e <- x$estimates
  cr2_dvs <- names(Filter(function(m) !is.null(m$cluster) && !isTRUE(m$bayes) &&
                            m$type %in% c("lm", "logit", "poisson", "negbin", "lmer"),
                          x$meta))
  rows <- e[e$dv %in% cr2_dvs & e$row_type %in% c("level", "term") & e$variable != "(Intercept)" & is.finite(e$df) &
              !is.na(e$estimate), , drop = FALSE]
  low <- rows$df < threshold
  if (!any(low)) return(NULL)
  sprintf(paste("%d of %d tests have CR2 Satterthwaite df below %d (smallest: %.1f);",
                "their p-values and CIs are unreliable with so few effective clusters."),
          sum(low), nrow(rows), threshold, min(rows$df[low]))
}

# Drops the rows of `omit`-matched variables; returns the trimmed results and the
# display labels of what was dropped (for the note).
omit_rows <- function(x, omit = NULL) {
  if (is.null(omit) || !nzchar(omit)) return(list(x = x, omitted = character()))
  e <- x$estimates
  drop <- !is.na(e$variable) & grepl(omit, e$variable, perl = TRUE)
  if (!any(drop)) {
    warning(sprintf("`omit` = '%s' matched no table rows.", omit), call. = FALSE)
    return(list(x = x, omitted = character()))
  }
  vars <- unique(e$variable[drop])
  labs <- vapply(vars, function(v) {
    r <- e[drop & e$variable == v, , drop = FALSE]
    h <- r$label[r$row_type == "header"]
    if (length(h)) h[1] else r$label[1]
  }, "")
  x$estimates <- e[!drop, , drop = FALSE]
  list(x = x, omitted = unname(labs))
}

as_cr_table <- function(x) {
  if (inherits(x, "cr_table")) return(x)
  if (inherits(x, "clean_results")) return(cr_table(x))
  stop("Expected a clean_results or cr_table object.", call. = FALSE)
}

# Word / HTML -----------------------------------------------------------------

#' Render a results table as a flextable (Word, HTML, PowerPoint)
#'
#' @param x A `clean_results` or `cr_table` object.
#' @param font,font_size Font family and size in points.
#' @return A `flextable`.
#' @export
cr_flextable <- function(x, font = "Times New Roman", font_size = 10) {
  g <- as_cr_table(x)$grid
  m <- grid_matrix(g)
  keys <- c("label", paste0("c", seq_len(ncol(m) - 1)))
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- keys
  k <- length(g$dv_labels)
  ft <- flextable::flextable(df, col_keys = keys)

  head_rows <- list(g$dv_labels, g$model_types, g$sub_head)
  head_rows <- head_rows[!vapply(head_rows, is.null, logical(1))]
  bottom <- head_rows[[length(head_rows)]]
  bottom_vals <- stats::setNames(as.list(c("", if (length(bottom) == ncol(m) - 1) bottom
                                           else rep(bottom, each = g$per))), keys)
  ft <- flextable::set_header_labels(ft, values = bottom_vals)
  if (length(bottom) == k && g$per > 1) {
    for (j in seq_len(k)) ft <- flextable::merge_at(ft, i = 1, j = 1 + ((j - 1) * g$per + 1):(j * g$per),
                                                    part = "header")
  }
  for (h in rev(head_rows[-length(head_rows)])) {
    ft <- flextable::add_header_row(ft, values = c("", h), colwidths = c(1, rep(g$per, k)))
  }

  types <- vapply(g$rows, `[[`, "", "type")
  indent <- vapply(g$rows, `[[`, 0L, "indent")

  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::align(ft, j = -1, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::bold(ft, i = 1, part = "header")
  if (!is.null(g$model_types)) {
    ft <- flextable::italic(ft, i = 2, part = "header")
  }
  if (!is.null(g$sub_head)) {
    hi <- length(head_rows)
    ital <- which(g$sub_head %in% c("b", "p", "OR", "SE", "d")) + 1
    ft <- flextable::italic(ft, i = hi, j = ital, part = "header")
    ft <- flextable::italic(ft, i = hi, j = setdiff(seq_len(ncol(m))[-1], ital), italic = FALSE,
                            part = "header")
  }
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  if (any(indent > 0)) ft <- flextable::padding(ft, i = which(indent > 0), j = 1, padding.left = 14)
  if (any(types == "section")) ft <- flextable::italic(ft, i = which(types == "section"), j = 1)

  if (g$per > 1) {
    for (i in which(types == "stat")) {
      for (j in seq_len(k)) {
        ft <- flextable::merge_at(ft, i = i, j = 1 + ((j - 1) * g$per + 1):(j * g$per))
      }
    }
  }
  first_block <- which(types == "section" & vapply(g$rows, `[[`, "", "label") %in%
                         c("Random effects", "Model fit"))[1]
  if (!is.na(first_block)) {
    ft <- flextable::hline(ft, i = first_block - 1,
                           border = officer::fp_border(width = 0.75))
  }

  if (length(g$notes)) {
    ft <- flextable::add_footer_lines(ft, values = g$notes)
    if (startsWith(g$notes[1], "Note. ")) {
      ft <- flextable::compose(ft, i = 1, j = 1, part = "footer",
        value = flextable::as_paragraph(flextable::as_i("Note."), " ",
                                        sub("^Note\\. ", "", g$notes[1])))
    }
    ft <- flextable::font(ft, fontname = font, part = "footer")
    ft <- flextable::fontsize(ft, size = font_size - 1, part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
  }
  ft <- flextable::fix_border_issues(ft)
  flextable::autofit(ft)
}

# LaTeX -----------------------------------------------------------------------

#' Render a results table as LaTeX (booktabs + threeparttable)
#'
#' Produces a self-contained `table` float for Overleaf (`\input{}` it), or
#' only the `threeparttable` block when `float = FALSE` (used inside Quarto,
#' which supplies the float and caption). Requires `\usepackage{booktabs}` and
#' `\usepackage{threeparttable}`; compiles with pdfLaTeX, XeLaTeX, or LuaLaTeX.
#'
#' @param x A `clean_results` or `cr_table` object.
#' @param caption,label Caption and `\label{}` key (e.g. `"tab:main"`).
#' @param float Wrap in a `table` environment.
#' @param placement Float placement specifier.
#' @param size Font size command applied inside the float, e.g. `"\\small"`.
#' @return A character string of LaTeX code.
#' @export
cr_latex <- function(x, caption = NULL, label = NULL, float = TRUE, placement = "htbp",
                     size = "\\small") {
  g <- as_cr_table(x)$grid
  m <- grid_matrix(g)
  k <- length(g$dv_labels)
  nc <- ncol(m)
  types <- vapply(g$rows, `[[`, "", "type")
  indent <- vapply(g$rows, `[[`, 0L, "indent")
  esc <- function(s) latex_text(s)

  span_row <- function(vals, fmt = "%s") {
    cells <- vapply(vals, function(v) sprintf("\\multicolumn{%d}{c}{%s}", g$per, sprintf(fmt, esc(v))), "")
    paste0(" & ", paste(cells, collapse = " & "), " \\\\")
  }
  cmid <- paste(vapply(seq_len(k), function(j) {
    sprintf("\\cmidrule(lr){%d-%d}", 2 + (j - 1) * g$per, 1 + j * g$per)
  }, ""), collapse = " ")

  head <- c("\\toprule", span_row(g$dv_labels, "%s"))
  if (!is.null(g$model_types)) head <- c(head, span_row(g$model_types, "\\textit{%s}"))
  head <- c(head, cmid)
  sh <- vapply(g$sub_head, function(s) {
    if (s %in% c("b", "p", "d")) sprintf("$%s$", s)
    else if (s %in% c("OR", "SE")) sprintf("\\textit{%s}", s)
    else esc(s)
  }, "")
  head <- c(head, paste0(" & ", paste(sh, collapse = " & "), " \\\\"))
  head <- c(head, "\\midrule")

  body <- character()
  for (i in seq_along(g$rows)) {
    r <- g$rows[[i]]
    lab <- esc(r$label)
    if (r$type == "section") {
      if (r$label %in% c("Random effects", "Model fit")) body <- c(body, "\\midrule")
      else body <- c(body, "\\addlinespace")
      body <- c(body, sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", nc, lab))
      next
    }
    if (indent[i] > 0) lab <- paste0("\\quad ", lab)
    cells <- esc(r$cells)
    if (r$type == "stat" && g$per > 1) {
      cells <- vapply(seq_len(k), function(j) {
        sprintf("\\multicolumn{%d}{c}{%s}", g$per, cells[(j - 1) * g$per + 1])
      }, "")
    }
    body <- c(body, paste0(lab, " & ", paste(cells, collapse = " & "), " \\\\"))
  }

  colspec <- paste0("l", strrep("c", nc - 1))
  tab <- c(
    "\\begin{threeparttable}",
    sprintf("\\begin{tabular}{%s}", colspec),
    head, body, "\\bottomrule", "\\end{tabular}"
  )
  if (length(g$notes)) {
    notes <- esc(g$notes)
    notes <- sub("^Note\\. ", "\\\\textit{Note.} ", notes)
    tab <- c(tab, "\\begin{tablenotes}[flushleft]", "\\footnotesize",
             paste0("\\item ", notes), "\\end{tablenotes}")
  }
  tab <- c(tab, "\\end{threeparttable}")

  if (float) {
    tab <- c(
      sprintf("\\begin{table}[%s]", placement), "\\centering",
      if (!is.null(size)) size,
      if (!is.null(caption)) sprintf("\\caption{%s}", esc(caption)),
      if (!is.null(label)) sprintf("\\label{%s}", label),
      tab, "\\end{table}"
    )
  } else if (!is.null(size)) {
    tab <- c("\\begingroup", size, tab, "\\endgroup")
  }
  paste(tab, collapse = "\n")
}

latex_text <- function(s) {
  s <- gsub("\\", "\001", s, fixed = TRUE)
  s <- gsub("([&%$#_{}])", "\\\\\\1", s)
  s <- gsub("~", "\\textasciitilde{}", s, fixed = TRUE)
  s <- gsub("^", "\\textasciicircum{}", s, fixed = TRUE)
  s <- gsub("\001", "\\textbackslash{}", s, fixed = TRUE)
  rep <- c(
    "\u03c3\u00b2" = "$\\sigma^2$",
    "\u03c4\u2080\u2080" = "$\\tau_{00}$",
    "\u03c4" = "$\\tau$",
    "R\u00b2" = "$R^2$",
    "\u00b2" = "$^2$",
    "\u00d7" = "$\\times$",
    "\u2014" = "---",
    "\u2013" = "--",
    "<" = "$<$",
    ">" = "$>$"
  )
  for (nm in names(rep)) s <- gsub(nm, rep[[nm]], s, fixed = TRUE)
  gsub("(^|[[ (])-([0-9.])", "\\1$-$\\2", s)
}

latex_deps <- function() {
  dep <- function(name) structure(list(name = name, options = NULL, extra_lines = NULL),
                                   class = "latex_dependency")
  list(dep("booktabs"), dep("threeparttable"))
}

# Console / knitr -------------------------------------------------------------

#' @export
print.cr_table <- function(x, ...) {
  g <- x$grid
  m <- grid_matrix(g)
  indent <- vapply(g$rows, `[[`, 0L, "indent")
  m[, 1] <- paste0(strrep("  ", indent), m[, 1])
  k <- length(g$dv_labels)
  head <- list(g$dv_labels, g$model_types, g$sub_head)
  head <- head[!vapply(head, is.null, logical(1))]
  w <- pmax(apply(m, 2, function(col) max(nchar(col), 0)), 1)
  block_w <- vapply(seq_len(k), function(j) {
    cols <- 1 + ((j - 1) * g$per + 1):(j * g$per)
    sum(w[cols]) + 2 * (g$per - 1)
  }, numeric(1))
  for (j in seq_len(k)) {
    need <- max(vapply(head, function(h) if (length(h) == k) nchar(h[j]) else 0L, 0L))
    if (need > block_w[j]) {
      last <- 1 + j * g$per
      w[last] <- w[last] + need - block_w[j]
      block_w[j] <- need
    }
  }
  pad <- function(s, n, side = "both") formatC(s, width = n, flag = if (side == "left") "-" else " ")
  center <- function(s, n) {
    extra <- n - nchar(s); l <- floor(extra / 2)
    paste0(strrep(" ", l), s, strrep(" ", extra - l))
  }
  total <- w[1] + 2 + sum(block_w) + 3 * (k - 1)
  rule <- strrep("\u2500", total)
  cat(rule, "\n")
  for (h in head) {
    if (length(h) == k) {
      line <- paste(vapply(seq_len(k), function(j) center(h[j], block_w[j]), ""), collapse = "   ")
    } else {
      line <- paste(vapply(seq_len(k), function(j) {
        cols <- ((j - 1) * g$per + 1):(j * g$per)
        paste(vapply(cols, function(cc) center(h[cc], w[cc + 1]), ""), collapse = "  ")
      }, ""), collapse = "   ")
    }
    cat(pad("", w[1], "left"), " ", line, "\n", sep = "")
  }
  cat(rule, "\n")
  for (i in seq_len(nrow(m))) {
    cells <- vapply(seq_len(k), function(j) {
      cols <- ((j - 1) * g$per + 1):(j * g$per)
      paste(vapply(cols, function(cc) center(m[i, cc + 1], w[cc + 1]), ""), collapse = "  ")
    }, "")
    cat(pad(m[i, 1], w[1], "left"), "  ", paste(cells, collapse = "   "), "\n", sep = "")
  }
  cat(rule, "\n")
  for (n in g$notes) cat(strwrap(n, width = max(60, min(total, 100))), sep = "\n")
  invisible(x)
}

#' @export
print.clean_results <- function(x, ...) {
  print(cr_table(x), ...)
  invisible(x)
}

#' @exportS3Method knitr::knit_print
knit_print.cr_table <- function(x, options = NULL, ...) {
  if (knitr::is_latex_output()) {
    # raw block so pandoc (R Markdown and Quarto) passes the LaTeX through untouched
    tex <- paste0("\n```{=latex}\n", cr_latex(x, float = FALSE), "\n```\n")
    return(knitr::asis_output(tex, meta = latex_deps()))
  }
  knitr::knit_print(cr_flextable(x), options = options, ...)
}

#' @exportS3Method knitr::knit_print
knit_print.clean_results <- function(x, options = NULL, ...) {
  knit_print.cr_table(cr_table(x), options = options, ...)
}
