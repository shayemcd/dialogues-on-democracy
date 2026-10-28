# One row schema for every reportable result ---------------------------------
#
# Regressions (clean_results) and simple tests (cr_tests) are flattened into the
# same columns so text, hypotheses, ground truth, and verification all work on
# any source the same way.

row_cols <- c(
  "id", "kind", "model_type", "dv", "dv_label", "section", "variable",
  "variable_label", "level", "level_label", "group1", "group2", "subgroup",
  "is_ref", "est_name", "estimate", "std.error", "stat_name", "statistic", "df",
  "df2", "conf.low", "conf.high", "p.value", "p.within", "adjustment",
  "es_name", "es", "es.low", "es.high", "n", "m1", "sd1", "n1", "m2", "sd2", "n2",
  "conf_level", "pd", "interval"
)

empty_rows <- function(n = 0) {
  chr <- c("id", "kind", "model_type", "dv", "dv_label", "section", "variable",
           "variable_label", "level", "level_label", "group1", "group2", "subgroup",
           "est_name", "stat_name", "adjustment", "es_name", "interval")
  out <- lapply(row_cols, function(cl) {
    if (cl %in% chr) rep(NA_character_, n) else if (cl == "is_ref") rep(FALSE, n)
    else rep(NA_real_, n)
  })
  as.data.frame(stats::setNames(out, row_cols), stringsAsFactors = FALSE)
}

#' Flatten results into one row per reportable estimate
#'
#' The common format behind [cr_report()], [cr_ground_truth()], [cr_verify()],
#' and [cr_expect()]. Works for [clean_regress()] results and for simple tests
#' ([cr_ttest()], [cr_chisq()], [cr_anova()], [cr_cor()]).
#'
#' @param x A `clean_results` or `cr_tests` object.
#' @param ... Unused.
#' @return A data frame.
#' @export
cr_rows <- function(x, ...) UseMethod("cr_rows")

#' @export
cr_rows.cr_tests <- function(x, ...) x$rows

#' @export
cr_rows.clean_results <- function(x, ...) {
  e <- x$estimates
  out <- list()
  for (dv in x$settings$dvs) {
    ed <- e[e$dv == dv, , drop = FALSE]
    meta <- x$meta[[dv]]
    hdr <- ed[ed$row_type == "header", , drop = FALSE]
    vlab <- stats::setNames(hdr$label, paste(hdr$section, hdr$variable))
    ref <- ed[ed$is_ref %in% TRUE, , drop = FALSE]
    ref_lab <- stats::setNames(sub(" (ref.)", "", ref$label, fixed = TRUE), ref$variable)
    keep <- ed[ed$row_type %in% c("term", "level", "ref"), , drop = FALSE]
    if (!nrow(keep)) next
    r <- empty_rows(nrow(keep))
    r$id <- paste(dv, keep$key, sep = "::")
    r$kind <- "regression"
    r$model_type <- meta$type
    r$dv <- dv
    r$dv_label <- unname(x$settings$dv_labels[dv])
    r$section <- keep$section
    r$variable <- keep$variable
    is_lev <- keep$row_type %in% c("level", "ref")
    r$variable_label <- ifelse(is_lev, unname(vlab[paste(keep$section, keep$variable)]),
                               keep$label)
    r$level <- ifelse(is_lev, keep$level, NA_character_)
    r$level_label <- ifelse(is_lev, sub(" (ref.)", "", keep$label, fixed = TRUE), NA_character_)
    r$is_ref <- keep$is_ref %in% TRUE
    for (i in which(is_lev)) {
      if (keep$section[i] == "contrast") {
        parts <- strsplit(r$level_label[i], " vs. ", fixed = TRUE)[[1]]
        r$group1[i] <- parts[1]; r$group2[i] <- parts[2]
      } else {
        r$group1[i] <- r$level_label[i]
        r$group2[i] <- unname(ref_lab[keep$variable[i]])
      }
    }
    r$est_name <- est_symbol(meta)
    r$estimate <- keep$estimate
    r$std.error <- keep$std.error
    r$statistic <- keep$statistic
    r$df <- keep$df
    r$stat_name <- ifelse(is.na(keep$statistic), NA_character_,
                          ifelse(is.finite(keep$df), "t", "z"))
    r$pd <- if ("pd" %in% names(keep)) keep$pd else NA_real_
    r$interval <- if (isTRUE(meta$bayes)) "CrI" else "CI"
    if ("by_level" %in% names(keep)) r$subgroup <- keep$by_level
    r$conf.low <- keep$conf.low
    r$conf.high <- keep$conf.high
    r$p.value <- keep$p.value
    r$p.within <- keep$p.within
    adj_w <- ifelse(keep$section == "contrast" & x$settings$contrast_adjust != "none",
                    paste(adjust_name(x$settings$contrast_adjust), "within factor"), NA)
    adj_d <- ifelse(keep$dv_adjusted %in% TRUE,
                    paste(adjust_name(x$settings$dv_adjust), "across outcomes"), NA)
    r$adjustment <- vapply(seq_len(nrow(keep)), function(i) {
      a <- stats::na.omit(c(adj_w[i], adj_d[i]))
      if (length(a)) paste(a, collapse = "; ") else NA_character_
    }, "")
    if ("es" %in% names(keep)) {
      r$es <- keep$es; r$es.low <- keep$es.low; r$es.high <- keep$es.high
      r$es_name <- ifelse(is.na(keep$es), NA_character_, "d")
    }
    r$n <- meta$n
    r$conf_level <- if ("ci_level" %in% names(keep)) keep$ci_level else x$settings$conf_level
    out[[dv]] <- r
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# Find exactly one row. `dv`, `variable`, and `level` accept raw names or
# displayed labels; contrasts are "A vs. B". Correlations match either order.
cr_find <- function(x, dv, variable = NULL, level = NULL, subgroup = NULL) {
  r <- cr_rows(x)
  where <- function(msg) sprintf("%s. Run cr_rows() on the source to see what is available.", msg)
  is_dv <- function(v) r$dv %in% v | r$dv_label %in% v
  is_var <- function(v) r$variable %in% v | r$variable_label %in% v
  m <- r[is_dv(dv), , drop = FALSE]
  if (!nrow(m) && !any(is_var(dv))) {
    stop(where(sprintf("Outcome '%s' not found", dv)), call. = FALSE)
  }
  if (!is.null(subgroup)) m <- m[m$subgroup %in% subgroup, , drop = FALSE]
  if (!is.null(variable)) {
    hit <- m$variable %in% variable | m$variable_label %in% variable |
      (is.na(m$level) & m$id %in% paste(m$dv, variable, sep = "::"))
    m <- m[hit, , drop = FALSE]
    if (!nrow(m)) {
      # correlations are symmetric: accept the two variables in either order
      m <- r[is_var(dv) & is_dv(variable) & r$kind == "cor", , drop = FALSE]
      if (!is.null(subgroup)) m <- m[m$subgroup %in% subgroup, , drop = FALSE]
    }
    if (!nrow(m)) stop(where(sprintf("'%s' not found for outcome '%s'", variable, dv)),
                       call. = FALSE)
  }
  if (!is.null(level)) {
    sec <- if (grepl(" vs. ", level, fixed = TRUE)) "contrast" else "fixed"
    cand <- m[(m$level %in% level | m$level_label %in% level) &
                (m$kind != "regression" | m$section == sec), , drop = FALSE]
    if (!nrow(cand)) {
      levs <- unique(stats::na.omit(m$level_label))
      stop(where(sprintf("Level '%s' not found. Available: %s", level,
                         paste(levs, collapse = ", "))), call. = FALSE)
    }
    m <- cand
  } else if (nrow(m) > 1) {
    m <- m[is.na(m$level), , drop = FALSE]
  }
  if (nrow(m) != 1) {
    stop(where(sprintf("Could not find a unique result for %s%s%s in '%s'",
                       variable %||% "", if (!is.null(level)) " / " else "", level %||% "", dv)),
         call. = FALSE)
  }
  if (isTRUE(m$is_ref)) {
    stop("That is the reference category; it has no estimate of its own.", call. = FALSE)
  }
  m
}
