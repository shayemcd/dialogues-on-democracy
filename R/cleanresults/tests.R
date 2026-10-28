# Simple tests that share the regression results' reporting machinery -------------
#
# Each returns a `cr_tests` object whose rows follow cr_rows(), so they work
# with cr_inline(), cr_report(), cr_h(), cr_ground_truth(), and cr_verify().

new_tests <- function(rows, test, adjust_text = NULL) {
  rows <- rows[, row_cols]
  rownames(rows) <- NULL
  structure(list(rows = rows, test = test, adjust_text = adjust_text), class = "cr_tests")
}

# Which pairs of levels to compare. Each pair is c(a, b) and reports a - b.
resolve_pairs <- function(levs, pairs = "all", ref = NULL) {
  if (identical(pairs, "all")) {
    cmb <- utils::combn(levs, 2)
    return(lapply(seq_len(ncol(cmb)), function(j) rev(cmb[, j])))  # later minus earlier
  }
  if (identical(pairs, "ref")) {
    ref <- ref %||% levs[1]
    if (!ref %in% levs) stop(sprintf("Reference '%s' is not a level.", ref), call. = FALSE)
    return(lapply(setdiff(levs, ref), function(l) c(l, ref)))
  }
  if (is.character(pairs)) pairs <- strsplit(pairs, " vs. ", fixed = TRUE)
  bad <- setdiff(unlist(pairs), levs)
  if (length(bad)) stop(sprintf("Unknown level(s) in `pairs`: %s", paste(bad, collapse = ", ")),
                        call. = FALSE)
  pairs
}

group_levels <- function(g) {
  if (is.factor(g)) return(levels(droplevels(g)))
  if (is.logical(g)) return(c("FALSE", "TRUE"))
  sort(unique(as.character(g)))
}

# CI for a standardized difference from its t statistic via the noncentral t.
nct_ci <- function(t, df, scale, level) {
  if (!is.finite(t) || !is.finite(df)) return(c(NA_real_, NA_real_))
  a <- (1 - level) / 2
  f <- function(ncp, target) suppressWarnings(stats::pt(t, df, ncp)) - target
  w <- 10 + 2 * abs(t)
  lo <- tryCatch(stats::uniroot(f, c(t - w, t + w), target = 1 - a)$root, error = function(e) NA)
  hi <- tryCatch(stats::uniroot(f, c(t - w, t + w), target = a)$root, error = function(e) NA)
  c(lo, hi) * scale
}

# Confidence level that matches a p-value correction across m comparisons.
# Holm, Hochberg, and FDR have no matching CI, so Bonferroni intervals are the
# standard simultaneous companion (they are never narrower than needed).
adjusted_level <- function(conf_level, adjust, m) {
  if (adjust == "none" || m < 2) return(conf_level)
  1 - (1 - conf_level) / m
}

ci_note <- function(conf_level, level, m) {
  if (isTRUE(all.equal(level, conf_level))) return(NULL)
  sprintf("CIs Bonferroni-adjusted for %d comparisons (%s%% level)", m,
          format(round(100 * level, 1)))
}

# How many comparisons a CI must cover: the pairs (when p is adjusted across
# them) times the outcomes (when p is also adjusted across outcomes).
n_comparisons <- function(k, adjust, n_dv, dv_adjust) {
  (if (adjust != "none") k else 1) * (if (dv_adjust != "none" && n_dv > 1) n_dv else 1)
}

make_id <- function(...) {
  parts <- c(...)
  paste(parts[!is.na(parts)], collapse = "::")
}

split_by <- function(data, by) {
  if (is.null(by)) return(list(list(name = NA_character_, data = data)))
  lv <- group_levels(data[[by]])
  lapply(lv, function(l) list(name = l, data = data[as.character(data[[by]]) == l, , drop = FALSE]))
}

finish_adjust <- function(rows, adjust, dv_adjust) {
  rows$p.within <- rows$p.value
  fam <- paste(rows$dv, rows$subgroup)
  if (adjust != "none") {
    for (g in unique(fam)) {
      ix <- which(fam == g)
      if (length(ix) > 1) rows$p.within[ix] <- stats::p.adjust(rows$p.value[ix], adjust)
    }
  }
  rows$p.value <- rows$p.within
  n_dv <- length(unique(rows$dv))
  if (dv_adjust != "none" && n_dv > 1) {
    key <- paste(rows$level, rows$subgroup)
    for (g in unique(key)) {
      ix <- which(key == g)
      rows$p.value[ix] <- stats::p.adjust(rows$p.within[ix], dv_adjust)
    }
  }
  adj <- c(if (adjust != "none" && any(table(fam) > 1)) paste(adjust_name(adjust), "across pairs"),
           if (dv_adjust != "none" && n_dv > 1) paste(adjust_name(dv_adjust), "across outcomes"))
  rows$adjustment <- if (length(adj)) paste(adj, collapse = "; ") else NA_character_
  rows
}

#' Pairwise t-tests with Cohen's d for every pair of conditions
#'
#' Compares groups two at a time (all pairs, each versus a reference, or pairs
#' you choose), with Welch *t*-tests, a multiple-comparison correction across
#' the pairs, and Cohen's *d* (or Hedges' *g*) with a noncentral-*t* CI.
#' `cr_d()` is the same function, named for when *d* is what you are after.
#'
#' @param data A data frame.
#' @param dv One or more outcome columns.
#' @param group Grouping (condition) column.
#' @param pairs `"all"` (every pair), `"ref"` (each level versus `ref`), or
#'   specific pairs as `"Loss vs. Control"` strings or `c("Loss", "Control")`
#'   vectors. Each pair reports the first level minus the second.
#' @param ref Reference level when `pairs = "ref"` (default: first level).
#' @param id Participant column for within-subject (paired) comparisons.
#' @param by Optional column: run every comparison within each subgroup.
#' @param adjust Correction across the pairs of each outcome (any
#'   [stats::p.adjust()] method). When on, CIs (mean difference and *d*) are
#'   Bonferroni-adjusted to match, e.g. 98.3% CIs for 3 pairs, so a CI that
#'   excludes zero agrees with the corrected *p*. `adjust = "none"` gives
#'   ordinary 95% CIs.
#' @param dv_adjust Correction across outcomes for each pair. CIs widen to
#'   match (Bonferroni across pairs x outcomes).
#' @param effect `"d"` (Cohen's *d*: pooled SD between subjects, *d_z* paired)
#'   or `"g"` (Hedges' small-sample correction).
#' @param conf_level Confidence level.
#' @param labels,level_labels,dv_labels,clean_labels Display labels, as in
#'   [clean_regress()].
#' @return A `cr_tests` object.
#' @export
#' @examples
#' d <- data.frame(cond = rep(c("Control", "Gain", "Loss"), each = 30),
#'                 trust = rnorm(90, rep(c(3, 3.4, 3.1), each = 30)))
#' cr_d(d, "trust", "cond")                    # all three pairs
#' cr_d(d, "trust", "cond", pairs = "ref")      # each vs. Control
cr_ttest <- function(data, dv, group, pairs = "all", ref = NULL, id = NULL, by = NULL,
                     adjust = "holm", dv_adjust = "none", effect = c("d", "g"),
                     conf_level = 0.95, labels = NULL, level_labels = NULL, dv_labels = NULL,
                     clean_labels = TRUE) {
  effect <- match.arg(effect)
  check_cols(data, c(dv, group, id, by))
  lab <- make_labeller(c(labels, dv_labels), level_labels, clean_labels)
  levs <- group_levels(data[[group]])
  prs <- resolve_pairs(levs, pairs, ref)
  n_cmp <- n_comparisons(length(prs), adjust, length(dv), dv_adjust)
  lvl <- adjusted_level(conf_level, "any", n_cmp)
  out <- list()
  for (y in dv) for (sg in split_by(data, by)) for (p in prs) {
    dd <- sg$data
    a <- dd[as.character(dd[[group]]) == p[1], , drop = FALSE]
    b <- dd[as.character(dd[[group]]) == p[2], , drop = FALSE]
    r <- empty_rows(1)
    if (is.null(id)) {
      xa <- stats::na.omit(a[[y]]); xb <- stats::na.omit(b[[y]])
      n1 <- length(xa); n2 <- length(xb)
      if (n1 < 2 || n2 < 2) next
      tt <- stats::t.test(xa, xb, conf.level = lvl)
      sp <- sqrt(((n1 - 1) * stats::var(xa) + (n2 - 1) * stats::var(xb)) / (n1 + n2 - 2))
      es <- (mean(xa) - mean(xb)) / sp
      dfp <- n1 + n2 - 2
      ci <- nct_ci(es * sqrt(n1 * n2 / (n1 + n2)), dfp, sqrt((n1 + n2) / (n1 * n2)), lvl)
      r$model_type <- "Welch t-test"
      r$n <- n1 + n2
    } else {
      m <- merge(a[, c(id, y)], b[, c(id, y)], by = id, suffixes = c(".a", ".b"))
      m <- m[stats::complete.cases(m), , drop = FALSE]
      xa <- m[[paste0(y, ".a")]]; xb <- m[[paste0(y, ".b")]]
      n1 <- n2 <- nrow(m)
      if (n1 < 2) next
      tt <- stats::t.test(xa, xb, paired = TRUE, conf.level = lvl)
      dif <- xa - xb
      es <- mean(dif) / stats::sd(dif)
      dfp <- n1 - 1
      ci <- nct_ci(es * sqrt(n1), dfp, 1 / sqrt(n1), lvl)
      r$model_type <- "paired t-test"
      r$n <- n1
    }
    if (effect == "g") {
      J <- 1 - 3 / (4 * dfp - 1)
      es <- es * J; ci <- ci * J
    }
    r$id <- make_id("ttest", y, group, paste(p, collapse = " vs. "), sg$name)
    r$kind <- "ttest"
    r$dv <- y; r$dv_label <- lab$var(y)
    r$section <- "pairwise"
    r$variable <- group; r$variable_label <- lab$var(group)
    r$level <- paste(p[1], "vs.", p[2])
    r$group1 <- lab$level(group, p[1]); r$group2 <- lab$level(group, p[2])
    r$level_label <- paste(r$group1, "vs.", r$group2)
    r$subgroup <- sg$name
    r$est_name <- "Diff."
    r$estimate <- mean(xa) - mean(xb)
    r$std.error <- unname(tt$stderr)
    r$stat_name <- "t"; r$statistic <- unname(tt$statistic); r$df <- unname(tt$parameter)
    r$conf.low <- tt$conf.int[1]; r$conf.high <- tt$conf.int[2]
    r$p.value <- tt$p.value
    r$es_name <- effect; r$es <- es; r$es.low <- ci[1]; r$es.high <- ci[2]
    r$m1 <- mean(xa); r$sd1 <- stats::sd(xa); r$n1 <- n1
    r$m2 <- mean(xb); r$sd2 <- stats::sd(xb); r$n2 <- n2
    r$conf_level <- lvl
    out[[length(out) + 1]] <- r
  }
  if (!length(out)) stop("No comparisons had at least two observations per group.", call. = FALSE)
  rows <- finish_adjust(do.call(rbind, out), adjust, dv_adjust)
  rows$adjustment <- add_note(rows$adjustment, ci_note(conf_level, lvl, n_cmp))
  new_tests(rows, if (is.null(id)) "Welch t-test" else "paired t-test", unique(rows$adjustment))
}

#' Cohen's d for every pair of conditions
#'
#' For a data frame, the same as [cr_ttest()]. For [clean_regress()] results,
#' returns the model-based standardized differences: each factor level versus
#' the reference and every pairwise contrast, divided by the pooled
#' within-group SD of the outcome (`lm`) or the total SD (`lmer`), with CIs
#' from the same robust SEs.
#'
#' @param x A data frame or a `clean_results` object.
#' @param ... For data frames, arguments to [cr_ttest()]. For results, `dv`
#'   and `variable` to filter, and `pairs` (`"all"` contrasts, `"ref"`
#'   levels versus the reference, or `"A vs. B"` strings).
#' @return A `cr_tests` object (data) or a data frame (results).
#' @export
cr_d <- function(x, ...) UseMethod("cr_d")

#' @export
cr_d.data.frame <- function(x, ...) cr_ttest(x, ...)

#' @export
cr_d.clean_results <- function(x, dv = NULL, variable = NULL, pairs = "all", ...) {
  r <- cr_rows(x)
  r <- r[!is.na(r$es), , drop = FALSE]
  if (!nrow(r)) {
    stop("No standardized differences: d is available for linear (lm/lmer) models with factors.",
         call. = FALSE)
  }
  if (!is.null(dv)) r <- r[r$dv %in% dv | r$dv_label %in% dv, , drop = FALSE]
  if (!is.null(variable)) r <- r[r$variable %in% variable | r$variable_label %in% variable, , drop = FALSE]
  if (identical(pairs, "ref")) {
    r <- r[r$section == "fixed", , drop = FALSE]
  } else if (identical(pairs, "all")) {
    if (!any(r$section == "contrast")) {
      stop("No pairwise contrasts in these results. Set `contrasts` in clean_regress(), ",
           "or use pairs = \"ref\".", call. = FALSE)
    }
    r <- r[r$section == "contrast", , drop = FALSE]
  } else {
    r <- r[r$level %in% pairs | r$level_label %in% pairs, , drop = FALSE]
  }
  r <- r[, c("dv_label", "variable_label", "level_label", "estimate", "es", "es.low", "es.high",
             "p.value", "adjustment")]
  names(r) <- c("outcome", "variable", "comparison", "difference", "d", "d.low", "d.high",
                "p.value", "adjustment")
  rownames(r) <- NULL
  r
}

#' Chi-square (or Fisher's exact) tests
#'
#' An overall test of association between an outcome and a grouping variable,
#' with Cramer's *V*. For a binary outcome, `pairs` adds 2 x 2 comparisons
#' between groups with the proportions, a correction across pairs, and
#' Cohen's *h*.
#'
#' @inheritParams cr_ttest
#' @param pairs `NULL` (overall test only), `"all"`, `"ref"`, or specific pairs.
#' @param exact `"auto"` (Fisher's exact test when any expected count is
#'   below 5), `TRUE`, or `FALSE`.
#' @param success For pairwise comparisons, the outcome value counted as a
#'   success (default: the last level, `TRUE`, or `1`).
#' @return A `cr_tests` object.
#' @export
cr_chisq <- function(data, dv, group, pairs = NULL, ref = NULL, by = NULL, exact = "auto",
                     success = NULL, adjust = "holm", dv_adjust = "none", conf_level = 0.95,
                     labels = NULL, level_labels = NULL, dv_labels = NULL, clean_labels = TRUE) {
  check_cols(data, c(dv, group, by))
  lab <- make_labeller(c(labels, dv_labels), level_labels, clean_labels)
  out <- list()
  for (y in dv) for (sg in split_by(data, by)) {
    dd <- sg$data[stats::complete.cases(sg$data[, c(y, group)]), , drop = FALSE]
    tab <- table(as.character(dd[[group]]), as.character(dd[[y]]))
    cs <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
    use_exact <- isTRUE(exact) || (identical(exact, "auto") && any(cs$expected < 5))
    r <- empty_rows(1)
    r$id <- make_id("chisq", y, group, "overall", sg$name)
    r$kind <- "chisq"; r$model_type <- "chi-square test"
    r$dv <- y; r$dv_label <- lab$var(y); r$section <- "overall"
    r$variable <- group; r$variable_label <- lab$var(group); r$subgroup <- sg$name
    r$n <- sum(tab)
    r$stat_name <- "chi2"; r$statistic <- unname(cs$statistic); r$df <- unname(cs$parameter)
    r$p.value <- cs$p.value
    if (use_exact) {
      r$p.value <- stats::fisher.test(tab, simulate.p.value = any(dim(tab) > 2) && sum(tab) > 200,
                                      B = 1e4)$p.value
      r$stat_name <- "Fisher's exact"; r$model_type <- "Fisher's exact test"
    }
    r$es_name <- "V"
    r$es <- sqrt(unname(cs$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
    r$conf_level <- conf_level
    out[[length(out) + 1]] <- r

    if (!is.null(pairs)) {
      yv <- group_levels(dd[[y]])
      if (length(yv) != 2) stop("Pairwise comparisons need a binary outcome.", call. = FALSE)
      succ <- as.character(success %||% if ("TRUE" %in% yv) "TRUE" else if ("1" %in% yv) "1" else yv[2])
      prs <- resolve_pairs(group_levels(dd[[group]]), pairs, ref)
      n_cmp <- n_comparisons(length(prs), adjust, length(dv), dv_adjust)
      plvl <- adjusted_level(conf_level, "any", n_cmp)
      for (p in prs) {
        xa <- as.character(dd[[y]][as.character(dd[[group]]) == p[1]])
        xb <- as.character(dd[[y]][as.character(dd[[group]]) == p[2]])
        s1 <- sum(xa == succ); s2 <- sum(xb == succ); n1 <- length(xa); n2 <- length(xb)
        t2 <- matrix(c(s1, n1 - s1, s2, n2 - s2), 2, byrow = TRUE)
        c2 <- suppressWarnings(stats::chisq.test(t2, correct = FALSE))
        pr <- suppressWarnings(stats::prop.test(c(s1, s2), c(n1, n2), correct = FALSE,
                                                conf.level = plvl))
        p1 <- s1 / n1; p2 <- s2 / n2
        rr <- empty_rows(1)
        rr$id <- make_id("chisq", y, group, paste(p, collapse = " vs. "), sg$name)
        rr$kind <- "chisq"; rr$model_type <- "chi-square test"
        rr$dv <- y; rr$dv_label <- lab$var(y); rr$section <- "pairwise"
        rr$variable <- group; rr$variable_label <- lab$var(group); rr$subgroup <- sg$name
        rr$level <- paste(p[1], "vs.", p[2])
        rr$group1 <- lab$level(group, p[1]); rr$group2 <- lab$level(group, p[2])
        rr$level_label <- paste(rr$group1, "vs.", rr$group2)
        rr$est_name <- "RD"; rr$estimate <- p1 - p2
        rr$conf.low <- pr$conf.int[1]; rr$conf.high <- pr$conf.int[2]
        rr$stat_name <- "chi2"; rr$statistic <- unname(c2$statistic); rr$df <- 1
        rr$p.value <- c2$p.value
        if (isTRUE(exact) || (identical(exact, "auto") && any(c2$expected < 5))) {
          rr$p.value <- stats::fisher.test(t2)$p.value
          rr$stat_name <- "Fisher's exact"; rr$model_type <- "Fisher's exact test"
        }
        h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
        z <- stats::qnorm(1 - (1 - plvl) / 2) * sqrt(1 / n1 + 1 / n2)
        rr$es_name <- "h"; rr$es <- h; rr$es.low <- h - z; rr$es.high <- h + z
        rr$m1 <- p1; rr$n1 <- n1; rr$m2 <- p2; rr$n2 <- n2; rr$n <- n1 + n2
        rr$conf_level <- plvl
        rr$adjustment <- ci_note(conf_level, plvl, n_cmp) %||% NA_character_
        out[[length(out) + 1]] <- rr
      }
    }
  }
  rows <- do.call(rbind, out)
  pw <- rows$section == "pairwise"
  rows$p.within <- rows$p.value
  if (any(pw)) {
    notes <- rows$adjustment[pw]
    adj <- finish_adjust(rows[pw, , drop = FALSE], adjust, dv_adjust)
    adj$adjustment <- ifelse(is.na(notes), adj$adjustment,
                             ifelse(is.na(adj$adjustment), notes, paste(adj$adjustment, notes, sep = "; ")))
    rows[pw, ] <- adj
  }
  new_tests(rows, "chi-square", if (any(pw)) unique(rows$adjustment[pw]) else NULL)
}

#' One-way ANOVA with eta squared
#'
#' @inheritParams cr_ttest
#' @param welch Use Welch's ANOVA (unequal variances).
#' @return A `cr_tests` object. For pairwise follow-ups use [cr_ttest()], or
#'   [clean_regress()] with `contrasts` for model-based (e.g. Tukey) contrasts.
#' @export
cr_anova <- function(data, dv, group, by = NULL, welch = FALSE, labels = NULL,
                     level_labels = NULL, dv_labels = NULL, clean_labels = TRUE) {
  check_cols(data, c(dv, group, by))
  lab <- make_labeller(c(labels, dv_labels), level_labels, clean_labels)
  out <- list()
  for (y in dv) for (sg in split_by(data, by)) {
    dd <- sg$data[stats::complete.cases(sg$data[, c(y, group)]), , drop = FALSE]
    dd[[group]] <- factor(as.character(dd[[group]]))
    f <- stats::as.formula(sprintf("`%s` ~ `%s`", y, group))
    a <- summary(stats::aov(f, data = dd))[[1]]
    ss_b <- a[1, "Sum Sq"]; ss_w <- a[2, "Sum Sq"]; ms_w <- a[2, "Mean Sq"]
    r <- empty_rows(1)
    r$id <- make_id("anova", y, group, sg$name)
    r$kind <- "anova"; r$model_type <- if (welch) "Welch ANOVA" else "one-way ANOVA"
    r$dv <- y; r$dv_label <- lab$var(y); r$section <- "overall"
    r$variable <- group; r$variable_label <- lab$var(group); r$subgroup <- sg$name
    r$stat_name <- "F"
    if (welch) {
      w <- stats::oneway.test(f, data = dd, var.equal = FALSE)
      r$statistic <- unname(w$statistic); r$df <- unname(w$parameter[1])
      r$df2 <- unname(w$parameter[2]); r$p.value <- w$p.value
    } else {
      r$statistic <- a[1, "F value"]; r$df <- a[1, "Df"]; r$df2 <- a[2, "Df"]
      r$p.value <- a[1, "Pr(>F)"]
    }
    r$es_name <- "eta2"; r$es <- ss_b / (ss_b + ss_w)
    r$n <- nrow(dd); r$conf_level <- 0.95
    out[[length(out) + 1]] <- r
  }
  rows <- do.call(rbind, out)
  rows$p.within <- rows$p.value
  new_tests(rows, "ANOVA")
}

#' Correlations between every pair of variables
#'
#' @param data A data frame.
#' @param vars Columns to correlate (every pair is tested).
#' @param method `"pearson"` or `"spearman"`.
#' @param adjust Correction across all pairs.
#' @param labels Named vector of display labels for variables.
#' @param clean_labels Tidy names without an explicit label (see [clean_label()]).
#' @inheritParams cr_ttest
#' @return A `cr_tests` object.
#' @export
cr_cor <- function(data, vars, method = c("pearson", "spearman"), adjust = "none", by = NULL,
                   conf_level = 0.95, labels = NULL, clean_labels = TRUE) {
  method <- match.arg(method)
  check_cols(data, c(vars, by))
  lab <- make_labeller(labels, NULL, clean_labels)
  cmb <- utils::combn(vars, 2)
  lvl <- adjusted_level(conf_level, adjust, ncol(cmb))
  out <- list()
  for (sg in split_by(data, by)) for (j in seq_len(ncol(cmb))) {
    a <- cmb[1, j]; b <- cmb[2, j]
    dd <- sg$data[stats::complete.cases(sg$data[, c(a, b)]), , drop = FALSE]
    ct <- suppressWarnings(stats::cor.test(dd[[a]], dd[[b]], method = method,
                                           conf.level = lvl, exact = FALSE))
    r <- empty_rows(1)
    r$id <- make_id("cor", a, b, sg$name)
    r$kind <- "cor"; r$model_type <- paste(method, "correlation")
    r$dv <- a; r$dv_label <- lab$var(a)
    r$variable <- b; r$variable_label <- lab$var(b); r$subgroup <- sg$name
    r$section <- "pairwise"
    r$est_name <- if (method == "pearson") "r" else "r_s"
    r$estimate <- unname(ct$estimate)
    if (method == "pearson") {
      r$conf.low <- ct$conf.int[1]; r$conf.high <- ct$conf.int[2]
      r$stat_name <- "t"; r$statistic <- unname(ct$statistic); r$df <- unname(ct$parameter)
    } else {
      r$df <- nrow(dd) - 2
    }
    r$p.value <- ct$p.value
    r$n <- nrow(dd); r$conf_level <- lvl
    out[[length(out) + 1]] <- r
  }
  rows <- do.call(rbind, out)
  rows$p.within <- rows$p.value
  if (adjust != "none") {
    for (g in unique(rows$subgroup)) {
      ix <- which(rows$subgroup %in% g)
      rows$p.value[ix] <- rows$p.within[ix] <- stats::p.adjust(rows$p.value[ix], adjust)
    }
    rows$adjustment <- paste(c(paste(adjust_name(adjust), "across pairs"),
                               ci_note(conf_level, lvl, ncol(cmb))), collapse = "; ")
  }
  new_tests(rows, "correlation", if (adjust != "none") rows$adjustment[1])
}

add_note <- function(x, note) {
  if (is.null(note)) return(x)
  ifelse(is.na(x), note, paste(x, note, sep = "; "))
}

check_cols <- function(data, cols) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop(sprintf("Columns not found: %s", paste(miss, collapse = ", ")), call. = FALSE)
}

#' @export
print.cr_tests <- function(x, digits = 2, ...) {
  r <- x$rows
  cat(sprintf("%s (%d result%s)\n", x$test, nrow(r), if (nrow(r) == 1) "" else "s"))
  if (!is.null(x$adjust_text) && length(x$adjust_text) && !all(is.na(x$adjust_text))) {
    cat("p-values adjusted:", paste(stats::na.omit(x$adjust_text), collapse = "; "), "\n")
  }
  for (i in seq_len(nrow(r))) {
    what <- describe_row(r[i, ])
    cat(sprintf("  %s\n    %s\n", what, stats_text(r[i, ], c(if (!is.na(r$estimate[i])) "est",
                                                               "ci", "test", "p", "es"),
                                                    "plain", digits = digits)))
  }
  invisible(x)
}
