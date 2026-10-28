# Display grid ---------------------------------------------------------------
#
# One backend-neutral grid (labels, cells, row types, headers, notes) feeds the
# Word, LaTeX, HTML, and text renderers so every format shows the same table.

fmt_num <- function(x, digits = 2) {
  out <- ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
  zero <- formatC(0, format = "f", digits = digits)
  out[out == paste0("-", zero)] <- zero
  out
}

fmt_p <- function(p, digits = 3, apa = TRUE) {
  thresh <- 10^-digits
  out <- formatC(p, format = "f", digits = digits)
  out[!is.na(p) & p < thresh] <- paste0("< ", formatC(thresh, format = "f", digits = digits))
  if (apa) out <- sub("^(< )?0\\.", "\\1.", out)
  out[is.na(p)] <- ""
  out
}

fmt_stars <- function(p, cuts = c(0.05, 0.01, 0.001)) {
  s <- ifelse(is.na(p), "", ifelse(p < cuts[3], "***", ifelse(p < cuts[2], "**",
         ifelse(p < cuts[1], "*", ""))))
  s
}

fmt_pd <- function(pd) ifelse(is.na(pd), "", sprintf("%s%%", formatC(100 * pd, format = "f", digits = 1)))

est_symbol <- function(meta) meta$est_name %||% (if (isTRUE(meta$exponentiate)) "OR" else "b")

fmt_ci <- function(lo, hi, digits = 2) {
  ifelse(is.na(lo), "", sprintf("[%s, %s]", fmt_num(lo, digits), fmt_num(hi, digits)))
}

adjust_name <- function(m) {
  nm <- c(holm = "Holm", hochberg = "Hochberg", hommel = "Hommel", bonferroni = "Bonferroni",
          BH = "Benjamini-Hochberg (FDR)", fdr = "Benjamini-Hochberg (FDR)",
          BY = "Benjamini-Yekutieli", tukey = "Tukey", sidak = "Sidak",
          scheffe = "Scheffe", mvt = "multivariate-t")
  if (m %in% names(nm)) unname(nm[m]) else m
}

# Resolve a style preset plus any explicit overrides into one display spec.
table_spec <- function(style = "apa", uncertainty = NULL, p = NULL, layout = NULL,
                       contrasts = "include", effect_size = FALSE) {
  style <- match.arg(style, c("apa", "stargazer", "compact", "ci", "se"))
  preset <- switch(style,
    apa = , ci = list(uncertainty = "ci", p = "value", layout = "columns"),
    stargazer = , se = list(uncertainty = "se", p = "stars", layout = "stacked"),
    compact = list(uncertainty = "ci", p = "stars", layout = "inline")
  )
  spec <- list(
    uncertainty = match.arg(uncertainty %||% preset$uncertainty, c("ci", "se", "none")),
    p = match.arg(p %||% preset$p, c("value", "stars", "both", "none")),
    layout = match.arg(layout %||% preset$layout, c("columns", "stacked", "inline")),
    contrasts = match.arg(contrasts, c("include", "exclude", "only"))
  )
  spec$stars <- spec$p %in% c("stars", "both")
  spec$p_col <- spec$p %in% c("value", "both")
  spec$unc_col <- spec$layout == "columns" && spec$uncertainty != "none"
  spec$effect_size <- isTRUE(effect_size)
  spec$cols <- c("est", if (spec$unc_col) "unc", if (spec$effect_size) "es",
                 if (spec$p_col) "p")
  spec
}

build_grid <- function(x, spec = table_spec(), digits = 2, p_digits = 3,
                       fit = TRUE, random = TRUE, show_model_type = TRUE, note = NULL) {
  dvs <- x$settings$dvs
  est <- x$estimates
  only <- spec$contrasts == "only"
  if (only && !any(est$section == "contrast")) {
    stop("No pairwise contrasts to show. Set `contrasts` in clean_regress().", call. = FALSE)
  }
  per <- length(spec$cols)
  ncell <- per * length(dvs)
  rows <- list()
  add_row <- function(label, type, indent, cells = rep("", ncell)) {
    rows[[length(rows) + 1]] <<- list(label = label, type = type, indent = indent, cells = cells)
  }
  block <- function(j) ((j - 1) * per + 1):(j * per)
  unc_txt <- function(r) {
    if (is.na(r$estimate)) return("")   # not estimable (e.g. a collinear term dropped by the fitter)
    switch(spec$uncertainty,
      se = sprintf("(%s)", fmt_num(r$std.error, digits)),
      ci = fmt_ci(r$conf.low, r$conf.high, digits),
      none = "")
  }

  emit_section <- function(sec) {
    sub <- est[est$section == sec, , drop = FALSE]
    keys <- unique(sub$key)
    keys <- c(intersect("(Intercept)", keys), setdiff(keys, "(Intercept)"))
    for (k in keys) {
      r0 <- sub[sub$key == k, , drop = FALSE][1, ]
      indent <- if (r0$row_type %in% c("level", "ref")) 1L else 0L
      cells <- rep("", ncell); below <- rep("", ncell)
      for (j in seq_along(dvs)) {
        r <- sub[sub$key == k & sub$dv == dvs[j], , drop = FALSE]
        if (!nrow(r) || r$row_type[1] == "header") next
        r <- r[1, ]
        b <- block(j)
        if (isTRUE(r$is_ref)) {
          cells[b[1]] <- "\u2014"
          next
        }
        e <- fmt_num(r$estimate, digits)
        if (spec$stars) e <- paste0(e, fmt_stars(r$p.value))
        if (spec$layout == "inline" && spec$uncertainty != "none") e <- paste(e, unc_txt(r))
        vals <- c(est = e,
                  unc = if (spec$unc_col) unc_txt(r),
                  es = if (spec$effect_size) fmt_num(r$es, digits),
                  p = if (spec$p_col) {
                    if (is.na(r$p.value) && !is.na(r$pd)) fmt_pd(r$pd) else fmt_p(r$p.value, p_digits)
                  })
        cells[b] <- vals[spec$cols]
        if (spec$layout == "stacked") below[b[1]] <- unc_txt(r)
      }
      add_row(r0$label, r0$row_type, indent, cells)
      if (spec$layout == "stacked" && spec$uncertainty != "none" &&
          r0$row_type %in% c("term", "level")) {
        add_row("", "se", indent, below)
      }
    }
  }

  if (!only) emit_section("fixed")
  if (spec$contrasts != "exclude" && any(est$section == "contrast")) {
    if (!only) add_row("Pairwise contrasts", "section", 0L)
    emit_section("contrast")
  }

  emit_stats <- function(which, title) {
    nms <- unique(unlist(lapply(dvs, function(d) names(x$stats[[d]][[which]]))))
    nms <- c(setdiff(nms, "AIC"), intersect(nms, "AIC"))
    if (!length(nms)) return()
    add_row(title, "section", 0L)
    for (nm in nms) {
      cells <- rep("", ncell)
      for (j in seq_along(dvs)) {
        v <- x$stats[[dvs[j]]][[which]][[nm]]
        if (is.null(v) || is.na(v)) next
        integer_stat <- nm == "Observations" || startsWith(nm, "N (")
        cells[block(j)[1]] <- if (integer_stat) format(v, big.mark = ",") else fmt_num(v, digits)
      }
      add_row(nm, "stat", 1L, cells)
    }
  }
  if (!only && random) emit_stats("random", "Random effects")
  if (!only && fit) emit_stats("fit", "Model fit")

  sub_head <- unlist(lapply(dvs, function(d) {
    bayes <- isTRUE(x$meta[[d]]$bayes)
    ci_lab <- sprintf("%g%% %s", 100 * x$settings$conf_level, if (bayes) "CrI" else "CI")
    unc_lab <- switch(spec$uncertainty, se = if (bayes) "MAD" else "SE", ci = ci_lab, none = "")
    e <- if (x$meta[[d]]$exponentiate) est_symbol(x$meta[[d]]) else if (only) "Diff." else "b"
    if (spec$layout == "inline" && spec$uncertainty != "none") {
      e <- paste(e, if (spec$uncertainty == "se") sprintf("(%s)", unc_lab) else sprintf("[%s]", ci_lab))
    }
    c(est = e, unc = unc_lab, es = "d", p = if (bayes) "pd" else "p")[spec$cols]
  }), use.names = FALSE)
  mtype <- vapply(dvs, function(d) .type_labels[[x$meta[[d]]$type]], "")

  list(
    rows = rows,
    spec = spec,
    per = per,
    dv_labels = unname(x$settings$dv_labels[dvs]),
    model_types = if (show_model_type) unname(mtype) else NULL,
    sub_head = sub_head,
    notes = c(table_notes(x, spec, note),
              if (any(is.na(est$estimate) & est$row_type %in% c("term", "level") & !est$is_ref))
                "Empty cells: coefficient not estimable in that model (collinear or no observations).")
  )
}

table_notes <- function(x, spec, note = NULL) {
  dvs <- x$settings$dvs
  meta <- x$meta
  lab <- x$settings$dv_labels
  only <- spec$contrasts == "only"
  show_contrasts <- spec$contrasts != "exclude" && any(x$estimates$section == "contrast")
  out <- character()

  syms <- vapply(meta, est_symbol, "")
  types <- vapply(meta, `[[`, "", "type")
  bayes <- vapply(meta, function(m) isTRUE(m$bayes), logical(1))
  any_lin <- any(syms == "b")
  any_or <- any(syms != "b")
  or_logit <- any(syms == "OR" & !is_ordinal(types))
  or_ord <- any(syms == "OR" & is_ordinal(types))
  irr <- any(syms == "IRR")
  abbr <- c(
    if (any_lin) {
      if (only) "Diff. = difference in estimated marginal means" else "b = unstandardized coefficient"
    },
    if (or_logit) if (only) "OR = ratio of odds" else "OR = odds ratio",
    if (or_ord) "OR (ordinal models) = cumulative odds ratio (proportional odds)",
    if (irr) "IRR = incidence rate ratio (ratio of means)",
    if (spec$uncertainty == "se") {
      c(if (any(!bayes)) "SE = standard error", if (any(bayes)) "MAD = posterior median absolute deviation")
    },
    if (spec$uncertainty == "ci") {
      c(if (any(!bayes)) "CI = confidence interval", if (any(bayes)) "CrI = credible interval")
    },
    if (any(bayes) && spec$p != "none") "pd = probability of direction (share of the posterior with the estimate's sign)"
  )
  out <- c(out, paste0(paste(abbr, collapse = "; "), "."))

  se_txt <- vapply(meta, `[[`, character(1), "se_text")[!bayes]
  what <- c(if (spec$uncertainty == "se") "SEs", if (spec$uncertainty == "ci") "CIs",
            if (spec$p != "none") "p-values")
  what <- if (length(what)) paste(what, collapse = " and ") else "Inference"
  if (length(se_txt) && length(unique(se_txt)) == 1) {
    out <- c(out, sprintf("%s use %s%s.", what, se_txt[1],
                          if (any(bayes)) sprintf(" (%s)", paste(lab[names(se_txt)], collapse = ", ")) else ""))
  } else if (length(se_txt)) {
    parts <- vapply(unique(se_txt), function(t) {
      sprintf("%s (%s)", t, paste(lab[names(se_txt)[se_txt == t]], collapse = ", "))
    }, character(1))
    out <- c(out, sprintf("%s use %s.", what, paste(parts, collapse = "; ")))
  }
  if (any(bayes)) {
    out <- c(out, sprintf(
      "%s report%s posterior medians with %g%% equal-tailed credible intervals (brms) and are not adjusted for multiple comparisons.",
      if (all(bayes)) "All models" else sprintf("Bayesian models (%s)", paste(lab[names(meta)[bayes]], collapse = ", ")),
      "", 100 * x$settings$conf_level))
  }
  fb <- vapply(meta, `[[`, logical(1), "robust_fallback")
  if (any(fb)) {
    fb_types <- unique(tolower(.type_labels[types[fb]]))
    out <- c(out, sprintf(
      "Robust SEs are not available for %s models, so %s model-based Wald SEs.",
      paste(fb_types, collapse = ", "), if (sum(fb) == 1) "that model uses" else "those models use"))
  }

  if (spec$layout != "columns" && spec$uncertainty != "none") {
    out <- c(out, if (spec$uncertainty == "se") {
      if (any_or) "Standard errors in parentheses (delta-method SEs for ratios)."
      else "Standard errors in parentheses."
    } else sprintf("%g%% CIs in brackets.", 100 * x$settings$conf_level))
  } else if (spec$uncertainty == "se" && any_or) {
    out <- c(out, "SEs for ratios are delta-method SEs.")
  }
  if (!only && any(x$estimates$is_ref, na.rm = TRUE)) {
    out <- c(out, "(ref.) marks the reference category for each factor.")
  }
  if (show_contrasts) {
    adj <- x$settings$contrast_adjust
    ci_adj <- if (adj %in% c("tukey", "scheffe", "sidak", "bonferroni", "mvt")) adj else "bonferroni"
    adj_txt <- if (adj == "none") "are unadjusted" else if (ci_adj == adj) {
      sprintf("and CIs are %s-adjusted within each factor", adjust_name(adj))
    } else {
      sprintf("are %s-adjusted and their CIs %s-adjusted within each factor",
              adjust_name(adj), adjust_name(ci_adj))
    }
    or_txt <- if (any_or) " (ratios for logistic, ordinal, and count models)" else ""
    lead <- if (only) "Contrasts" else "Pairwise contrasts"
    by <- unique(unlist(lapply(meta, `[[`, "contrast_by")))
    if (length(by)) {
      adj_txt <- sub("within each factor", "within each factor and moderator level", adj_txt)
      lead <- paste(lead, sprintf("within each level of %s",
                                  paste(vapply(by, function(b) lc_first(clean_label(b)), ""),
                                        collapse = ", ")))
    }
    out <- c(out, if (all(bayes)) {
      sprintf("%s compare estimated marginal means averaged over other predictors%s.", lead, or_txt)
    } else sprintf(
      "%s compare estimated marginal means averaged over other predictors%s; their p-values %s.",
      lead, or_txt, adj_txt))
  }
  if (x$settings$dv_adjust != "none" && length(dvs) > 1) {
    terms <- x$settings$dv_adjust_terms
    which_txt <- if (is.null(terms)) "All non-intercept p-values and CIs" else
      sprintf("p-values and CIs for %s", paste(x$settings$var_labels[terms], collapse = ", "))
    out <- c(out, sprintf("%s are additionally %s-adjusted across the %d outcomes.",
                          which_txt, adjust_name(x$settings$dv_adjust), length(dvs)))
  }
  if (spec$effect_size && any(!is.na(x$estimates$es))) {
    out <- c(out, paste("d = standardized mean difference: the difference divided by the pooled",
                        "within-group SD of the outcome (linear models) or the total SD of one",
                        "observation, residual plus random-effect variance (linear mixed models)."))
  }
  if (spec$stars) out <- c(out, "* p < .05. ** p < .01. *** p < .001.")
  c(paste("Note.", paste(out, collapse = " ")), note)
}

grid_matrix <- function(g) {
  lab <- vapply(g$rows, `[[`, "", "label")
  cells <- do.call(rbind, lapply(g$rows, `[[`, "cells"))
  cbind(lab, cells)
}
