# Pairwise contrasts via emmeans ---------------------------------------------
#
# Contrasts compare estimated marginal means (averaged over other factors),
# use the same covariance matrix as the coefficient rows, and are adjusted
# within each factor by `contrast_adjust`. Ratio models (logistic, ordinal,
# count) are contrasted on the link scale and exponentiated. With `by`, the
# contrasts are simple effects within each level of the moderator.

pairwise_rows <- function(model, type, data, vars, V, se, adjust, conf_level,
                          exponentiate, lab_fun = make_labeller(), mf = NULL, by = NULL) {
  if (!length(vars)) return(NULL)
  mf <- mf %||% stats::model.frame(model)
  bayes <- is_bayes(type)
  out <- list()
  for (v in vars) {
    if (!v %in% names(mf) || is.null(factor_levels(mf[[v]]))) next
    by_v <- if (!is.null(by) && by != v && by %in% names(mf)) by else NULL
    em_args <- list(object = model, specs = v, data = data)
    if (!is.null(by_v)) em_args$by <- by_v
    if (!bayes && se == "robust" && robust_available(type)) em_args$vcov. <- as.matrix(V)
    if (type == "lmer") em_args$lmer.df <- "satterthwaite"
    em <- suppressMessages(do.call(emmeans::emmeans, em_args))
    pr <- emmeans::contrast(em, method = "revpairwise", adjust = if (bayes) "none" else adjust)
    s <- as.data.frame(summary(pr, infer = c(TRUE, TRUE), level = conf_level))
    # emmeans keeps the model's df even with vcov. = CR2; use the CR2
    # Satterthwaite df so contrasts match the coefficient rows (robust_df.R).
    if (!bayes && se == "robust" && uses_cr2(V)) {
      s <- cr2_summary(s, pr, model, V, conf_level, adjust = adjust)
    }
    if (bayes) {
      dp <- as.data.frame(bayestestR::describe_posterior(
        pr, centrality = "median", dispersion = TRUE, ci = conf_level, ci_method = "eti",
        test = "p_direction"))
      vals <- data.frame(estimate = dp$Median, std.error = dp$MAD, statistic = NA_real_,
                         df = NA_real_, p.value = NA_real_, conf.low = dp$CI_low,
                         conf.high = dp$CI_high, pd = dp$pd)
    } else {
      lcl <- grep("(lower\\.CL|LCL|asymp\\.LCL)$", names(s), value = TRUE)[1]
      ucl <- grep("(upper\\.CL|UCL|asymp\\.UCL)$", names(s), value = TRUE)[1]
      stat_col <- intersect(c("t.ratio", "z.ratio"), names(s))[1]
      vals <- data.frame(estimate = s$estimate, std.error = s$SE, statistic = s[[stat_col]],
                         df = s$df, p.value = s$p.value, conf.low = s[[lcl]],
                         conf.high = s[[ucl]], pd = NA_real_)
    }
    if (exponentiate) vals <- exponentiate_rows(vals)
    pairs <- match_pairs(as.character(s$contrast), as.character(em@levels[[v]]))
    raw <- paste(pairs$a, "vs.", pairs$b)
    lab <- paste(vapply(pairs$a, function(l) lab_fun$level(v, l), ""), "vs.",
                 vapply(pairs$b, function(l) lab_fun$level(v, l), ""))
    by_raw <- if (!is.null(by_v)) as.character(s[[by_v]]) else rep(NA_character_, nrow(s))
    vlab <- lab_fun$var(v)
    for (g in unique(by_raw)) {
      ix <- which(by_raw %in% g)
      tag <- if (is.na(g)) "" else paste0("|", by_v, "=", g)
      by_lab <- if (is.na(g)) NA_character_ else
        sprintf("%s: %s", lab_fun$var(by_v), lab_fun$level(by_v, g))
      header <- data.frame(
        key = paste0("contrast::", v, tag, "::header"), term = NA_character_, variable = v,
        level = NA_character_, label = if (is.na(g)) vlab else sprintf("%s (%s)", vlab, by_lab),
        row_type = "header", is_ref = FALSE,
        estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, df = NA_real_,
        p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_, pd = NA_real_,
        by_level = by_lab, stringsAsFactors = FALSE
      )
      body <- data.frame(
        key = paste0("contrast::", v, tag, "::", raw[ix]), term = raw[ix], variable = v,
        level = raw[ix], label = lab[ix], row_type = "level", is_ref = FALSE,
        vals[ix, , drop = FALSE], by_level = by_lab, stringsAsFactors = FALSE
      )
      out[[paste(v, g)]] <- rbind(header, body)
    }
  }
  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  res$section <- "contrast"
  rownames(res) <- NULL
  res
}

# Recover which two levels each emmeans contrast compares. emmeans writes
# "A - B", or "(A) - (B)" when a level has spaces or symbols, and "/" for ratios.
match_pairs <- function(contrast, levels) {
  wrap <- function(l) if (grepl("^[[:alnum:]_.]+$", l)) l else paste0("(", l, ")")
  grid <- expand.grid(a = levels, b = levels, stringsAsFactors = FALSE)
  grid <- grid[grid$a != grid$b, ]
  keys <- unlist(lapply(c(" - ", " / "), function(op) {
    stats::setNames(seq_len(nrow(grid)),
                    paste0(vapply(grid$a, wrap, ""), op, vapply(grid$b, wrap, "")))
  }))
  keys2 <- stats::setNames(keys, gsub("[()]", "", names(keys)))
  idx <- keys[contrast]
  miss <- is.na(idx)
  idx[miss] <- keys2[gsub("[()]", "", contrast[miss])]
  if (anyNA(idx)) {
    stop("Could not match contrast labels to factor levels: ",
         paste(contrast[is.na(idx)], collapse = ", "), call. = FALSE)
  }
  grid[idx, ]
}
