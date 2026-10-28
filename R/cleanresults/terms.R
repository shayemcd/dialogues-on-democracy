# Map coefficients to display rows -------------------------------------------
#
# Returns one row per displayed line of the fixed-effects block, including a
# header row and a reference-level row for every treatment-coded factor.

factor_levels <- function(x) {
  if (is.factor(x)) return(levels(x))
  if (is.logical(x)) return(c("FALSE", "TRUE"))
  if (is.character(x)) return(sort(unique(x)))
  NULL
}

term_map <- function(parts, lab_fun = make_labeller(), intercept = TRUE) {
  X <- parts$X
  assign <- attr(X, "assign")
  cn <- colnames(X)
  tt <- parts$terms
  labs <- attr(tt, "term.labels")
  mf <- parts$mf
  rows <- list()
  add <- function(...) rows[[length(rows) + 1]] <<- data.frame(..., stringsAsFactors = FALSE)

  # factor variables in the model, used to prettify interaction labels
  in_mf <- labs[labs %in% names(mf)]
  lgl_vars <- in_mf[vapply(in_mf, function(l) is.logical(mf[[l]]), logical(1))]
  fac_vars <- setdiff(in_mf[vapply(in_mf, function(l) !is.null(factor_levels(mf[[l]])), logical(1))],
                      lgl_vars)

  if (intercept && any(assign == 0)) {
    add(key = "(Intercept)", term = cn[assign == 0][1], variable = "(Intercept)",
        level = NA_character_, label = "Intercept", row_type = "term", is_ref = FALSE)
  }

  for (i in seq_along(labs)) {
    lab <- labs[i]
    cols <- cn[assign == i]
    levs <- if (lab %in% names(mf)) factor_levels(mf[[lab]]) else NULL
    # TRUE/FALSE predictors read better as one row ("Female") than as a factor
    if (lab %in% names(mf) && is.logical(mf[[lab]])) levs <- NULL
    treatment <- !is.null(levs) &&
      sum(!paste0(lab, levs) %in% cols) == 1 &&
      all(cols %in% paste0(lab, levs))

    if (treatment) {
      vlab <- lab_fun$var(lab)
      add(key = paste0(lab, "::header"), term = NA_character_, variable = lab,
          level = NA_character_, label = vlab, row_type = "header", is_ref = FALSE)
      for (lv in levs) {
        coef_name <- paste0(lab, lv)
        is_ref <- !coef_name %in% cols
        add(key = paste0(lab, "::", lv),
            term = if (is_ref) NA_character_ else coef_name,
            variable = lab, level = lv,
            label = if (is_ref) paste0(lab_fun$level(lab, lv), " (ref.)") else lab_fun$level(lab, lv),
            row_type = if (is_ref) "ref" else "level", is_ref = is_ref)
      }
    } else {
      for (cc in cols) {
        lbl <- if (length(cols) == 1 && !grepl(":", lab)) {
          lab_fun$var(lab)
        } else {
          interaction_label(cc, fac_vars, lab_fun, lgl_vars)
        }
        add(key = cc, term = cc, variable = lab, level = NA_character_,
            label = lbl, row_type = "term", is_ref = FALSE)
      }
    }
  }
  do.call(rbind, rows)
}

# "conditionLoss:age" -> "Loss x Age"
interaction_label <- function(coef_name, fac_vars, lab_fun, lgl_vars = character()) {
  parts <- strsplit(coef_name, ":", fixed = TRUE)[[1]]
  pretty <- vapply(parts, function(p) {
    if (p %in% paste0(lgl_vars, "TRUE")) return(lab_fun$var(sub("TRUE$", "", p)))
    hit <- fac_vars[startsWith(p, fac_vars)]
    if (length(hit)) {
      v <- hit[which.max(nchar(hit))]
      return(lab_fun$level(v, sub(v, "", p, fixed = TRUE)))
    }
    lab_fun$var(p)
  }, character(1))
  paste(pretty, collapse = " \u00d7 ")
}
