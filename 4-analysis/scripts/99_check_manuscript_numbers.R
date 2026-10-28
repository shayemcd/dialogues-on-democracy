# 99_check_manuscript_numbers.R
# Recomputes the numbers quoted in the manuscript text (Method, Results, Discussion) and the supplement prose from
# data/clean_dfs.RData and the saved results, and compares them with the values typed into the text. The manuscript has no
# live R code, so this script is the link between the text and the analysis: re-run it after editing either one.
#
# Each row of the output has: id, where the number appears, the value in the text ("manuscript"), the recomputed value,
# and OK / MISMATCH. Tolerances match the number of decimals reported. Writes results/manuscript_number_check.csv.
#
# Values are typed in from the current manuscript text. A MISMATCH means the text and the data disagree; the "note" column
# says what the recomputation measures where that is not obvious.

suppressMessages({library(dplyr); library(tidyr); library(stringr); library(readxl); library(sandwich); library(lmtest); library(emmeans); library(pwr)})
set.seed(42)
local({
  if (!file.exists(".here") && !file.exists("../.here")) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
  here::i_am("scripts/99_check_manuscript_numbers.R")
})
root <- here::here()
e <- new.env(); load(file.path(root, "data/clean_dfs.RData"), envir = e)
dfc  <- ungroup(e$df_consp); dfc2 <- ungroup(e$df_consp_2); dfb <- ungroup(e$df_both); dfa <- ungroup(e$df_all)
to_chr <- function(d) { d$ConditionFeedback <- factor(as.character(d$ConditionFeedback)); d$ConditionChat <- factor(as.character(d$ConditionChat)); d }
dfc <- to_chr(dfc); dfc2 <- to_chr(dfc2); dfb <- to_chr(dfb); dfa <- to_chr(dfa)
dfc$ConditionFeedback  <- relevel(dfc$ConditionFeedback,  "Adversarial Dialogue"); dfc$ConditionChat <- relevel(dfc$ConditionChat, "Chat")
dfc2$ConditionFeedback <- relevel(dfc2$ConditionFeedback, "Control")

L <- list()
add <- function(id, where, claim, got, tol = 0.005, note = "") {
  if (length(got) != 1 || (is.numeric(got) && is.na(got))) { L[[length(L) + 1]] <<- data.frame(id = id, where = where, manuscript = as.character(claim), recomputed = "NA/empty", status = "MISMATCH", note = paste("could not compute;", note), stringsAsFactors = FALSE); return(invisible()) }
  claim_num <- suppressWarnings(as.numeric(claim)); got_num <- suppressWarnings(as.numeric(got))
  ok <- if (!is.na(claim_num) && !is.na(got_num)) abs(claim_num - got_num) <= tol + 1e-9 else identical(as.character(claim), as.character(got))
  L[[length(L) + 1]] <<- data.frame(id = id, where = where, manuscript = as.character(claim), recomputed = if (is.numeric(got)) format(round(got, 4), nsmall = 0) else as.character(got),
                                    status = if (ok) "OK" else "MISMATCH", note = note, stringsAsFactors = FALSE)
}
pfix <- function(p) ifelse(p < .001, "<.001", sub("^0", "", formatC(p, format = "f", digits = 3)))
addp <- function(id, where, claim_p, got_p, note = "") {   # claim_p: numeric p or "<.001"
  if (length(got_p) != 1 || is.na(got_p)) { L[[length(L) + 1]] <<- data.frame(id = id, where = where, manuscript = as.character(claim_p), recomputed = "NA/empty", status = "MISMATCH", note = paste("could not compute;", note), stringsAsFactors = FALSE); return(invisible()) }
  if (is.character(claim_p)) { ok <- got_p < .001 } else ok <- abs(round(got_p, 3) - claim_p) < 1e-9 || abs(got_p - claim_p) <= 0.0005 + 1e-9
  L[[length(L) + 1]] <<- data.frame(id = id, where = where, manuscript = as.character(claim_p), recomputed = pfix(got_p), status = if (ok) "OK" else "MISMATCH", note = note, stringsAsFactors = FALSE)
}

# ---------- helpers mirroring 05_results_effects.Rmd ----------
hc1 <- function(m) { ct <- coeftest(m, vcov. = vcovHC(m, "HC1")); data.frame(term = rownames(ct), est = ct[,1], se = ct[,2], t = ct[,3], p = ct[,4], row.names = NULL) }
fit <- function(dv, pre, d, study, extra = "") hc1(lm(as.formula(paste(dv, "~", pre, "+ ConditionFeedback", if (study == 1) "+ ConditionChat", extra)), data = d))
perc <- function(dv, d, study) { dd <- d[is.finite(d[[dv]]) & !is.na(d[[dv]]), ]; hc1(lm(as.formula(paste(dv, "~ ConditionFeedback", if (study == 1) "+ ConditionChat")), data = dd)) }
cmicalc <- function(df) { j <- 1 - 3 / (4 * df - 1); j }
dppc2 <- function(d, pre, post) {   # mirrors calculate_dppc2(): row order = group order in summarise (factor levels)
  g <- d %>% group_by(ConditionFeedback) %>% summarise(pre_m = mean(.data[[pre]], na.rm=TRUE), pre_sd = sd(.data[[pre]], na.rm=TRUE), post_m = mean(.data[[post]], na.rm=TRUE), n = n(), .groups = "drop")
  C <- g[1, ]; T <- g[2, ]
  sdp <- sqrt(((T$n - 1) * T$pre_sd^2 + (C$n - 1) * C$pre_sd^2) / (T$n + C$n - 2))
  metafor:::.cmicalc(T$n + C$n - 2) * ((T$post_m - T$pre_m) - (C$post_m - C$pre_m)) / sdp
}
gt <- function(h, term) h[h$term == paste0("ConditionFeedback", term), ]
emm_p <- function(dv, pre, d, study, a, b) {
  m <- lm(as.formula(paste(dv, "~ ConditionFeedback", if (study == 1) "+ ConditionChat", "+", pre)), data = d)
  em <- emmeans(m, pairwise ~ ConditionFeedback, vcov. = vcovHC(m, "HC1")); s <- as.data.frame(em$contrasts)
  s$contrast <- gsub("[()]", "", s$contrast)
  s[s$contrast %in% c(paste(a, "-", b), paste(b, "-", a)), "p.value"]
}

# ================= METHODS: sample =================
n_all <- nrow(dfb); add("M1", "Abstract/Method: N total", 1848, n_all, 0)
add("M2", "Participants: S1 n", 1215, nrow(dfc), 0); add("M3", "Participants: S2 n", 633, nrow(dfc2), 0)
s1fb <- table(dfc$ConditionFeedback); s2fb <- table(dfc2$ConditionFeedback)
add("M4a", "Participants S1: smallest feedback-condition n ('396 to 419')", 396, min(s1fb), 0)
add("M4b", "Participants S1: largest feedback-condition n", 419, max(s1fb), 0)
cells1 <- table(dfc$ConditionFeedback, dfc$ConditionChat)
add("M4c", "Participants S1: smallest message x interface cell n ('183 to 218')", 183, min(cells1), 0, note = paste("cell sizes:", paste(cells1, collapse = "/")))
add("M4d", "Participants S1: largest message x interface cell n", 218, max(cells1), 0)
add("M5a", "Participants S2: smallest condition n ('191 to 223')", 191, min(s2fb), 0); add("M5b", "Participants S2: largest condition n", 223, max(s2fb), 0)
dem <- function(d, study) {
  n <- nrow(d); g <- as.character(d$GenderClean); r <- as.character(d$Race)
  list(fem = sum(g == "Female", na.rm = TRUE), mal = sum(g == "Male", na.rm = TRUE), n = n,
       fem_pct = 100 * mean(g == "Female", na.rm = TRUE), mal_pct = 100 * mean(g == "Male", na.rm = TRUE),
       age_m = mean(as.numeric(d$Age), na.rm = TRUE), age_sd = sd(as.numeric(d$Age), na.rm = TRUE), age_min = min(as.numeric(d$Age), na.rm=TRUE), age_max = max(as.numeric(d$Age), na.rm=TRUE),
       white = 100 * mean(r == "White", na.rm = TRUE), black = 100 * mean(r == "Black" | r == "Black or African American", na.rm = TRUE), asian = 100 * mean(r == "Asian", na.rm = TRUE),
       rep_m = mean(as.numeric(d$DemRep_C), na.rm = TRUE), rep_sd = sd(as.numeric(d$DemRep_C), na.rm = TRUE))
}
d1 <- dem(dfc, 1); d2 <- dem(dfc2, 2)
cat("Race levels:", paste(unique(as.character(dfc$Race)), collapse = " | "), "\n"); cat("Gender levels:", paste(unique(as.character(dfc$GenderClean)), collapse = " | "), "\n")
add("D1a", "S1 females n", 715, d1$fem, 0); add("D1b", "S1 females %", 58.85, d1$fem_pct); add("D1c", "S1 males n", 479, d1$mal, 0); add("D1d", "S1 males %", 39.42, d1$mal_pct)
add("D1e", "S1 age mean", 55.77, d1$age_m); add("D1f", "S1 age SD", 17.29, d1$age_sd); add("D1g", "S1 age min", 18, d1$age_min, 0); add("D1h", "S1 age max", 98, d1$age_max, 0)
add("D1i", "S1 White %", 92.14, d1$white); add("D1j", "S1 Black %", 3.16, d1$black); add("D1k", "S1 Asian %", 2.13, d1$asian)
add("D1l", "S1 Republican mean", 5.21, d1$rep_m); add("D1m", "S1 Republican SD", 0.75, d1$rep_sd)
add("D2a", "S2 females n", 346, d2$fem, 0); add("D2b", "S2 females %", 54.66, d2$fem_pct); add("D2c", "S2 males n", 275, d2$mal, 0); add("D2d", "S2 males %", 43.44, d2$mal_pct)
add("D2e", "S2 age mean", 54.14, d2$age_m); add("D2f", "S2 age SD", 18.64, d2$age_sd); add("D2g", "S2 age min", 18, d2$age_min, 0); add("D2h", "S2 age max", 93, d2$age_max, 0)
add("D2i", "S2 White %", 90.73, d2$white); add("D2j", "S2 Black %", 5.53, d2$black); add("D2k", "S2 Asian %", 1.46, d2$asian)
add("D2l", "S2 Republican mean", 5.19, d2$rep_m); add("D2m", "S2 Republican SD", 0.72, d2$rep_sd)

# randomization / attrition
fb <- table(dfb$ConditionFeedback)
add("A1", "3 withdrew before randomization", 3, sum(is.na(dfb$ConditionFeedback)), 0)
add("A2", "randomized N", 1845, sum(!is.na(dfb$ConditionFeedback)), 0)
add("A3a", "Adversarial n", 590, fb[["Adversarial Dialogue"]], 0); add("A3b", "Belief-Tailored n", 619, fb[["Belief-Tailored"]], 0)
add("A3c", "Belief & Values n", 419, fb[["Belief & Values-Tailored"]], 0); add("A3d", "Control n", 217, fb[["Control"]], 0)
add("A4", "Method: participants whose open-ended response was NLP-coded (4,230)", 4230, sum(!is.na(dfa$Conspiracy)), 0, note = paste("rows in df_all:", nrow(dfa)))
add("A5", "Method: other survey records, screened out or ended before the question (4,751)", 4751, sum(is.na(dfa$Conspiracy)), 0)

# attrition: analytic sample (df_both) participants randomized to a condition, as in Table A3 (03a_method_descriptives.R)
da <- dfb %>% filter(!is.na(ConditionChat), !is.na(ConditionFeedback))
at <- da %>% group_by(Study, ConditionChat, ConditionFeedback) %>% summarise(n = n(), k = sum(AttritDurExp == 1, na.rm = TRUE), pct = 100 * mean(AttritDurExp == 1, na.rm = TRUE), .groups = "drop")
s1at <- at %>% filter(Study == 1); s2at <- at %>% filter(Study == 2)
add("AT1a", "S1 attrition min % (Qualtrics + Adversarial)", 3.28, min(s1at$pct)); add("AT1b", "S1 attrition min n", 6, s1at$k[which.min(s1at$pct)], 0)
add("AT1c", "S1 attrition max % (Chat + Belief-Tailored)", 14.81, max(s1at$pct)); add("AT1d", "S1 attrition max n", 28, s1at$k[which.max(s1at$pct)], 0)
add("AT2a", "S2 attrition Adversarial %", 12.57, s2at$pct[s2at$ConditionFeedback == "Adversarial Dialogue"]); add("AT2b", "S2 Adv n dropped", 24, s2at$k[s2at$ConditionFeedback == "Adversarial Dialogue"], 0)
add("AT2c", "S2 attrition Belief-Tailored %", 13.45, s2at$pct[s2at$ConditionFeedback == "Belief-Tailored"]); add("AT2d", "S2 BT n dropped", 30, s2at$k[s2at$ConditionFeedback == "Belief-Tailored"], 0)
add("AT2e", "S2 attrition Control %", 14.75, s2at$pct[s2at$ConditionFeedback == "Control"]); add("AT2f", "S2 Control n dropped", 32, s2at$k[s2at$ConditionFeedback == "Control"], 0)
da1 <- da %>% filter(Study == 1) %>% droplevels(); da2 <- da %>% filter(Study == 2) %>% droplevels()
da1$cell <- interaction(da1$ConditionChat, da1$ConditionFeedback)   # 3 message x 2 interface cells
cs <- function(x, y) suppressWarnings(chisq.test(table(x, y)))
c_all <- cs(da1$AttritDurExp, da1$cell); c_int <- cs(da1$AttritDurExp, da1$ConditionChat); c_fb <- cs(da1$AttritDurExp, da1$ConditionFeedback); c_s2 <- cs(da2$AttritDurExp, da2$ConditionFeedback)
add("AT3a", "S1 attrition chi2(5)", 23.25, unname(c_all$statistic)); add("AT3b", "S1 attrition df", 5, unname(c_all$parameter), 0)
add("AT3c", "S1 interface chi2(1)", 8.26, unname(c_int$statistic)); addp("AT3c2", "S1 interface p", .004, c_int$p.value); add("AT3d", "S1 feedback chi2(2)", 10.71, unname(c_fb$statistic)); addp("AT3e", "S1 feedback p", .005, c_fb$p.value); addp("AT3a2", "S1 omnibus p", "<.001", c_all$p.value)
add("AT3f", "S2 chi2(2)", 0.42, unname(c_s2$statistic)); addp("AT3g", "S2 p", .811, c_s2$p.value)

# speeders
sp <- function(d) mean(d$`Duration (in seconds)` <= 600, na.rm = TRUE) * 100
da1$Speeder <- as.integer(da1$`Duration (in seconds)` <= 600); da2$Speeder <- as.integer(da2$`Duration (in seconds)` <= 600)
sps <- c(cs(da1$Speeder, da1$cell)$p.value, cs(da1$Speeder, da1$ConditionChat)$p.value, cs(da1$Speeder, da1$ConditionFeedback)$p.value, cs(da2$Speeder, da2$ConditionFeedback)$p.value)
add("SP1", "speeder x condition chi-square p's all > .13 (min p)", 0.13, min(sps), 0.0005, note = paste("p's:", paste(round(sps, 3), collapse = ", "), "| claim holds if min >= .13"))
L[[length(L)]]$status <- if (min(sps) > .13) "OK" else "MISMATCH"

# ================= RESULTS =================
dvs <- list(den = c("ER0Norm_Post", "ER0Norm_Pre_center", "ER0Norm_Post_std", "ER0_PercChange", "ER0Norm_Pre_center"),
            cc  = c("Sureness_Post_1", "Sureness_Pre_center", "Sureness_Post_1_std", "Sureness_PercChange", "Sureness_Pre_center"))
rep_effect <- function(tag, key, d, study, term, claim) {
  v <- dvs[[key]]; h <- gt(fit(v[1], v[2], d, study), term); hs <- gt(fit(v[3], v[2], d, study), term); hp <- gt(perc(v[4], d, study), term)
  dd <- if (study == 1) { ref <- "Adversarial Dialogue" } else { ref <- "Control" }
  sub <- d %>% filter(ConditionFeedback %in% c(ref, term)); sub$ConditionFeedback <- factor(as.character(sub$ConditionFeedback), levels = c(ref, term))
  dv <- dppc2(sub, v[2], v[1])
  add(paste0(tag, "_b"), paste(tag, "b"), claim$b, h$est); add(paste0(tag, "_beta"), paste(tag, "beta"), claim$beta, hs$est)
  addp(paste0(tag, "_p"), paste(tag, "p"), claim$p, h$p)
  add(paste0(tag, "_d"), paste(tag, "d"), claim$d, dv); add(paste0(tag, "_pct"), paste(tag, "delta%"), claim$pct, 100 * hp$est)
}
# Study 1 (ref = Adversarial Dialogue)
rep_effect("S1_BT_den", "den", dfc, 1, "Belief-Tailored",          list(b = -3.76, beta = -.17, p = .002, d = -.21, pct = -7.66))
rep_effect("S1_BV_den", "den", dfc, 1, "Belief & Values-Tailored",  list(b = -2.45, beta = -.11, p = .035, d = -.16, pct = -4.89))
rep_effect("S1_BT_cc",  "cc",  dfc, 1, "Belief-Tailored",          list(b = -3.79, beta = -.20, p = .004, d = -.28, pct = -4.56))
h <- gt(fit("Sureness_Post_1", "Sureness_Pre_center", dfc, 1), "Belief & Values-Tailored"); addp("S1_BV_cc_p", "S1 B&V claim certainty p", .064, h$p)
# EMM BT vs B&V
addp("S1_emm_den", "S1 BT vs B&V (EMM) 2020", .560, emm_p("ER0Norm_Post", "ER0Norm_Pre_center", dfc, 1, "Belief-Tailored", "Belief & Values-Tailored"))
addp("S1_emm_cc", "S1 BT vs B&V (EMM) claim", .553, emm_p("Sureness_Post_1", "Sureness_Pre_center", dfc, 1, "Belief-Tailored", "Belief & Values-Tailored"))
# Study 2 (ref = Control)
rep_effect("S2_BT_den", "den", dfc2, 2, "Belief-Tailored", list(b = -3.33, beta = -.15, p = .015, d = -.18, pct = -4.70))
rep_effect("S2_BT_cc",  "cc",  dfc2, 2, "Belief-Tailored", list(b = -3.95, beta = -.21, p = .003, d = -.19, pct = -3.49))
addp("S2_adv_den_p", "S2 adversarial 2020 p", .081, gt(fit("ER0Norm_Post", "ER0Norm_Pre_center", dfc2, 2), "Adversarial Dialogue")$p)
addp("S2_adv_cc_p", "S2 adversarial claim p", .644, gt(fit("Sureness_Post_1", "Sureness_Pre_center", dfc2, 2), "Adversarial Dialogue")$p)
addp("S2_emm_den", "S2 BT vs Adversarial (EMM) 2020", .821, emm_p("ER0Norm_Post", "ER0Norm_Pre_center", dfc2, 2, "Belief-Tailored", "Adversarial Dialogue"))

# other outcomes: 'Non-significant p's ranged from .066 to .993'
others <- c("ER1_Post","ER2_Post","ER2020_Post","ER2024_Post","ER_Post","AffPol_Post"); pres <- c("ER1_Pre_center","ER2_Pre_center","ER2020_Pre_center","ER2024_Pre_center","ERNorm_Pre_center","AffPol_Pre_center")
ps <- c()
for (i in seq_along(others)) for (st in 1:2) { d <- if (st == 1) dfc else dfc2; h <- fit(others[i], pres[i], d, st); h <- h[grepl("^ConditionFeedback", h$term), ]; ps <- c(ps, setNames(h$p, paste0("S", st, "_", others[i], "_", sub("ConditionFeedback", "", h$term)))) }
nonsig <- ps[ps >= .05]; add("O1", "non-sig p range lower bound (.066)", .066, min(nonsig), 0.0005); add("O2", "non-sig p range upper bound (.993)", .993, max(nonsig), 0.0005)

# ---- any decrease / die-hard shares ----
add("DH1", "die-hard share (CCDieHard==1) %", 33.06, 100 * mean(dfb$CCDieHard == 1, na.rm = TRUE))
add("AC1", "any decrease in >=1 outcome (%)", 74.5, 100 * mean(dfb$AnyChange == 1, na.rm = TRUE), 0.05, note = "AnyChange coding in 01_cleaning; check definition below")
ac <- dfb %>% filter(!is.na(ConditionFeedback)) %>% group_by(ConditionFeedback) %>% summarise(pct = 100 * mean(AnyChange == 1, na.rm = TRUE), .groups = "drop")
add("AC2", "any change, Belief-Tailored %", 77.15, ac$pct[ac$ConditionFeedback == "Belief-Tailored"], 0.05); add("AC3", "any change, values-tailored %", 76.25, ac$pct[ac$ConditionFeedback == "Belief & Values-Tailored"], 0.05)
ct <- dfb %>% filter(!is.na(ConditionFeedback)) %>% group_by(ConditionFeedback) %>% summarise(pct_dec = 100 * mean(CCChangeType == "Decrease", na.rm = TRUE), .groups = "drop")
add("CD1", "claim certainty decrease: control %", 31, ct$pct_dec[ct$ConditionFeedback == "Control"], 0.5); add("CD2", "adversarial %", 34, ct$pct_dec[ct$ConditionFeedback == "Adversarial Dialogue"], 0.5)
add("CD3", "values-tailored %", 41, ct$pct_dec[ct$ConditionFeedback == "Belief & Values-Tailored"], 0.5); add("CD4", "belief-tailored %", 41, ct$pct_dec[ct$ConditionFeedback == "Belief-Tailored"], 0.5)
add("CD5", "'38% on average' decreased claim certainty", 38, 100 * mean(dfb$CCChangeType == "Decrease", na.rm = TRUE), 0.5)
dhc <- dfb %>% filter(CCDieHard == 1); dhd <- dfb %>% filter(Den2020DieHard == 1)
add("NC1", "2020-denialism die-hards with no change %", 70, 100 * mean(dhd$Den2020DieHardNoChange == 1, na.rm = TRUE), 0.5, note = "see var definitions")
add("NC2", "claim-certainty die-hards with no change %", 72, 100 * mean(dhc$CCDieHardNoChange == 1, na.rm = TRUE), 0.5)
add("NC3", "no-change die-hards 2020 as % of full sample", 15, 100 * mean(dfb$Den2020DieHardNoChange == 1, na.rm = TRUE), 0.5)
add("NC4", "no-change die-hards CC as % of full sample", 22, 100 * mean(dfb$CCDieHardNoChange == 1, na.rm = TRUE), 0.5)

# ---- die-hard models (levels, claim-certainty die-hards) ----
dh_fit <- function(dv, pre, d, study, term, dh) { dd <- d %>% filter(CCDieHard == dh); gt(fit(dv, pre, dd, study), term) }
dh_perc <- function(dv, d, study, term, dh) { dd <- d %>% filter(CCDieHard == dh); gt(perc(dv, dd, study), term) }
h <- dh_fit("ER0Norm_Post", "ER0Norm_Pre_center", dfc, 1, "Belief-Tailored", 1);  add("H1b", "S1 DH BT 2020 b", -4.16, h$est); addp("H1p", "S1 DH BT 2020 p", .045, h$p); add("H1pct", "S1 DH BT 2020 delta%", -7.0, 100 * dh_perc("ER0_PercChange", dfc, 1, "Belief-Tailored", 1)$est, 0.05)
h <- dh_fit("ER0Norm_Post", "ER0Norm_Pre_center", dfc, 1, "Belief & Values-Tailored", 1); add("H2b", "S1 DH B&V 2020 b", -3.47, h$est); addp("H2p", "S1 DH B&V 2020 p", .024, h$p); add("H2pct", "S1 DH B&V 2020 delta%", -5.6, 100 * dh_perc("ER0_PercChange", dfc, 1, "Belief & Values-Tailored", 1)$est, 0.05)
h <- dh_fit("Sureness_Post_1", "Sureness_Pre_center", dfc, 1, "Belief-Tailored", 1); add("H3b", "S1 DH BT claim b", -6.10, h$est); addp("H3p", "S1 DH BT claim p", .004, h$p)
h <- dh_fit("Sureness_Post_1", "Sureness_Pre_center", dfc, 1, "Belief & Values-Tailored", 1); add("H4b", "S1 DH B&V claim b", -4.51, h$est); addp("H4p", "S1 DH B&V claim p", .004, h$p)
h <- dh_fit("ER0Norm_Post", "ER0Norm_Pre_center", dfc2, 2, "Belief-Tailored", 1); add("H5b", "S2 DH BT 2020 b", -4.04, h$est); addp("H5p", "S2 DH BT 2020 p", .055, h$p); add("H5pct", "S2 DH BT 2020 delta%", -6.2, 100 * dh_perc("ER0_PercChange", dfc2, 2, "Belief-Tailored", 1)$est, 0.05)
h <- dh_fit("ER0Norm_Post", "ER0Norm_Pre_center", dfc, 1, "Belief-Tailored", 0);  add("H6b", "S1 NON-DH BT 2020 b", -3.37, h$est); addp("H6p", "S1 NON-DH BT 2020 p", .026, h$p); add("H6pct", "S1 NON-DH BT 2020 delta%", -8.1, 100 * dh_perc("ER0_PercChange", dfc, 1, "Belief-Tailored", 0)$est, 0.05)
# interaction tests condition x die-hard (32 = 8 outcomes x 2 studies x 2 conditions in S1 / 1-2 in S2)
dvs8 <- c("ER0Norm_Post","Sureness_Post_1","ER1_Post","ER2_Post","ER2020_Post","ER2024_Post","ER_Post","AffPol_Post"); pre8 <- c("ER0Norm_Pre_center","Sureness_Pre_center","ER1_Pre_center","ER2_Pre_center","ER2020_Pre_center","ER2024_Pre_center","ERNorm_Pre_center","AffPol_Pre_center")
ip <- c(); for (i in 1:8) for (st in 1:2) { d <- if (st == 1) dfc else dfc2; h <- fit(dvs8[i], pre8[i], d, st, extra = "") ; m <- lm(as.formula(paste(dvs8[i], "~", pre8[i], "+ ConditionFeedback * CCDieHard", if (st == 1) "+ ConditionChat")), data = d); hh <- hc1(m); hh <- hh[grepl(":CCDieHard", hh$term), ]; ip <- c(ip, setNames(hh$p, paste0("S", st, "_", dvs8[i], "_", hh$term))) }
# baseline interactions
bp <- c(); for (i in 1:8) for (st in 1:2) { d <- if (st == 1) dfc else dfc2; m <- lm(as.formula(paste(dvs8[i], "~ ConditionFeedback *", pre8[i], if (st == 1) "+ ConditionChat")), data = d); hh <- hc1(m); hh <- hh[grepl(paste0("^ConditionFeedback.*:", pre8[i], "$"), hh$term), ]; bp <- c(bp, setNames(hh$p, paste0("S", st, "_", dvs8[i], "_", hh$term))) }
add("IX2", "baseline-interaction tests count (text: 32 tests, 1 nominally sig)", 32, length(bp), 0, note = paste("n sig:", sum(bp<.05)))
m <- lm(ER0Norm_Post ~ ConditionFeedback * ER0Norm_Pre_center + ConditionChat, data = dfc); hh <- hc1(m); hh <- hh[hh$term == "ConditionFeedbackBelief-Tailored:ER0Norm_Pre_center", ]; add("IX3b", "S1 BT x baseline 2020 b", 0.15, hh$est); addp("IX3p", "S1 BT x baseline 2020 p", .021, hh$p)


# ================= Persuasion strategies and interactions (08_results_strategies.Rmd) =================
load(file.path(root, "data/df_strategy_coded.RData"), envir = e)
df_both_s <- ungroup(e$df_both); df_both_s$ConditionFeedback <- relevel(factor(as.character(df_both_s$ConditionFeedback)), ref = "Control")
strategy_items <- c("Rapport","Critical_Thinking","Alternative_Explanations","Credible_Sources","Harm","Stories_Examples","Encourage_Empathy","Socratic_Questioning","Conflicting_Evidence","Expert_Consensus","Common_Ground","Psychological_Needs","Inconsistencies_Fallacies","Analogies_Metaphors","Maintaining_Patience","Offer_Resources")
quality_items <- c("ArgumentQuality","Nuance","Specificity","Tone","EvidenceCount"); all_items <- c(strategy_items, quality_items)
cl <- list(Cognitive = c("Critical_Thinking","Alternative_Explanations","Credible_Sources","Conflicting_Evidence","Expert_Consensus","Inconsistencies_Fallacies"),
           Relational = c("Rapport","Encourage_Empathy","Common_Ground","Maintaining_Patience","Psychological_Needs"), Narrative = c("Stories_Examples","Analogies_Metaphors","Socratic_Questioning","Harm","Offer_Resources"))
sl <- e$df_strategy_long %>% rowwise() %>% mutate(Cognitive_r = mean(c_across(all_of(cl$Cognitive)), na.rm=TRUE), Relational_r = mean(c_across(all_of(cl$Relational)), na.rm=TRUE), Narrative_r = mean(c_across(all_of(cl$Narrative)), na.rm=TRUE)) %>% ungroup()
pa <- sl %>% group_by(ResponseId) %>% summarise(across(all_of(all_items), ~ mean(.x, na.rm=TRUE), .names = "{.col}_mean"), Cognitive_mean = mean(Cognitive_r, na.rm=TRUE), Relational_mean = mean(Relational_r, na.rm=TRUE), Narrative_mean = mean(Narrative_r, na.rm=TRUE), EvidenceCount_total = sum(EvidenceCount, na.rm=TRUE), .groups="drop")
d <- df_both_s %>% left_join(pa, by = "ResponseId") %>% ungroup()
strat <- readRDS(file.path(root, "results/strategy_quality_models.rds"))
strat_model_note <- "person-level models, HC1 SEs (08_results_strategies.Rmd)"
chk <- function(id, claim, got, tol = 0.005, where = "Results: persuasion strategies") add(id, where, claim, got, tol)
pchk <- function(id, claim, got, where = "Results: persuasion strategies") addp(id, where, if (claim <= .001) "<.001" else claim, got)
g <- function(res, strat_nm) res[res$Strategy == strat_nm, ]
# net-of-other-items models (all 21 constructs together), refit here from the data
net_fit <- function(dv, pre) {
  m <- lm(reformulate(c(pre, "ConditionFeedback", "Study", paste0(all_items, "_mean")), response = dv), data = d)
  ct <- coeftest(m, vcov. = vcovHC(m, "HC1")); rows <- paste0(all_items, "_mean")
  data.frame(Strategy = all_items, Estimate = ct[rows, 1], `Robust SE` = ct[rows, 2], `p value` = ct[rows, 4], check.names = FALSE)
}
r0 <- net_fit("ER0Norm_Post", "ER0Norm_Pre_center"); r1 <- net_fit("Sureness_Post_1", "Sureness_Pre_center")
# the saved table behind the supplement and Figure 5 must equal the refit
sv <- strat$net; sv0 <- sv[sv$Outcome == "2020 Denialism", ]; sv1 <- sv[sv$Outcome == "Claim Certainty", ]
add("S_saved1", "Supplement: saved strategy table matches refit (max |b| difference, 2020 Denialism)", 0, max(abs(sv0$estimate[match(r0$Strategy, sv0$construct)] - r0$Estimate)), 1e-6)
add("S_saved2", "Supplement: saved strategy table matches refit (max |b| difference, Claim Certainty)", 0, max(abs(sv1$estimate[match(r1$Strategy, sv1$construct)] - r1$Estimate)), 1e-6)
x <- g(r0, "Tone"); chk("T_er0_b", -4.10, x$Estimate); chk("T_er0_se", 1.07, x$`Robust SE`); chk("T_er0_t", -3.82, x$Estimate / x$`Robust SE`, 0.01); pchk("T_er0_p", .001, x$`p value`)
x <- g(r1, "Tone"); chk("T_cc_b", -4.36, x$Estimate); chk("T_cc_se", 1.12, x$`Robust SE`); chk("T_cc_t", -3.89, x$Estimate / x$`Robust SE`, 0.01); pchk("T_cc_p", .001, x$`p value`)
x <- g(r0, "ArgumentQuality"); chk("AQ_er0_b", -1.78, x$Estimate); chk("AQ_er0_se", 0.89, x$`Robust SE`); chk("AQ_er0_t", -2.01, x$Estimate / x$`Robust SE`, 0.01); pchk("AQ_er0_p", .044, x$`p value`)
x <- g(r1, "ArgumentQuality"); pchk("AQ_cc_p", .067, x$`p value`)
x <- g(r0, "Credible_Sources"); chk("CS_er0_b", -1.13, x$Estimate); chk("CS_er0_se", 0.52, x$`Robust SE`); chk("CS_er0_t", -2.17, x$Estimate / x$`Robust SE`, 0.01); pchk("CS_er0_p", .030, x$`p value`)
x <- g(r0, "Conflicting_Evidence"); chk("CE_er0_b", 1.65, x$Estimate); chk("CE_er0_se", 0.43, x$`Robust SE`); chk("CE_er0_t", 3.88, x$Estimate / x$`Robust SE`, 0.01); pchk("CE_er0_p", .001, x$`p value`)
x <- g(r1, "Conflicting_Evidence"); chk("CE_cc_b", 1.12, x$Estimate); pchk("CE_cc_p", .019, x$`p value`)
x <- g(r0, "Nuance"); chk("N_er0_b", 0.81, x$Estimate); pchk("N_er0_p", .005, x$`p value`)
x <- g(r1, "Nuance"); chk("N_cc_b", 0.91, x$Estimate); pchk("N_cc_p", .006, x$`p value`)
# AQ x Tone
m <- lm(ER0Norm_Post ~ ER0Norm_Pre_center + ConditionFeedback + Study + ArgumentQuality_mean * Tone_mean + Nuance_mean + Specificity_mean + EvidenceCount_total + Cognitive_mean + Relational_mean + Narrative_mean, data = d)
ct <- coeftest(m, vcov. = vcovHC(m, "HC1")); r <- ct["ArgumentQuality_mean:Tone_mean", ]
chk("AQxT_b", -1.00, r[1]); chk("AQxT_se", 0.29, r[2]); chk("AQxT_t", -3.51, r[3], 0.01); pchk("AQxT_p", .001, r[4])
tm <- mean(d$Tone_mean, na.rm=TRUE); ts <- sd(d$Tone_mean, na.rm=TRUE)
et <- emtrends(m, ~ Tone_mean, var = "ArgumentQuality_mean", at = list(Tone_mean = tm + c(-1,0,1)*ts), vcov. = vcovHC(m, "HC1")); es <- as.data.frame(summary(et, infer = c(TRUE, TRUE)))
chk("slope-1SD_b", -1.45, es$ArgumentQuality_mean.trend[1]); chk("slope-1SD_se", 0.84, es$SE[1]); pchk("slope-1SD_p", .083, es$p.value[1])
chk("slope+1SD_b", -3.19, es$ArgumentQuality_mean.trend[3]); chk("slope+1SD_se", 0.97, es$SE[3]); addp("slope+1SD_p", "Results: persuasion strategies", .001, es$p.value[3])
# joint F & by-condition AQ slopes
m_cond <- lm(ER0Norm_Post ~ ER0Norm_Pre_center + Study + ConditionFeedback * ArgumentQuality_mean + Tone_mean + Specificity_mean + EvidenceCount_total + Cognitive_mean + Relational_mean + Narrative_mean + Nuance_mean, data = d)
m_base <- lm(ER0Norm_Post ~ ER0Norm_Pre_center + ConditionFeedback + Study + ArgumentQuality_mean + Nuance_mean + Specificity_mean + Tone_mean + EvidenceCount_total + Cognitive_mean + Relational_mean + Narrative_mean, data = d)
m_full <- lm(ER0Norm_Post ~ ER0Norm_Pre_center + Study + ConditionFeedback * (ArgumentQuality_mean + Nuance_mean + Tone_mean + Specificity_mean) + EvidenceCount_total + Cognitive_mean + Relational_mean + Narrative_mean, data = d)
ci <- model.frame(m_full); names(ci)[1] <- "ER0Norm_Post"; mbm <- lm(formula(m_base), data = ci); jt <- anova(mbm, m_full)
chk("F", 1.60, jt$F[2], 0.01); chk("F_df1", 12, jt$Df[2], 0); chk("F_df2", 1613, jt$Res.Df[2], 0); pchk("F_p", .084, jt$`Pr(>F)`[2])
sc <- as.data.frame(summary(emtrends(m_cond, ~ ConditionFeedback, var = "ArgumentQuality_mean", vcov. = vcovHC(m_cond, "HC1")), infer = c(TRUE, TRUE)))
v <- function(cn) sc[sc$ConditionFeedback == cn, ]
chk("AQ_BT_b", -3.44, v("Belief-Tailored")$ArgumentQuality_mean.trend); chk("AQ_BT_se", 1.06, v("Belief-Tailored")$SE); addp("AQ_BT_p", "Results: persuasion strategies", .001, v("Belief-Tailored")$p.value)
chk("AQ_Adv_b", -1.90, v("Adversarial Dialogue")$ArgumentQuality_mean.trend); chk("AQ_Adv_se", 0.96, v("Adversarial Dialogue")$SE); pchk("AQ_Adv_p", .047, v("Adversarial Dialogue")$p.value)
chk("AQ_BV_b", -1.64, v("Belief & Values-Tailored")$ArgumentQuality_mean.trend); chk("AQ_BV_se", 1.75, v("Belief & Values-Tailored")$SE); pchk("AQ_BV_p", .349, v("Belief & Values-Tailored")$p.value)

# ================= Additional checks =================
# Sample definition: the analytic sample also requires baseline Claim Certainty >= 50 (Drop == "Keep" in 01_cleaning.Rmd); disclosed in the
# preregistration-deviations table (supplement) and the Analysis section (n = 35)
nocc <- dfa %>% filter(ScreenedDummy == 0, Conspiracy == "Conspiracy", Drop %in% c("Keep", "CC >= 50"))
add("X1", "Analysis/supplement: participants excluded for baseline Claim Certainty < 50", 35, sum(nocc$Drop == "CC >= 50"), 0, note = "disclosed in supplement tbl-prereg-deviations")
add("X1b", "Supplement: N retaining them", 1883, nrow(nocc), 0)
# messageType (derived in 01_cleaning.Rmd) must have no missing values among randomized participants
add("X2", "Data: messageType missing among randomized participants", 0, sum(is.na(dfb$messageType) & !is.na(dfb$ConditionFeedback)), 0)

# Power: minimum detectable effect at 80% power
n1 <- table(dfb$ConditionFeedback[dfb$Study == 1]); n2 <- table(dfb$ConditionFeedback[dfb$Study == 2])
mdd <- function(a, b) pwr.t2n.test(n1 = a, n2 = b, power = .8, sig.level = .05)$d
add("P1", "Participants: S1 detectable d at 80% power", 0.20, mdd(n1[["Adversarial Dialogue"]], n1[["Belief-Tailored"]]), 0.01)
add("P2", "Participants: S2 detectable d at 80% power", 0.28, mdd(n2[["Control"]], n2[["Adversarial Dialogue"]]), 0.01)

# Conversation length and words (Method)
dl <- dfb %>% pivot_longer(cols = starts_with("userResponse") | starts_with("GPTResponse"), names_to = c("Speaker", "Round"), names_pattern = "(user|GPT)Response_(\\d)", values_to = "ResponseText") %>% mutate(Round = as.numeric(Round)) %>% filter(Round < 5, !is.na(Round))
dl <- dl %>% filter(!is.na(ResponseText), ResponseText != "")   # non-empty turns, as in 03a_method_descriptives.R
dl$w <- str_count(dl$ResponseText, "\\w+")
cond_t <- left_join(dfb %>% filter(!is.na(ConditionFeedback)) %>% group_by(ConditionFeedback) %>% summarise(t = mean(ConvoTime, na.rm = TRUE), .groups = "drop"),
                    dl %>% group_by(ConditionFeedback) %>% summarise(w = mean(w, na.rm = TRUE), .groups = "drop"), by = "ConditionFeedback")
gt_ <- function(cn, col) cond_t[[col]][which(as.character(cond_t$ConditionFeedback) == cn)]
add("V1", "Method: mean conversation length (min), analytic sample", 7.65, mean(dfb$ConvoTime, na.rm = TRUE), 0.005, note = "per participant, df_both")
add("V2", "Method: GPT words per response", 195, mean(dl$w[dl$Speaker == "GPT"], na.rm = TRUE), 0.5); add("V3", "Method: participant words per response", 40.5, mean(dl$w[dl$Speaker == "user"], na.rm = TRUE), 0.05)
add("V4", "Method: cats & dogs minutes", 4.45, gt_("Control", "t")); add("V5", "Method: adversarial minutes", 5.31, gt_("Adversarial Dialogue", "t")); add("V6", "Method: belief & values minutes", 10.6, gt_("Belief & Values-Tailored", "t"), 0.05); add("V7", "Method: belief-tailored minutes", 9.05, gt_("Belief-Tailored", "t"))
add("V8", "Method: cats & dogs words (both speakers)", 71.5, gt_("Control", "w"), 0.05); add("V9", "Method: adversarial words", 59.6, gt_("Adversarial Dialogue", "w"), 0.05); add("V10", "Method: belief & values words", 155, gt_("Belief & Values-Tailored", "w"), 0.5); add("V11", "Method: belief-tailored words", 157, gt_("Belief-Tailored", "w"), 0.5)

# Themes (df_convosfin.xlsx: 1,655 GPT-coded conversations)
dc <- read_excel(file.path(root, "data/df_convosfin.xlsx"))
th <- prop.table(table(dc$ConspiracyTheme_Main)) * 100
add("T1", "Results: election integrity theme %", 40, th[["Election Integrity"]], 0.5); add("T2", "Results: voter process/legal manipulation %", 25, th[["Voter Process and Legal Manipulation"]], 0.5); add("T3", "Results: no specific claim %", 13, th[["No Specific Claim"]], 0.5)

# Evidence counts (df_strategy_coded.RData: turn-level coding summed across conversation rounds per participant)
# Option B: Evaluated turn-by-turn with strict factual claims/statistics/sources prompt.
# In Table 1 (@tbl-conditions), treatment condition counts are rounded to "5-6 pieces of evidence within the entire conversation on average"
# (Belief & Values: 4.96 rounds to 5; Belief-Tailored: 5.53 rounds to 6).
ev_strat <- d %>% filter(!is.na(ConditionFeedback), !is.na(EvidenceCount_total)) %>% group_by(ConditionFeedback) %>% 
  summarise(m = mean(EvidenceCount_total), z = 100 * mean(EvidenceCount_total == 0), .groups = "drop")
e_s_strat <- d %>% filter(ConditionFeedback == "Adversarial Dialogue", !is.na(EvidenceCount_total)) %>% group_by(Study) %>% 
  summarise(m = mean(EvidenceCount_total), z = 100 * mean(EvidenceCount_total == 0), .groups = "drop")

add("E1", "Method: adversarial conversations without evidence %", 87.32, ev_strat$z[ev_strat$ConditionFeedback == "Adversarial Dialogue"], 0.01)
add("E2", "Method: adversarial mean evidence", 0.16, ev_strat$m[ev_strat$ConditionFeedback == "Adversarial Dialogue"], 0.005)
add("E3", "Method: adversarial mean evidence, Study 1", 0.23, e_s_strat$m[e_s_strat$Study == 1], 0.005)
add("E4", "Method: adversarial mean evidence, Study 2", 0.01, e_s_strat$m[e_s_strat$Study == 2], 0.005)
add("E5", "Method: evidence in treatment conditions, low (rounded)", 5, round(ev_strat$m[ev_strat$ConditionFeedback == "Belief & Values-Tailored"]), 0.01)
add("E6", "Method: evidence in treatment conditions, high (rounded)", 6, round(ev_strat$m[ev_strat$ConditionFeedback == "Belief-Tailored"]), 0.01)

# Mediation (Discussion): indirect effect share of the total effect, belief-tailored
med <- readRDS(file.path(root, "results/mediation_params.rds"))
share <- function(f) { p <- med$params_2020[[f]]; p$est[p$label == "indirect"] / p$est[p$label == "total"] }
add("D1", "Discussion: indirect share, both studies pooled vs adversarial (40%)", 0.40, share("fit_det"), 0.005)
add("D2", "Discussion: indirect share, Study 1 vs adversarial (31%)", 0.31, share("fit_det.s1"), 0.005); add("D3", "Discussion: indirect share, Study 2 vs control (37%)", 0.37, share("fit_detcd.s2"), 0.005)

# Causal forests (supplement): omnibus calibration tests all non-significant
cf <- readRDS(file.path(root, "results/cf_heterogeneity_checks.rds"))
add("C1", "Supplement: smallest omnibus one-sided p", 0.548, min(cf$cal_diff_p_onesided), 0.0005)
# Discussion: AI trust is not a BLP-significant covariate in any of the eight forests (the text says AI use / AI familiarity rank highest in the SHAP figure, not AI trust)
add("C2", "Discussion: eight forests in which genai_trust is BLP-significant", 0, sum(grepl("genai_trust", cf$blp_sig)), 0)

# ---- supplement: baseline Claim Certainty exclusion (sensitivity) and Tukey-adjusted contrasts ----
m02 <- readRDS(file.path(root, "results/models_effects.rds"))
sens <- function(models, dv, term) { h <- hc1(models[[dv]]); h[h$term == paste0("ConditionFeedback", term), ] }
x <- sens(m02$nocc_s1, "ER0Norm_Post", "Belief-Tailored");       add("N1b", "Supplement: no-CC-exclusion S1 BT 2020 b", -3.34, x$est);  addp("N1p", "S1 BT 2020 p", .006, x$p)
x <- sens(m02$nocc_s1, "Sureness_Post_1", "Belief-Tailored");    add("N2b", "Supplement: no-CC-exclusion S1 BT claim b", -3.50, x$est); addp("N2p", "S1 BT claim p", .009, x$p)
x <- sens(m02$nocc_s2, "ER0Norm_Post", "Belief-Tailored");       add("N3b", "Supplement: no-CC-exclusion S2 BT 2020 b", -3.39, x$est);  addp("N3p", "S2 BT 2020 p", .013, x$p)
x <- sens(m02$nocc_s2, "Sureness_Post_1", "Belief-Tailored");    add("N4b", "Supplement: no-CC-exclusion S2 BT claim b", -3.44, x$est); addp("N4p", "S2 BT claim p", .011, x$p)
x <- sens(m02$nocc_s1, "ER0Norm_Post", "Belief & Values-Tailored"); add("N5b", "Supplement: no-CC-exclusion S1 B&V 2020 b", -1.87, x$est); addp("N5p", "Results/supplement: S1 B&V 2020 p without the exclusion (.111)", .111, x$p)
tk <- as.data.frame(m02$emm$s1); tk <- tk[tk$outcome == "ER0Norm_Post", ]
addp("TK1", "Results: Tukey-adjusted S1 B&V vs adversarial, 2020 (.089)", .089, tk$adj.p.value[grepl("^Adversarial Dialogue - \\(Belief & Values", tk$contrast)])
addp("TK2", "Results: Tukey-adjusted S1 BT vs adversarial, 2020 (.006)", .006, tk$adj.p.value[grepl("^Adversarial Dialogue - \\(Belief-Tailored", tk$contrast)])
tk2 <- as.data.frame(m02$emm$s1); tk2 <- tk2[tk2$outcome == "Sureness_Post_1", ]
addp("TK3", "Results: Tukey-adjusted S1 BT vs adversarial, claim certainty (.011)", .011, tk2$adj.p.value[grepl("^Adversarial Dialogue - \\(Belief-Tailored", tk2$contrast)])

res <- bind_rows(L)
dir.create(file.path(root, "results"), showWarnings = FALSE)
write.csv(res, file.path(root, "results/manuscript_number_check.csv"), row.names = FALSE)
cat(sprintf("\n%d checks: %d OK, %d MISMATCH\n", nrow(res), sum(res$status == "OK"), sum(res$status == "MISMATCH")))
if (any(res$status == "MISMATCH")) { cat("\nMISMATCHES:\n"); print(res[res$status == "MISMATCH", c("id", "where", "manuscript", "recomputed", "note")], row.names = FALSE, right = FALSE) }
