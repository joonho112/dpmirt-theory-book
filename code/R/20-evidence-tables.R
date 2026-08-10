## 20-evidence-tables.R — evidence tables for Parts VIII–IX (v3).
## Reads the companion volumes' FROZEN artifacts and the IRW papers' analysis
## frames; never refits their models. Every table carries its
## source in the caption (chapters add it) and this script asserts the frames
## against the published headline values so a stale or pre-correction snapshot
## fails the build instead of entering the book. Chapters read tables/T-*.rds
## and never compute — same rule as 07-tables.R.

source("code/R/00-paths.R")
dir.create(file.path(paths$tables, "supplement"), showWarnings = FALSE, recursive = TRUE)

save_tbl <- function(obj, id) {
  saveRDS(obj, file.path(paths$tables, paste0(id, ".rds")))
  write.csv(obj, file.path(paths$tables, "supplement", paste0(id, ".csv")),
            row.names = FALSE, na = "")
  cat(sprintf("  %-24s %d rows\n", id, nrow(obj)))
}

near <- function(x, target, tol) abs(x - target) <= tol

## ---- sources --------------------------------------------------------------
## Simulation: the authoritative v3 store, including the external-review
## correction to the H6 shape split. Case study: the SECOND EDITION is the cited volume, but
## its frozen analysis layer stays in the first edition's repository, which
## the second edition itself reads; so do we.
facts_path <- file.path(inputs$sim_book_v3, "data", "derived", "book-facts.rds")
sim_cond_path <- file.path(inputs$sim_book_v3, "data", "derived", "cond.rds")
sim_evidence_path <- file.path(inputs$sim_book_v3, "data", "derived", "evidence.rds")
seed_pairs_path <- file.path(inputs$case_study, "results", "verification",
                             "P3-selection-noise-pairs.csv")
stopifnot(file.exists(facts_path), file.exists(sim_cond_path),
          file.exists(sim_evidence_path), file.exists(seed_pairs_path))
F <- readRDS(facts_path)
sim_cond <- as.data.frame(readRDS(sim_cond_path))
sim_evidence <- as.data.frame(readRDS(sim_evidence_path))

cs_tbl <- function(f) {
  p <- file.path(inputs$case_study, "tables", f)
  stopifnot(file.exists(p))
  read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
}

irw_rel_path <- file.path(inputs$case_study, "data", "raw", "reliability_dataset.csv")
irw_rel_public_path <- file.path(IRW, "irw-reliability-replication",
                                 "data-derived", "reliability_dataset_public879.csv")
irw_shape_path <- file.path(IRW, "irw-normality-replication",
                            "data-derived", "nonbootstrap-dataset.csv")
stopifnot(file.exists(irw_rel_path), file.exists(irw_rel_public_path),
          file.exists(irw_shape_path))

fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")
fmt_p <- function(p) {
  ifelse(p < 1e-4, sprintf("%.1e", p), formatC(p, digits = 4, format = "f"))
}

## ---- T-evid-confirmatory : the preregistered primary family + control -----
## Read from the simulation volume's frozen fact store (data/derived/
## book-facts.rds), which its external review reproduced independently.
rows <- list(
  c("H1", "Primary contrast at the design centre (KS)", "H1"),
  c("H2", "Deepening per doubling of $N$ (KS)", "H2"),
  c("H3", "Rasch versus 2PL difference in differences (KS)", "H3"),
  c("H4", "Non-inferiority on individual accuracy (MSEL)", "H4"),
  c("H5", "Focused versus broad elicitation (KS)", "H5"),
  c("Control", "Calibration control on normal cells (KS)", "cal")
)
T_conf <- do.call(rbind, lapply(rows, function(r) {
  x <- F[[r[3]]]
  data.frame(
    Hypothesis = r[1],
    `What it estimates` = r[2],
    `Estimate (log ratio)` = fmt(x$estimate),
    `95% CI` = sprintf("[%s, %s]", fmt(x$ci_low), fmt(x$ci_high)),
    `Ratio scale` = fmt(exp(x$estimate), 2),
    `Holm $p$` = if (is.na(x$p_value_holm)) "—" else fmt_p(x$p_value_holm),
    Decision = if (x$decision == "not_evaluated") "control (not a test)" else x$decision,
    check.names = FALSE, stringsAsFactors = FALSE)
}))
stopifnot(near(F$H1$estimate, -0.221, 0.001), near(F$H4$estimate, -0.070, 0.001),
          near(F$H4$ni_upper_one_sided_95, -0.061, 0.001),
          near(F$cal$estimate, 0.016, 0.001))
save_tbl(T_conf, "T-evid-confirmatory")

## ---- T-evid-secondary : H6, H7, the H6 shape split ------------------------
T_sec <- data.frame(
  Quantity = c(
    "H6: reliability-tier slope of the primary contrast (KS)",
    "H6 diagnostic: the same slope on normal cells",
    "H6 refit, bimodal cells only",
    "H6 refit, skewed cells only",
    "H7: MSEL-versus-KS trade-off of GR against PM"),
  `Estimate (log)` = c(fmt(F$H6$estimate), fmt(F$H6_normal_diag$estimate),
                       fmt(F$H6_refit$slope_bimodal["est"]),
                       fmt(F$H6_refit$slope_skew["est"]),
                       fmt(F$H7$estimate)),
  SE = c(fmt(F$H6$std_error), fmt(F$H6_normal_diag$std_error),
         fmt(F$H6_refit$slope_bimodal["se"]), fmt(F$H6_refit$slope_skew["se"]),
         fmt(F$H7$std_error)),
  `95% CI` = c(
    sprintf("[%s, %s]", fmt(F$H6$ci_low), fmt(F$H6$ci_high)),
    sprintf("[%s, %s]", fmt(F$H6_normal_diag$ci_low), fmt(F$H6_normal_diag$ci_high)),
    "—", "—",
    sprintf("[%s, %s]", fmt(F$H7$ci_low), fmt(F$H7$ci_high))),
  Reading = c(
    "the DP advantage shrinks steeply as reliability falls",
    "covers zero: no tier trend where there is no shape to recover",
    "the bimodal slope is about three times the skew slope",
    "see previous row",
    "GR pays on individual accuracy while usually winning on the distribution"),
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(near(F$H6$estimate, -0.514, 0.001), near(F$H7$estimate, 0.281, 0.001),
          near(unname(F$H6_refit$slope_bimodal["est"]), -0.775, 0.001),
          near(unname(F$H6_refit$slope_skew["est"]), -0.253, 0.001))
save_tbl(T_sec, "T-evid-secondary")

## ---- T-evid-h7-census : condition-by-prior point-estimate census ----------
## Reconstruct the source figure's descriptive PM-versus-GR census from the
## frozen condition means. This is a recount of a released derived store, not a
## model refit or a replacement for the pooled H7 test above.
h7_keys <- c("condition_key", "irt", "N", "rho", "shape", "prior_arm")
h7_piece <- function(metric, summary, value_name) {
  keep <- sim_cond$metric_family == metric &
    sim_cond$posterior_summary == summary &
    sim_cond$prior_arm %in% c("gaussian", "dp_focused")
  out <- sim_cond[keep, c(h7_keys, "equal_form_weighted_loss_mean"), drop = FALSE]
  names(out)[ncol(out)] <- value_name
  stopifnot(nrow(out) == 240L, !anyDuplicated(out[h7_keys]))
  out
}
h7_points <- Reduce(
  function(x, y) merge(x, y, by = h7_keys, all = FALSE, sort = FALSE),
  list(h7_piece("KS_EDF", "PM", "ks_pm"),
       h7_piece("KS_EDF", "GR", "ks_gr"),
       h7_piece("MSEL", "PM", "msel_pm"),
       h7_piece("MSEL", "GR", "msel_gr")))
h7_points$gap_ks <- log(h7_points$ks_pm / h7_points$ks_gr)
h7_points$gap_msel <- log(h7_points$msel_pm / h7_points$msel_gr)
h7_points$predicted_quadrant <- h7_points$gap_ks > 0 & h7_points$gap_msel < 0
h7_exception <- h7_points[!h7_points$predicted_quadrant, , drop = FALSE]
h7_n_predicted <- sum(h7_points$predicted_quadrant)
stopifnot(nrow(h7_points) == 240L, h7_n_predicted == 239L,
          nrow(h7_exception) == 1L,
          h7_exception$condition_key == "2pl|500|0.9|bimodal_strong",
          h7_exception$prior_arm == "gaussian",
          near(h7_exception$gap_ks, -0.004062294, 1e-9),
          near(h7_exception$gap_msel, -0.1553222, 1e-7),
          near(exp(F$H7$estimate) - 1, 0.324078, 1e-6))
T_h7_census <- data.frame(
  `Point-estimate census` = sprintf("%d of %d in the predicted quadrant",
                                    h7_n_predicted, nrow(h7_points)),
  `Near-parity exception` = "2PL, N = 500, target reliability = .9, sharply bimodal",
  Prior = "Gaussian",
  `KS gap, log(PM / GR)` = fmt(h7_exception$gap_ks, 3),
  `MSEL gap, log(PM / GR)` = fmt(h7_exception$gap_msel, 3),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_h7_census, "T-evid-h7-census")

## ---- T-evid-census : evidence labels and the flag decomposition -----------
E <- F$evidence
primary_evidence <- sim_evidence[
  sim_evidence$contrast_id == "contrast_dp_focused_gr_vs_gaussian_gr", , drop = FALSE]
flag_evidence <- primary_evidence[
  primary_evidence$evidence_label == "harm_flag", , drop = FALSE]
parity_tol <- 1e-12
n_cross <- sum(flag_evidence$ci_low <= 1 + parity_tol &
                 flag_evidence$ci_high > 1 + parity_tol)
n_touch <- sum(flag_evidence$ci_low <= 1 + parity_tol &
                 abs(flag_evidence$ci_high - 1) <= parity_tol)
n_strict_below <- sum(flag_evidence$ci_high < 1 - parity_tol)
n_strict_above <- sum(flag_evidence$ci_low > 1 + parity_tol)
T_census <- data.frame(
  Category = c("Strong win", "Win", "Tie", "Flag: interval crosses 1",
               "Flag: interval touches 1 from below",
               "Flag: interval lies strictly below 1",
               "Flag: interval lies strictly above 1 (all normal-shape)",
               "Opportunity-region mean ratio"),
  Cells = c(E$strong_win, E$win, E$tie, n_cross, n_touch, n_strict_below,
            n_strict_above,
            sprintf("%s over %d cells", fmt(E$opportunity_mean_rr, 2), E$n_opportunity)),
  stringsAsFactors = FALSE)
stopifnot(E$strong_win == 45, E$win == 17, E$tie == 1,
          E$harm_span == 49, E$harm_below == 5, E$harm_evidence == 3,
          nrow(primary_evidence) == 120L, nrow(flag_evidence) == 57L,
          n_cross == 49L, n_touch == 2L,
          n_strict_below == 3L, n_strict_above == 3L,
          n_cross + n_touch + n_strict_below + n_strict_above == 57L,
          all(flag_evidence$shape[flag_evidence$ci_low > 1 + parity_tol] == "normal"),
          near(E$opportunity_mean_rr, 0.726, 0.001))
save_tbl(T_census, "T-evid-census")

## ---- T-evid-safety : the seven fired cells --------------------------------
S <- as.data.frame(F$safety$fired)
T_safety <- data.frame(
  Condition = sprintf("%s, $N = %d$, $\\bar\\rho = %.1f$, bimodal",
                      ifelse(S$irt == "2pl", "2PL", "Rasch"), S$N, S$rho),
  `MSEL ratio` = fmt(S$rr_point, 3),
  `95% CI` = sprintf("[%s, %s]", fmt(S$ci_low, 3), fmt(S$ci_high, 3)),
  Label = S$msel_safety_label,
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(nrow(S) == 7, F$safety$n_block == 2, F$safety$n_caution == 5,
          F$safety$all_bimodal, near(F$safety$max_rr, 1.138, 0.001))
save_tbl(T_safety, "T-evid-safety")

## ---- T-evid-levers : lever decomposition + crossed-lever competition ------
L <- as.data.frame(F$lever)
lab <- c(KS_EDF = "KS (distribution)", MSEL = "squared error (individual)",
         MSELR = "rank family", quantile = "quantile", tail_cutoff = "tail")
T_lever <- data.frame(
  `Loss family` = unname(lab[L$metric_family]),
  `Summary-swap range factor (geometric mean)` = fmt(exp(L$summary_lever_log), 2),
  `Prior-swap range factor (geometric mean)` = fmt(exp(L$prior_lever_log), 2),
  `Share of spread: summary` = sprintf("%.0f%%", 100 * L$share_summary),
  `Share of spread: prior` = sprintf("%.0f%%", 100 * L$share_prior),
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(near(exp(L$summary_lever_log[L$metric_family == "KS_EDF"]), 1.52, 0.01),
          near(L$share_summary[L$metric_family == "KS_EDF"], 0.824, 0.001),
          !anyNA(T_lever$`Loss family`),
          identical(T_lever$`Loss family`[L$metric_family == "tail_cutoff"], "tail"))
save_tbl(T_lever, "T-evid-levers")

P <- F$pair
T_crossed <- data.frame(
  Comparison = c(
    "Gaussian + GR against DP(focused) + PM, all 120 cells",
    "Gaussian + GR against DP(focused) + PM, non-normal cells",
    "Summary swap alone (Gaussian + GR against Gaussian + PM), non-normal",
    "Prior swap alone (DP(focused) + PM against Gaussian + PM), non-normal",
    "Both levers (DP(focused) + GR against Gaussian + PM), non-normal"),
  `Mean KS ratio` = fmt(c(P$g_gr__dpf_pm_all$mean_ratio, P$g_gr__dpf_pm_nonnormal$mean_ratio,
                          P$g_gr__g_pm_nonnormal$mean_ratio, P$dpf_pm__g_pm_nonnormal$mean_ratio,
                          P$dpf_gr__g_pm_nonnormal$mean_ratio), 3),
  `Cells won` = sprintf("%d of %d",
                        c(P$g_gr__dpf_pm_all$win_cells, P$g_gr__dpf_pm_nonnormal$win_cells,
                          P$g_gr__g_pm_nonnormal$win_cells, P$dpf_pm__g_pm_nonnormal$win_cells,
                          P$dpf_gr__g_pm_nonnormal$win_cells),
                        c(P$g_gr__dpf_pm_all$n_cells, P$g_gr__dpf_pm_nonnormal$n_cells,
                          P$g_gr__g_pm_nonnormal$n_cells, P$dpf_pm__g_pm_nonnormal$n_cells,
                          P$dpf_gr__g_pm_nonnormal$n_cells)),
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(P$g_gr__dpf_pm_all$win_cells == 100)
save_tbl(T_crossed, "T-evid-crossed")

## ---- T-ladder-v2 : the realized reliability ladder, with c* ---------------
## The instrument was NOT test length alone: each cell also carries a
## calibrated global discrimination multiplier c*. Read from the frozen
## rollup so the book cannot repeat the length-only story.
Ld <- as.data.frame(F$ladder$rollup)
T_ladder <- data.frame(
  Model = ifelse(Ld$model == "2pl", "2PL", "Rasch"),
  `Target $\\bar\\rho$` = fmt(Ld$target, 1),
  `Achieved (mean)` = fmt(Ld$achieved, 3),
  `Items (mean)` = fmt(Ld$n_items, 1),
  `Discrimination multiplier $c^*$ (mean)` = fmt(Ld$c_star, 3),
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(nrow(Ld) == 10, max(abs(Ld$achieved - Ld$target)) < 0.005,
          any(abs(Ld$c_star - 1) > 0.01))
save_tbl(T_ladder, "T-ladder-v2")

## ---- T-irw-reliability : the corpus distribution, paper 1 -----------------
rel <- read.csv(irw_rel_path, stringsAsFactors = FALSE)
rel_public <- read.csv(irw_rel_public_path, stringsAsFactors = FALSE)
eap <- rel$rel_eap_emp[!is.na(rel$rel_eap_emp)]
wle <- rel$rel_wle_sep[!is.na(rel$rel_wle_sep)]
stopifnot(nrow(rel) == 889L, nrow(rel_public) == 879L,
          length(eap) == 889L, length(wle) == 879L,
          near(median(eap), 0.859, 0.001), near(mean(eap < 0.80), 0.298, 0.005),
          near(median(wle), 0.801, 0.001), near(mean(wle < 0.80), 0.498, 0.005))
T_irw_rel <- data.frame(
  Quantity = c("Units", "Median", "Share below .80", "Share below .70",
               "Share below .50", "True (deconvolved) share below .80"),
  `EAP empirical reliability` = c(
    length(eap), fmt(median(eap)), sprintf("%.0f%%", 100 * mean(eap < .80)),
    sprintf("%.0f%%", 100 * mean(eap < .70)), sprintf("%.0f%%", 100 * mean(eap < .50)),
    "30.3%"),
  `WLE separation reliability` = c(
    length(wle), fmt(median(wle)), sprintf("%.0f%%", 100 * mean(wle < .80)),
    sprintf("%.0f%%", 100 * mean(wle < .70)), sprintf("%.0f%%", 100 * mean(wle < .50)),
    "51.8%"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_irw_rel, "T-irw-reliability")
## The two deconvolved shares are the manuscript's own three-level model
## estimates (paper 1, Results), carried as published values; everything else
## in the table is computed from the frozen complete 889-unit analysis snapshot
## above. The public derived release has 879 rows because ten source rows cannot
## be redistributed; it is checked here but is not the source of this table.

## ---- T-irw-shape : latent-departure prevalence, paper 2 -------------------
nb <- read.csv(irw_shape_path, stringsAsFactors = FALSE)
ks <- nb$ks_normal[!is.na(nb$ks_normal)]
stopifnot(length(ks) == 504,
          near(median(ks), 0.109, 0.001),
          sum(ks > .10) == 282, sum(ks > .15) == 155, sum(ks > .20) == 93)
sk <- nb$skew[!is.na(nb$ks_normal)]
dp <- nb$dip_stat[!is.na(nb$ks_normal)]
T_irw_shape <- data.frame(
  Quantity = c("Units with a valid shape estimate",
               "Median KS distance from normality",
               "Departures of at least .05", "Departures of at least .10",
               "Departures of at least .15", "Departures of at least .20",
               "Median absolute skewness", "Median dip statistic"),
  Value = c(length(ks), fmt(median(ks)),
            sprintf("%d (%.0f%%)", sum(ks > .05), 100 * mean(ks > .05)),
            sprintf("%d (%.0f%%)", sum(ks > .10), 100 * mean(ks > .10)),
            sprintf("%d (%.0f%%)", sum(ks > .15), 100 * mean(ks > .15)),
            sprintf("%d (%.0f%%)", sum(ks > .20), 100 * mean(ks > .20)),
            fmt(median(abs(sk))), fmt(median(dp))),
  stringsAsFactors = FALSE)
save_tbl(T_irw_shape, "T-irw-shape")

## ---- T-shape-null : spurious shape at small n, case-study calibration -----
nc <- cs_tbl("P1-T6-null-calibration.csv")
keep <- nc$J == 12 | (nc$n == 100 & nc$J == 45)
T_null <- data.frame(
  `Persons $n$` = nc$n[keep],
  `Items $J$` = nc$J[keep],
  `Median KS under a truly normal $G$` = fmt(nc$ks_median[keep]),
  `Replicates with dip > .03` = sprintf("%.0f%%", nc$pct_dip_gt_03[keep]),
  check.names = FALSE, stringsAsFactors = FALSE)
o <- order(T_null[["Persons $n$"]], T_null[["Items $J$"]])
T_null <- T_null[o, ]
stopifnot(near(nc$ks_median[nc$n == 100 & nc$J == 12], 0.196, 0.001),
          near(nc$pct_dip_gt_03[nc$n == 100 & nc$J == 12], 95.2, 0.1))
save_tbl(T_null, "T-shape-null")

## ---- T-materiality : the case-study's materiality-by-family census --------
mt <- cs_tbl("P3-T17-materiality-by-family.csv")
T_mat <- data.frame(
  `Shape class` = mt$shape,
  `Case-cells` = mt$cells,
  `Individual scores` = mt$F1_scores,
  `Reported distribution` = mt$F2_distribution,
  `Rankings` = mt$F3_rankings,
  `Tails and cuts` = mt$F4_tails,
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(mt$F2_distribution[mt$shape == "bimodal"] == 7,
          mt$F1_scores[mt$shape == "bimodal"] == 1)
save_tbl(T_mat, "T-materiality")

## ---- T-seed-floor : seed-driven selection churn ---------------------------
sf <- cs_tbl("P3-T15-selection-churn-sources.csv")
T_seed <- data.frame(
  `Shape class` = sf$shape,
  Source = sf$source,
  `Top-decile members changed` = sprintf("%.1f%%", sf$pct_changed),
  check.names = FALSE, stringsAsFactors = FALSE)
T_seed <- T_seed[order(T_seed$`Shape class`, T_seed$Source), ]
stopifnot(near(max(sf$pct_changed[sf$source == "same method, different seed"]), 6.5, 0.1))
save_tbl(T_seed, "T-seed-floor")

## ---- T-seed-extrema : overall versus displayed same-method scope -----------
seed_pairs <- read.csv(seed_pairs_path, stringsAsFactors = FALSE, check.names = FALSE)
seed_overall_max <- max(seed_pairs$pct_changed)
seed_overall_rows <- seed_pairs[near(seed_pairs$pct_changed, seed_overall_max, 1e-10), ]
seed_figure_rows <- seed_pairs[seed_pairs$arm == "gaussian" & seed_pairs$summ == "PM", ]
seed_figure_max <- max(seed_figure_rows$pct_changed)
seed_figure_worst <- seed_figure_rows[
  near(seed_figure_rows$pct_changed, seed_figure_max, 1e-10), ]
stopifnot(nrow(seed_pairs) == 234L, nrow(seed_figure_rows) == 26L,
          near(seed_overall_max, 32.35294, 1e-5),
          nrow(seed_overall_rows) == 3L,
          near(seed_figure_max, 28, 1e-10),
          nrow(seed_figure_worst) == 1L,
          seed_figure_worst$case_id == "C4", seed_figure_worst$item_model == "rasch")
T_seed_extrema <- data.frame(
  Scope = c("All stored same-method prior-summary pairs",
            "Gaussian + PM subset plotted in the case-study figure"),
  `Pairs searched` = c(nrow(seed_pairs), nrow(seed_figure_rows)),
  `Worst top-decile churn` = c(sprintf("%.2f%%", seed_overall_max),
                               sprintf("%.0f%%", seed_figure_max)),
  `Maximizing case-cell and method` = c(
    "C12 Rasch, DP(broad) + PM/CB; C12 2PL, DP(broad) + GR",
    "C4 Rasch, Gaussian + PM"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_seed_extrema, "T-seed-extrema")

## ---- T-rel-gaps : the two reliability functionals on the thirteen cases ---
rg <- cs_tbl("P1-T2-reliability-gaps.csv")
T_relgap <- data.frame(
  Case = rg$case_id,
  `mirt marginal, Rasch` = fmt(rg$marg_rasch),
  `mirt marginal, 2PL` = fmt(rg$marg_2pl),
  `$\\bar\\rho$, Rasch` = fmt(rg$rhobar_rasch),
  `$\\bar\\rho$, 2PL` = fmt(rg$rhobar_2pl),
  `Gaps agree in sign` = ifelse(sign(rg$marg_gap) == sign(rg$rhobar_gap), "yes", "no"),
  check.names = FALSE, stringsAsFactors = FALSE)
T_relgap <- T_relgap[order(T_relgap$Case), ]
c4 <- rg[rg$case_id == "C4", ]
stopifnot(near(c4$marg_rasch, 0.296, 0.001), near(c4$marg_2pl, 0.778, 0.001),
          near(c4$rhobar_rasch, 0.772, 0.001), near(c4$rhobar_2pl, 0.261, 0.001))
save_tbl(T_relgap, "T-rel-gaps")

cat("evidence tables complete\n")
