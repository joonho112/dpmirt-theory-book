## 24-figures-v2-evidence.R - evidence figures for Parts VIII-IX (v3).
## Reads the companion volumes' frozen artifacts, the reliability paper's
## frozen complete analysis snapshot, and the shape paper's corrected released
## frame. Every number plotted here comes from those sources; the same
## assertions as 20-evidence-tables.R guard the headline values.
## Chapters embed; they never plot.

source("code/R/00-paths.R")
source("code/R/21-style-v2.R")
FIG <- paths$figures
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

F <- readRDS(file.path(inputs$sim_book_v3, "data", "derived", "book-facts.rds"))
tblv <- function(id) readRDS(file.path(paths$tables, paste0(id, ".rds")))

## ---- F-evidence-forest : the confirmatory record at a glance --------------
rows <- rbind(
  data.frame(id = "H1", label = "H1: DP vs Gaussian at the design centre (KS)",
             family = "primary", x = F$H1$estimate, lo = F$H1$ci_low, hi = F$H1$ci_high),
  data.frame(id = "H2", label = "H2: deepening per doubling of N (KS)",
             family = "primary", x = F$H2$estimate, lo = F$H2$ci_low, hi = F$H2$ci_high),
  data.frame(id = "H3", label = "H3: 2PL minus Rasch difference (KS)",
             family = "primary", x = F$H3$estimate, lo = F$H3$ci_low, hi = F$H3$ci_high),
  data.frame(id = "H4", label = "H4: individual-accuracy non-inferiority (MSEL)",
             family = "primary", x = F$H4$estimate, lo = F$H4$ci_low, hi = F$H4$ci_high),
  data.frame(id = "H5", label = "H5: focused vs broad elicitation (KS)",
             family = "primary", x = F$H5$estimate, lo = F$H5$ci_low, hi = F$H5$ci_high),
  data.frame(id = "H6", label = "H6: slope over reliability tiers (KS)",
             family = "secondary", x = F$H6$estimate, lo = F$H6$ci_low, hi = F$H6$ci_high),
  data.frame(id = "H7", label = "H7: GR pays MSEL where it wins KS",
             family = "secondary", x = F$H7$estimate, lo = F$H7$ci_low, hi = F$H7$ci_high),
  data.frame(id = "C", label = "Control: normal cells, DP vs Gaussian (KS)",
             family = "control", x = F$cal$estimate, lo = F$cal$ci_low, hi = F$cal$ci_high)
)
rows$ypos <- rev(seq_len(nrow(rows)))
ni <- log(1.05)
p <- ggplot(rows, aes(x, ypos, colour = family)) +
  geom_vline(xintercept = 0, colour = INK2, linewidth = 0.4) +
  annotate("segment", x = ni, xend = ni,
           y = 4.55, yend = 5.45, colour = PAL[4], linewidth = 0.6) +
  annotate("text", x = ni + 0.015, y = 5.0, hjust = 0, size = 2.8,
           colour = PAL[4], family = "Helvetica",
           label = "H4 non-inferiority margin, log(1.05)") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                width = 0.16, linewidth = 0.55) +
  geom_point(size = 2.1) +
  scale_colour_manual(values = c(control = PAL[5], primary = PAL[1],
                                 secondary = PAL[3]), guide = "none") +
  scale_x_continuous(
    name = "estimate on the log-ratio scale (negative favours the flexible arm)",
    sec.axis = sec_axis(~ exp(.), name = "ratio scale",
                        breaks = c(0.6, 0.8, 1.0, 1.2))) +
  scale_y_continuous(breaks = rows$ypos, labels = rows$label, name = NULL) +
  theory_theme(10.8) +
  theme(panel.grid.major.y = element_blank())
save_fig2("F-evidence-forest",
  paste("**The companion simulation's confirmatory record, on one axis.** Point",
        "estimates and 95% intervals for the five preregistered hypotheses",
        "(blue), the two secondary hypotheses (green), and the calibration",
        "control (grey), on the log loss-ratio scale; every interval is read",
        "from the simulation volume's frozen fact store, and all seven",
        "hypothesis tests were supported in one Holm family each",
        "[@lee_simulation_2026]. H1 through H3, H5, and H6 are KS-loss",
        "contrasts; H4 is the individual-accuracy safety bound with its",
        "one-sided margin marked; H7 is the trade-off between the two loss",
        "families; the control is the deliberate falsification check on normal",
        "cells, where flexibility buys nothing and costs 1.6 percent. Generated",
        "by code/R/24-figures-v2-evidence.R."),
  p, w = 8.2, h = 4.6)

## ---- F-prediction-map : ch. 13's predictions against the realized record --
b <- rbind(
  box_row(20, 88, 36, 15,
          "Individual estimates:\nmodestly affected,\nlargest at middling reliability",
          fill = "#EFF4FA", colour = PAL[1], fontface = "bold"),
  box_row(20, 55, 36, 15,
          "The recovered distribution:\nseverely affected,\nin the assumed shape's direction",
          fill = "#EFF4FA", colour = PAL[1], fontface = "bold"),
  box_row(20, 21, 36, 15,
          "Ranks: a separate,\nconditional question",
          fill = "#EFF4FA", colour = PAL[1], fontface = "bold"),

  box_row(72, 88, 50, 15, sprintf(
    "H4: flexible arm %.0f%% better on average\n(upper bound %.3f, margin %.3f) -\nexcept seven bimodal low-reliability cells,\nratios %.2f to %.2f",
    100 * (1 - exp(F$H4$estimate)), F$H4$ni_upper_one_sided_95, log(1.05),
    min(F$safety$fired$rr_point), max(F$safety$fired$rr_point))),
  box_row(72, 55, 50, 15, sprintf(
    "H1: KS ratio %.2f at the design centre,\n%.2f over the opportunity region;\ncontrol %.3f on normal cells\n(nothing to recover, nothing gained)",
    exp(F$H1$estimate), F$evidence$opportunity_mean_rr, exp(F$cal$estimate))),
  box_row(72, 21, 50, 15,
    "rank losses nearly flat across all nine\ncombinations; the reliability tier,\nnot the method, carries the variance")
)
a <- rbind(
  arrow_row(38.5, 88, 46.5, 88), arrow_row(38.5, 55, 46.5, 55),
  arrow_row(38.5, 21, 46.5, 21)
)
hd <- data.frame(x = c(20, 72), y = 99.2,
                 label = c("Predicted in Chapter 13 (mechanism)",
                           "Returned by the simulation (evidence)"))
p <- draw_diagram(b, a, xlim = c(0, 100), ylim = c(10, 103)) +
  geom_text(data = hd, aes(x, y, label = label), fontface = "italic",
            colour = INK2, size = 3.1, family = "Helvetica")
save_fig2("F-prediction-map",
  paste("**Three predictions, three verdicts.** The goal-specific predictions",
        "@sec-ch13-goal stated as mechanisms, next to what the preregistered",
        "simulation returned for each [@lee_simulation_2026]. The individual-score",
        "prediction held with a quantified exception region (the seven bimodal",
        "cells at the lowest reliability tiers); the distribution prediction held",
        "with the largest effects exactly where the mechanism put them; the rank",
        "prediction resolved to near-flatness, with reliability rather than",
        "method choice carrying the variance. All numbers are read from the",
        "simulation volume's frozen fact store at build time. Generated by",
        "code/R/24-figures-v2-evidence.R."),
  p, w = 8.6, h = 4.4)

## ---- F-c4-reversal : two functionals on the thirteen real cases -----------
rg <- read.csv(file.path(inputs$case_study, "tables", "P1-T2-reliability-gaps.csv"),
               stringsAsFactors = FALSE)
rg <- rg[order(as.numeric(sub("^C", "", rg$case_id))), ]
long <- rbind(
  data.frame(case = rg$case_id,
             fun = "Gaussian-fit mirt marginal_rxx\n(information-ratio catalogue)",
             rasch = rg$marg_rasch, twopl = rg$marg_2pl),
  data.frame(case = rg$case_id,
             fun = "Empirical-histogram MSEM rho-bar\n(separate fitted object)",
             rasch = rg$rhobar_rasch, twopl = rg$rhobar_2pl)
)
long$fun <- factor(long$fun, levels = c(
  "Gaussian-fit mirt marginal_rxx\n(information-ratio catalogue)",
  "Empirical-histogram MSEM rho-bar\n(separate fitted object)"
))
long$case <- factor(long$case, levels = rev(rg$case_id))
long$delta <- long$twopl - long$rasch
long$dir <- ifelse(long$delta == 0, "Tie",
                   ifelse(long$delta > 0, "2PL higher", "Rasch higher"))
arrow_rows <- long[long$dir != "Tie", , drop = FALSE]
tie_rows <- long[long$dir == "Tie", , drop = FALSE]
stopifnot(nrow(tie_rows) == 1L, as.character(tie_rows$case) == "C13",
          grepl("mirt marginal_rxx", tie_rows$fun, fixed = TRUE),
          tie_rows$rasch == 0.706, tie_rows$twopl == 0.706)
p <- ggplot(long, aes(y = case)) +
  geom_segment(data = arrow_rows,
               aes(x = rasch, xend = twopl, yend = case, colour = dir),
               linewidth = 0.7,
               arrow = arrow(length = unit(1.7, "mm"), type = "closed")) +
  geom_point(aes(x = rasch), colour = INK, size = 1.6) +
  geom_point(data = tie_rows, aes(x = rasch, colour = dir), shape = 4,
             size = 2.7, stroke = 0.9) +
  facet_wrap(~fun) +
  scale_colour_manual(values = c("2PL higher" = PAL[1],
                                 "Rasch higher" = PAL[2], "Tie" = PAL[5]),
                      limits = c("2PL higher", "Rasch higher", "Tie"),
                      drop = FALSE, name = NULL) +
  scale_x_continuous(name = "reliability coefficient (dot = Rasch, arrowhead = 2PL)",
                     limits = c(0.2, 1)) +
  labs(y = NULL) +
  theory_theme(10.8)
save_fig2("F-c4-reversal",
  paste("**On real tests, the two reliability functionals disagree about the",
        "item model - and can disagree in sign.** Each arrow runs from a case's",
        "Rasch coefficient (dot) to its 2PL coefficient (arrowhead), for the",
        "thirteen Item Response Warehouse cases of the companion case-study",
        "volume [@lee_casestudy_2026]. Blue arrows point right when the 2PL value",
        "is higher; orange arrows point left when it is lower; the grey cross is",
        "the exact C13 tie at .706 in the Gaussian-fit panel. The left panel is",
        "`mirt::marginal_rxx` from each Gaussian fit, an information-ratio",
        "catalogue coefficient. The right panel is the MSEM functional from each",
        "separate empirical-histogram fit. They agree on the Rasch-to-2PL direction",
        "in six of thirteen cases, and C4 moves by roughly half a point in opposite",
        "directions. These are coefficients on identical responses but distinct",
        "fitted objects, not a same-fit causal diagnosis. Values are the frozen",
        "P1-T2 catalogue. Generated by code/R/24-figures-v2-evidence.R."),
  p, w = 8.4, h = 4.6)

## ---- F-irw-reliability : what reliability real tests deliver --------------
rel <- read.csv(file.path(inputs$case_study, "data", "raw", "reliability_dataset.csv"),
                stringsAsFactors = FALSE)
rel_public <- read.csv(file.path(IRW, "irw-reliability-replication", "data-derived",
                                 "reliability_dataset_public879.csv"),
                       stringsAsFactors = FALSE)
eap <- rel$rel_eap_emp[!is.na(rel$rel_eap_emp)]
wle <- rel$rel_wle_sep[!is.na(rel$rel_wle_sep)]
stopifnot(nrow(rel) == 889, nrow(rel_public) == 879,
          length(eap) == 889, length(wle) == 879,
          sum(is.na(rel$rel_wle_sep)) == 10,
          abs(median(eap) - 0.859) < 0.001,
          abs(median(wle) - 0.801) < 0.001)
wle_p <- pmax(wle, 0)   ## 21 negative separation values shown at zero
d <- rbind(data.frame(x = eap, est = "EAP empirical (lenient)"),
           data.frame(x = wle_p, est = "WLE separation (strict)"))
sh <- data.frame(
  est = c("EAP empirical (lenient)", "WLE separation (strict)"),
  med = c(median(eap), median(wle)),
  sub80 = c(mean(eap < .80), mean(wle < .80))
)
p <- ggplot(d, aes(x, colour = est, fill = est)) +
  geom_vline(xintercept = c(0.7, 0.8), colour = "grey70", linetype = "22",
             linewidth = 0.4) +
  stat_ecdf(linewidth = 0.85, pad = FALSE) +
  geom_segment(data = sh, aes(x = med, xend = med, y = 0, yend = 0.5,
                              colour = est),
               linetype = "42", linewidth = 0.45, show.legend = FALSE) +
  annotate("text", x = 0.075, y = 0.565, hjust = 0, size = 3.0, colour = PAL[2],
           family = "Helvetica",
           label = sprintf("%.0f%% of units fall below .80\nunder the strict coefficient",
                           100 * sh$sub80[2])) +
  annotate("text", x = 0.075, y = 0.33, hjust = 0, size = 3.0, colour = PAL[1],
           family = "Helvetica",
           label = sprintf("%.0f%% under the lenient one", 100 * sh$sub80[1])) +
  scale_colour_manual(values = PAL[c(1, 2)], name = NULL) +
  scale_fill_manual(values = PAL[c(1, 2)], name = NULL) +
  scale_x_continuous(name = "marginal reliability", limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(name = "cumulative share within coefficient") +
  theory_theme(11)
save_fig2("F-irw-reliability",
  paste("**The reliability real item-response data actually deliver.**",
        "Cumulative distributions of marginal reliability in the complete Item",
        "Response Warehouse analysis [@lee_reliability-distribution_2026]. The",
        "EAP empirical series contains 889 nonmissing estimates (median .859;",
        "30 percent below .80), whereas the WLE separation series contains 879",
        "nonmissing estimates (median .801; 50 percent below .80); dotted",
        "verticals mark the .70 and .80 conventions and dashed stems mark the",
        "medians. Twenty-one negative separation estimates are drawn at zero.",
        "The figure is computed from the frozen complete analysis snapshot used",
        "for the 889-dataset manuscript, not from the 879-row public release.",
        "Generated by code/R/24-figures-v2-evidence.R."),
  p, w = 7.8, h = 4.6)

## ---- F-irw-shapes : how far real latent-distribution estimates sit --------
## from normality (paper 2's corrected 504-unit frame)
nb <- read.csv(file.path(IRW, "irw-normality-replication",
                         "data-derived", "nonbootstrap-dataset.csv"),
               stringsAsFactors = FALSE)
nb <- nb[!is.na(nb$ks_normal), ]
nb$abs_skew <- abs(nb$skew)
nb$skew_clipped <- nb$abs_skew > 6
nb$abs_skew_plot <- pmin(nb$abs_skew, 6)
stopifnot(nrow(nb) == 504, abs(median(nb$ks_normal) - 0.109) < 0.001,
          abs(median(nb$dip_stat) - 0.018) < 0.001,
          sum(nb$skew_clipped) == 3L,
          length(unique(nb$dip_stat[nb$skew_clipped])) == 3L)
pa <- ggplot(nb, aes(ks_normal)) +
  geom_histogram(binwidth = 0.02, boundary = 0, fill = PAL[1],
                 colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = c(.10, .15, .20), colour = INK2, linetype = "22",
             linewidth = 0.4) +
  annotate("text", x = .105, y = 62, hjust = 0, size = 2.8, colour = INK2,
           family = "Helvetica",
           label = sprintf("%.0f%% beyond .10", 100 * mean(nb$ks_normal > .10))) +
  annotate("text", x = .155, y = 55, hjust = 0, size = 2.8, colour = INK2,
           family = "Helvetica",
           label = sprintf("%.0f%% beyond .15", 100 * mean(nb$ks_normal > .15))) +
  annotate("text", x = .205, y = 48, hjust = 0, size = 2.8, colour = INK2,
           family = "Helvetica",
           label = sprintf("%.0f%% beyond .20", 100 * mean(nb$ks_normal > .20))) +
  scale_x_continuous(name = "KS distance from the normal calibration") +
  scale_y_continuous(name = "datasets") +
  labs(subtitle = "How far the fitted latent distribution sits from normal") +
  theory_theme(10.5)
pb <- ggplot(nb, aes(abs_skew_plot, dip_stat)) +
  geom_point(data = nb[!nb$skew_clipped, ],
             aes(colour = ks_normal > .10), size = 1.15, alpha = 0.75) +
  geom_point(data = nb[nb$skew_clipped, ],
             aes(colour = ks_normal > .10), shape = 17, size = 1.15,
             alpha = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "grey72", `TRUE` = PAL[2]),
                      labels = c("KS at most .10", "KS beyond .10"), name = NULL) +
  annotate("text", x = 5.9, y = 0.128, hjust = 1, size = 2.7,
           colour = INK2, family = "Helvetica",
           label = "3 estimates exceed 6\n(triangles at boundary)") +
  scale_x_continuous(name = "absolute skewness (values beyond 6 shown at boundary)") +
  scale_y_continuous(name = "dip statistic (multimodality)") +
  coord_cartesian(xlim = c(0, 6)) +
  labs(subtitle = "Skew is the common departure; multimodality is rare") +
  theory_theme(10.5)
p <- pa + pb
save_fig2("F-irw-shapes",
  paste("**Estimated latent distributions in real data are routinely non-normal,",
        "and the common departure is skew, not bimodality.** Left: the KS",
        "distance between each dataset's flexibly estimated latent distribution",
        "and its normal calibration, across the 504 Item Response Warehouse",
        "datasets of the corpus study [@lee_theta-nonnormality_2026]; the median",
        "is .109, and the dashed landmarks give the shares beyond ten, fifteen,",
        "and twenty cumulative-probability points. Right: the same units by",
        "absolute skewness and the dip statistic (axis clipped at six; three units",
        "sit beyond it and are shown as triangles at the boundary) - large",
        "departures are mostly",
        "skewed, and pronounced multimodality is a minority feature (median dip",
        ".018), which bears directly on where the bimodal generating conditions",
        "of the companion simulation sit relative to practice (@sec-ch28).",
        "Computed from the study's corrected released frame; the build asserts",
        "the published prevalence values before this figure is written.",
        "Generated by code/R/24-figures-v2-evidence.R."),
  p, w = 8.6, h = 4.2)

## ---- F-shape-null : spurious shape at case-study sample sizes -------------
nc <- read.csv(file.path(inputs$case_study, "tables", "P1-T6-null-calibration.csv"),
               stringsAsFactors = FALSE)
j12 <- nc[nc$J == 12, ]
j12 <- j12[order(j12$n), ]
n100 <- nc[nc$n == 100, ]
pa <- ggplot(j12, aes(n, pct_dip_gt_03)) +
  geom_line(colour = PAL[2], linewidth = 0.8) +
  geom_point(colour = PAL[2], size = 2) +
  geom_point(data = n100, aes(n, pct_dip_gt_03), shape = 1, size = 2,
             colour = PAL[1], stroke = 0.7) +
  annotate("text", x = 340, y = 79, hjust = 0, size = 2.9, colour = PAL[1],
           family = "Helvetica",
           label = "open circles: 12 to 45 items\nat n = 100 - more items\ndo not repair the artefact") +
  scale_x_continuous(name = "respondents", trans = "log10",
                     breaks = c(100, 200, 350, 500, 1000, 1500)) +
  scale_y_continuous(name = "% of truly normal replicates with dip > .03",
                     limits = c(0, 100)) +
  labs(subtitle = "Spurious multimodality under a truly normal latent trait") +
  theory_theme(10.5)
pb <- ggplot(j12, aes(n, ks_median)) +
  geom_line(colour = PAL[1], linewidth = 0.8) +
  geom_point(colour = PAL[1], size = 2) +
  scale_x_continuous(name = "respondents", trans = "log10",
                     breaks = c(100, 200, 350, 500, 1000, 1500)) +
  scale_y_continuous(name = "median KS of the refitted estimate",
                     limits = c(0, 0.21)) +
  labs(subtitle = "The estimation noise floor falls only with respondents") +
  theory_theme(10.5)
p <- pa + pb
save_fig2("F-shape-null",
  paste("**Below a few hundred respondents, an estimated latent shape is mostly",
        "artefact.** The case-study volume refitted truly normal data at its own",
        "cases' design sizes [@lee_casestudy_2026]: at one hundred respondents",
        "and twelve items, 95 percent of replicates show a dip statistic beyond",
        ".03 - spurious bimodality - and the refitted estimate sits a median .196",
        "from the normal truth. Adding items does not repair the artefact (open",
        "circles), only respondents do, and the transition sits near three to",
        "five hundred - which is why the corpus study of",
        "@fig-irw-shapes imposes a 500-respondent floor and why @sec-ch28 treats",
        "small-sample shape verdicts as undetermined rather than normal. Read",
        "from the case-study volume's frozen null-calibration table (P1-T6).",
        "Generated by code/R/24-figures-v2-evidence.R."),
  p, w = 8.4, h = 4.2)

cat("evidence figures complete\n")
