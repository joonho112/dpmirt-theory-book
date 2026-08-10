## 07-tables.R — the canonical table store.
## Chapters read tables/T-*.rds and never compute. Every table is built here.

source("code/R/00-paths.R")
dir.create(file.path(paths$tables, "supplement"), showWarnings = FALSE, recursive = TRUE)

save_tbl <- function(obj, id) {
  saveRDS(obj, file.path(paths$tables, paste0(id, ".rds")))
  write.csv(obj, file.path(paths$tables, "supplement", paste0(id, ".csv")),
            row.names = FALSE, na = "")
  cat(sprintf("  %-22s %d rows\n", id, nrow(obj)))
}

## ---- T-notation : the master notation table (book ch. 2) ----------------
nr <- read.csv(file.path(paths$manifest, "notation-register.csv"),
               stringsAsFactors = FALSE, na.strings = "")
nr[is.na(nr)] <- ""
T_notation <- data.frame(
  Symbol  = paste0("$", nr$symbol, "$"),
  Meaning = nr$meaning,
  Note    = ifelse(nzchar(nr$note), nr$note,
                   ifelse(nzchar(nr$collides_with),
                          paste0("collides with: ", nr$collides_with), "")),
  stringsAsFactors = FALSE
)
## the index bounds read better unwrapped
T_notation$Symbol[nr$symbol %in% c("P", "I", "p", "i")] <-
  paste0("$", nr$symbol[nr$symbol %in% c("P", "I", "p", "i")], "$")

## ---- T-frameworks : distributional assumption vs prior (book ch. 2) -----
T_frameworks <- data.frame(
  Question = c(
    "What is $G$?",
    "Are the $\\theta_p$ random?",
    "Are the $\\beta_i$ random?",
    "What is done with $G$'s parameters?",
    "How is $G$ relaxed?",
    "Does shrinkage occur?"),
  `Frequentist (MML)` = c(
    "A population distribution: the law of $\\theta$ in the population persons were sampled from",
    "Yes — random effects, integrated out",
    "No — fixed structural parameters, no distribution assigned",
    "Estimated by maximising the marginal likelihood",
    "Nonparametric MML: estimate $G$ as a discrete or smoothed distribution",
    "Yes — via empirical Bayes, plugging $(\\hat\\mu_\\theta,\\hat\\sigma^2_\\theta)$ into the same weight"),
  `Bayesian (hierarchical)` = c(
    "A prior for $\\theta_p$ — but a *hierarchical* one, which is why the distinction is subtle",
    "Yes — assigned a prior",
    "Yes — assigned a prior, which is the extra assumption",
    "Assigned hyperpriors and integrated over",
    "Semiparametric priors: a Dirichlet process mixture over $G$",
    "Yes — the same weight, with hyperparameter uncertainty carried"),
  check.names = FALSE, stringsAsFactors = FALSE)

## ---- T-levels : level vocabulary (book ch. 2) ---------------------------
T_levels <- data.frame(
  Level = c("Level 1 — measurement", "Level 2 — population", "Level 3 — hyperprior"),
  Describes = c(
    "The observation process conditional on the parameters: the likelihood",
    "The distribution of the Level-1 parameters — $\\theta_p \\sim G$; under item centering, iid Gaussian auxiliaries induce a dependent prior on constrained $\\boldsymbol\\beta$",
    "The distribution of the parameters governing Level 2"),
  `Why not "stage"` = c(
    "\"Stage\" names a step in an estimation procedure, not a layer of the model",
    "MML followed by ability estimation is two stages of *estimation* within one Level-2 model",
    "The two vocabularies cut the model differently, and conflating them is what made the manuscript's usage unreadable"),
  check.names = FALSE, stringsAsFactors = FALSE)

cat("tables written:\n")
save_tbl(T_notation,   "T-notation")
save_tbl(T_frameworks, "T-frameworks")
save_tbl(T_levels,     "T-levels")

## ---- T-identification : what must be restricted (book ch. 4) -----------
T_identification <- data.frame(
  `Specification` = c(
    "Fixed effects — the $\\theta_p$ are $P$ unknown parameters",
    "Random effects — $\\theta_p \\sim G$, $G$ known up to scale $\\sigma$",
    "Random effects — $\\theta_p \\sim G$, $G$ with free location $\\mu$ and scale $\\sigma$",
    "Semiparametric — $G$ itself an unknown parameter"),
  `Parameters of interest` = c(
    "$(\\boldsymbol\\theta, \\boldsymbol\\beta)$, and $\\boldsymbol\\lambda$ under the 2PL",
    "$(\\boldsymbol\\beta, \\sigma)$, and $\\boldsymbol\\lambda$ under the 2PL",
    "$(\\boldsymbol\\beta, \\mu, \\sigma)$, and $\\boldsymbol\\lambda$ under the 2PL",
    "$(\\boldsymbol\\beta, G)$"),
  `Rasch` = c(
    "$\\beta_1 = 0$",
    "**none within this known-shape family** — $\\sigma$ is identified by a continuous strictly increasing bivariate-probability map under the stated regularity conditions",
    "$\\beta_1 = 0$",
    "$\\beta_1 = 0$ identifies $\\boldsymbol\\beta$ and $I+1$ response-score functionals; **full $G$ and its variance are not generally identified** (@sec-ch16)"),
  `2PL` = c(
    "$\\beta_1 = 0$ and $\\lambda_1 = 1$",
    "$\\lambda_1 = 1$",
    "$\\lambda_1 = 1$ and $\\beta_1 = 0$; requires $I \\ge 3$",
    "**open problem**"),
  check.names = FALSE, stringsAsFactors = FALSE)

save_tbl(T_identification, "T-identification")

## ---- T-estimation : five approaches to incidental parameters (ch. 5) ----
T_estimation <- data.frame(
  Method = c("Joint ML (JML)", "Rasch conditional ML (CML)",
             "Restricted marginal ML", "Unrestricted marginal ML",
             "Joint Bayesian marginal model"),
  `Incidental parameters` = c(
    "maximized over, alongside the items",
    "conditioned away exactly, using $r_p$",
    "integrated against a specified parametric $G$",
    "integrated against a freely estimated mixing distribution",
    "integrated jointly under priors on items and $G$"),
  `Assumption about $G$` = c(
    "none",
    "none",
    "a specified family, usually Gaussian",
    "no parametric shape; a finite-support maximizer",
    "a prior on a parametric or semiparametric $G$"),
  `Model and identification scope` = c(
    "Rasch or identified 2PL; anchors still required",
    "Rasch only; exact person-total conditioning",
    "Rasch or identified parametric 2PL",
    "finite-item Rasch identifies anchored items and finitely many $G$ functionals, not full $G$",
    "a proper posterior does not establish likelihood identification"),
  `Asymptotic qualification` = c(
    "inconsistent for fixed $I$; consistent under Haberman's joint-growth conditions",
    "item-consistent as $P \\to \\infty$ with fixed $I$, under regularity",
    "consistency requires correct $G$ family, identification, and regularity",
    "no categorical full-$G$ consistency claim; unrestricted estimation can be unsatisfactory",
    "concentration claims require a separately identified model and prior-support conditions"),
  `2PL status` = c(
    "available with location/scale constraints",
    "**no ordinary Rasch analogue** (@prp-nosuff)",
    "available for an identified parametric 2PL",
    "**semiparametric identification open**",
    "DPM×2PL recovery is exploratory and convention-dependent"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_estimation, "T-estimation")

## ---- T-estimators : person-parameter estimators (ch. 6) -----------------
T_estimators <- data.frame(
  Estimator = c("ML", "WLE", "MAP (Bayesian modal)", "EAP (posterior mean)"),
  `Weight f in eq. 6.2` = c(
    "a positive constant",
    "w with d log w / d theta = J / (2 J-info)",
    "a prior density for theta",
    "not a member: the posterior mean, not a mode"),
  `Bias with items known` = c(
    "$-J/(2\\mathcal{J}^2)$, order $I^{-1}$",
    "**$o(I^{-1})$** under Warm's conditions; same first-order asymptotic variance as ML",
    "ML bias $+\\,(\\partial\\ln f/\\partial\\theta)/\\mathcal{J}$",
    "shrunk toward $\\mu_\\theta$; minimizes squared-error loss"),
  `Finite at a perfect or zero score` = c("**no**", "yes", "yes", "yes"),
  `Information used` = c("within-person only", "within-person only",
                         "within-person and population", "within-person and population"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_estimators, "T-estimators")

## ---- T-reliability-zoo : coefficients that all claim the name (ch. 8) ----
## Every implementation formula below was read from the named package source,
## not from documentation or recollection. Versions are recorded in the caption.
T_zoo <- data.frame(
  Coefficient = c(
    "$\\bar w$ (this programme)",
    "Person-separation reliability",
    "EAP reliability",
    "Marginal reliability $\\rho_\\Theta$ (standard; mirt caveat)",
    "Average-information $\\tilde\\rho$",
    "Coefficient $\\alpha$",
    "Guttman's $\\lambda_2$"),
  `True variance is` = c(
    "**assumed known** ($\\sigma^2_\\theta$, set by design)",
    "$\\operatorname{Var}(\\hat\\theta) - \\mathrm{MSEM}$ (subtracted)",
    "$\\operatorname{Var}(\\hat\\theta)$ itself (estimates are shrunk)",
    "$\\sigma^2_\\theta$, pointwise",
    "$\\sigma^2_\\theta$",
    "not identified; bounded",
    "not identified; bounded"),
  Formula = c(
    "$\\sigma^2_\\theta/(\\sigma^2_\\theta+\\mathrm{MSEM})$",
    "$1-\\mathrm{MSEM}/\\operatorname{Var}(\\hat\\theta)$",
    "$\\operatorname{Var}(\\hat\\theta)/(\\operatorname{Var}(\\hat\\theta)+\\bar\\sigma^2_{\\text{post}})$",
    "$\\operatorname{E}_G[\\sigma^2_\\theta\\mathcal{J}/(\\sigma^2_\\theta\\mathcal{J}+1)]$; mirt computes $\\operatorname{E}_G[\\mathcal{J}/(\\mathcal{J}+\\sigma^2_\\theta)]$",
    "$\\sigma^2_\\theta\\bar{\\mathcal{J}}/(\\sigma^2_\\theta\\bar{\\mathcal{J}}+1)$",
    "$\\frac{n}{n-1}(1-\\sum_i V_i/V_t)$",
    "lower bound using squared covariances"),
  Scale = c("latent", "latent", "latent", "latent", "latent",
            "sum score", "sum score"),
  `Read from` = c(
    "manuscript sec. 2.1",
    "`TAM::WLErel`; `mirt::empirical_rxx(T_as_X = TRUE)`",
    "`mirt::empirical_rxx` (default)",
    "Andersson & Xin (2018, eq. 13), scale-adapted; `mirt::marginal_rxx` agrees only at $\\sigma^2_\\theta=1$",
    "`IRTsimrel::compute_rho_tilde`",
    "Cronbach (1951)",
    "Guttman (1945, sec. 13)"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_zoo, "T-reliability-zoo")

## ---- T-flexible-g : the genealogy of "let G be flexible" (ch. 14) --------
## Every row is characterized from the cited work's own stated aims and method.
T_flexg <- data.frame(
  Family = c(
    "Normal (baseline)",
    "Empirical histogram",
    "Spline density",
    "Ramsay curve",
    "Davidian curve",
    "Skew-normal / parametric",
    "Finite normal mixture",
    "Nonparametric deconvolution",
    "Dirichlet process mixture"),
  `Can represent` = c(
    "nothing beyond location and scale",
    "any shape on the quadrature grid",
    "smooth densities, any modality",
    "smooth densities, any modality",
    "smooth densities, any modality",
    "skew (and tails, family-dependent); **not** multimodality",
    "skew, multimodality, heavy tails",
    "any density, subject to smoothness",
    "skew, multimodality, heavy tails"),
  `Must be fixed in advance` = c(
    "—",
    "quadrature points",
    "knots and degree",
    "degree and knots (chosen by model selection)",
    "degree of the polynomial",
    "the parametric family",
    "the number of components, or a prior on it under an MFM",
    "smoothness / regularization",
    "concentration and base measure; the occupied partition count is induced"),
  `Small samples` = c(
    "most stable",
    "numerically least stable of the flexible options",
    "needs larger calibration samples",
    "needs larger calibration samples",
    "needs larger calibration samples",
    "few parameters, so relatively stable",
    "overfitting, weak occupancy, and label switching",
    "slow nonparametric rates",
    "partition and density can remain prior-sensitive"),
  `Posterior over G` = c(
    "no (MML)", "no (MML)", "no (MML)", "no (MML)",
    "no under MML; **yes** in the MCMC form",
    "depends on fitting method",
    "yes if fitted by MCMC",
    "no (point estimate with standard errors)",
    "**yes**"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_flexg, "T-flexible-g")

## ---- T-dp-representations : three views of one object (ch. 15) ----------
T_dprep <- data.frame(
  Representation = c("Ferguson's definition", "Stick-breaking",
                     "Pólya urn / Blackwell--MacQueen", "Chinese restaurant process"),
  `What it says` = c(
    "for every measurable partition, the vector of probabilities is Dirichlet with parameter $(\\alpha(A_1),\\dots,\\alpha(A_k))$",
    "$P=\\sum_n p_n \\delta_{Y_n}$ with $p_n=\\theta_n\\prod_{m<n}(1-\\theta_m)$, $\\theta_m\\sim\\mathrm{Beta}(1,\\alpha(\\mathcal{X}))$, $Y_n \\sim G_0$",
    "draw $X_n$ from $\\alpha+\\sum_{i<n}\\delta_{X_i}$, normalized; the empirical measure converges to a discrete $P$ distributed as Ferguson's",
    "customer $n$ joins an occupied table with probability proportional to its size, or a new one with probability proportional to $\\alpha(\\mathcal{X})$"),
  `What it makes obvious` = c(
    "conjugacy: the posterior is $\\mathrm{DP}(\\alpha+\\sum_i\\delta_{X_i})$",
    "**almost-sure discreteness**, and the size of the largest atom",
    "the predictive rule, hence exchangeability and the link to the CRP",
    "the induced partition, hence $K_J$ and Antoniak's law"),
  `What it hides` = c(
    "that draws are discrete; existence needs Kolmogorov",
    "the partition structure",
    "that $\\alpha$ is a measure and not a number, if written carelessly",
    "the random measure entirely — this is the level R2 objected to"),
  Source = c("Ferguson (1973)", "Sethuraman (1994)",
             "Blackwell \\& MacQueen (1973)", "induced; Antoniak (1974)"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_dprep, "T-dp-representations")

## ---- T-alpha-elicitation : source-specific, nonexclusive approaches -----
T_alpha <- data.frame(
  Source = c("Paganin et al. (2022)", "Antonelli et al. (2016)",
             "Lee et al. (2025)", "Murugiah \\& Sweeting (2012)",
             "Dorazio (2009); Rodríguez (2013)", "Vicentini \\& Jermyn (2025)",
             "Lee (2026)"),
  `What the source does` = c(
    "examines induced $\\operatorname{E}[K_N]$ and $\\operatorname{Var}(K_N)$; uses Gamma$(2,4)$ in simulation and Gamma$(1,3)$ for real data",
    "compares moment matching, diffuse Gamma, KL, fixed-$\\alpha$, empirical-Bayes, and importance-sampling strategies",
    "elicits a full distribution for $K$ via a chi-square construction and fits a Gamma prior by Dorazio's KL method",
    "develops hyperparameter selection with and without subjective information and supplies defaults",
    "elicits through induced cluster behaviour; derives an objective Ewens Jeffreys prior",
    "specifies $p(\\alpha\\mid\\eta)$ through prior information on meaningful induced quantities",
    "converts cluster-count beliefs into Gamma hyperpriors by two-stage moment matching; a dual-anchor protocol constrains cluster counts **and** weight concentration jointly"),
  `Target or diagnostic` = c(
    "mean and variance of occupied $K_N$",
    "several direct and induced criteria",
    "the complete elicited distribution of $K$",
    "subjective and default selection regimes",
    "partition behaviour or information geometry",
    "the induced quantity chosen by the analyst",
    "$\\operatorname{E}[K_J]$ and the largest-weight exceedance probability, as a pair"),
  `Important limitation` = c(
    "the paper does not recommend one Gamma prior reused in every setting",
    "the compared strategies are alternatives within one paper, not one diffuse stance",
    "reducing the method to matching $\\operatorname{E}[K]$ omits its defining step",
    "reducing the framework to sensitivity analysis omits its selection method and defaults",
    "Rodríguez's prior is proper but has **no finite moments**",
    "matching one induced quantity can leave others extreme",
    "the two anchors can conflict, and the protocol resolves the conflict by explicit trade rather than dissolving it"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_alpha, "T-alpha-elicitation")

## ---- T-goals : three goals, three losses, three Bayes actions (ch. 17) ----
T_goals <- data.frame(
  Goal = c("The individual", "The ranks", "The distribution"),
  Estimand = c("$\\theta_p$, person by person",
               "$R_p=\\sum_q \\mathbb{1}\\{\\theta_q\\le\\theta_p\\}$",
               "$G_N(t)=N^{-1}\\sum_p \\mathbb{1}\\{\\theta_p\\le t\\}$, the **realized** EDF"),
  Loss = c("$\\sum_p w_p(a_p-\\theta_p)^2$ (WSEL)",
           "squared error on ranks",
           "$\\int\\{A(t)-G_N(t)\\}^2\\,dt$ (ISEL)"),
  `Bayes action` = c("posterior mean $\\eta_p$",
                     "$\\hat R_p$, the discretized $\\operatorname{E}[R_p\\mid\\mathbf u]$",
                     "$\\hat G_N$, discrete with mass $1/N$"),
  `Decision it serves` = c("person-specific point reports under numeric error costs",
                           "rank or percentile reporting; a published ordering",
                           "realized-ensemble proportions, spread, modality"),
  `What it gets wrong elsewhere` = c(
    "the ensemble can be under-dispersed; the exact $\\sqrt{\\bar w}$ ratio needs the constant-error Gaussian model",
    "it is not a threshold-classification rule; rank values still carry uncertainty",
    "the induced person values are generally not WSEL-optimal"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_goals, "T-goals")

## ---- T-three-goals-counterexample : correct ISEL action (ch. 17) ---------
## Shen & Louis Theorem 1: for K = 2 the equal-mass ISEL action uses the
## .25 and .75 quantiles of Gbar, not posterior order-statistic expectations.
cx_eta <- c(-0.6, 0.6)
cx_var <- c(0.5, 0.2)
cx_Gbar <- function(t)
  0.5 * pnorm(t, cx_eta[1], sqrt(cx_var[1])) +
  0.5 * pnorm(t, cx_eta[2], sqrt(cx_var[2]))
cx_quant <- vapply(c(0.25, 0.75), function(prob)
  uniroot(function(t) cx_Gbar(t) - prob, c(-10, 10), tol = 1e-13)$root,
  numeric(1))
cx_s <- sqrt(sum(cx_var))
cx_d <- (cx_eta[1] - cx_eta[2]) / cx_s
cx_max <- cx_eta[1] * pnorm(cx_d) + cx_eta[2] * pnorm(-cx_d) + cx_s * dnorm(cx_d)
cx_order <- c(sum(cx_eta) - cx_max, cx_max)
cx_isel <- function(atoms) integrate(function(t) {
  action_cdf <- (t >= atoms[1]) / 2 + (t >= atoms[2]) / 2
  (action_cdf - cx_Gbar(t))^2
}, lower = -Inf, upper = Inf, subdivisions = 2000L,
rel.tol = 1e-11, stop.on.error = TRUE)$value
cx_actions <- list(
  `Posterior means (WSEL optimum)` = cx_eta,
  `Midpoint quantiles (ISEL optimum)` = cx_quant,
  `Order-statistic means (not ISEL-optimal)` = cx_order)
T_three_goals_counterexample <- do.call(rbind, lapply(names(cx_actions), function(nm) {
  z <- cx_actions[[nm]]
  data.frame(
    Action = nm,
    `Lower atom` = z[1],
    `Upper atom` = z[2],
    Spread = diff(z),
    `Excess WSEL` = sum((z - cx_eta)^2),
    ISEL = cx_isel(z),
    check.names = FALSE, stringsAsFactors = FALSE)
}))
save_tbl(T_three_goals_counterexample, "T-three-goals-counterexample")

## ---- T-gr-worked : the GR construction, worked exactly (ch. 19) ----------
## Eight units, independent normal posteriors. Everything below is computed,
## not transcribed: Gbar by eq. (3), Uhat by eq. (4), posterior ranks by eq. (1).
gr_eta <- c(-1.6, -0.9, -0.4, -0.1, 0.2, 0.6, 1.1, 1.8)
gr_lam <- c(0.50, 0.40, 0.35, 0.30, 0.30, 0.35, 0.45, 0.60)
gr_K   <- length(gr_eta)
gr_grid <- seq(-6, 6, length.out = 6001)
gr_Gbar <- vapply(gr_grid, function(t) mean(pnorm(t, gr_eta, sqrt(gr_lam))), numeric(1))
gr_Uhat <- approxfun(gr_Gbar, gr_grid, ties = "ordered", rule = 2)((2 * (1:gr_K) - 1) / (2 * gr_K))
gr_Rbar <- vapply(seq_len(gr_K), function(k) sum(vapply(seq_len(gr_K), function(q)
  if (q == k) 1 else pnorm(0, gr_eta[q] - gr_eta[k], sqrt(gr_lam[k] + gr_lam[q])),
  numeric(1))), numeric(1))
if (anyDuplicated(round(gr_Rbar, 12)))
  stop("T-gr-worked requires distinct expected ranks; exact ties need randomized refinement")
gr_Rhat <- rank(gr_Rbar, ties.method = "min")
gr_a <- sqrt(1 + mean(gr_lam) / (sum((gr_eta - mean(gr_eta))^2) / (gr_K - 1)))
T_gr <- data.frame(
  `Posterior mean $\\eta_p$` = sprintf("%.3f", gr_eta),
  `Posterior var $v_p$` = sprintf("%.2f", gr_lam),
  `Posterior rank $\\bar R_p$` = sprintf("%.2f", gr_Rbar),
  `Discretized $\\hat R_p$` = gr_Rhat,
  `Mass point $\\hat U_j$` = sprintf("%.3f", gr_Uhat),
  `GR estimate` = sprintf("%.3f", gr_Uhat[gr_Rhat]),
  `CB estimate` = sprintf("%.3f", mean(gr_eta) + gr_a * (gr_eta - mean(gr_eta))),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_gr, "T-gr-worked")

## ---- T-corrections : the revision's worklist (Appendix C) ----------------
## Generated from manifest/corrections.csv so the appendix cannot drift from
## the register. Ordered by severity, then by chapter.
cx <- read.csv(file.path(paths$manifest, "corrections.csv"),
               stringsAsFactors = FALSE, check.names = FALSE)
sev_order <- c(moderate = 1L, minor = 2L, none = 3L)
if (any(is.na(sev_order[cx$severity])))
  stop("T-corrections: unknown severity in corrections.csv: ",
       paste(setdiff(cx$severity, names(sev_order)), collapse = ", "))
cx <- cx[order(sev_order[cx$severity], suppressWarnings(as.integer(cx$chapter)), cx$id), ]
T_corrections <- data.frame(
  ID = cx$id,
  Severity = cx$severity,
  `Where in the manuscript` = cx$locus,
  `What it says` = cx$what_the_manuscript_says,
  `Why that is wrong` = cx$why_wrong,
  `What the revision should do` = cx$fix_in,
  `Source consulted` = cx$source_locator,
  `Author ruling` = ifelse(nzchar(cx$ruled_on),
                           paste0(cx$ruling, " (ruled ", cx$ruled_on, ")"),
                           cx$ruling),
  `Book chapter` = cx$chapter,
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_corrections, "T-corrections")

## A compact index, for the appendix's opening table.
T_corrections_index <- data.frame(
  ID = cx$id,
  Severity = cx$severity,
  `Where` = cx$locus,
  `Book chapter` = paste0("ch. ", cx$chapter),
  `Ruled` = ifelse(nzchar(cx$ruled_on), cx$ruled_on, "—"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_corrections_index, "T-corrections-index")

## ---- T-lee-delineation : this work against Lee et al. (2025) (ch. 21) ----
## Every cell in the Lee et al. column is read from the source at the locator
## given in manifest/part-vii-source-receipts.csv, not from the manuscript's or
## a reviewer's summary of it.
T_lee <- data.frame(
  Dimension = c(
    "Unit",
    "Parameter",
    "Level-1 data the model consumes",
    "Within-unit error",
    "Error variance and the parameter",
    "Nuisance parameters estimated jointly",
    "Scale of the parameter",
    "Reliability",
    "Reliability actually reached",
    "Inferential goals",
    "Prior arms compared",
    "Summary methods compared"),
  `Lee et al. (2025)` = c(
    "Site in a multisite trial; $J = 25, 50, 75, 100, 300$ simulated, $J = 38$ in the application",
    "Site average treatment effect $\\tau_j$",
    "Two summary statistics per site, $\\hat\\tau_j$ and $\\widehat{se}^2_j$, from site $j$'s own sample",
    "Gaussian plug-in first stage: $\\widehat{se}^2_j$ is estimated, then treated as **known**, $\\hat\\tau_j \\mid \\tau_j \\sim N(\\tau_j, \\widehat{se}^2_j)$",
    "Assumed unrelated; zero correlation between $\\tau_j$ and $\\widehat{se}^2_j$ is named as a key limitation",
    "None; the standard errors are plugged in",
    "The outcome's own scale (effect size, or log odds ratio converted to Cohen's $d$)",
    "Informativeness $I = \\sigma^2/(\\sigma^2 + \\text{geometric mean of } \\widehat{se}^2_j)$; set by the average site size and by $\\sigma$, and prospective design advice is given",
    "$I \\in [.01, .71]$, mean $.25$; the application sits at $I = .04$ and $.06$",
    "Three: individual effects, ranks, and the EDF",
    "Gaussian; DP-diffuse; DP-inform",
    "PM; CB; GR"),
  `This work` = c(
    "Person taking a test",
    "Latent trait $\\theta_p$",
    "The full vector of **binary item responses**",
    "No known error variance exists; precision is the Fisher information $\\mathcal{J}(\\theta_p)$",
    "**A function of the parameter by construction**, so shrinkage is heteroscedastic",
    "Item parameters $\\boldsymbol\\beta$, and under the 2PL the discriminations too",
    "Identified only up to a location constraint; under the 2PL, location and scale",
    "$\\bar w$; set by test length, which is the instrument the test developer controls",
    "Varied prospectively through test length; not numerically comparable with $I$ without an explicit design mapping",
    "The same three, plus **classification against a cut score**",
    "Gaussian; DP with a broad $\\alpha$ prior; DP with a $K$-matched $\\alpha$ prior",
    "PM; CB; GR"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_lee, "T-lee-delineation")

## ---- T-lee-transfer : what carries over, and what does not (ch. 21) ------
T_transfer <- data.frame(
  Element = c(
    "Three inferential goals and their losses",
    "Constrained Bayes",
    "Triple-goal (GR)",
    "$K$-matched $\\alpha$ elicitation",
    "DP prior on the unit-level distribution",
    "Reliability as the governing quantity",
    "Heteroscedastic shrinkage",
    "Semiparametric identification of $G$",
    "Classification against a cut score"),
  Origin = c(
    "Shen and Louis (1998)",
    "Ghosh (1992)",
    "Shen and Louis (1998)",
    "Lee et al. (2025), building on Dorazio (2009)",
    "Ferguson (1973); Lo (1984); used for IRT by Duncan and MacEachern (2008) and others",
    "Lee et al. (2025)",
    "The IRT response likelihood and its trait-dependent information",
    "San Mart\u00edn et al. (2011)",
    "Ghosh (1992), with related threshold and classification precedents"),
  `Status here` = c(
    "Transfers unchanged; the attribution belongs to Shen and Louis, and Lee et al. make it",
    "Transfers unchanged",
    "Transfers unchanged",
    "Transfers; the mechanism is theirs and is used here",
    "Transfers; the placement on a latent trait is older than either paper",
    "Transfers as the point of contact; the design-to-information mapping differs by setting",
    "An IRT-specific consequence of the response likelihood, not new general machinery",
    "Restated; no analogue arises in the multisite setting",
    "Added as an explicit fourth target in this comparison; not claimed as a new decision problem"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_transfer, "T-lee-transfer")

## ---- T-extensions : does the argument survive outside Rasch? (ch. 22) ----
## Each row is read from the primary source at the locator in
## manifest/part-vii-source-receipts.csv. "Open" means this book found no
## answer in what it has read, not that none exists.
T_ext <- data.frame(
  `Model family` = c(
    "Rasch (1PL)",
    "2PL",
    "Partial credit (PCM)",
    "Generalized partial credit (GPCM)",
    "Graded response (GRM)",
    "MIRT",
    "Restricted latent class (CDM/DCM)"),
  `Sufficient statistic for the person` = c(
    "Yes --- the number correct",
    "No --- the weighted score depends on the unknown discriminations",
    "**Yes** --- the total count of completed steps",
    "No --- the slope $a_j$ enters the exponent",
    "**No** --- the form prevents separation even with no discrimination parameter",
    "A vector score, only under equal discriminations within dimension",
    "Not applicable --- the latent variable is a discrete profile"),
  `Distribution-free item calibration` = c(
    "CML", "None", "CML", "None", "None", "None",
    "Not applicable"),
  `What must be constrained` = c(
    "Location",
    "Location and scale",
    "Location",
    "Location and scale",
    "Location and scale",
    "Location, scale, and rotation",
    "The design matrix, not the scale"),
  `Shrinkage and summary arguments` = c(
    "Hold", "Hold", "Hold", "Hold", "Hold",
    "Coordinatewise precedents exist; a joint transformation-aware action remains open",
    "A bounded discrete analogue exists, but the estimands and actions differ"),
  `Do CB and GR apply?` = c(
    "Yes", "Yes", "Yes", "Yes", "Yes",
    "Coordinatewise **yes**; a canonical joint action is open",
    "No direct unordered-profile action supplied; an analogue is open"),
  `What is identified about the latent distribution` = c(
    "$I+1$ functionals of $G$ from $I$ items",
    "Open --- the semiparametric 2PL is unsolved",
    "Open in this book",
    "Open in this book",
    "Open in this book",
    "Open; recovery of item--trait structure generally degrades under non-normality, with method-specific exceptions",
    "Known items: grouped equivalence-class masses; unknown items: only under C1/C2 or adjusted-$\\Gamma$ conditions"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_ext, "T-extensions")

## ---- T-open-problems : what this book does not settle (ch. 23) ----------
## "Blocked on" says whose problem it is: this book could have answered it and
## did not, the companion simulation study owns it, or the field has no answer.
T_open <- data.frame(
  ID = sprintf("OP-%02d", 1:9),
  Problem = c(
    "Finite-$N$ behaviour of the DPM posterior for $G$ under weak identification",
    "Whether the rank proposition holds outside its stated conditions",
    "How far the incompatibility theorem generalizes",
    "A joint, transformation-aware multivariate CB/TG action",
    "What a finite polytomous test identifies about $G$",
    "How location/scale conventions transform priors on interpretable functionals of $G$",
    "What the normal working model costs at $I < 15$",
    "Whether the sum-score fallacy changes a published conclusion",
    "How much survives a misspecified *measurement* model"),
  `Where it arose` = c(
    "ch. 16", "ch. 20", "ch. 17", "ch. 18, 19, 22", "ch. 22",
    "ch. 4, 15, 16", "ch. 11", "ch. 13", "ch. 3, 22"),
  `What is known` = c(
    "Under the anchored Rasch conditions the observable law is generated by the $I+1$ displayed integral evaluations of $G$, subject to their identities; guarantees for flexible priors are asymptotic",
    "Reliability dominates and the shape residual is measurable; on the displayed grid it peaks near $\\bar w = 0.915$ and declines",
    "An existence proof on two overlapping normals; the degenerate coincident case is exhibited",
    "Coordinatewise multivariate CB/TG exists; a canonical joint loss, ordering, or EDF action is not supplied",
    "The PCM keeps sufficiency and the GRM does not; the semiparametric question is not posed here",
    "Item centering and common atom translations leave $K$ unchanged; other interpretable functionals can depend on location/scale convention",
    "The exact posterior is computable; the working model is an approximation whose error is unquantified here",
    "Sum scores can be unimodal under a bimodal $G$ (TV $= 0.035$ on the worked case)",
    "Every result here assumes the item model is correct"),
  `Blocked on` = c(
    "The field", "This book", "This book", "The field", "This book",
    "This book", "This book", "A literature audit", "The field"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_open, "T-open-problems")

## ---- T-proof-index : claim-level proof coverage (App. A) -----------------
## The result register defines the claims; proof-inventory.csv independently
## classifies their coverage.  Exact set parity prevents either surface drifting.
rrp <- read.csv(file.path(paths$manifest, "result-register.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
pinv <- read.csv(file.path(paths$manifest, "proof-inventory.csv"),
                 stringsAsFactors = FALSE, check.names = FALSE)
need_pinv <- c("id", "proof_location", "proof_status", "coverage_note")
stopifnot(all(need_pinv %in% names(pinv)), !anyDuplicated(pinv$id),
          setequal(pinv$id, rrp$id),
          all(pinv$proof_status %in% c("proved-here", "partial-derivation", "source-only")),
          all(nzchar(pinv$proof_location)), all(nzchar(pinv$coverage_note)))
pinv <- pinv[match(rrp$id, pinv$id), ]
T_proof_index <- data.frame(
  ID = rrp$id,
  Result = rrp$label,
  Provenance = rrp$provenance,
  `Proof` = pinv$proof_location,
  `Proof status` = pinv$proof_status,
  Coverage = pinv$coverage_note,
  `Source locator` = ifelse(nzchar(rrp$source_locator) & !is.na(rrp$source_locator),
                            rrp$source_locator, "—"),
  check.names = FALSE, stringsAsFactors = FALSE)
T_proof_index <- T_proof_index[order(match(T_proof_index$`Proof status`,
  c("proved-here", "partial-derivation", "source-only")), T_proof_index$ID), ]
save_tbl(T_proof_index, "T-proof-index")

## ---- T-crosswalk : this book's symbols against six sources (App. B) ------
## Every cell is read at the locator in manifest/part-vii-source-receipts.csv.
## "--" means the source has no symbol for the concept, not that one was missed.
T_crosswalk <- data.frame(
  Concept = c(
    "Person index", "Item index", "Latent trait / ability",
    "Item difficulty", "Item discrimination", "Response variable",
    "Latent distribution", "Posterior mean of the trait",
    "Posterior variance of the trait", "Number of items",
    "Number of persons or units", "DP concentration",
    "Reliability of the unit-level estimate", "Slope-intercept form"),
  `This book` = c(
    "$p$", "$i$", "$\\theta_p$", "$\\beta_i$", "$\\lambda_i$", "$U_{pi}$, $u_{pi}$",
    "$G$", "$\\eta_p$", "$v_p$", "$I$", "$P$", "$\\alpha$", "$\\bar w$", "--"),
  `Debelak et al. (2022)` = c(
    "$p$", "$i$", "$\\theta_p$", "$\\beta_i$", "--", "$U_{pi}$",
    "--", "--", "--", "--", "--", "--", "--", "--"),
  `Fox (2010)` = c(
    "**$i$**", "**$k$**", "$\\theta_i$", "in $\\boldsymbol\\xi$", "in $\\boldsymbol\\xi$",
    "$Y_{ik}$", "$p(\\theta_i \\mid \\boldsymbol\\theta_P)$", "--", "--", "--", "$N$",
    "--", "--", "--"),
  `Baker & Kim (2004)` = c(
    "$j$", "$i$", "$\\theta_j$", "$\\beta_i$", "**$\\alpha_i$**", "--",
    "--", "--", "--", "$n$", "$N$", "--", "--", "$\\zeta_i = -\\alpha_i\\beta_i$"),
  `Paganin et al. (2022)` = c(
    "$j$", "$i$", "**$\\eta_j$**", "$\\beta_i$", "$\\lambda_i$", "$y_{ij}$",
    "$G$", "--", "--", "$I$", "$N$", "$\\alpha$", "--",
    "$\\gamma_i = -\\lambda_i\\beta_i$"),
  `Lee et al. (2025)` = c(
    "$j$ (site)", "--", "$\\tau_j$", "--", "--", "$\\hat\\tau_j$",
    "$G$", "$\\tau^*_j$", "$V_j$", "--", "$J$", "$\\alpha$", "**$I$**", "--"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_crosswalk, "T-crosswalk")

## ---- T-collisions : the symbols that mean different things (App. B) ------
T_collisions <- data.frame(
  Symbol = c("$I$", "$\\eta$", "$\\alpha$", "$i$ and $k$", "$\\lambda_p$"),
  `In this book` = c(
    "number of items", "posterior mean of $\\theta_p$",
    "Dirichlet process concentration", "$i$ indexes items; persons are $p$",
    "not used; discrimination is $\\lambda_i$"),
  `Elsewhere` = c(
    "Lee et al.'s **informativeness**, an average reliability in $[0,1]$",
    "Paganin et al.'s **latent ability**, this book's $\\theta_p$",
    "Baker and Kim's **item discrimination** $\\alpha_i$",
    "Fox indexes **persons** by $i$ and **items** by $k$ --- both reversed",
    "Shen and Louis's **posterior variance**, this book's $v_p$"),
  `Why it matters` = c(
    "Both are central and both appear in @sec-ch21; a reader moving between the two papers can read a reliability as a test length",
    "@sec-ch11 and Part VI use $\\eta_p$ for the posterior mean on every page",
    "The DP literature and the 2PL literature both reach for $\\alpha$; @sec-ch15 and @sec-ch16 use it only for concentration",
    "Every formula in Fox's ch. 2 transposes against this book's",
    "The collision that forced this book's one notation departure (@sec-ch02)"),
  check.names = FALSE, stringsAsFactors = FALSE)
save_tbl(T_collisions, "T-collisions")

## ---- T-acquisition-* : live source-debt ledger (App. E) -----------------
## refs/acquisition-ledger.csv is authoritative.  Appendix E displays it; the
## generated table adds only stable source-line locators and presentation names.
acq_file <- file.path(paths$refs, "acquisition-ledger.csv")
acq <- read.csv(acq_file, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                check.names = FALSE)
acq_need <- c("key", "work", "year", "needed_by_chapter", "criticality",
              "status", "r5_route", "owner", "date")
stopifnot(identical(names(acq), acq_need), nrow(acq) == 33L,
          !anyDuplicated(acq$key), all(!is.na(acq$key) & nzchar(acq$key)),
          all(!is.na(acq$work) & nzchar(acq$work)),
          all(!is.na(acq$status) & nzchar(acq$status)),
          all(!is.na(acq$owner) & nzchar(acq$owner)))
shown <- acq
shown[is.na(shown)] <- ""
T_acquisition <- data.frame(
  Key = shown$key,
  Work = shown$work,
  Year = shown$year,
  `Needed by chapter` = shown$needed_by_chapter,
  Criticality = shown$criticality,
  Status = shown$status,
  `R5 route` = shown$r5_route,
  Owner = shown$owner,
  Date = shown$date,
  Locator = sprintf("refs/acquisition-ledger.csv:%d", seq_len(nrow(shown)) + 1L),
  check.names = FALSE, stringsAsFactors = FALSE)
T_acquisition_debt <- rbind(
  data.frame(Dimension = "Status", Category = names(table(acq$status)),
             Entries = as.integer(table(acq$status))),
  data.frame(Dimension = "Criticality", Category = names(table(acq$criticality)),
             Entries = as.integer(table(acq$criticality))))
save_tbl(T_acquisition, "T-acquisition-ledger")
save_tbl(T_acquisition_debt, "T-acquisition-debt")
