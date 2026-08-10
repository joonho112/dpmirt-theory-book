## 23-figures-v2-theory.R - computed theory illustrations added in v2.
## Everything here is exact quadrature or a seeded illustration; captions say
## which. Chapters embed; they never plot.

source("code/R/00-paths.R")
source("code/R/21-style-v2.R")
FIG <- paths$figures
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

save_figure_data <- function(obj, id) {
  saveRDS(obj, file.path(paths$tables, paste0(id, ".rds")))
  write.csv(obj, file.path(paths$tables, "supplement", paste0(id, ".csv")),
            row.names = FALSE, na = "")
}

## shared quadrature grid
TH <- seq(-6, 6, length.out = 601)
WG <- dnorm(TH); WG <- WG / sum(WG)

pbin_by_score <- function(pmat) {
  ## pmat: items x length(TH) success probabilities -> (I+1) x length(TH)
  I <- nrow(pmat)
  out <- matrix(0, I + 1, ncol(pmat))
  out[1, ] <- 1
  for (i in seq_len(I)) {
    p <- pmat[i, ]
    new <- out * 0
    new[1, ] <- out[1, ] * (1 - p)
    for (r in 2:(i + 1)) new[r, ] <- out[r, ] * (1 - p) + out[r - 1, ] * p
    out <- new
  }
  out
}

info_fun <- function(lambda, beta) {
  ## J(theta) on TH for a 2PL test
  p <- plogis(outer(lambda, TH) - lambda * beta)  ## items x TH via recycling
  colSums(lambda^2 * p * (1 - p))
}
## note: outer(lambda, TH) - lambda*beta gives lambda_i*(TH_j - beta_i) columnwise

## ---- F-dp-draws : what a DP draw, and a DPM draw, look like ---------------
set.seed(415)
stickdraw <- function(alpha, n_stick = 500) {
  stopifnot(length(alpha) == 1L, is.finite(alpha), alpha > 0,
            length(n_stick) == 1L, n_stick >= 2)
  ## Completed finite blocked representation: the final break receives all
  ## residual mass, so every plotted object is a probability distribution.
  v <- c(rbeta(n_stick - 1L, 1, alpha), 1)
  w <- numeric(n_stick)
  remaining <- 1
  for (j in seq_len(n_stick)) {
    w[j] <- v[j] * remaining
    remaining <- remaining * (1 - v[j])
  }
  z <- rnorm(n_stick)
  residual_before <- c(1, 1 - cumsum(w[-n_stick]))
  stopifnot(all(is.finite(w)), all(w >= 0),
            abs(w[1] - v[1]) < 1e-15,
            max(abs(w - v * residual_before)) < 1e-12,
            abs(remaining) < 1e-15, abs(sum(w) - 1) < 1e-12)
  list(w = w, z = z, residual_at_last = w[n_stick])
}
xs <- seq(-3.4, 3.4, length.out = 341)
cdf_rows <- list(); den_rows <- list(); dp_summary_rows <- list()
for (al in c(1, 10, 100)) {
  for (d in 1:22) {
    s <- stickdraw(al)
    cdf_rows[[length(cdf_rows) + 1]] <- data.frame(
      alpha = al, draw = d, x = xs,
      y = colSums(s$w * outer(s$z, xs, "<=")))
    dp_summary_rows[[length(dp_summary_rows) + 1]] <- data.frame(
      panel = "DP CDF", alpha = al, draw = d, n_stick = length(s$w),
      weight_sum = sum(s$w), residual_at_last = s$residual_at_last,
      n_kernel_sigma_gt_3 = NA_integer_, max_kernel_sigma = NA_real_,
      rows_share_draws = FALSE)
  }
  for (d in 1:9) {
    ## DPM of normals with the package-default base measure of ch. 16:
    ## mu ~ N(0, 2), sigma^2 ~ Inv-Gamma(2.01, 1.01)
    s <- stickdraw(al, 300)
    mu <- rnorm(300, 0, sqrt(2))
    sg <- sqrt(1 / rgamma(300, 2.01, rate = 1.01))
    f <- sapply(xs, function(x) sum(s$w * dnorm(x, mu, sg)))
    den_rows[[length(den_rows) + 1]] <- data.frame(
      alpha = al, draw = d, x = xs, y = f)
    dp_summary_rows[[length(dp_summary_rows) + 1]] <- data.frame(
      panel = "DPM density", alpha = al, draw = d, n_stick = length(s$w),
      weight_sum = sum(s$w), residual_at_last = s$residual_at_last,
      n_kernel_sigma_gt_3 = sum(sg > 3), max_kernel_sigma = max(sg),
      rows_share_draws = FALSE)
  }
}
cdfs <- do.call(rbind, cdf_rows); dens <- do.call(rbind, den_rows)
dp_summary <- do.call(rbind, dp_summary_rows)
stopifnot(max(abs(dp_summary$weight_sum - 1)) < 1e-12,
          all(cdfs$y >= -1e-12 & cdfs$y <= 1 + 1e-12),
          all(is.finite(dens$y) & dens$y >= 0),
          any(dp_summary$n_kernel_sigma_gt_3 > 0, na.rm = TRUE),
          !any(dp_summary$rows_share_draws))
save_figure_data(dp_summary, "F-dp-draws-summary")
cdfs$alpha_f <- factor(cdfs$alpha, labels = c("alpha == 1", "alpha == 10", "alpha == 100"))
dens$alpha_f <- factor(dens$alpha, labels = c("alpha == 1", "alpha == 10", "alpha == 100"))
pa <- ggplot(cdfs, aes(x, y, group = draw)) +
  geom_step(colour = PAL[1], alpha = 0.30, linewidth = 0.3) +
  stat_function(fun = pnorm, inherit.aes = FALSE, colour = INK,
                linetype = "22", linewidth = 0.5) +
  facet_wrap(~alpha_f, nrow = 1, labeller = label_parsed) +
  labs(x = NULL, y = "draws of G", subtitle = "Draws from the Dirichlet process are discrete distributions around the base measure (dashed)") +
  theory_theme(10.5) + theme(axis.text.x = element_blank())
pb <- ggplot(dens, aes(x, y, group = draw)) +
  geom_line(colour = PAL[2], alpha = 0.55, linewidth = 0.35) +
  facet_wrap(~alpha_f, nrow = 1, labeller = label_parsed) +
  labs(x = expression(theta), y = "draws of the DPM density",
       subtitle = "Independent DPM draws: completed sticks mixed through normal kernels") +
  theory_theme(10.5) + theme(strip.text = element_blank())
p <- pa / pb
save_fig2("F-dp-draws",
  paste("**What the prior actually says G might be.** Top row: twenty-two draws",
        "of $G$ from $\\mathrm{DP}(\\alpha, N(0,1))$ by completed finite",
        "stick-breaking",
        "at three concentrations, against the base measure (dashed). Every draw",
        "is discrete; small $\\alpha$ concentrates the mass on a few atoms, large",
        "$\\alpha$ scatters it until the draws hug the base. Bottom row: nine",
        "independent draws of the DPM density of @eq-dpm-irt, mixing normal kernels",
        "over the",
        "package-default base measure of @sec-ch16-model ($\\mu \\sim N(0,2)$,",
        "$\\sigma^2 \\sim$ Inv-Gamma(2.01, 1.01)); the kernel turns discrete draws",
        "into continuous shapes, and the same concentration now governs how many",
        "effective components a shape carries. The two rows do not reuse the same",
        "atoms or weights. The final stick receives all residual mass (500 atoms",
        "above, 300 below), every weight vector sums to one, and kernel scales are",
        "drawn without clipping; values above 3 are retained. Seeded illustration;",
        "mass diagnostics are frozen in tables/F-dp-draws-summary.rds. Generated by",
        "code/R/23-figures-v2-theory.R."),
  p, w = 8.6, h = 5.2)

## ---- F-cb-affine : what an affine repair can and cannot reach -------------
## Genuine chapter-defined CB in the independent-posterior population limit.
## Standardize the generating mixture to Var(G)=1, then set error variance to
## one: both signal reliability and the normal-working-prior weight are 0.5.
cb_tier <- 0.5
m_raw <- 1
s_raw <- 0.35
signal_sd <- sqrt(m_raw^2 + s_raw^2)
m_g <- m_raw / signal_sd
s_g <- s_raw / signal_sd
signal_var <- m_g^2 + s_g^2
error_var <- signal_var * (1 - cb_tier) / cb_tier
working_prior_var <- 1
working_weight <- working_prior_var / (working_prior_var + error_var)
posterior_var <- 1 / (1 / working_prior_var + 1 / error_var)
signal_reliability <- signal_var / (signal_var + error_var)

mix_d <- function(x, m = m_g, s = s_g) {
  0.5 * dnorm(x, -m, s) + 0.5 * dnorm(x, m, s)
}
s_y <- sqrt(s_g^2 + error_var)
f_y <- function(x) 0.5 * dnorm(x, -m_g, s_y) + 0.5 * dnorm(x, m_g, s_y)
f_pm <- function(x) f_y(x / working_weight) / working_weight
var_pm <- working_weight^2 * (signal_var + error_var)
cb_factor <- sqrt(1 + posterior_var / var_pm)
var_cb <- cb_factor^2 * var_pm
f_cb <- function(x) f_pm(x / cb_factor) / cb_factor
xs <- seq(-3.2, 3.2, length.out = 641)
df <- rbind(
  data.frame(x = xs, y = mix_d(xs), what = "true G (bimodal)"),
  data.frame(x = xs, y = f_pm(xs), what = "posterior-mean ensemble"),
  data.frame(x = xs, y = f_cb(xs), what = "constrained-Bayes ensemble")
)
df$what <- factor(df$what, levels = unique(df$what))
mode_count <- function(y) sum(diff(sign(diff(y))) < 0)
n_mode_true <- mode_count(mix_d(xs))
n_mode_pm <- mode_count(f_pm(xs))
n_mode_cb <- mode_count(f_cb(xs))
true_mode <- optimize(function(x) -mix_d(x), interval = c(0, 1.5))$minimum
stopifnot(abs(signal_var - 1) < 1e-12,
          abs(signal_reliability - cb_tier) < 1e-12,
          abs(working_weight - cb_tier) < 1e-12,
          abs(posterior_var - 0.5) < 1e-12,
          abs(var_pm - 0.5) < 1e-12,
          abs(cb_factor - sqrt(2)) < 1e-12,
          abs(var_cb - 1) < 1e-12,
          identical(c(n_mode_true, n_mode_pm, n_mode_cb), c(2L, 1L, 1L)))
save_figure_data(
  data.frame(
    construction = "chapter CB; independent-posterior population limit",
    signal_mean = 0, signal_variance = signal_var,
    component_location = m_g, component_sd = s_g,
    measurement_error_variance = error_var,
    signal_reliability = signal_reliability,
    working_prior_variance = working_prior_var,
    posterior_weight = working_weight,
    posterior_variance = posterior_var,
    posterior_mean_ensemble_variance = var_pm,
    cb_factor = cb_factor, cb_target_variance = var_pm + posterior_var,
    cb_ensemble_variance = var_cb,
    true_modes = n_mode_true, posterior_mean_modes = n_mode_pm,
    cb_modes = n_mode_cb),
  "F-cb-affine-summary")
p <- ggplot(df, aes(x, y, colour = what, linetype = what)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(INK, PAL[1], PAL[2]), name = NULL) +
  scale_linetype_manual(values = c("solid", "solid", "42"), name = NULL) +
  annotate("segment", x = c(-true_mode, true_mode),
           xend = c(-true_mode, true_mode), y = -0.015, yend = 0.012,
           colour = INK, linewidth = 0.5) +
  annotate("text", x = 0, y = max(df$y) * 1.08,
           label = "genuine CB: a = sqrt(2)\ntarget variance restored; shape unchanged",
           size = 3.1, colour = INK2, family = "Helvetica", lineheight = 1.05) +
  labs(x = expression(theta), y = "density",
       subtitle = "Standardized Var(G) = 1; signal reliability = 0.500") +
  theory_theme(11)
save_fig2("F-cb-affine",
  paste("**Genuine constrained Bayes restores the target variance, and only",
        "that.** This is the independent-posterior population-limit construction",
        "of @eq-cb-a, not an oracle variance match. The symmetric bimodal",
        "population is standardized to mean zero and variance one; measurement",
        "error variance one makes the signal reliability and normal-working-prior",
        "posterior weight exactly 0.500. The posterior variance and posterior-mean",
        "ensemble variance are each 0.500, so the chapter-defined CB factor is",
        "$a=\\sqrt{1+0.5/0.5}=\\sqrt{2}$. The posterior-mean ensemble is already",
        "unimodal. CB is a positive affine map of it (@prp-cb-affine), so it",
        "matches the target variance one while retaining that wrong shape; the",
        "two marked locations are the true mixture modes. Reaching the shape",
        "requires @sec-ch19. All curves are closed-form; values and assertions are",
        "frozen in tables/F-cb-affine-summary.rds. Generated by",
        "code/R/23-figures-v2-theory.R."),
  p, w = 7.6, h = 4.3)

## ---- F-shrink-deviation : the sqrt(rho) rule against exact Rasch EAP ------
## Exact score-class computation: for each design, the EAP ensemble SD ratio
## (which equals the square root of the EAP empirical reliability) against the
## constant-error prediction sqrt(rho-bar).
designs <- expand.grid(I = c(5, 8, 12, 18, 27, 40, 60), shift = c(0, 1.5))
rows <- lapply(seq_len(nrow(designs)), function(k) {
  I <- designs$I[k]; sh <- designs$shift[k]
  beta <- qnorm(ppoints(I)) + sh
  pmat <- plogis(outer(-beta, TH, "+"))          ## items x TH, lambda = 1
  J <- colSums(pmat * (1 - pmat))
  rho_bar <- 1 / (1 + sum(WG / J))
  S <- pbin_by_score(pmat)                        ## (I+1) x TH
  pr <- as.numeric(S %*% WG)
  eap <- as.numeric(S %*% (WG * TH)) / pr
  m <- sum(pr * eap)
  ratio <- sqrt(sum(pr * (eap - m)^2))            ## sd of ensemble; Var(theta)=1
  data.frame(I = I, shift = sh, rho_bar = rho_bar, ratio = ratio)
})
sd_df <- do.call(rbind, rows)
sd_df$design <- ifelse(sd_df$shift == 0, "targeted items", "mistargeted items (+1.5)")
save_figure_data(sd_df, "F-shrink-deviation-summary")
p <- ggplot(sd_df, aes(rho_bar, ratio, colour = design)) +
  stat_function(fun = sqrt, inherit.aes = FALSE, colour = INK,
                linetype = "22", linewidth = 0.55) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.9) +
  annotate("text", x = 0.865, y = 0.842, label = "sqrt(bar(w))~~'(constant-error prediction)'",
           parse = TRUE, size = 3.0, colour = INK, angle = 24, family = "Helvetica") +
  scale_colour_manual(values = PAL[c(1, 2)], name = NULL) +
  scale_x_continuous(name = expression("MSEM reliability  " * bar(rho)),
                     limits = c(0.35, 1), breaks = seq(0.4, 1, 0.1)) +
  scale_y_continuous(name = "EAP ensemble SD (truth = 1)", limits = c(0.35, 1)) +
  theory_theme(11)
save_fig2("F-shrink-deviation",
  paste("**The square-root shrinkage rule is a working-model identity, not a",
        "Rasch theorem.** Each point is an exact computation for a Rasch design",
        "(5 to 60 items, difficulties at normal quantiles, $G = N(0,1)$): the",
        "realized spread of the EAP ensemble against the dashed",
        "$\\sqrt{\\bar w}$ prediction that is exact under the constant-error",
        "Gaussian working model of @sec-ch11. The rule tracks the exact",
        "computation closely but not exactly: in these designs the realized",
        "spread sits slightly above it, and mistargeting the difficulties by",
        "1.5 logits separates the two further. The gap exists because the",
        "ensemble spread is governed by the EAP-variance functional while",
        "$\\bar w$ averages error on the variance scale, and the two averages",
        "part company as information becomes uneven over the population",
        "(@sec-ch08-functionals); its direction is a property of the design,",
        "not a constant of the model. Exact quadrature over score classes; no",
        "simulation. Values frozen in tables/F-shrink-deviation-summary.rds.",
        "Generated by code/R/23-figures-v2-theory.R."),
  p, w = 7.4, h = 4.4)

## ---- F-tif-concentration : one quadratic budget, two allocations ----------
I <- 20
beta <- qnorm(ppoints(I))
lam_h <- rep(1, I)
lam_c <- exp(seq(log(0.45), log(2.2), length.out = I))
lam_c <- lam_c * sqrt(I / sum(lam_c^2))  ## equal summed-peak / quadratic budget
## strongest items on the most central difficulties: rank positions by |beta|
## and hand the largest lambda to the smallest |beta|
lam_tmp <- numeric(I)
lam_tmp[order(abs(beta))] <- sort(lam_c, decreasing = TRUE)
lam_c <- lam_tmp
assignment_cor <- cor(abs(beta), lam_c)
tif <- function(lambda) {
  p <- plogis(sweep(outer(lambda, TH), 1, lambda * beta, "-"))
  colSums(lambda^2 * p * (1 - p))
}
J_h <- tif(lam_h); J_c <- tif(lam_c)
rho_of <- function(J) 1 / (1 + sum(WG / J))
rho_h <- rho_of(J_h); rho_c <- rho_of(J_c)
quadratic_budget_h <- sum(lam_h^2)
quadratic_budget_c <- sum(lam_c^2)
summed_peak_h <- quadratic_budget_h / 4
summed_peak_c <- quadratic_budget_c / 4
## For the logistic 2PL item information, integral J_i(theta) dtheta = lambda_i.
integrated_info_h <- sum(lam_h)
integrated_info_c <- sum(lam_c)
integrated_loss_pct <- 100 * (1 - integrated_info_c / integrated_info_h)
stopifnot(assignment_cor < -0.9,
          abs(quadratic_budget_h - 20) < 1e-12,
          abs(quadratic_budget_c - 20) < 1e-12,
          abs(summed_peak_h - 5) < 1e-12,
          abs(summed_peak_c - 5) < 1e-12,
          abs(integrated_info_h - 20) < 1e-12,
          abs(integrated_info_c - 18.094614840817957) < 1e-10,
          abs(integrated_loss_pct - 9.526925795910) < 1e-10,
          max(J_c) > max(J_h), rho_c < rho_h)
save_figure_data(
  data.frame(
    design = c("uniform discriminations", "concentrated discriminations"),
    n_items = I,
    sum_lambda_sq = c(quadratic_budget_h, quadratic_budget_c),
    summed_item_peak_height = c(summed_peak_h, summed_peak_c),
    integrated_information = c(integrated_info_h, integrated_info_c),
    integrated_information_vs_uniform_pct =
      100 * c(1, integrated_info_c / integrated_info_h),
    integrated_information_lower_than_uniform_pct = c(0, integrated_loss_pct),
    assignment_cor_abs_beta_lambda = c(NA_real_, assignment_cor),
    peak_test_information = c(max(J_h), max(J_c)),
    rho_bar = c(rho_h, rho_c)),
  "F-tif-concentration-summary")
dfa <- rbind(data.frame(x = TH, y = J_h, test = "uniform discriminations"),
             data.frame(x = TH, y = J_c, test = "concentrated discriminations"))
pa <- ggplot(dfa, aes(x, y, colour = test)) +
  geom_area(data = data.frame(x = TH, y = dnorm(TH) * 9), aes(x, y),
            inherit.aes = FALSE, fill = "grey92", colour = NA) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = PAL[c(1, 2)], name = NULL) +
  coord_cartesian(xlim = c(-4, 4)) +
  labs(x = expression(theta), y = "test information",
       subtitle = paste0("sum(lambda^2) quadratic budget: 20 both\n",
                         "Integrated J: 20.0000 vs 18.0946 (-9.53%)")) +
  theory_theme(10.5)
dfb <- rbind(data.frame(x = TH, y = 1 / sqrt(J_h), test = "uniform discriminations"),
             data.frame(x = TH, y = 1 / sqrt(J_c), test = "concentrated discriminations"))
pb <- ggplot(dfb, aes(x, y, colour = test)) +
  geom_area(data = data.frame(x = TH, y = dnorm(TH) * 3.2), aes(x, y),
            inherit.aes = FALSE, fill = "grey92", colour = NA) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = PAL[c(1, 2)], name = NULL, guide = "none") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(0, 3.4)) +
  annotate("text", x = -3.95, y = 3.2, hjust = 0, family = "Helvetica",
           label = sprintf("bar(rho) == %.3f~'(uniform)'", rho_h),
           parse = TRUE, size = 3.0, colour = PAL[1]) +
  annotate("text", x = -3.95, y = 2.9, hjust = 0, family = "Helvetica",
           label = sprintf("bar(rho) == %.3f~'(concentrated)'", rho_c),
           parse = TRUE, size = 3.0, colour = PAL[2]) +
  labs(x = expression(theta), y = "standard error of measurement",
       subtitle = "What the peak buys, the tails pay") +
  theory_theme(10.5)
p <- pa + pb + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
save_fig2("F-tif-concentration",
  paste("**Equal quadratic-discrimination budgets do not conserve integrated",
        "information.** Two twenty-item tests share the same difficulties",
        "(normal quantiles) and $\\sum_i \\lambda_i^2=20$, equivalently the",
        "same summed item-peak budget $\\sum_i \\lambda_i^2/4=5$. One spreads",
        "that budget uniformly; the other assigns its strongest items to the",
        "centre of the population (shaded). But",
        "$\\int \\mathcal{J}(\\theta)\\,d\\theta=\\sum_i\\lambda_i$, so the",
        "integrated information is 20.0000 for the uniform test and 18.0946 for",
        "the concentrated test, 9.53% lower. (a) The concentrated allocation",
        "nonetheless has a higher central peak and lower tails. (b) On the",
        "measurement-error scale, its tail deserts cost enough that its MSEM",
        "reliability $\\bar\\rho$ is lower despite the higher central peak.",
        "Exact quadrature; budgets, areas, peaks, and reliabilities are frozen in",
        "tables/F-tif-concentration-summary.rds. Generated by",
        "code/R/23-figures-v2-theory.R."),
  p, w = 8.6, h = 4.3)

## ---- F-rel-functionals : an existence construction for opposite ordering --
I2 <- 16
beta2 <- qnorm(ppoints(I2)) * 1.3
lam_r <- rep(1, I2)
tower_idx <- abs(beta2) < 0.62
lam_t <- ifelse(tower_idx, 3.5, 0.22)              ## tower + weak remainder
n_tower_items <- sum(tower_idx)
quadratic_budget_r <- sum(lam_r^2)
quadratic_budget_t <- sum(lam_t^2)
resource_ratio <- quadratic_budget_t / quadratic_budget_r
J_r <- info_fun(lam_r, beta2)
J_t <- info_fun(lam_t, beta2)
rho_r <- rho_of(J_r); rho_t <- rho_of(J_t)

## posterior-variance-based (EAP empirical) reliability by seeded simulation
sim_rel <- function(lambda, beta, n = 4000, seed = 2026) {
  set.seed(seed)
  th <- rnorm(n)
  p <- plogis(sweep(outer(th, beta, "-"), 2, lambda, "*"))
  u <- matrix(rbinom(length(p), 1, p), nrow = n)
  ## posterior over TH per person
  lp_item <- function(j) {
    pj <- plogis(lambda[j] * (TH - beta[j]))
    outer(u[, j], log(pj)) + outer(1 - u[, j], log(1 - pj))
  }
  ll <- Reduce(`+`, lapply(seq_along(beta), lp_item))     ## n x TH
  ll <- ll + matrix(log(WG), n, length(TH), byrow = TRUE)
  ll <- ll - apply(ll, 1, max)
  W <- exp(ll); W <- W / rowSums(W)
  eap <- as.numeric(W %*% TH)
  var(eap)   ## Var(theta) = 1, so this is the EAP empirical reliability
}
rel_r <- sim_rel(lam_r, beta2)
rel_t <- sim_rel(lam_t, beta2)
stopifnot(n_tower_items == 6L,
          abs(quadratic_budget_r - 16) < 1e-12,
          abs(quadratic_budget_t - 73.984) < 1e-12,
          abs(resource_ratio - 4.624) < 1e-12,
          rho_t < rho_r, rel_t > rel_r,
          (rho_r - rho_t) * (rel_r - rel_t) < 0)
save_figure_data(
  data.frame(
    model = c("uniform", "tower"),
    n_items = I2,
    high_discrimination_item_count = c(0L, n_tower_items),
    tower_design_high_discrimination_item_count = n_tower_items,
    sum_lambda_sq = c(quadratic_budget_r, quadratic_budget_t),
    discrimination_sq_resource_ratio_vs_uniform = c(1, resource_ratio),
    rho_bar = c(rho_r, rho_t),
    rho_eap = c(rel_r, rel_t)),
  "F-rel-functionals-summary")

lvl <- c("uniform discriminations", "an information tower")
dfa <- rbind(data.frame(x = TH, y = J_r, test = lvl[1]),
             data.frame(x = TH, y = J_t, test = lvl[2]))
dfa$test <- factor(dfa$test, levels = lvl)
pa <- ggplot(dfa, aes(x, y, colour = test)) +
  geom_area(data = data.frame(x = TH, y = dnorm(TH) * 16), aes(x, y),
            inherit.aes = FALSE, fill = "grey92", colour = NA) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = PAL[c(1, 2)], name = NULL) +
  coord_cartesian(xlim = c(-3.6, 3.6)) +
  labs(x = expression(theta), y = "test information",
       subtitle = paste0("Same 16 difficulties; 6 tower items; ",
                         "sum(lambda^2) ratio = 4.624")) +
  theory_theme(10.5)
dfb <- rbind(data.frame(x = TH, y = 1 / J_r, test = lvl[1]),
             data.frame(x = TH, y = 1 / J_t, test = lvl[2]))
dfb$test <- factor(dfb$test, levels = lvl)
pb <- ggplot(dfb, aes(x, y, colour = test)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = c(-1.96, 1.96), colour = "grey75",
             linetype = "22", linewidth = 0.4) +
  annotate("text", x = 0, y = 0.028, label = "95% of the population",
           size = 2.7, colour = INK2, family = "Helvetica") +
  scale_colour_manual(values = PAL[c(1, 2)], name = NULL, guide = "none") +
  scale_y_log10() +
  coord_cartesian(xlim = c(-3.6, 3.6), ylim = c(0.02, 400)) +
  annotate("text", x = -3.5, y = 220, hjust = 0, family = "Helvetica", size = 2.9,
           colour = INK,
           label = sprintf("'MSEM functional '*bar(rho)*': %.2f (uniform), %.2f (tower)'",
                           rho_r, rho_t),
           parse = TRUE) +
  annotate("text", x = -3.5, y = 90, hjust = 0, family = "Helvetica", size = 2.9,
           colour = INK,
           label = sprintf("EAP-variance functional: %.2f (uniform), %.2f (tower)",
                           rel_r, rel_t)) +
  labs(x = expression(theta), y = expression(1 / italic(J)(theta) ~ " (log scale)"),
       subtitle = "The integrand the MSEM functional averages") +
  theory_theme(10.5)
p <- pa + pb + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
save_fig2("F-rel-functionals",
  paste("**A stylized existence construction, not a reconstruction of a",
        "case-study fit.** The uniform-discrimination test and information-tower",
        "test have the same sixteen difficulties; six central items form the",
        "tower. The tower is resource-confounded rather than equal-budget: its",
        "$\\sum_i\\lambda_i^2$ is 73.984 versus 16.000, a 4.624-fold ratio.",
        "Panel (b) shows the integrand $1/\\mathcal{J}$ on a log scale. Exact",
        "quadrature gives the MSEM functional $\\bar\\rho$, and a seeded",
        "4{,}000-person posterior computation gives the EAP-variance functional;",
        "the two functionals order these chosen tests oppositely. This demonstrates",
        "possibility; it does not establish why either column of the real-test",
        "catalogue moves. Values and construction diagnostics are frozen in",
        "tables/F-rel-functionals-summary.rds.",
        "Generated by code/R/23-figures-v2-theory.R."),
  p, w = 8.6, h = 4.3)

cat("theory figures complete\n")
