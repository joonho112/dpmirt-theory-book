## 16-dpm-fit-artifact.R — the book's ONE live model fit.
##
## Everything else in code/R is deterministic or a fixed-seed illustration. This
## script runs a real DPM and a real Gaussian Rasch fit on one simulated dataset
## and caches the summaries F-dpm-fit needs, so the figure script stays fast and
## the fit is reproducible from a single seed. Run it before 09-figures.R when
## the cache is absent; the figure falls back to the cache otherwise.
##
## The dataset is deliberately modest — 400 persons, 25 items, a bimodal G — so
## that the run finishes in minutes and so that the ch. 16 point about finite
## tests identifying only I+1 functionals is visible rather than asymptotic.

source("code/R/00-paths.R")
suppressMessages({library(nimble); library(DPMirt)})

set.seed(2026)
P <- 400L; I <- 25L
th <- ifelse(runif(P) < 0.5, rnorm(P, -1.0, 0.45), rnorm(P, 1.0, 0.45))
th <- (th - mean(th)) / sd(th)
b  <- seq(-1.8, 1.8, length.out = I); b <- b - mean(b)
Y  <- 1 * (matrix(runif(P * I), P, I) < plogis(outer(th, b, "-")))

run <- function(prior) {
  sp <- dpmirt_spec(Y, model = "rasch", prior = prior,
                    identification = "constrained_item")
  dpmirt_sample(dpmirt_compile(sp), niter = 6000L, nburnin = 3000L,
                thin = 3L, seed = 7L, verbose = FALSE)
}
fit_dpm <- run("dpm")
fit_nor <- run("normal")

draws <- function(f, nm) {
  s <- f$samples; if (is.list(s) && !is.matrix(s)) s <- s[[1]]
  s[, grep(paste0("^", nm, "(\\[|$)"), colnames(s)), drop = FALSE]
}
grid <- seq(-3.5, 3.5, length.out = 401)

## posterior density of G under the DPM: each draw is the finite mixture the
## realized partition induces, weighted by cluster size.
zi <- draws(fit_dpm, "zi"); mu <- draws(fit_dpm, "muTilde"); s2 <- draws(fit_dpm, "s2Tilde")
Ddpm <- t(vapply(seq_len(nrow(zi)), function(t) {
  z <- as.integer(zi[t, ])
  rowSums(vapply(seq_len(P), function(p)
    dnorm(grid, mu[t, z[p]], sqrt(s2[t, z[p]])), numeric(length(grid)))) / P
}, numeric(length(grid))))
K_post <- apply(zi, 1, function(z) length(unique(z)))
alpha_post <- as.numeric(draws(fit_dpm, "alpha"))

## posterior density of G under the Gaussian model
mun <- as.numeric(draws(fit_nor, "mu")); s2n <- as.numeric(draws(fit_nor, "s2.eta"))
Dnor <- t(vapply(seq_along(mun), function(t) dnorm(grid, mun[t], sqrt(s2n[t])),
                 numeric(length(grid))))

## the true generating density, on the standardized scale actually used
true_d <- {
  raw <- 0.5 * dnorm(grid, -1.0, 0.45) + 0.5 * dnorm(grid, 1.0, 0.45)
  mu0 <- sum(grid * raw) / sum(raw)
  s0  <- sqrt(sum((grid - mu0)^2 * raw) / sum(raw))
  approx((grid - mu0) / s0, raw * s0, xout = grid, rule = 2)$y
}

## exact prior-predictive pmf of K under alpha ~ Gamma(a, b): Antoniak integrated
## over the alpha prior. Needed so the figure compares like with like — the review
## of 2026-08-04 found that comparing E[K | alpha] draws against the posterior is
## not a prior-versus-posterior comparison at all.
log_s1 <- function(n) {
  lg <- 0
  if (n == 1) return(lg)
  for (m in 2:n) {
    prev <- lg; new <- rep(-Inf, m)
    for (k in 1:m) {
      a <- if (k >= 2) prev[k - 1] else -Inf
      b <- if (k <= m - 1) prev[k] + log(m - 1) else -Inf
      new[k] <- if (is.infinite(a) && is.infinite(b)) -Inf else
        max(a, b) + log1p(exp(-abs(a - b)))
    }
    lg <- new
  }
  lg
}
ls1 <- log_s1(P)
agrid <- seq(1e-6, 8, length.out = 8000)
aw <- dgamma(agrid, shape = 1, rate = 3); aw <- aw / sum(aw)
K_prior_pmf <- rowSums(vapply(seq_along(agrid), function(i)
  aw[i] * exp(ls1 + (1:P) * log(agrid[i]) -
              (lgamma(agrid[i] + P) - lgamma(agrid[i]))), numeric(P)))
K_post_pmf <- as.numeric(table(factor(K_post, levels = 1:P)) / length(K_post))

out <- list(
  K_prior_pmf = K_prior_pmf, K_post_pmf = K_post_pmf,
  K_tv = 0.5 * sum(abs(K_prior_pmf - K_post_pmf)),
  grid = grid, true = true_d / (sum(true_d) * diff(grid)[1]),
  dpm_mean = colMeans(Ddpm), dpm_lo = apply(Ddpm, 2, quantile, 0.05),
  dpm_hi = apply(Ddpm, 2, quantile, 0.95), nor_mean = colMeans(Dnor),
  K = K_post, alpha = alpha_post, theta_true = th, P = P, I = I,
  n_draws = nrow(zi), alpha_prior = c(a = 1, b = 3))
saveRDS(out, file.path(paths$tables, "F-dpm-fit-cache.rds"))
cat(sprintf("cached: %d draws; K posterior median %d, range %d-%d; alpha median %.2f\n",
            nrow(zi), median(K_post), min(K_post), max(K_post), median(alpha_post)))
