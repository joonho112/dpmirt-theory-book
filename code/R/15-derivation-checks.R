## 15-derivation-checks.R — V8.
## Every result the register marks `derived-here` has at least one numerical
## identity or invariance check here; selected adapted/restated calculations are
## checked as well. These checks corroborate implementations and worked algebra.
## They are not proofs of the results and do not validate every sentence of prose.
## A register-to-check coverage gate exits non-zero if a derived result is omitted.
##
## Deterministic where possible. Where a simulation is used the seed is fixed
## and the tolerance is set from the Monte Carlo error, not tuned to pass.

source("code/R/00-paths.R")
source("code/R/02-rank-concordance.R")
env_or <- function(name, default) {
  z <- Sys.getenv(name); if (nzchar(z)) z else default
}

results <- list()
check <- function(id, label, ok, detail = "") {
  results[[length(results) + 1L]] <<- data.frame(
    id = id, label = label, pass = isTRUE(ok), detail = detail,
    stringsAsFactors = FALSE)
  cat(sprintf("  %-14s %-52s %s\n", id, label, if (isTRUE(ok)) "ok" else "FAIL"))
}
near <- function(a, b, tol = 1e-8) all(abs(a - b) < tol)

## shared machinery -------------------------------------------------------
rasch_info <- function(theta, beta)
  vapply(theta, function(z) sum(plogis(z - beta) * (1 - plogis(z - beta))), numeric(1))
quad <- function(n = 801, lo = -8, hi = 8) {
  t <- seq(lo, hi, length.out = n); w <- dnorm(t); list(t = t, w = w / sum(w))
}

beta20 <- seq(-1.6, 1.6, length.out = 20)
Q <- quad()
J <- rasch_info(Q$t, beta20)
se2 <- 1 / J

## PROP-03-2 — fixed-lambda weighted-score factorization
lam3 <- c(1, 1, 2); b3 <- c(-0.8, 0.3, 1.1)
u3a <- c(1, 1, 0); u3b <- c(0, 0, 1)       # same lambda-weighted score
lik3 <- function(th, u) {
  pp <- plogis(lam3 * (th - b3))
  prod(pp^u * (1 - pp)^(1 - u))
}
th3 <- c(-2, -0.2, 1.7)
rat3 <- vapply(th3, function(th) lik3(th, u3a) / lik3(th, u3b), numeric(1))
check("PROP-03-2", "same fixed-lambda weighted score gives theta-free likelihood ratio",
      near(rat3, rep(rat3[1], length(rat3)), 1e-10) &&
        !near(sum(lam3 * u3a), sum(c(1, 2, 3) * u3a)),
      sprintf("ratio range %.10f--%.10f", min(rat3), max(rat3)))

## PROP-04-2 / PROP-04-3 — location and 2PL scale orbits
th4 <- c(-1.1, 0.4, 1.7); b4 <- c(-0.7, 0.2, 1.3); l4 <- c(0.7, 1.0, 1.4)
eta4 <- outer(th4, b4, "-")
check("PROP-04-2", "common Rasch location shift preserves all response probabilities",
      near(plogis(eta4), plogis(outer(th4 + 2.3, b4 + 2.3, "-")), 1e-12), "")
c4 <- 1.8
check("PROP-04-3", "2PL joint scale transformation preserves logits; Rasch scaling does not",
      near(sweep(outer(c4 * th4, c4 * b4, "-"), 2, l4 / c4, "*"),
           sweep(eta4, 2, l4, "*"), 1e-12) &&
        !near(plogis(c4 * eta4), plogis(eta4), 1e-6), "")
## The chapter-26 reporting representative uses the full positive-affine orbit:
## c = mean(beta), a = geometric mean(lambda).
c4_aff <- mean(b4)
a4_aff <- exp(mean(log(l4)))
th4_aff <- a4_aff * (th4 - c4_aff)
b4_aff <- a4_aff * (b4 - c4_aff)
l4_aff <- l4 / a4_aff
check("PROP-04-3b", "positive-affine 2PL normalization preserves logits and centers both item blocks",
      near(sweep(outer(th4_aff, b4_aff, "-"), 2, l4_aff, "*"),
           sweep(eta4, 2, l4, "*"), 1e-12) &&
        near(mean(b4_aff), 0, 1e-12) &&
        near(exp(mean(log(l4_aff))), 1, 1e-12),
      sprintf("a = %.6f; mean beta* = %.2e; GM lambda* = %.12f",
              a4_aff, mean(b4_aff), exp(mean(log(l4_aff)))))

## THM-04-4F — three fixed-effects representatives of the same orbit
cs4 <- c(anchor = -b4[1], item_center = -mean(b4), person_center = -mean(th4))
repr4 <- lapply(cs4, function(cc) list(theta = th4 + cc, beta = b4 + cc))
same_eta4 <- vapply(repr4, function(z) near(outer(z$theta, z$beta, "-"), eta4), logical(1))
constraints4 <- c(repr4$anchor$beta[1],
                  mean(repr4$item_center$beta),
                  mean(repr4$person_center$theta))
diffs4 <- vapply(repr4, function(z)
  near(diff(z$theta), diff(th4)) && near(diff(z$beta), diff(b4)), logical(1))
check("THM-04-4F", "anchor and two centering rules select invariant fixed-effect representatives",
      all(same_eta4) && near(constraints4, rep(0, 3), 1e-12) && all(diffs4), "")

## THM-04-5R — translated mixing distribution and items preserve a marginal pattern law
g4 <- c(0.2, 0.5, 0.3); supp4 <- c(-1.4, 0.1, 1.8); u4 <- c(1, 0, 1)
patprob4 <- function(support, beta) sum(g4 * vapply(support, function(th) {
  pp <- plogis(th - beta); prod(pp^u4 * (1 - pp)^(1 - u4))
}, numeric(1)))
check("THM-04-5R", "shifting item locations and the mixing law preserves a marginal pattern probability",
      near(patprob4(supp4, b4), patprob4(supp4 - 0.9, b4 - 0.9), 1e-12), "")

## PROP-06-2 — implicit sensitivity used by the independent-calibration delta method
b6p <- c(-1.0, -0.3, 0.2, 0.9, 1.4); u6p <- c(1, 1, 1, 0, 0)
root6 <- function(beta) uniroot(function(th) sum(u6p - plogis(th - beta)),
                                c(-8, 8), tol = 1e-12)$root
th6 <- root6(b6p); vv6 <- plogis(th6 - b6p) * (1 - plogis(th6 - b6p))
g6 <- vv6 / sum(vv6); eps6 <- 1e-5
g6_num <- vapply(seq_along(b6p), function(j) {
  up <- dn <- b6p; up[j] <- up[j] + eps6; dn[j] <- dn[j] - eps6
  (root6(up) - root6(dn)) / (2 * eps6)
}, numeric(1))
V6 <- diag(c(0.02, 0.03, 0.01, 0.04, 0.02))
check("PROP-06-2", "implicit item-parameter sensitivity matches finite differences",
      near(g6, g6_num, 1e-7) && drop(t(g6) %*% V6 %*% g6) > 0,
      sprintf("max derivative error %.2e", max(abs(g6 - g6_num))))

## PROP-07-1 — RMSEM >= mean local standard error, equality iff se constant
msem <- sum(Q$w * se2)
check("PROP-07-1", "RMSEM >= mean se, strict when se varies",
      sqrt(msem) > sum(Q$w * sqrt(se2)),
      sprintf("RMSEM %.6f vs mean se %.6f", sqrt(msem), sum(Q$w * sqrt(se2))))
check("PROP-07-1b", "equality holds when se is constant",
      near(sqrt(sum(Q$w * 0.36)), sum(Q$w * 0.6)),
      "constant se^2 = 0.36")

## PROP-08-1 — mean shrinkage weight >= w-bar, equality iff se^2 constant
s2 <- 1
wp <- s2 / (s2 + se2); wbar <- s2 / (s2 + msem)
check("PROP-08-1", "mean w_p >= w-bar, strict when se^2 varies",
      sum(Q$w * wp) > wbar,
      sprintf("mean w_p %.6f vs w-bar %.6f", sum(Q$w * wp), wbar))
check("PROP-08-1b", "equality holds when se^2 is constant",
      near(sum(Q$w * (s2 / (s2 + 0.4))), s2 / (s2 + 0.4)),
      "constant se^2 = 0.4")

## PROP-08-2 — separation, strata, and the reliability identity
S <- sqrt(s2) / sqrt(msem); H <- (4 * S + 1) / 3
check("PROP-08-2", "w-bar = S^2/(1+S^2) with S = sigma/RMSEM",
      near(wbar, S^2 / (1 + S^2)), sprintf("S %.6f, H %.6f", S, H))
## and the C-001 arithmetic: exact vs rounded-RMSEM separation
tg <- c(0.5, 0.6, 0.7, 0.8, 0.9)
## half-up rounding, as the C-001 claim states. R's round() is half-to-even and
## sends 1.25 to 1.2, so the distinction is load-bearing here, not pedantic.
r1 <- function(x) floor(x * 10 + 0.5) / 10
rm_exact <- sqrt((1 - tg) / tg)
S_exact <- r1(1 / rm_exact)
S_from_rounded <- r1(1 / r1(rm_exact))
check("C-001", "printed S column reproduces from ROUNDED RMSEM only",
      identical(S_from_rounded, c(1.0, 1.3, 1.4, 2.0, 3.3)) &&
        identical(S_exact, c(1.0, 1.2, 1.5, 2.0, 3.0)),
      paste0("exact ", paste(S_exact, collapse = "/"),
             " ; from rounded ", paste(S_from_rounded, collapse = "/")))
check("C-001b", "printed H column reproduces from EXACT S",
      identical(r1((4 / rm_exact + 1) / 3), c(1.7, 2.0, 2.4, 3.0, 4.3)),
      "H is exact while S beside it is not")

## THM-08-3 — the sandwich w-bar <= rho_Theta <= rho-tilde, over four shapes of G
sandwich_ok <- TRUE; gaps <- numeric()
for (gname in c("normal", "t3", "skew", "bimodal")) {
  t <- seq(-12, 12, length.out = 6001)
  d <- switch(gname,
    normal  = dnorm(t),
    t3      = dt(t / sqrt(3), df = 3),
    skew    = 0.75 * dnorm(t, -0.6, 0.5) + 0.25 * dnorm(t, 1.8, 0.9),
    bimodal = 0.5 * dnorm(t, -1.2, 0.5) + 0.5 * dnorm(t, 1.2, 0.5))
  w <- d / sum(d); mu <- sum(w * t); sd_ <- sqrt(sum(w * (t - mu)^2))
  tz <- (t - mu) / sd_; k <- abs(tz) < 8; tz <- tz[k]; w <- w[k] / sum(w[k])
  for (I in c(5, 20, 95)) {
    b <- seq(-1.6, 1.6, length.out = I); Jv <- rasch_info(tz, b)
    wb <- 1 / (1 + sum(w / Jv))
    rTh <- sum(w * Jv / (Jv + 1))
    rTil <- sum(w * Jv) / (sum(w * Jv) + 1)
    sandwich_ok <- sandwich_ok && (wb <= rTh + 1e-12) && (rTh <= rTil + 1e-12)
    gaps <- c(gaps, rTil - wb)
  }
}
check("THM-08-3", "w-bar <= rho_Theta <= rho-tilde in all 12 cells", sandwich_ok,
      sprintf("largest outer gap %.4f", max(gaps)))
check("THM-08-3b", "equality when information is constant",
      { Jc <- rep(4, 100); wq <- rep(1 / 100, 100)
        a <- 1 / (1 + sum(wq / Jc)); b2 <- sum(wq * Jc / (Jc + 1))
        c2 <- sum(wq * Jc) / (sum(wq * Jc) + 1); near(a, b2) && near(b2, c2) },
      "all three coincide")
check("ch8-gap-size", "every outer gap in the reported grid is below 0.03",
      max(gaps) < 0.03, sprintf("max %.4f", max(gaps)))

## eq-two-forms — the two empirical reliability branches differ by m^2/(v(v+m))
v <- 1.3864; m <- 0.3110
check("EQ-TWO-FORMS", "v/(v+m) - (1 - m/v) = m^2/(v(v+m)) > 0",
      near(v / (v + m) - (1 - m / v), m^2 / (v * (v + m))) && m^2 / (v * (v + m)) > 0,
      sprintf("%.6f", m^2 / (v * (v + m))))

## PROP-09-1 — recompute 2PL probabilities at each scale candidate
lambda20 <- seq(0.7, 1.3, length.out = length(beta20))
j2pl <- function(cc) {
  eta <- outer(Q$t, beta20, "-")
  eta <- sweep(eta, 2L, cc * lambda20, "*")
  pp <- plogis(eta)
  rowSums(sweep(pp * (1 - pp), 2L, (cc * lambda20)^2, "*"))
}
cstar <- function(target, metric) {
  f <- function(cc) {
    Jv <- j2pl(cc)
    if (metric == "msem") 1 / (1 + sum(Q$w / Jv)) else
      sum(Q$w * Jv) / (sum(Q$w * Jv) + 1)
  }
  uniroot(function(cc) f(cc) - target, c(0.05, 2.35))$root
}
ok_c <- TRUE
for (tt in c(0.6, 0.75)) ok_c <- ok_c && (cstar(tt, "msem") >= cstar(tt, "info") - 1e-8)
check("PROP-09-1", "c* under MSEM >= c* under average-information", ok_c,
      sprintf("at 0.75: %.4f vs %.4f", cstar(0.75, "msem"), cstar(0.75, "info")))
check("PROP-09-1b", "scaled 2PL information is not obtained by freezing probabilities",
      max(abs(j2pl(1.8) - 1.8^2 * j2pl(1))) > 0.1,
      sprintf("max absolute difference %.4f", max(abs(j2pl(1.8) - 1.8^2 * j2pl(1)))))

## PROP-10-2 — moments of the mean-centred item prior
Ii <- 12; s2b <- 3
Cm <- diag(Ii) - matrix(1 / Ii, Ii, Ii)
Sig <- s2b * Cm
check("PROP-10-2", "Var = s2(1-1/I), Cov = -s2/I, Cor = -1/(I-1), rank I-1",
      near(Sig[1, 1], s2b * (1 - 1 / Ii)) && near(Sig[1, 2], -s2b / Ii) &&
        near(Sig[1, 2] / Sig[1, 1], -1 / (Ii - 1)) && qr(Sig)$rank == Ii - 1L,
      sprintf("Var %.4f (not %.1f)", Sig[1, 1], s2b))
check("PROP-10-2b", "the induced draws sum to zero",
      { set.seed(3); B <- matrix(rnorm(50 * Ii, 0, sqrt(s2b)), 50, Ii)
        near(rowSums(B - rowMeans(B)), rep(0, 50), 1e-10) }, "")
## and the implemented hyperprior's moments
a <- 2.01; bb <- 1.01
check("eq-hyperpriors", "Inv-Gamma(2.01,1.01) has mean 1 and variance 100",
      near(bb / (a - 1), 1, 1e-12) && near(bb^2 / ((a - 1)^2 * (a - 2)), 100, 1e-9),
      sprintf("mean %.6f variance %.4f", bb / (a - 1), bb^2 / ((a - 1)^2 * (a - 2))))

## THM-11-1 — the working-model posterior against direct numerical integration
th_hat <- 1.3; sse <- 0.5; mu0 <- -0.2; s20 <- 1.4
gr <- seq(-15, 15, length.out = 40001); dx <- gr[2] - gr[1]
post <- dnorm(gr, th_hat, sqrt(sse)) * dnorm(gr, mu0, sqrt(s20)); post <- post / sum(post * dx)
num_mean <- sum(gr * post * dx); num_var <- sum((gr - num_mean)^2 * post * dx)
wpp <- s20 / (s20 + sse)
check("THM-11-1", "closed-form posterior mean matches numerical integration",
      near(num_mean, wpp * th_hat + (1 - wpp) * mu0, 1e-6),
      sprintf("%.8f vs %.8f", num_mean, wpp * th_hat + (1 - wpp) * mu0))
check("THM-11-1b", "posterior variance = w se^2 = (1-w) sigma^2",
      near(num_var, wpp * sse, 1e-6) && near(wpp * sse, (1 - wpp) * s20),
      sprintf("%.8f", num_var))

## PROP-11-3 — ensemble identities and Louis's square-root moment repair
set.seed(5); P <- 4e5; sg2 <- 1; sev <- 0.6; w1 <- sg2 / (sg2 + sev)
th <- rnorm(P, 0, sqrt(sg2)); hat <- th + rnorm(P, 0, sqrt(sev))
mc <- 4 / sqrt(P)                                   # generous Monte Carlo tolerance
check("PROP-11-3", "Var(ML) = sigma^2 + MSEM", near(var(hat), sg2 + sev, mc),
      sprintf("%.5f vs %.5f", var(hat), sg2 + sev))
check("PROP-11-3b", "Var(EAP) = w-bar sigma^2 exactly",
      near(var(w1 * hat), w1 * sg2, mc),
      sprintf("%.5f vs %.5f", var(w1 * hat), w1 * sg2))
check("PROP-11-3c", "SD(EAP)/sigma = sqrt(w-bar)",
      near(sd(w1 * hat) / sqrt(sg2), sqrt(w1), mc),
      sprintf("%.5f vs %.5f", sd(w1 * hat) / sqrt(sg2), sqrt(w1)))
check("PROP-11-3d", "weight sqrt(w) restores the ensemble variance (Louis)",
      near(var(sqrt(w1) * hat), sg2, mc),
      sprintf("%.5f vs %.5f", var(sqrt(w1) * hat), sg2))
## Exact posterior under a genuinely skewed discrete G and one Bernoulli item.
ths <- c(-1.2, -0.1, 2.3); gs <- c(0.60, 0.25, 0.15)
p1s <- plogis(ths - 0.4)
liks <- rbind(1 - p1s, p1s)
joint_s <- sweep(liks, 2L, gs, "*")
py_s <- rowSums(joint_s)
post_s <- joint_s / py_s
pm_s <- drop(post_s %*% ths)
pv_s <- vapply(seq_len(nrow(post_s)), function(y)
  sum(post_s[y, ] * (ths - pm_s[y])^2), numeric(1))
mu_s <- sum(gs * ths); var_s <- sum(gs * (ths - mu_s)^2)
check("PROP-11-3e", "exact posterior total-variance identity under a skewed G",
      near(sum(py_s * pv_s) + sum(py_s * (pm_s - mu_s)^2), var_s, 1e-12),
      sprintf("prior variance %.8f", var_s))

## THM-15-2 — stick-breaking. The T3.10 sweep found this was the one named
## derivation with no numerical check (2026-08-04). Two exact consequences of
## Sethuraman's construction are testable: the mean weight sequence is geometric,
## and the induced random measure has the Dirichlet marginal on any set.
alpha_sb <- 2.5
n_sb <- 1:12
check("THM-15-2", "stick-breaking mean weights are geometric: E[p_n] = a^(n-1)/(1+a)^n",
      { set.seed(152); R <- 4e5; N <- 400L
        V <- matrix(rbeta(R * N, 1, alpha_sb), R, N)
        P <- V * t(apply(cbind(1, 1 - V[, -N, drop = FALSE]), 1L, cumprod))
        near(colMeans(P)[n_sb],
             alpha_sb^(n_sb - 1) / (1 + alpha_sb)^n_sb, 3e-3) },
      sprintf("alpha = %.1f, first 12 weights", alpha_sb))
check("THM-15-2b", "simulated first two moments agree with the sourced Beta marginal",
      { set.seed(153); R <- 3e5; N <- 600L; q <- 0.3
        V <- matrix(rbeta(R * N, 1, alpha_sb), R, N)
        P <- V * t(apply(cbind(1, 1 - V[, -N, drop = FALSE]), 1L, cumprod))
        inA <- matrix(runif(R * N) < q, R, N)          # Y_n ~ G_0, G_0(A) = q
        PA <- rowSums(P * inA)
        near(mean(PA), q, 3e-3) &&
          near(var(PA), q * (1 - q) / (1 + alpha_sb), 3e-3) },
      "moment corroboration only; the distributional theorem remains source-only")
check("APP-A-1-1", "E[log(1-V)] = -1/alpha for V ~ Beta(1,alpha)",
      { numeric_value <- integrate(
          function(v) log1p(-v) * alpha_sb * (1-v)^(alpha_sb-1),
          0, 1, rel.tol = 1e-10)$value
        analytic_value <- digamma(alpha_sb) - digamma(alpha_sb + 1)
        near(numeric_value, -1/alpha_sb, 1e-8) &&
          near(analytic_value, -1/alpha_sb, 1e-12) },
      "sign and exact integral used in Appendix A A.1.1")
check("THM-15-2c", "the weights sum to one: truncation error decays geometrically",
      { set.seed(154); V <- rbeta(2000, 1, alpha_sb)
        1 - sum(V * cumprod(c(1, 1 - V[-2000]))) < 1e-10 },
      "tail mass beyond 2000 sticks")

## THM-15-4 — Antoniak's pmf for K_J and its digamma mean
log_s1 <- function(n) {                       # log |s(n,k)|, k = 1..n
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
k_pmf <- function(n, al) exp(log_s1(n) + (1:n) * log(al) - (lgamma(al + n) - lgamma(al)))
ok_k <- TRUE; detail_k <- character()
for (n in c(20, 200)) for (al in c(0.5, 1, 5)) {
  pk <- k_pmf(n, al)
  ok_k <- ok_k && near(sum(pk), 1, 1e-9) &&
    near(sum((1:n) * pk), al * (digamma(al + n) - digamma(al)), 1e-8)
}
check("THM-15-4", "Antoniak pmf sums to 1 and its mean is the digamma form", ok_k,
      sprintf("alpha=1, J=200: E[K] = %.4f", 1 * (digamma(1 + 200) - digamma(1))))
check("THM-15-4b", "K_J grows like alpha log J: tenfold J adds ~2.3 alpha",
      { f <- function(al, J) al * (digamma(al + J) - digamma(al))
        d <- f(1, 1e5) - f(1, 1e4); near(d, log(10), 0.02) },
      sprintf("increment %.4f vs log 10 = %.4f",
              (digamma(1 + 1e5) - digamma(1)) - (digamma(1 + 1e4) - digamma(1)), log(10)))
check("THM-15-4c", "alpha = 0.80 is what matching E[K_200] = 5 requires",
      near(uniroot(function(a) a * (digamma(a + 200) - digamma(a)) - 5,
                   c(1e-4, 500))$root, 0.797, 2e-3), "ch15 sec. 8")

## PROP-13-1 — the sum-score counterexamples, computed exactly
pbin <- function(p) { d <- 1; for (pi in p) d <- c(d * (1 - pi), 0) + c(0, d * pi); d }
ss_dist <- function(b, tq, wq) {
  o <- numeric(length(b) + 1L)
  for (j in seq_along(tq)) o <- o + wq[j] * pbin(plogis(tq[j] - b))
  o / sum(o)
}
std_G <- function(dens, n = 4001) {
  t <- seq(-12, 12, length.out = n); w <- dens(t); w <- w / sum(w)
  mu <- sum(w * t); sd_ <- sqrt(sum(w * (t - mu)^2)); list(t = (t - mu) / sd_, w = w)
}
smom <- function(x, p) { m <- sum(p * x); v <- sum(p * (x - m)^2)
  c(skew = sum(p * (x - m)^3) / v^1.5) }
Gnorm <- std_G(function(t) dnorm(t))
Gbim  <- std_G(function(t) 0.5 * dnorm(t, -1, 0.30) + 0.5 * dnorm(t, 1, 0.30))
d_a <- ss_dist(rep(-1.5, 5), Gnorm$t, Gnorm$w)
check("PROP-13-1a", "normal G, 5 easy items: score skew -0.99, 38% at ceiling",
      near(unname(smom(0:5, d_a)["skew"]), -0.987, 1e-3) && near(d_a[6], 0.383, 1e-3),
      sprintf("skew %.3f, P(max) %.3f", smom(0:5, d_a)["skew"], d_a[6]))
b6 <- seq(-2.5, 2.5, length.out = 6)
d_b <- ss_dist(b6, Gbim$t, Gbim$w); d_bn <- ss_dist(b6, Gnorm$t, Gnorm$w)
nmax_b <- sum(diff(sign(diff(d_b))) < 0)
check("PROP-13-1b", "bimodal G, 6 spread items: unimodal score pmf, TV = 0.035",
      nmax_b == 1L && near(0.5 * sum(abs(d_b - d_bn)), 0.0351, 5e-4) &&
        near(unname(smom(0:6, d_b)["skew"]), 0, 1e-6),
      sprintf("maxima %d, TV %.4f", nmax_b, 0.5 * sum(abs(d_b - d_bn))))
check("PROP-13-1c", "TV grows with test length: .035 / .066 / .172 / .305",
      { tvs <- vapply(list(seq(-2.5, 2.5, length.out = 6), seq(-2.5, 2.5, length.out = 9),
                           seq(-1.6, 1.6, length.out = 20), seq(-1.6, 1.6, length.out = 60)),
          function(b) 0.5 * sum(abs(ss_dist(b, Gbim$t, Gbim$w) - ss_dist(b, Gnorm$t, Gnorm$w))),
          numeric(1))
        near(round(tvs, 3), c(0.035, 0.066, 0.172, 0.305), 1e-9) && all(diff(tvs) > 0) },
      "monotone in I")

## PROP-12-3 exception — the stochastic-order result the ch. 12 repair rests on.
## The critical finding of review 012 was that non-constant information does NOT
## reorder posterior means under a common item set. That repair was prose-only;
## this makes it falsifiable. Exact: the EAP is computed for every attainable
## total score by quadrature, under three shapes of G.
pb_ss <- function(p) { d <- 1; for (pi in p) d <- c(d * (1 - pi), 0) + c(0, d * pi); d }
eap_by_score <- function(beta, gdens) {
  q <- seq(-8, 8, length.out = 2001)
  L <- t(vapply(q, function(t) pb_ss(plogis(t - beta)), numeric(length(beta) + 1L)))
  g <- gdens(q); g <- g / sum(g)
  vapply(seq_len(ncol(L)), function(k) { w <- L[, k] * g; sum(q * w) / sum(w) }, numeric(1))
}
b25 <- seq(-1.8, 1.8, length.out = 25)
gs <- list(normal  = function(q) dnorm(q),
           bimodal = function(q) 0.5 * dnorm(q, -1, 0.3) + 0.5 * dnorm(q, 1, 0.3),
           skew    = function(q) dgamma(q + 3, 2, 1))
mono <- vapply(gs, function(gd) all(diff(eap_by_score(b25, gd)) > 0), logical(1))
check("PROP-12-3x", "common-item Rasch: EAP is monotone in the total score for every G",
      all(mono), paste(names(mono), mono, collapse = "; "))
## and the converse: unequal information ACROSS FORMS can reorder, which is the
## regime the chapter now says the empirical-Bayes rank literature occupies.
eA <- eap_by_score(b25, gs$normal)
eB <- eap_by_score(seq(-1.8, 1.8, length.out = 8), gs$normal)
mlq <- function(beta) {
  q <- seq(-8, 8, length.out = 2001)
  L <- t(vapply(q, function(t) pb_ss(plogis(t - beta)), numeric(length(beta) + 1L)))
  vapply(seq_len(ncol(L)), function(k) q[which.max(L[, k])], numeric(1))
}
mA <- mlq(b25); mB <- mlq(seq(-1.8, 1.8, length.out = 8))
iA <- 2:(length(eA) - 1); iB <- 2:(length(eB) - 1)
flips <- sum(outer(mA[iA], mB[iB], ">") != outer(eA[iA], eB[iB], ">"))
check("PROP-12-3y", "across two test forms the ML and EAP orderings do disagree",
      flips > 0, sprintf("%d disagreeing cross-form pairs", flips))

## PROP-17-1 / THM-17-3 — the WSEL decomposition, and the incompatibility
## counterexample chapter 17 states. Independent normal posteriors, K = 2.
set.seed(17)
eta17 <- c(-0.6, 0.6); lam17 <- c(0.5, 0.2); R17 <- 2e6
draw17 <- cbind(rnorm(R17, eta17[1], sqrt(lam17[1])), rnorm(R17, eta17[2], sqrt(lam17[2])))
## WSEL at an arbitrary action equals sum (a-eta)^2 + sum v
a_try <- c(0.15, -0.4)
check("PROP-17-1", "E[WSEL] = sum (a - eta)^2 + sum v, minimized at the means",
      near(mean(rowSums((matrix(a_try, R17, 2, byrow = TRUE) - draw17)^2)),
           sum((a_try - eta17)^2) + sum(lam17), 5e-3),
      sprintf("MC %.5f vs algebra %.5f",
              mean(rowSums((matrix(a_try, R17, 2, byrow = TRUE) - draw17)^2)),
              sum((a_try - eta17)^2) + sum(lam17)))
Gbar17 <- function(t)
  0.5 * pnorm(t, eta17[1], sqrt(lam17[1])) +
  0.5 * pnorm(t, eta17[2], sqrt(lam17[2]))
U17 <- vapply(c(0.25, 0.75), function(prob)
  uniroot(function(t) Gbar17(t) - prob, c(-10, 10), tol = 1e-13)$root,
  numeric(1))
s17 <- sqrt(sum(lam17)); d17 <- (eta17[1] - eta17[2]) / s17
mx17 <- eta17[1] * pnorm(d17) + eta17[2] * pnorm(-d17) + s17 * dnorm(d17)
os17 <- c(sum(eta17) - mx17, mx17)
isel17 <- function(atoms) integrate(function(t) {
  A <- (t >= atoms[1]) / 2 + (t >= atoms[2]) / 2
  (A - Gbar17(t))^2
}, -Inf, Inf, subdivisions = 2000L, rel.tol = 1e-11)$value
tab17 <- readRDS(file.path(paths$tables, "T-three-goals-counterexample.rds"))
check("THM-17-3", "Theorem 1 action uses Gbar midpoint quantiles and differs from WSEL means",
      near(Gbar17(U17), c(0.25, 0.75), 1e-10) &&
        !near(U17, eta17, 1e-4) &&
        near(U17, as.numeric(tab17[2, c("Lower atom", "Upper atom")]), 1e-10),
      sprintf("U = (%.6f, %.6f); spread %.6f", U17[1], U17[2], diff(U17)))
check("THM-17-3b", "midpoint-quantile action beats order-statistic means under ISEL",
      isel17(U17) < isel17(os17) && isel17(U17) < isel17(eta17),
      sprintf("ISEL quantile %.7f; order-stat %.7f; WSEL means %.7f",
              isel17(U17), isel17(os17), isel17(eta17)))

## THM-18-1 / PROP-18-2 — the CB inflation factor, its moment match, and the
## affine invariance that bounds what CB can repair.
set.seed(18); m18 <- 40L
eta18 <- rnorm(m18); lam18 <- runif(m18, 0.1, 0.9)
H2_18 <- sum((eta18 - mean(eta18))^2)
H1_18 <- sum(lam18) * (1 - 1 / m18)
a_ghosh <- sqrt(1 + H1_18 / H2_18)
V_eta <- mean((eta18 - mean(eta18))^2); l_bar <- mean(lam18)
check("THM-18-1", "Ghosh's a = sqrt(1 + H1/H2) equals the per-unit (1-1/P) form",
      near(a_ghosh, sqrt(1 + (1 - 1 / m18) * l_bar / V_eta)),
      sprintf("a = %.6f", a_ghosh))
s_eta18 <- var(eta18)
a_pkg18 <- sqrt(1 + l_bar / s_eta18)
check("THM-18-1b", "R sample-variance form equals the exact finite-P independent specialization",
      near(s_eta18, m18 / (m18 - 1) * V_eta, 1e-12) &&
        near(a_pkg18, a_ghosh, 1e-12),
      sprintf("P = %d; package factor = Ghosh factor = %.8f", m18, a_pkg18))
cb18 <- mean(eta18) + (eta18 - mean(eta18)) * a_ghosh
check("THM-18-1c", "the CB ensemble variance matches V_eta + (1-1/P) v-bar",
      near(mean((cb18 - mean(cb18))^2), V_eta + (1 - 1 / m18) * l_bar),
      sprintf("%.6f", mean((cb18 - mean(cb18))^2)))
set.seed(181); B18 <- matrix(rnorm(m18 * 6L), m18, 6L)
Sigma18 <- tcrossprod(B18) / 6
M18 <- diag(m18) - matrix(1 / m18, m18, m18)
H1_trace18 <- sum(diag(M18 %*% Sigma18 %*% M18))
H1_expand18 <- (1 - 1 / m18) * sum(diag(Sigma18)) -
  2 / m18 * sum(Sigma18[upper.tri(Sigma18)])
check("THM-18-1d", "Ghosh H1 equals marginal-variance term minus cross-person covariance term",
      near(H1_trace18, H1_expand18, 1e-10),
      sprintf("trace %.8f; expansion %.8f", H1_trace18, H1_expand18))
std_mom <- function(x, k) mean((x - mean(x))^k) / sd(x)^k
check("PROP-18-2", "CB preserves ranks and standardized moments of orders 3 through 6",
      identical(rank(cb18), rank(eta18)) &&
        all(vapply(3:6, function(k) near(std_mom(cb18, k), std_mom(eta18, k), 1e-9), logical(1))),
      "affine invariance, orders 3 through 6")

## THM-19-1 / PROP-19-2 / PROP-19-3 — the GR construction on eight units.
eta19 <- c(-1.6, -0.9, -0.4, -0.1, 0.2, 0.6, 1.1, 1.8)
lam19 <- c(0.50, 0.40, 0.35, 0.30, 0.30, 0.35, 0.45, 0.60); K19 <- length(eta19)
g19 <- seq(-6, 6, length.out = 6001)
Gbar19 <- vapply(g19, function(t) mean(pnorm(t, eta19, sqrt(lam19))), numeric(1))
U19 <- approxfun(Gbar19, g19, ties = "ordered", rule = 2)((2 * (1:K19) - 1) / (2 * K19))
Rbar19 <- vapply(seq_len(K19), function(k) sum(vapply(seq_len(K19), function(q)
  if (q == k) 1 else pnorm(0, eta19[q] - eta19[k], sqrt(lam19[k] + lam19[q])),
  numeric(1))), numeric(1))
stopifnot(!anyDuplicated(round(Rbar19, 12)))
Rhat19 <- rank(Rbar19, ties.method = "min")
gr19 <- U19[Rhat19]
a19 <- sqrt(1 + mean(lam19) / (sum((eta19 - mean(eta19))^2) / (K19 - 1)))
cb19 <- mean(eta19) + a19 * (eta19 - mean(eta19))
check("PROP-19-2", "the GR ensemble is exactly the mass points of G-hat, permuted",
      { tie_r <- c(1.5, 1.5, 3:8)
        p1 <- 1:8; p2 <- c(2, 1, 3:8)
        tie_loss <- function(p) mean((p - tie_r)^2)
        near(sort(gr19), sort(U19)) && identical(rank(gr19), rank(eta19)) &&
          near(sum(Rbar19), K19 * (K19 + 1) / 2) &&
          near(tie_loss(p1), tie_loss(p2)) &&
          near(sort(U19[p1]), sort(U19[p2])) },
      "EDF carried; expected-rank sum invariant; tied block refinements are loss-equivalent")
check("PROP-19-3", "general GR regret and eq. (14) simplification use their stated permutations",
      { general <- mean((U19[Rhat19] - eta19)^2)
        sorted <- mean((U19 - sort(eta19))^2)
        alt <- rev(seq_len(K19))
        alt_general <- mean((U19[alt] - eta19)^2)
        general_direct <- mean((gr19 - eta19)^2)
        near(general, general_direct) && near(general, sorted) &&
          near(alt_general, mean((U19[alt] - eta19)^2)) && !near(alt_general, sorted) },
      sprintf("regret_GR = %.4f vs CB's %.4f; rank agreement = %s",
              mean((gr19 - eta19)^2), mean((cb19 - eta19)^2),
              identical(Rhat19, rank(eta19))))
check("PROP-19-3b", "CB attains the target ensemble variance and GR does not",
      { sv <- function(x) sum((x - mean(x))^2) / (K19 - 1)
        tgt <- mean(lam19) + sv(eta19)
        near(sv(cb19), tgt, 1e-8) && sv(gr19) < tgt },
      sprintf("target %.3f; CB %.3f; GR %.3f", mean(lam19) + sum((eta19-mean(eta19))^2)/(K19-1),
              sum((cb19-mean(cb19))^2)/(K19-1), sum((gr19-mean(gr19))^2)/(K19-1)))
check("eq-gbar", "Gbar(t) is a probability in [0,1] and is nondecreasing",
      all(Gbar19 >= 0 & Gbar19 <= 1) && all(diff(Gbar19) >= -1e-12) &&
        near(mean(pnorm(0, eta19, sqrt(lam19))), 0.476, 1e-3),
      sprintf("Gbar(0) = %.4f", mean(pnorm(0, eta19, sqrt(lam19)))))

## PROP-20-1 — one production definition, fixed-wbar comparison, and convergence receipt.
sum20 <- readRDS(file.path(paths$tables, "F-rank-reliability-summary.rds"))
gap20 <- readRDS(file.path(paths$tables, "F-rank-reliability-matched-gaps.rds"))
conv20 <- readRDS(file.path(paths$tables, "F-rank-reliability-convergence.rds"))
metric20 <- function(id) sum20$value[match(id, sum20$metric)]
effect20 <- metric20("reliability_effect")
max_gap20 <- metric20("maximum_matched_shape_gap")
check("PROP-20-1", "matched-grid reliability effect exceeds the maximum shape gap fourfold",
      effect20 > 4 * max_gap20 &&
        near(effect20 / max_gap20, metric20("effect_to_max_gap_ratio"), 1e-10),
      sprintf("effect %.5f over wbar %.2f--%.2f; max matched gap %.5f; ratio %.2f",
              effect20, metric20("comparison_wbar_low"),
              metric20("comparison_wbar_high"), max_gap20, effect20 / max_gap20))
check("PROP-20-1b", "matched shape gap has an interior peak and declines on the displayed tail",
      { imax <- which.max(gap20$gap)
        imax > 1L && imax < nrow(gap20) &&
          tail(gap20$gap, 1) < gap20$gap[imax] &&
          near(gap20$wbar[imax], metric20("wbar_at_maximum_gap"), 1e-10) &&
          max(conv20$absolute_concordance_difference) < 1e-3 &&
          max(conv20$absolute_wbar_difference) < 2.1e-3 },
      sprintf("peak %.5f at %.3f; tail %.5f; fine/coarse concordance max diff %.6f",
              max_gap20, metric20("wbar_at_maximum_gap"), tail(gap20$gap, 1),
              max(conv20$absolute_concordance_difference)))
check("PROP-20-1c", "bimodal G has the most near-ties under full-support unit-variance quadrature",
      { nt <- vapply(rank_shape_names, function(s)
          metric20(paste0("near_tie_mass_", gsub("[^a-z]+", "_", s))), numeric(1))
        qmom <- vapply(rank_shape_names, function(s) {
          G <- rank_quadrature(s, 800L)
          c(mean = sum(G$weight * G$theta), variance = sum(G$weight * G$theta^2))
        }, numeric(2))
        which.max(nt) == match("bimodal", rank_shape_names) &&
          all(abs(qmom["mean", ]) < 0.02) && all(abs(qmom["variance", ] - 1) < 0.03) },
      "near-tie mechanism; logistic heavy-tail replaces the invalid truncated t3 working curve")

## PROP-26-1 — a finite known-item 2PL observes only its response-pattern
## integrals.  With K = 2^I patterns, K+1 support points force a nonzero
## null-space direction; small opposite perturbations give distinct laws with
## exactly the same K pattern probabilities.
I26 <- 2L
beta26 <- c(-0.8, 0.9)
lambda26 <- c(0.7, 1.4)
patterns26 <- as.matrix(expand.grid(rep(list(0:1), I26)))
theta26 <- seq(-3, 3, length.out = 2^I26 + 1L)
Q26 <- vapply(theta26, function(theta) {
  pp <- plogis(lambda26 * (theta - beta26))
  apply(patterns26, 1L, function(u)
    prod(pp^u * (1 - pp)^(1 - u)))
}, numeric(nrow(patterns26)))
sv26 <- svd(Q26, nu = 0L, nv = ncol(Q26))
h26 <- sv26$v[, ncol(Q26)]
h26 <- h26 / max(abs(h26))
w26 <- rep(1 / ncol(Q26), ncol(Q26))
eps26 <- 0.08
w26_plus <- w26 + eps26 * h26
w26_minus <- w26 - eps26 * h26
pattern_plus26 <- drop(Q26 %*% w26_plus)
pattern_minus26 <- drop(Q26 %*% w26_minus)
tv26 <- 0.5 * sum(abs(w26_plus - w26_minus))
check("PROP-26-1", "distinct finite-support G laws have identical known-2PL pattern integrals",
      nrow(Q26) == 2^I26 && ncol(Q26) == 2^I26 + 1L &&
        near(colSums(Q26), rep(1, ncol(Q26)), 1e-12) &&
        max(abs(Q26 %*% h26)) < 1e-12 &&
        near(sum(h26), 0, 1e-12) &&
        min(w26_plus) > 0 && min(w26_minus) > 0 &&
        near(sum(w26_plus), 1, 1e-12) &&
        near(sum(w26_minus), 1, 1e-12) && tv26 > 0.1 &&
        near(pattern_plus26, pattern_minus26, 1e-12) &&
        near(sum(pattern_plus26), 1, 1e-12),
      sprintf("I=%d: %d probabilities (%d independent at most); support TV %.6f; max pattern gap %.2e",
              I26, 2^I26, 2^I26 - 1L, tv26,
              max(abs(pattern_plus26 - pattern_minus26))))

## F-stein — the risk expression against the two values James & Stein state
js_risk <- function(p, s2) {
  lam <- s2 / 2; k <- 0:max(200L, ceiling(lam + 12 * sqrt(lam + 1)))
  p - (p - 2)^2 * sum(dpois(k, lam) / (p - 2 + 2 * k))
}
check("F-stein", "risk = 2 at the origin for every p; -> p as ||xi||^2 grows",
      near(js_risk(3, 0), 2) && near(js_risk(10, 0), 2) && near(js_risk(200, 0), 2) &&
        near(js_risk(10, 4e4), 10, 1e-2),
      sprintf("p=200 at ||xi||^2/p = 1 : %.4f of p", js_risk(200, 200) / 200))

## ------------------------------------------------------------------------
## ---- Appendix A: claims made in the collected proofs ---------------------
## A.2.1 -- the observable pattern law factorizes into item parameters and the
## I+1 score-indexed evaluations of G, including their normalization identity.
set.seed(11)
I_a <- 5
beta_a <- c(-1.4, -0.6, 0.1, 0.8, 1.7)
gq <- seq(-8, 8, length.out = 20001)
Gd <- 0.6 * dnorm(gq, -1.1, 0.5) + 0.4 * dnorm(gq, 1.3, 0.7)   # bimodal G
Gd <- Gd / sum(Gd)
mu_r <- vapply(0:I_a, function(r)
  sum(Gd * exp(r * gq) / apply(outer(gq, beta_a, function(t, b) 1 + exp(t - b)), 1, prod)),
  numeric(1))
pats <- as.matrix(expand.grid(rep(list(0:1), I_a)))
direct <- apply(pats, 1, function(u)
  sum(Gd * apply(outer(gq, seq_len(I_a), function(t, i)
    exp(u[i] * (t - beta_a[i])) / (1 + exp(t - beta_a[i]))), 1, prod)))
viafun <- apply(pats, 1, function(u) exp(-sum(u * beta_a)) * mu_r[sum(u) + 1L])
check("APP-A-2-1", "pattern law = exp(-sum u_i beta_i) * mu_{r}: I+1 functionals suffice",
      near(direct, viafun, 1e-10) && abs(sum(direct) - 1) < 1e-8)
esym <- 1
for (z in exp(-beta_a)) esym <- c(esym, 0) + c(0, esym * z)
check("APP-A-2-1b", "displayed evaluations satisfy the probability-normalization identity",
      near(sum(esym * mu_r), 1, 1e-10),
      "distinct values in one example are not a dimension proof")

## A.2.2 -- the realized DPM mean is non-degenerate: E[sum p_n^2] = 1/(1+alpha).
alpha_a <- 0.8
n_rep <- 4000; n_stick <- 600
sq <- replicate(n_rep, {
  V <- rbeta(n_stick, 1, alpha_a)
  p <- V * c(1, cumprod(1 - V)[-n_stick])
  sum(p^2)
})
base_var_a <- 2
check("APP-A-2-2", "finite positive base variance yields Var(realized DP mean)=v0/(1+alpha)",
      base_var_a > 0 && is.finite(base_var_a) &&
        abs(base_var_a * mean(sq) - base_var_a / (1 + alpha_a)) < 0.02 && all(sq > 0),
      "a centered point-mass base is excluded by the stated positive-variance assumption")

## A.3.2 -- Jensen on 1/x: the MSEM error summary is the larger at any design.
th <- seq(-3, 3, length.out = 4001); wt <- dnorm(th); wt <- wt / sum(wt)
info <- rowSums(outer(th, beta_a, function(t, b) {
  pp <- plogis(t - b); pp * (1 - pp)
}))
check("APP-A-3-2", "E[1/J(theta)] >= 1/E[J(theta)], strict when information varies",
      sum(wt / info) > 1 / sum(wt * info))

tab <- do.call(rbind, results)
dir.create(paths$verification, recursive = TRUE, showWarnings = FALSE)
rr_file <- env_or("DPMIRT_RESULT_REGISTER", file.path(paths$manifest, "result-register.csv"))
rr <- read.csv(rr_file, stringsAsFactors = FALSE, na.strings = c("", "NA"),
               check.names = FALSE)
if (anyDuplicated(rr$id) || any(is.na(rr$id)) || any(!nzchar(rr$id)))
  stop("V8 result register requires unique nonblank IDs")
tab$scope <- ifelse(grepl("^(PROP-13|F-|ch8)", tab$id), "example-grid",
                    ifelse(grepl("^(C-|EQ-)", tab$id), "artifact-arithmetic",
                           "identity-or-invariance"))
app_map <- c("APP-A-1-1" = "THM-15-2", "APP-A-2-1" = "PROP-16-4",
             "APP-A-2-1b" = "PROP-16-4", "APP-A-2-2" = "PROP-16-3",
             "APP-A-3-2" = "PROP-09-1")
tab$result_id <- vapply(tab$id, function(check_id) {
  if (check_id %in% names(app_map)) return(unname(app_map[[check_id]]))
  hit <- rr$id[startsWith(check_id, rr$id)]
  if (!length(hit)) NA_character_ else hit[which.max(nchar(hit))]
}, character(1))
write.csv(tab, file.path(paths$verification, "derivation-checks.csv"), row.names = FALSE)
derived <- rr$id[rr$provenance == "derived-here"]
coverage <- do.call(rbind, lapply(derived, function(id) {
  keep <- !is.na(tab$result_id) & nzchar(tab$result_id) & tab$result_id == id
  hits <- tab$id[keep]
  data.frame(result_id = id, covered = length(hits) > 0L &&
               all(!is.na(hits) & nzchar(hits)),
             check_ids = paste(hits, collapse = ";"), stringsAsFactors = FALSE)
}))
if (any(is.na(coverage$check_ids)) || any(grepl("(^|;)NA($|;)", coverage$check_ids)))
  stop("V8 derivation coverage contains an NA check ID")
write.csv(coverage, file.path(paths$verification, "derivation-coverage.csv"), row.names = FALSE)
result_like <- grepl("^(THM|PROP|APP-A)-", tab$id)
unmapped <- tab$id[result_like & is.na(tab$result_id)]
cat(sprintf("\nV8 numerical checks: %d run; %d failed; derived-result coverage: %d/%d; unmapped result checks: %d\n",
            nrow(tab), sum(!tab$pass), sum(coverage$covered), nrow(coverage), length(unmapped)))
if (any(!tab$pass) || any(!coverage$covered) || length(unmapped)) {
  print(tab[!tab$pass, c("id", "label", "detail")], row.names = FALSE)
  if (any(!coverage$covered))
    print(coverage[!coverage$covered, ], row.names = FALSE)
  if (length(unmapped)) print(unmapped)
  quit(status = 1L)
}
