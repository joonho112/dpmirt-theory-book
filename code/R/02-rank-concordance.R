## 02-rank-concordance.R — shared deterministic machinery for PROP-20-1.
##
## The figure generator and V8 source this file. Latent distributions are represented by
## midpoint-quantile quadrature on their full support; no common [-6,6] truncation is used.
## Score ties receive half credit, matching randomized ordering within a tied score pair.

rank_midpoints <- function(n) (seq_len(n) - 0.5) / n

rank_shape_names <- c("normal", "bimodal", "skewed", "heavy-tailed logistic")

rank_quadrature <- function(shape, n = 800L) {
  stopifnot(shape %in% rank_shape_names, n >= 100L)
  if (shape == "bimodal") {
    m <- as.integer(n %/% 2L)
    p <- rank_midpoints(m)
    theta <- c(qnorm(p, -1, 0.3), qnorm(p, 1, 0.3)) / sqrt(1 + 0.3^2)
    weight <- rep(1 / (2 * m), 2 * m)
  } else {
    p <- rank_midpoints(as.integer(n))
    theta <- switch(shape,
      normal = qnorm(p),
      skewed = (qgamma(p, shape = 2, rate = 1) - 2) / sqrt(2),
      `heavy-tailed logistic` = qlogis(p) * sqrt(3) / pi)
    weight <- rep(1 / length(theta), length(theta))
  }
  ord <- order(theta)
  list(theta = theta[ord], weight = weight[ord], shape = shape)
}

rank_score_pmf <- function(prob) {
  out <- 1
  for (p in prob) out <- c(out * (1 - p), 0) + c(0, out * p)
  out
}

rank_score_matrix <- function(beta, theta) {
  t(vapply(theta, function(z)
    rank_score_pmf(plogis(z - beta)), numeric(length(beta) + 1L)))
}

rank_wbar <- function(beta, quadrature) {
  information <- vapply(quadrature$theta, function(z) {
    p <- plogis(z - beta)
    sum(p * (1 - p))
  }, numeric(1))
  1 / (1 + sum(quadrature$weight / information))
}

rank_concordance <- function(beta, quadrature) {
  likelihood <- rank_score_matrix(beta, quadrature$theta)
  accumulated <- numeric(ncol(likelihood))
  concordance <- 0
  for (j in seq_len(nrow(likelihood))) {
    lower_score_mass <- c(0, head(cumsum(accumulated), -1))
    concordance <- concordance + 2 * quadrature$weight[j] *
      sum(likelihood[j, ] * (lower_score_mass + 0.5 * accumulated))
    accumulated <- accumulated + quadrature$weight[j] * likelihood[j, ]
  }
  ## Equal quadrature nodes approximate a continuous G; exclude the artificial event that
  ## both independently drawn persons select the same numerical quadrature node.
  concordance / (1 - sum(quadrature$weight^2))
}

rank_near_tie_mass <- function(quadrature, delta = 0.25) {
  distance <- abs(outer(quadrature$theta, quadrature$theta, "-"))
  sum(outer(quadrature$weight, quadrature$weight)[distance < delta])
}

rank_reliability_curve <- function(item_counts = c(6, 10, 16, 25, 40, 60, 90, 130, 180),
                                   n = 800L) {
  out <- vector("list", length(rank_shape_names))
  for (s in seq_along(rank_shape_names)) {
    shape <- rank_shape_names[s]
    quadrature <- rank_quadrature(shape, n)
    vals <- t(vapply(item_counts, function(items) {
      beta <- seq(-1.6, 1.6, length.out = items)
      c(wbar = rank_wbar(beta, quadrature),
        concordance = rank_concordance(beta, quadrature))
    }, numeric(2)))
    out[[s]] <- data.frame(
      shape = shape, items = item_counts,
      wbar = vals[, "wbar"], concordance = vals[, "concordance"],
      quadrature_nodes = length(quadrature$theta),
      stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

rank_match_reliability <- function(curves, n_grid = 61L) {
  split_curve <- split(curves, curves$shape)
  lower <- max(vapply(split_curve, function(z) min(z$wbar), numeric(1)))
  upper <- min(vapply(split_curve, function(z) max(z$wbar), numeric(1)))
  lower <- ceiling(lower * 100) / 100
  upper <- floor(upper * 100) / 100
  if (lower >= upper) stop("latent-shape curves have no common reliability support")
  reliability <- seq(lower, upper, length.out = n_grid)
  matched <- do.call(rbind, lapply(split_curve, function(z) {
    z <- z[order(z$wbar), ]
    data.frame(
      shape = z$shape[1], wbar = reliability,
      concordance = approx(z$wbar, z$concordance, xout = reliability,
                           method = "linear", ties = "ordered", rule = 1)$y,
      stringsAsFactors = FALSE)
  }))
  rownames(matched) <- NULL
  matched
}

rank_gap_curve <- function(matched) {
  split_w <- split(matched, matched$wbar)
  data.frame(
    wbar = as.numeric(names(split_w)),
    minimum = vapply(split_w, function(z) min(z$concordance), numeric(1)),
    maximum = vapply(split_w, function(z) max(z$concordance), numeric(1)),
    mean = vapply(split_w, function(z) mean(z$concordance), numeric(1)),
    stringsAsFactors = FALSE) |>
    transform(gap = maximum - minimum) |>
    (function(z) z[order(z$wbar), ])()
}

rank_reliability_analysis <- function(n = 800L,
                                      item_counts = c(6, 10, 16, 25, 40, 60, 90, 130, 180),
                                      n_grid = 61L) {
  curves <- rank_reliability_curve(item_counts, n)
  matched <- rank_match_reliability(curves, n_grid)
  gaps <- rank_gap_curve(matched)
  near_ties <- data.frame(
    shape = rank_shape_names,
    near_tie_mass = vapply(rank_shape_names, function(shape)
      rank_near_tie_mass(rank_quadrature(shape, n)), numeric(1)),
    stringsAsFactors = FALSE)
  list(curves = curves, matched = matched, gaps = gaps, near_ties = near_ties,
       n = n, item_counts = item_counts)
}

rank_reliability_summary <- function(fine, coarse) {
  effect_low <- min(fine$gaps$wbar)
  effect_high <- max(fine$gaps$wbar)
  interpolate_gap <- function(x, y, at) approx(x, y, xout = at, rule = 2)$y
  mean_low <- interpolate_gap(fine$gaps$wbar, fine$gaps$mean, effect_low)
  mean_high <- interpolate_gap(fine$gaps$wbar, fine$gaps$mean, effect_high)
  effect <- mean_high - mean_low
  imax <- which.max(fine$gaps$gap)
  max_gap <- fine$gaps$gap[imax]
  max_gap_w <- fine$gaps$wbar[imax]

  curve_pair <- merge(
    fine$curves[, c("shape", "items", "wbar", "concordance")],
    coarse$curves[, c("shape", "items", "wbar", "concordance")],
    by = c("shape", "items"), suffixes = c("_fine", "_coarse"))
  max_conc_diff <- max(abs(curve_pair$concordance_fine - curve_pair$concordance_coarse))
  max_wbar_diff <- max(abs(curve_pair$wbar_fine - curve_pair$wbar_coarse))

  summary <- data.frame(
    metric = c("quadrature_nodes_fine", "quadrature_nodes_coarse",
               "comparison_wbar_low", "comparison_wbar_high",
               "mean_concordance_low", "mean_concordance_high",
               "reliability_effect", "maximum_matched_shape_gap",
               "wbar_at_maximum_gap", "effect_to_max_gap_ratio",
               "highest_common_wbar", "gap_at_highest_common_wbar",
               "maximum_fine_coarse_concordance_difference",
               "maximum_fine_coarse_wbar_difference"),
    value = c(fine$n, coarse$n, effect_low, effect_high, mean_low, mean_high,
              effect, max_gap, max_gap_w, effect / max_gap,
              max(fine$gaps$wbar), tail(fine$gaps$gap, 1),
              max_conc_diff, max_wbar_diff),
    stringsAsFactors = FALSE)
  near <- data.frame(
    metric = paste0("near_tie_mass_", gsub("[^a-z]+", "_", fine$near_ties$shape)),
    value = fine$near_ties$near_tie_mass,
    stringsAsFactors = FALSE)
  list(summary = rbind(summary, near), convergence = curve_pair)
}
