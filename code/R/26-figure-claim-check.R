## 26-figure-claim-check.R -- executable contracts for reader-facing figures.
##
## This checker binds the stable numerical and categorical claims in the v2/v3
## figure generators to their frozen source artifacts and generated captions.
## Its mutation fixtures operate only in memory and prove that the same
## validators reject the failure modes they are meant to guard.

source("code/R/00-paths.R")
setwd(paths$root)
dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)

near <- function(x, y, tol = 1e-10) {
  length(x) == length(y) && all(is.finite(x)) && all(is.finite(y)) &&
    max(abs(x - y)) <= tol
}

sha256 <- function(path) {
  z <- system2("shasum", c("-a", "256", shQuote(path)),
               stdout = TRUE, stderr = TRUE)
  if (!length(z)) return(NA_character_)
  sub("[[:space:]].*$", "", z[[1]])
}

caption_text <- function(id) {
  paste(readLines(file.path(paths$figures, paste0(id, ".md")), warn = FALSE),
        collapse = " ")
}

has_all <- function(text, needles) {
  text <- tolower(text)
  all(vapply(tolower(needles), function(z) grepl(z, text, fixed = TRUE), logical(1)))
}

has_none <- function(text, needles) {
  text <- tolower(text)
  all(!vapply(tolower(needles), function(z) grepl(z, text, fixed = TRUE), logical(1)))
}

checks <- list()
record_check <- function(id, claim, pass, detail) {
  pass <- isTRUE(pass)
  checks[[length(checks) + 1L]] <<- data.frame(
    check_id = id, claim = claim, pass = pass, detail = detail,
    stringsAsFactors = FALSE)
  invisible(pass)
}

fixtures <- list()
record_fixture <- function(id, mutation, rejected, detail) {
  fixtures[[length(fixtures) + 1L]] <<- data.frame(
    fixture_id = id, mutation = mutation, rejected = isTRUE(rejected),
    detail = detail, stringsAsFactors = FALSE)
}

## ---- concept captions and triple-goal arrow geometry ----------------------
flex_caption_ok <- function(txt) {
  has_all(txt, c("infinitely many population atoms", "count $k_j$",
                 "number occupied", "depends on sample size",
                 "finite population component count $k$")) &&
    has_none(txt, c("turns k into a posterior quantity",
                    "k becomes posterior"))
}

flex_cap <- caption_text("F-flexible-tree")
record_check(
  "FC01", "F-flexible-tree separates population K from occupied-sample K_J",
  flex_caption_ok(flex_cap),
  "DP population count is infinite; K_J is occupied, posterior, and sample-size dependent; MFM places a prior on finite population K")

make_gr_map <- function() {
  set.seed(1129)
  N <- 12L
  th_true <- sort(c(rnorm(6, -1, .4), rnorm(6, 1, .4)))
  pm <- 0.6 * (th_true + rnorm(N, 0, sqrt(1 / 0.6 - 1)))
  qs <- (2 * seq_len(N) - 1) / (2 * N)
  mass <- vapply(qs, function(pp) uniroot(function(x)
    0.5 * pnorm(x, -1, .4) + 0.5 * pnorm(x, 1, .4) - pp,
    c(-4, 4))$root, numeric(1))
  rk <- rank(pm)
  data.frame(start = pm, end = mass[rk], expected_end = mass[rk], rank = rk,
             direction = ifelse(mass[rk] > pm, "right", "left"),
             stringsAsFactors = FALSE)
}

validate_gr_map <- function(z) {
  nrow(z) == 12L && near(z$end, z$expected_end, 1e-14) &&
    identical(rank(z$start), rank(z$end)) &&
    all(sign(z$end - z$start) == ifelse(z$direction == "right", 1, -1)) &&
    any(z$direction == "right") && any(z$direction == "left")
}

gr_map <- make_gr_map()
gr_cap <- caption_text("F-gr-pipeline")
record_check(
  "FC02", "F-gr-pipeline arrowheads terminate at GR and left/right labels match geometry",
  validate_gr_map(gr_map) &&
    has_all(gr_cap, c("arrowhead ends at the triple-goal estimate",
                      "right-pointing arrows have gr greater than pm",
                      "left-pointing arrows have gr less than pm")),
  sprintf("12 arrows: %d right, %d left; rank permutation preserved",
          sum(gr_map$direction == "right"), sum(gr_map$direction == "left")))

## ---- case reliability catalogue: reversal, semantics, and exact tie -------
case_rel_path <- file.path(inputs$case_study, "tables",
                           "P1-T2-reliability-gaps.csv")
case_rel <- read.csv(case_rel_path, stringsAsFactors = FALSE,
                     check.names = FALSE)

validate_case_reliability <- function(x) {
  req <- c("case_id", "marg_rasch", "marg_2pl", "rhobar_rasch", "rhobar_2pl")
  if (!all(req %in% names(x)) || nrow(x) != 13L || anyDuplicated(x$case_id))
    return(FALSE)
  dm <- x$marg_2pl - x$marg_rasch
  dr <- x$rhobar_2pl - x$rhobar_rasch
  c4 <- x$case_id == "C4"
  c13 <- x$case_id == "C13"
  sum(sign(dm) == sign(dr)) == 6L && sum(dm == 0) == 1L &&
    sum(c13) == 1L && x$marg_rasch[c13] == 0.706 &&
    x$marg_2pl[c13] == 0.706 && dr[c13] > 0 &&
    sum(c4) == 1L && dm[c4] > 0 && dr[c4] < 0 &&
    near(c(x$marg_rasch[c4], x$marg_2pl[c4],
           x$rhobar_rasch[c4], x$rhobar_2pl[c4]),
         c(.296, .778, .772, .261), 1e-12)
}

c4_cap <- caption_text("F-c4-reversal")
record_check(
  "FC03", "F-c4-reversal uses distinct-fit semantics, C4 reversal, and a real C13 tie",
  validate_case_reliability(case_rel) &&
    has_all(c4_cap, c("information-ratio catalogue coefficient",
                      "separate empirical-histogram fit", "exact c13 tie",
                      "six of thirteen", "blue arrows point right",
                      "orange arrows point left", "distinct fitted objects")) &&
    has_none(c4_cap, "posterior-variance-based coefficient"),
  "13 cases; 6 direction agreements; C4 +.482 versus -.511; C13 marginal_rxx .706 = .706")

## ---- IRW reliability and shape source universes ---------------------------
full_rel_path <- file.path(inputs$case_study, "data", "raw",
                           "reliability_dataset.csv")
public_rel_path <- file.path(IRW, "irw-reliability-replication", "data-derived",
                             "reliability_dataset_public879.csv")
full_rel <- read.csv(full_rel_path, stringsAsFactors = FALSE, check.names = FALSE)
public_rel <- read.csv(public_rel_path, stringsAsFactors = FALSE, check.names = FALSE)

validate_irw_reliability <- function(frozen, public) {
  req <- c("rel_eap_emp", "rel_wle_sep")
  if (!all(req %in% names(frozen)) || !"rel_eap_emp" %in% names(public))
    return(FALSE)
  eap <- frozen$rel_eap_emp[!is.na(frozen$rel_eap_emp)]
  wle <- frozen$rel_wle_sep[!is.na(frozen$rel_wle_sep)]
  nrow(frozen) == 889L && nrow(public) == 879L &&
    length(eap) == 889L && length(wle) == 879L &&
    sum(wle < 0) == 21L &&
    abs(median(eap) - .8589313) < 1e-7 &&
    abs(median(wle) - .8006960) < 1e-7 &&
    round(100 * mean(eap < .8)) == 30 && round(100 * mean(wle < .8)) == 50
}

rel_cap <- caption_text("F-irw-reliability")
record_check(
  "FC04", "F-irw-reliability preserves WLE n=879 and frozen-889/public-879 provenance",
  validate_irw_reliability(full_rel, public_rel) &&
    has_all(rel_cap, c("wle separation series contains 879",
                      "frozen complete analysis snapshot",
                      "not from the 879-row public release",
                      "twenty-one negative separation estimates")),
  "frozen rows/EAP/WLE = 889/889/879; public rows = 879; 21 WLE values clipped to zero for plotting")

shape_path <- file.path(IRW, "irw-normality-replication", "data-derived",
                        "nonbootstrap-dataset.csv")
shape_raw <- read.csv(shape_path, stringsAsFactors = FALSE, check.names = FALSE)

validate_irw_shape <- function(x) {
  req <- c("ks_normal", "skew", "dip_stat")
  if (!all(req %in% names(x))) return(FALSE)
  x <- x[!is.na(x$ks_normal), , drop = FALSE]
  nrow(x) == 504L && sum(abs(x$skew) > 6) == 3L &&
    abs(median(x$ks_normal) - .109) < .001 &&
    abs(median(x$dip_stat) - .018) < .001
}

shape_cap <- caption_text("F-irw-shapes")
record_check(
  "FC05", "F-irw-shapes reports all three abs(skew)>6 clipped observations",
  validate_irw_shape(shape_raw) &&
    has_all(shape_cap, c("axis clipped at six", "three units",
                        "triangles at the boundary")) &&
    has_none(shape_cap, "one unit sits beyond"),
  "504 analysis rows; 3 estimates have absolute skewness above 6")

## ---- generator and frozen-source hashes -----------------------------------
generator_expected <- c(
  `code/R/22-figures-v2-concepts.R` =
    "f3a8f7843c860daddb2ea244f90e7b3028bcbf275fc618673aed59dbfa77d9ba",
  `code/R/23-figures-v2-theory.R` =
    "ac462c04dca0b23de12a646934923b6218026e98c6e570df2ca278e8c9791538",
  `code/R/24-figures-v2-evidence.R` =
    "4abfe8bab60a8f1a6af66301393294bcd0b8ff4a79aa52856056015a3cdc50bf"
)
generator_actual <- vapply(names(generator_expected), sha256, character(1))
validate_hash_vector <- function(actual, expected) {
  identical(names(actual), names(expected)) && !anyNA(actual) &&
    all(nzchar(actual)) && identical(unname(actual), unname(expected))
}
record_check(
  "FC06", "figure generator source hashes are pinned",
  validate_hash_vector(generator_actual, generator_expected),
  paste(sprintf("%s=%s", basename(names(generator_actual)),
                substr(generator_actual, 1, 12)), collapse = "; "))

ledger <- read.csv(file.path(paths$manifest, "external-source-ledger.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE)
source_ids <- c("SRC-CASE-RELIABILITY", "SRC-IRW-REL-FULL889",
                "SRC-IRW-REL-PUBLIC879", "SRC-IRW-SHAPE504")
source_rows <- ledger[match(source_ids, ledger$source_id), , drop = FALSE]
source_roots <- list(case_study_v1 = inputs$case_study,
                     project_root = PROJECT_ROOT)
source_paths <- vapply(seq_len(nrow(source_rows)), function(i) {
  file.path(source_roots[[source_rows$repository_key[i]]],
            source_rows$relative_artifact[i])
}, character(1))
source_actual <- vapply(source_paths, sha256, character(1))
source_expected <- setNames(source_rows$sha256, source_ids)
names(source_actual) <- source_ids
record_check(
  "FC07", "the four figure evidence sources match their frozen ledger hashes",
  !anyNA(source_rows$source_id) &&
    validate_hash_vector(source_actual, source_expected),
  paste(sprintf("%s=%s", source_ids, substr(source_actual, 1, 12)),
        collapse = "; "))

## ---- read-only contracts for the R23 theory figures -----------------------
rel_fun <- readRDS(file.path(paths$tables, "F-rel-functionals-summary.rds"))
validate_rel_reversal <- function(x) {
  req <- c("model", "n_items", "high_discrimination_item_count",
           "tower_design_high_discrimination_item_count", "sum_lambda_sq",
           "discrimination_sq_resource_ratio_vs_uniform", "rho_bar",
           "rho_eap")
  if (!all(req %in% names(x)) || nrow(x) != 2L)
    return(FALSE)
  u <- x[x$model == "uniform", , drop = FALSE]
  t <- x[x$model == "tower", , drop = FALSE]
  nrow(u) == 1L && nrow(t) == 1L &&
    identical(as.integer(c(u$n_items, t$n_items)), c(16L, 16L)) &&
    identical(as.integer(c(u$high_discrimination_item_count,
                           t$high_discrimination_item_count)), c(0L, 6L)) &&
    all(x$tower_design_high_discrimination_item_count == 6L) &&
    near(c(u$sum_lambda_sq, t$sum_lambda_sq), c(16, 73.984), 1e-12) &&
    near(c(u$discrimination_sq_resource_ratio_vs_uniform,
           t$discrimination_sq_resource_ratio_vs_uniform),
         c(1, 4.624), 1e-12) &&
    u$rho_bar > t$rho_bar && u$rho_eap < t$rho_eap &&
    near(c(u$rho_bar, t$rho_bar, u$rho_eap, t$rho_eap),
         c(.7256429, .5969117, .7307001, .8106988), 1e-6)
}
rel_fun_cap <- caption_text("F-rel-functionals")
record_check(
  "FC08", "F-rel-functionals has the asserted opposite ordering",
  validate_rel_reversal(rel_fun) &&
    has_all(rel_fun_cap, c("same sixteen difficulties", "six central items",
                           "73.984 versus 16.000", "4.624-fold ratio",
                           "order these chosen tests oppositely",
                           "resource-confounded rather than equal-budget")),
  sprintf("16 items; tower 6 high-discrimination items; resource ratio 4.624; MSEM %.6f>%.6f; EAP variance %.6f<%.6f",
          rel_fun$rho_bar[rel_fun$model == "uniform"],
          rel_fun$rho_bar[rel_fun$model == "tower"],
          rel_fun$rho_eap[rel_fun$model == "uniform"],
          rel_fun$rho_eap[rel_fun$model == "tower"]))

shrink <- readRDS(file.path(paths$tables, "F-shrink-deviation-summary.rds"))
validate_shrink <- function(x) {
  req <- c("I", "shift", "rho_bar", "ratio")
  if (!all(req %in% names(x)) || nrow(x) != 14L) return(FALSE)
  x$gap <- x$ratio - sqrt(x$rho_bar)
  a <- x[x$shift == 0, c("I", "gap")]
  b <- x[x$shift == 1.5, c("I", "gap")]
  m <- merge(a, b, by = "I", suffixes = c("_targeted", "_mistargeted"))
  nrow(m) == 7L && all(x$gap > 0) &&
    all(m$gap_mistargeted > m$gap_targeted)
}
shrink_cap <- caption_text("F-shrink-deviation")
record_check(
  "FC09", "F-shrink-deviation stays above sqrt(rho) and mistargeting widens the gap",
  validate_shrink(shrink) &&
    has_all(shrink_cap, c("working-model identity, not a rasch theorem",
                          "realized spread sits slightly above it",
                          "mistargeting the difficulties by 1.5 logits",
                          "no simulation")),
  sprintf("14 designs; positive gaps %.6f to %.6f",
          min(shrink$ratio - sqrt(shrink$rho_bar)),
          max(shrink$ratio - sqrt(shrink$rho_bar))))

make_budget <- function() {
  I <- 20L
  TH <- seq(-6, 6, length.out = 601)
  WG <- dnorm(TH); WG <- WG / sum(WG)
  beta <- qnorm(ppoints(I))
  uniform <- rep(1, I)
  concentrated <- exp(seq(log(.45), log(2.2), length.out = I))
  concentrated <- concentrated * sqrt(I / sum(concentrated^2))
  tmp <- numeric(I)
  tmp[order(abs(beta))] <- sort(concentrated, decreasing = TRUE)
  concentrated <- tmp
  tif <- function(lambda) {
    p <- plogis(sweep(outer(lambda, TH), 1, lambda * beta, "-"))
    colSums(lambda^2 * p * (1 - p))
  }
  rho <- function(lambda) {
    J <- tif(lambda)
    1 / (1 + sum(WG / J))
  }
  list(beta = beta, uniform = uniform, concentrated = concentrated,
       integrated_information = c(sum(uniform), sum(concentrated)),
       peak_test_information = c(max(tif(uniform)), max(tif(concentrated))),
       assignment_cor = cor(abs(beta), concentrated),
       rho = c(rho(uniform), rho(concentrated)))
}

validate_budget <- function(z) {
  near(sum(z$uniform^2), 20, 1e-12) &&
    near(sum(z$concentrated^2), 20, 1e-12) &&
    near(z$integrated_information, c(20, 18.094614840818), 1e-10) &&
    near(z$peak_test_information, c(4.146956453378, 4.823715068330),
         1e-10) &&
    near(z$assignment_cor, -.906093587551, 1e-10) &&
    all(diff(z$concentrated[order(abs(z$beta))]) <= 0) &&
    z$rho[1] > z$rho[2] &&
    near(z$rho, c(.777556235537, .764175965306), 1e-10)
}
budget <- make_budget()
tif_summary <- readRDS(file.path(paths$tables,
                                 "F-tif-concentration-summary.rds"))
validate_tif_summary <- function(x) {
  req <- c("design", "n_items", "sum_lambda_sq",
           "summed_item_peak_height", "integrated_information",
           "integrated_information_vs_uniform_pct",
           "integrated_information_lower_than_uniform_pct",
           "assignment_cor_abs_beta_lambda", "peak_test_information",
           "rho_bar")
  if (!all(req %in% names(x)) || nrow(x) != 2L) return(FALSE)
  u <- x[x$design == "uniform discriminations", , drop = FALSE]
  z <- x[x$design == "concentrated discriminations", , drop = FALSE]
  nrow(u) == 1L && nrow(z) == 1L &&
    identical(as.integer(c(u$n_items, z$n_items)), c(20L, 20L)) &&
    near(c(u$sum_lambda_sq, z$sum_lambda_sq), c(20, 20), 1e-12) &&
    near(c(u$summed_item_peak_height, z$summed_item_peak_height),
         c(5, 5), 1e-12) &&
    near(c(u$integrated_information, z$integrated_information),
         c(20, 18.094614840818), 1e-10) &&
    near(c(u$integrated_information_vs_uniform_pct,
           z$integrated_information_vs_uniform_pct),
         c(100, 90.473074204090), 1e-10) &&
    near(c(u$integrated_information_lower_than_uniform_pct,
           z$integrated_information_lower_than_uniform_pct),
         c(0, 9.526925795910), 1e-10) &&
    is.na(u$assignment_cor_abs_beta_lambda) &&
    near(z$assignment_cor_abs_beta_lambda, -.906093587551, 1e-10) &&
    near(c(u$peak_test_information, z$peak_test_information),
         c(4.146956453378, 4.823715068330), 1e-10) &&
    near(c(u$rho_bar, z$rho_bar), c(.777556235537, .764175965306),
         1e-10) &&
    z$peak_test_information > u$peak_test_information &&
    z$rho_bar < u$rho_bar
}
tif_cap <- caption_text("F-tif-concentration")
record_check(
  "FC10", "F-tif-concentration keeps equal quadratic budgets while distinguishing integrated information",
  validate_budget(budget) && validate_tif_summary(tif_summary) &&
    near(tif_summary$integrated_information,
         budget$integrated_information, 1e-10) &&
    near(tif_summary$peak_test_information,
         budget$peak_test_information, 1e-10) &&
    near(tif_summary$rho_bar, budget$rho, 1e-10) &&
    has_all(tif_cap, c("equal quadratic-discrimination budgets",
                       "do not conserve integrated information",
                       "sum_i \\lambda_i^2=20", "18.0946", "9.53% lower",
                       "strongest items to the centre")),
  sprintf("sum(lambda^2)=%.6f/%.6f; integrated J=%.6f/%.6f; rho %.6f>%.6f",
          sum(budget$uniform^2), sum(budget$concentrated^2),
          budget$integrated_information[1], budget$integrated_information[2],
          budget$rho[1], budget$rho[2]))

dp_mass_path <- file.path(paths$tables, "F-dp-draws-summary.rds")
dp_mass <- if (file.exists(dp_mass_path)) readRDS(dp_mass_path) else data.frame()
validate_dp_mass <- function(x) {
  req <- c("panel", "alpha", "draw", "n_stick", "weight_sum",
           "residual_at_last", "n_kernel_sigma_gt_3", "rows_share_draws")
  if (!all(req %in% names(x)) || nrow(x) != 93L) return(FALSE)
  all(abs(x$weight_sum - 1) < 1e-12) &&
    sum(x$panel == "DP CDF") == 66L &&
    sum(x$panel == "DPM density") == 27L &&
    all(x$n_stick[x$panel == "DP CDF"] == 500L) &&
    all(x$n_stick[x$panel == "DPM density"] == 300L) &&
    all(!x$rows_share_draws) &&
    any(x$n_kernel_sigma_gt_3 > 0, na.rm = TRUE)
}
dp_mass_cap <- caption_text("F-dp-draws")
record_check(
  "FC11", "F-dp-draws uses completed unit-mass sticks and independent rows",
  validate_dp_mass(dp_mass) &&
    has_all(dp_mass_cap, c("twenty-two draws", "nine independent draws",
                           "do not reuse the same atoms or weights",
                           "final stick receives all residual mass",
                           "every weight vector sums to one",
                           "without clipping")),
  if (nrow(dp_mass)) sprintf("%d draws; max |sum(w)-1| %.2e",
                            nrow(dp_mass), max(abs(dp_mass$weight_sum - 1)))
  else "missing F-dp-draws-summary.rds")

## ---- mutation fixtures ----------------------------------------------------
bad_flex <- sub("infinitely many population atoms",
                "a finite K that becomes posterior", flex_cap, fixed = TRUE)
record_fixture("FFX01", "collapse population K and occupied K_J",
               !flex_caption_ok(bad_flex), "caption semantics rejected")

bad_gr <- gr_map
bad_gr$end[1] <- bad_gr$start[1] - (bad_gr$end[1] - bad_gr$start[1])
record_fixture("FFX02", "reverse one GR arrow endpoint",
               !validate_gr_map(bad_gr), "geometry/caption endpoint contract rejected")

bad_tie <- case_rel
bad_tie$marg_2pl[bad_tie$case_id == "C13"] <- .707
record_fixture("FFX03", "turn C13 exact tie into a one-point increase",
               !validate_case_reliability(bad_tie), "C13 mutation rejected")

bad_reversal <- case_rel
bad_reversal$rhobar_2pl[bad_reversal$case_id == "C4"] <- .800
record_fixture("FFX04", "remove the C4 opposite-direction reversal",
               !validate_case_reliability(bad_reversal), "C4 mutation rejected")

bad_wle <- full_rel
bad_wle$rel_wle_sep[which(is.na(bad_wle$rel_wle_sep))[1]] <- 0
record_fixture("FFX05", "change frozen WLE nonmissing denominator to 880",
               !validate_irw_reliability(bad_wle, public_rel),
               "frozen reliability denominator mutation rejected")

bad_public <- rbind(public_rel, public_rel[1, , drop = FALSE])
record_fixture("FFX06", "change public reliability universe to 880 rows",
               !validate_irw_reliability(full_rel, bad_public),
               "public reliability denominator mutation rejected")

bad_shape <- shape_raw
idx_clip <- which(!is.na(bad_shape$skew) & abs(bad_shape$skew) > 6)[1]
bad_shape$skew[idx_clip] <- 5.9
record_fixture("FFX07", "reduce clipped abs(skew)>6 count from three to two",
               !validate_irw_shape(bad_shape), "shape clipping mutation rejected")

bad_generator_hash <- generator_expected
bad_generator_hash[1] <- paste(rep("0", 64), collapse = "")
record_fixture("FFX08", "generator hash drift",
               !validate_hash_vector(generator_actual, bad_generator_hash),
               "generator hash mutation rejected")

bad_source_hash <- source_expected
bad_source_hash[1] <- paste(rep("0", 64), collapse = "")
record_fixture("FFX09", "frozen source hash drift",
               !validate_hash_vector(source_actual, bad_source_hash),
               "source hash mutation rejected")

bad_rel_fun <- rel_fun
bad_rel_fun$rho_eap[bad_rel_fun$model == "tower"] <- .70
record_fixture("FFX10", "remove stylized reliability reversal",
               !validate_rel_reversal(bad_rel_fun),
               "opposite-order mutation rejected")

bad_shrink <- shrink
bad_shrink$ratio[1] <- sqrt(bad_shrink$rho_bar[1]) - .001
record_fixture("FFX11", "move one Rasch spread below sqrt(rho)",
               !validate_shrink(bad_shrink), "shrinkage-direction mutation rejected")

bad_budget <- tif_summary
bad_budget$sum_lambda_sq[bad_budget$design ==
                          "concentrated discriminations"] <- 20.1
record_fixture("FFX12", "break the equal squared-discrimination budget",
               !validate_tif_summary(bad_budget), "budget mutation rejected")

bad_integral <- tif_summary
bad_integral$integrated_information[bad_integral$design ==
                                      "concentrated discriminations"] <- 20
record_fixture("FFX13", "erase the integrated-information difference",
               !validate_tif_summary(bad_integral),
               "integrated-information mutation rejected")

if (nrow(dp_mass)) {
  bad_mass <- dp_mass
  bad_mass$weight_sum[1] <- .99
  mass_rejected <- !validate_dp_mass(bad_mass)
} else {
  mass_rejected <- FALSE
}
record_fixture("FFX14", "break a completed stick's unit-mass contract",
               mass_rejected, "stick-mass mutation rejected")

check_table <- do.call(rbind, checks)
fixture_table <- do.call(rbind, fixtures)
write.csv(check_table,
          file.path(paths$verification, "figure-claim-check.csv"),
          row.names = FALSE, na = "")
write.csv(fixture_table,
          file.path(paths$verification, "figure-claim-negative-fixtures.csv"),
          row.names = FALSE, na = "")

cat(sprintf("Figure claims: %d checks; %d failed; %d/%d mutation fixtures rejected\n",
            nrow(check_table), sum(!check_table$pass),
            sum(fixture_table$rejected), nrow(fixture_table)))
if (any(!check_table$pass)) print(check_table[!check_table$pass, ], row.names = FALSE)
if (any(!fixture_table$rejected))
  print(fixture_table[!fixture_table$rejected, ], row.names = FALSE)
if (any(!check_table$pass) || any(!fixture_table$rejected))
  stop("figure-claim guard failed")
