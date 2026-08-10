## 25-evidence-claim-check.R -- claim-level provenance and executable evidence guards.
##
## The register is the canonical v3 statement of the 54 audited evidence claims.
## This checker enforces its exact schema and source locators, then independently
## recomputes the highest-risk claims.  It does not rewrite chapters or companion
## stores.  Mutation fixtures prove that the guards reject the failure modes that
## motivated the register.

source("code/R/00-paths.R")

env_or <- function(name, default) {
  value <- Sys.getenv(name)
  if (nzchar(value)) value else default
}

register_file <- env_or(
  "DPMIRT_EVIDENCE_CLAIM_REGISTER",
  file.path(paths$manifest, "evidence-claim-register.csv")
)
out_dir <- env_or("DPMIRT_VERIFICATION_DIR", paths$verification)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
check_file <- file.path(out_dir, "evidence-claim-check.csv")
fixture_file <- file.path(out_dir, "evidence-claim-fixtures.csv")

assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}
near <- function(x, target, tolerance = 1e-8) {
  length(x) == length(target) && all(is.finite(x)) &&
    all(abs(x - target) <= tolerance)
}

## ---- register contract ----------------------------------------------------

expected_schema <- c(
  "audit_id", "chapter", "chapter_line", "claim_text", "source",
  "source_edition", "snapshot_locator", "field", "observed", "expected",
  "current_status", "assertion_mode", "evidence_receipt", "note"
)
assert(file.exists(register_file), "missing evidence claim register")
claims <- read.csv(register_file, stringsAsFactors = FALSE, check.names = FALSE)
assert(identical(names(claims), expected_schema),
       "evidence claim register schema or column order changed")
assert(nrow(claims) == 54L, "evidence claim register must contain exactly 54 rows")
assert(identical(claims$audit_id, sprintf("A%03d", seq_len(54L))),
       "evidence claim IDs must be unique and sequential A001--A054")
assert(!anyDuplicated(claims$audit_id), "duplicate evidence claim ID")

required_text <- setdiff(expected_schema, "chapter")
for (field_name in required_text) {
  value <- claims[[field_name]]
  assert(!any(is.na(value) | !nzchar(trimws(as.character(value)))),
         paste("blank required evidence-claim field:", field_name))
}
assert(all(claims$chapter %in% c(25L, 27L, 28L)),
       "evidence claims must belong to chapter 25, 27, or 28")
assert(identical(as.integer(table(claims$chapter)), c(12L, 18L, 24L)),
       "evidence claim chapter partition must remain 12/18/24")
assert(all(grepl("^[0-9]+(?:-[0-9]+)?$", claims$chapter_line, perl = TRUE)),
       "invalid chapter_line in evidence claim register")
assert(all(vapply(seq_len(nrow(claims)), function(i) {
  grepl(sprintf("book/chapters/%02d-", claims$chapter[i]),
        claims$evidence_receipt[i], fixed = TRUE)
}, logical(1))), "an evidence receipt does not name its registered chapter")

allowed_status <- c("VERIFIED", "CORRECTED_VALUE", "CORRECTED_SOURCE")
allowed_mode <- c("REGISTERED", "EXECUTABLE")
assert(all(claims$current_status %in% allowed_status),
       "unknown evidence-claim current_status")
assert(all(claims$assertion_mode %in% allowed_mode),
       "unknown evidence-claim assertion_mode")
assert(identical(claims$audit_id[claims$current_status == "CORRECTED_VALUE"],
                 c("A021", "A022", "A023", "A028", "A029", "A049")),
       "the six corrected-value claims changed")
assert(identical(claims$audit_id[claims$current_status == "CORRECTED_SOURCE"], "A032"),
       "the corrected-source claim must be A032")
assert(all(claims$observed == claims$expected),
       "canonical v3 observed and expected values must agree")

executable_ids <- c(
  "A021", "A022", "A023", "A027", "A028", "A029",
  "A031", "A032", "A033", "A048", "A049"
)
assert(identical(claims$audit_id[claims$assertion_mode == "EXECUTABLE"], executable_ids),
       "executable assertion inventory changed without checker coverage")

locators <- unique(trimws(unlist(strsplit(
  claims$snapshot_locator, ";", fixed = TRUE
))))
assert(length(locators) > 0L && all(nzchar(locators)), "blank snapshot locator")
assert(!any(startsWith(locators, "/") | grepl("(^|/)\\.\\.(/|$)", locators)),
       "snapshot locators must be project-root-relative and may not traverse upward")
snapshot_paths <- file.path(PROJECT_ROOT, locators)
missing_snapshots <- locators[!file.exists(snapshot_paths)]
assert(!length(missing_snapshots), paste(
  "missing registered evidence snapshot(s):",
  paste(missing_snapshots, collapse = ", ")
))

claim_expected <- function(id) {
  value <- claims$expected[match(id, claims$audit_id)]
  assert(length(value) == 1L && !is.na(value), paste("unknown audit id", id))
  value
}

checks <- list()
record_check <- function(id, label, condition, observed) {
  expected <- claim_expected(id)
  checks[[length(checks) + 1L]] <<- data.frame(
    audit_id = id,
    check = label,
    pass = isTRUE(condition) && identical(observed, expected),
    observed = observed,
    expected = expected,
    stringsAsFactors = FALSE
  )
}

## ---- validators used by both real checks and mutation fixtures ------------

validate_sim_facts <- function(facts) {
  for (id in c("H1", "H2", "H7")) {
    x <- facts[[id]]
    assert(is.list(x) && all(c("estimate", "ci_low", "ci_high") %in% names(x)),
           paste(id, "fact is incomplete"))
    assert(is.finite(x$estimate) && is.finite(x$ci_low) && is.finite(x$ci_high),
           paste(id, "contains a non-finite estimate or interval"))
    assert(x$ci_low <= x$estimate && x$estimate <= x$ci_high,
           paste(id, "estimate contradicts its unchanged interval"))
  }
  assert(near(facts$H1$estimate, -0.220886437060841, 1e-12),
         "H1 estimate drift")
  assert(near(facts$H2$estimate, -0.114430895577304, 1e-12),
         "H2 estimate drift")
  assert(near(facts$H7$estimate, 0.280716211125934, 1e-12),
         "H7 estimate drift")
  invisible(TRUE)
}

validate_lever <- function(lever_rows, lever_facts) {
  required_labels <- c("KS_EDF", "MSEL", "MSELR", "quantile", "tail_cutoff")
  assert(!any(is.na(lever_rows$metric_family) | !nzchar(lever_rows$metric_family)),
         "missing lever label")
  assert(setequal(unique(lever_rows$metric_family), required_labels),
         "lever label universe drift")
  assert(nrow(lever_facts) == 5L &&
           setequal(lever_facts$metric_family, required_labels),
         "lever fact labels drift")
  summary_geomean <- exp(tapply(
    lever_rows$summary_lever, lever_rows$metric_family, mean
  ))
  prior_geomean <- exp(tapply(
    lever_rows$prior_lever, lever_rows$metric_family, mean
  ))
  fact_summary <- setNames(exp(lever_facts$summary_lever_log),
                           lever_facts$metric_family)
  fact_prior <- setNames(exp(lever_facts$prior_lever_log),
                         lever_facts$metric_family)
  assert(near(summary_geomean[required_labels], fact_summary[required_labels], 1e-12),
         "summary lever aggregation drift")
  assert(near(prior_geomean[required_labels], fact_prior[required_labels], 1e-12),
         "prior lever aggregation drift")
  list(
    summary_geomean = summary_geomean,
    prior_geomean = prior_geomean,
    summary_median = tapply(exp(lever_rows$summary_lever),
                            lever_rows$metric_family, median),
    prior_median = tapply(exp(lever_rows$prior_lever),
                          lever_rows$metric_family, median)
  )
}

compute_h7_census <- function(condition_rows) {
  keep <- condition_rows$posterior_summary %in% c("PM", "GR") &
    condition_rows$metric_family %in% c("KS_EDF", "MSEL")
  dat <- condition_rows[keep, c(
    "condition_key", "prior_arm", "posterior_summary", "metric_family",
    "equal_form_weighted_loss_mean"
  )]
  ks <- reshape(
    dat[dat$metric_family == "KS_EDF",
        c("condition_key", "prior_arm", "posterior_summary",
          "equal_form_weighted_loss_mean")],
    idvar = c("condition_key", "prior_arm"), timevar = "posterior_summary",
    direction = "wide"
  )
  names(ks)[grepl("[.]GR$", names(ks))] <- "KS_GR"
  names(ks)[grepl("[.]PM$", names(ks))] <- "KS_PM"
  msel <- reshape(
    dat[dat$metric_family == "MSEL",
        c("condition_key", "prior_arm", "posterior_summary",
          "equal_form_weighted_loss_mean")],
    idvar = c("condition_key", "prior_arm"), timevar = "posterior_summary",
    direction = "wide"
  )
  names(msel)[grepl("[.]GR$", names(msel))] <- "MSEL_GR"
  names(msel)[grepl("[.]PM$", names(msel))] <- "MSEL_PM"
  joined <- merge(ks, msel, by = c("condition_key", "prior_arm"), sort = TRUE)
  joined <- joined[joined$prior_arm != "dp_broad", ]
  joined$gap_ks <- log(joined$KS_PM / joined$KS_GR)
  joined$gap_msel <- log(joined$MSEL_PM / joined$MSEL_GR)
  joined$predicted <- joined$gap_ks > 0 & joined$gap_msel < 0
  assert(nrow(joined) == 240L, "H7 condition-by-prior universe drift")
  assert(sum(joined$predicted) == 239L, "H7 quadrant census drift")
  exception <- joined[!joined$predicted, ]
  assert(nrow(exception) == 1L &&
           exception$condition_key == "2pl|500|0.9|bimodal_strong" &&
           exception$prior_arm == "gaussian" &&
           near(exception$gap_ks, -0.004062294, 1e-9) &&
           near(exception$gap_msel, -0.1553222, 1e-7),
         "H7 exception identity or value drift")
  joined
}

validate_census <- function(evidence_rows) {
  primary <- evidence_rows[
    evidence_rows$contrast_id == "contrast_dp_focused_gr_vs_gaussian_gr", ]
  assert(nrow(primary) == 120L, "primary evidence-cell universe drift")
  harm <- primary[primary$evidence_label == "harm_flag", ]
  at_one <- abs(harm$ci_high - 1) <= 1e-12
  result <- c(
    flags = nrow(harm),
    registered_span = sum(harm$ci_low <= 1 & harm$ci_high > 1),
    closed_contains = sum(harm$ci_low <= 1 & harm$ci_high >= 1),
    at_or_below = sum(harm$ci_high <= 1),
    strictly_below = sum(harm$ci_high < 1),
    ends_at_one = sum(at_one),
    real_cost = sum(harm$ci_low > 1)
  )
  assert(identical(unname(result), c(57L, 49L, 51L, 5L, 3L, 2L, 3L)),
         "primary evidence endpoint census drift")
  result
}

validate_reliability <- function(frozen, public, frozen_path, public_path,
                                 frozen_md5, public_md5) {
  actual_frozen_md5 <- unname(tools::md5sum(frozen_path))
  actual_public_md5 <- unname(tools::md5sum(public_path))
  assert(identical(actual_frozen_md5, frozen_md5),
         "frozen reliability snapshot hash drift")
  assert(identical(actual_public_md5, public_md5),
         "public reliability snapshot hash drift")
  result <- c(
    frozen_rows = nrow(frozen),
    public_rows = nrow(public),
    frozen_eap = sum(!is.na(frozen$rel_eap_emp)),
    frozen_wle = sum(!is.na(frozen$rel_wle_sep)),
    public_eap = sum(!is.na(public$rel_eap_emp)),
    public_wle = sum(!is.na(public$rel_wle_sep))
  )
  assert(identical(unname(result), c(889L, 879L, 889L, 879L, 879L, 869L)),
         "frozen/public reliability universe drift")
  assert(near(median(frozen$rel_eap_emp, na.rm = TRUE), 0.8589313, 1e-7) &&
           near(median(frozen$rel_wle_sep, na.rm = TRUE), 0.8006960, 1e-7) &&
           near(mean(frozen$rel_eap_emp < .8, na.rm = TRUE), 0.298088, 1e-6) &&
           near(mean(frozen$rel_wle_sep < .8, na.rm = TRUE), 0.498294, 1e-6),
         "frozen reliability headline drift")
  result
}

validate_seed <- function(case_facts, seed_table, pair_rows) {
  shapes <- c("bimodal", "normal", "skewed", "undetermined")
  assert(setequal(seed_table$shape, shapes), "seed shape universe drift")
  assert(all(shapes %in% names(case_facts$seed_churn)),
         "seed fact universe drift")
  values <- unlist(case_facts$seed_churn[shapes], use.names = TRUE)
  assert(near(values, c(6.451613, 4, 2, 8.695652), 1e-6),
         "same-method seed churn drift")
  tab_values <- setNames(seed_table$pct_changed_within, seed_table$shape)
  assert(near(tab_values[shapes], values[shapes], 1e-6),
         "seed table and v2 facts disagree")
  assert(nrow(pair_rows) == 234L, "all-pairs seed universe drift")
  overall_max <- max(pair_rows$pct_changed)
  max_rows <- pair_rows[
    abs(pair_rows$pct_changed - overall_max) <= 1e-10, ]
  max_ids <- sort(paste(
    max_rows$case_id, max_rows$item_model, max_rows$arm, max_rows$summ,
    sep = "|"
  ))
  expected_max_ids <- sort(c(
    "C12|rasch|dp_broad|PM",
    "C12|rasch|dp_broad|CB",
    "C12|twopl|dp_broad|GR"
  ))
  assert(near(overall_max, 32.35294, 1e-5) &&
           identical(max_ids, expected_max_ids),
         "all-pairs seed maximum or argmax set drift")

  gaussian_pm <- pair_rows[
    pair_rows$arm == "gaussian" & pair_rows$summ == "PM", ]
  gaussian_pm_max <- gaussian_pm[which.max(gaussian_pm$pct_changed), ]
  assert(nrow(gaussian_pm) == 26L && nrow(gaussian_pm_max) == 1L &&
           near(gaussian_pm_max$pct_changed, 28, 1e-12) &&
           gaussian_pm_max$case_id == "C4" &&
           gaussian_pm_max$item_model == "rasch",
         "Gaussian+PM seed subset maximum or argmax drift")
  list(
    values = values,
    overall_max = overall_max,
    max_rows = max_rows,
    gaussian_pm_n = nrow(gaussian_pm),
    gaussian_pm_max = gaussian_pm_max
  )
}

## ---- load authoritative snapshots and run real assertions -----------------

sim_facts_path <- file.path(inputs$sim_book_v3, "data", "derived", "book-facts.rds")
sim_cond_path <- file.path(inputs$sim_book_v3, "data", "derived", "cond.rds")
sim_lever_path <- file.path(inputs$sim_book_v3, "data", "derived", "lever.rds")
sim_evidence_path <- file.path(inputs$sim_book_v3, "data", "derived", "evidence.rds")
case_facts_path <- file.path(inputs$case_study_v2, "data", "derived", "book-facts.rds")
seed_table_path <- file.path(inputs$case_study, "tables",
                             "P3-T14-selection-noise-within-method.csv")
seed_pairs_path <- file.path(inputs$case_study, "results", "verification",
                             "P3-selection-noise-pairs.csv")
frozen_reliability_path <- file.path(inputs$case_study, "data", "raw",
                                     "reliability_dataset.csv")
public_reliability_path <- file.path(
  IRW, "irw-reliability-replication", "data-derived",
  "reliability_dataset_public879.csv"
)

sim_facts <- readRDS(sim_facts_path)
sim_cond <- readRDS(sim_cond_path)
sim_lever <- readRDS(sim_lever_path)
sim_evidence <- readRDS(sim_evidence_path)
case_facts <- readRDS(case_facts_path)
seed_table <- read.csv(seed_table_path, stringsAsFactors = FALSE, check.names = FALSE)
seed_pairs <- read.csv(seed_pairs_path, stringsAsFactors = FALSE, check.names = FALSE)
frozen_reliability <- read.csv(
  frozen_reliability_path, stringsAsFactors = FALSE, check.names = FALSE
)
public_reliability <- read.csv(
  public_reliability_path, stringsAsFactors = FALSE, check.names = FALSE
)

validate_sim_facts(sim_facts)
h7 <- compute_h7_census(sim_cond)
lever <- validate_lever(sim_lever, as.data.frame(sim_facts$lever))
census <- validate_census(sim_evidence)
reliability <- validate_reliability(
  frozen_reliability, public_reliability,
  frozen_reliability_path, public_reliability_path,
  "cfb1c5cb51a5940dd64383dfa12b574f",
  "60a56e08b6f105742f8c045a04d2d184"
)
seed <- validate_seed(case_facts, seed_table, seed_pairs)

h7_effect <- sprintf(
  "%.6f log units; %.4f%% on ratio scale",
  sim_facts$H7$estimate, 100 * (exp(sim_facts$H7$estimate) - 1)
)
record_check("A021", "H7 log and ratio-scale units",
             near(sim_facts$H7$estimate, 0.280716211125934, 1e-12), h7_effect)

h7_exception <- h7[!h7$predicted, ]
h7_census_text <- sprintf(
  "%d of %d; exception %s %s",
  sum(h7$predicted), nrow(h7), h7_exception$condition_key,
  tools::toTitleCase(h7_exception$prior_arm)
)
record_check("A022", "H7 condition-by-prior census",
             sum(h7$predicted) == 239L, h7_census_text)

lever_text <- sprintf(
  "KS geometric means %.6f/%.6f; true medians %.6f/%.6f",
  lever$summary_geomean["KS_EDF"], lever$prior_geomean["KS_EDF"],
  lever$summary_median["KS_EDF"], lever$prior_median["KS_EDF"]
)
record_check("A023", "lever geometric-mean aggregation and label",
             !near(lever$summary_geomean["KS_EDF"],
                   lever$summary_median["KS_EDF"], 1e-4), lever_text)

record_check("A027", "primary flag census", census["flags"] == 57,
             sprintf("%d", census["flags"]))
record_check(
  "A028", "inclusive/strict parity endpoint census",
  census["registered_span"] == 49 && census["ends_at_one"] == 2 &&
    census["closed_contains"] == 51,
  sprintf("%d cross parity; %d touch parity from below",
          census["registered_span"], census["ends_at_one"])
)
record_check(
  "A029", "at-or-below parity endpoint census",
  census["at_or_below"] == 5 && census["strictly_below"] == 3 &&
    census["ends_at_one"] == 2,
  sprintf("%d strictly below", census["strictly_below"])
)

record_check(
  "A031", "frozen complete reliability universe",
  reliability["frozen_rows"] == 889 && reliability["frozen_eap"] == 889,
  sprintf("%d rows; %d valid EAP",
          reliability["frozen_rows"], reliability["frozen_eap"])
)
record_check(
  "A032", "frozen versus public reliability source",
  reliability["frozen_rows"] == 889 && reliability["public_rows"] == 879,
  sprintf("frozen %d; public %d",
          reliability["frozen_rows"], reliability["public_rows"])
)
reliability_text <- sprintf(
  "EAP n=%d median %.7f share %.4f%%; WLE n=%d median %.7f share %.4f%%",
  reliability["frozen_eap"], median(frozen_reliability$rel_eap_emp, na.rm = TRUE),
  100 * mean(frozen_reliability$rel_eap_emp < .8, na.rm = TRUE),
  reliability["frozen_wle"], median(frozen_reliability$rel_wle_sep, na.rm = TRUE),
  100 * mean(frozen_reliability$rel_wle_sep < .8, na.rm = TRUE)
)
record_check("A033", "frozen reliability medians, shares, and valid n",
             reliability["frozen_wle"] == 879, reliability_text)

seed_text <- sprintf(
  "%.4f%%; %.0f%%; %.0f%%; %.4f%%",
  seed$values["bimodal"], seed$values["normal"], seed$values["skewed"],
  seed$values["undetermined"]
)
record_check("A048", "four-class same-method seed churn",
             length(seed$values) == 4L, seed_text)
seed_scope_text <- sprintf(
  "all 234: %.5f%% at %d C12 fits; Gaussian+PM %d: %.0f%% at C4 Rasch",
  seed$overall_max, nrow(seed$max_rows), seed$gaussian_pm_n,
  seed$gaussian_pm_max$pct_changed
)
record_check(
  "A049", "all-pairs and Gaussian+PM seed maxima",
  nrow(seed$max_rows) == 3L && seed$gaussian_pm_n == 26L,
  seed_scope_text
)

check_table <- do.call(rbind, checks)
assert(identical(unique(check_table$audit_id), executable_ids),
       "checker output does not cover every executable claim")
write.csv(check_table, check_file, row.names = FALSE, na = "")

## ---- mutation fixtures ----------------------------------------------------

fixtures <- list()
expect_rejection <- function(id, mutation, expression) {
  detail <- tryCatch({
    force(expression)
    "mutation was accepted"
  }, error = function(e) conditionMessage(e))
  rejected <- !identical(detail, "mutation was accepted")
  fixtures[[length(fixtures) + 1L]] <<- data.frame(
    fixture_id = id,
    mutation = mutation,
    rejected = rejected,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

sim_bad_h1 <- sim_facts
sim_bad_h1$H1$estimate <- sim_bad_h1$H1$estimate + 0.1
expect_rejection("FX01", "temporary in-memory H1 estimate mutation",
                 validate_sim_facts(sim_bad_h1))

sim_bad_h2 <- sim_facts
sim_bad_h2$H2$estimate <- -0.314
expect_rejection("FX02", "H2 estimate-only mutation with unchanged CI/prose contract",
                 validate_sim_facts(sim_bad_h2))

lever_bad <- sim_lever
lever_bad$metric_family[lever_bad$metric_family == "KS_EDF"] <- ""
expect_rejection("FX03", "missing lever metric label",
                 validate_lever(lever_bad, as.data.frame(sim_facts$lever)))

sim_bad_ci <- sim_facts
sim_bad_ci$H1$ci_low <- sim_bad_ci$H1$estimate + 0.01
sim_bad_ci$H1$ci_high <- sim_bad_ci$H1$estimate + 0.02
expect_rejection("FX04", "estimate outside a contradictory confidence interval",
                 validate_sim_facts(sim_bad_ci))

sim_bad_h7 <- sim_facts
sim_bad_h7$H7$estimate <- sim_bad_h7$H7$estimate + 0.01
expect_rejection("FX05", "H7 effect drift",
                 validate_sim_facts(sim_bad_h7))

seed_bad <- seed_table[seed_table$shape != "undetermined", ]
expect_rejection("FX06", "seed universe drops the undetermined class",
                 validate_seed(case_facts, seed_bad, seed_pairs))

expect_rejection(
  "FX07", "frozen reliability snapshot hash drift",
  validate_reliability(
    frozen_reliability, public_reliability,
    frozen_reliability_path, public_reliability_path,
    paste(rep("0", 32L), collapse = ""),
    "60a56e08b6f105742f8c045a04d2d184"
  )
)

fixture_table <- do.call(rbind, fixtures)
write.csv(fixture_table, fixture_file, row.names = FALSE, na = "")
assert(nrow(fixture_table) == 7L && all(fixture_table$rejected),
       "one or more evidence-claim mutation fixtures were not rejected")
assert(all(check_table$pass), "one or more executable evidence claims failed")

cat(sprintf(
  "Evidence claims: %d registered; %d executable checks passed; %d/%d mutation fixtures rejected\n",
  nrow(claims), nrow(check_table), sum(fixture_table$rejected), nrow(fixture_table)
))
