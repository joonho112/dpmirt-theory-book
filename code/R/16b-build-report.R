## 16b-build-report.R — generate and validate Appendix G's frozen inputs.
##
## This script is intentionally strict: a verifier output is usable only when it
## exists, has the expected current filename, is bound to this build's run token,
## and still matches the byte count and SHA-256 recorded immediately after its
## verifier passed.  Missing or stale output is fatal; it is never translated to NA
## or to a zero issue count.

source("code/R/00-paths.R")

env_or <- function(name, default) {
  z <- Sys.getenv(name)
  if (nzchar(z)) z else default
}
verification_dir <- env_or("DPMIRT_VERIFICATION_DIR", paths$verification)
tables_dir <- env_or("DPMIRT_TABLES_DIR", paths$tables)
validate_only <- identical(Sys.getenv("DPMIRT_BUILD_REPORT_VALIDATE_ONLY"), "1")

verifier_contract <- list(
  V1 = list(
    script = "code/R/03-citation-check.R",
    outputs = c("citation-check.csv", "result-check.csv",
                "part-vi-source-receipt-check.csv",
                "annotation-evidence-check.csv",
                "narrative-citation-check.csv")),
  V2 = list(
    script = "code/R/03-citation-check.R",
    outputs = "v1-v2-citation-anchor.log"),
  V3 = list(
    script = "code/R/10-notation-lint.R",
    outputs = c("notation-lint.csv", "notation-collisions.csv")),
  V4 = list(
    script = "code/R/11-cross-book-check.R",
    outputs = c("cross-book-check.csv", "cross-book-coverage.csv",
                "cross-book-canonical-vocabulary.csv")),
  V5 = list(
    script = "code/R/12-claim-map-check.R",
    outputs = "claim-map-check.csv"),
  V6 = list(
    script = "code/R/08-build-check.R",
    outputs = c("build-check.csv", "expected-render-warnings.csv",
                "figure-bindings.csv")),
  V7 = list(
    script = "code/R/13-release-marker-check.R",
    outputs = c("release-marker-check.csv", "novelty-claim-check.csv")),
  V8 = list(
    script = "code/R/15-derivation-checks.R",
    outputs = c("derivation-checks.csv", "derivation-coverage.csv")),
  V9 = list(
    script = "code/R/18-spec-coverage.R",
    outputs = c("spec-coverage.csv", "spec-contents-coverage.csv",
                "spec-content-items.csv"))
)
stopifnot(identical(names(verifier_contract), paste0("V", 1:9)))

## Claim-level checkers added after external review 025.  They sit outside the
## V1--V9 run-token contract because they audit *claims* rather than repository
## structure, but Appendix G must report them: leaving them undisclosed was the
## residue of finding F-L2-01, whose whole subject was machinery that certifies
## less than a reader assumes.  Each has an issue file and a negative-fixture
## file, and a fixture that fails to be rejected is an issue.
claim_check_contract <- list(
  `C1 evidence claims` = list(
    script = "code/R/25-evidence-claim-check.R",
    check_file = "evidence-claim-check.csv",
    fixture_file = "evidence-claim-fixtures.csv",
    audits = "the evidence-claim register's executable rows"),
  `C2 external sources` = list(
    script = "code/R/25-external-source-check.R",
    check_file = "external-source-ledger-check.csv",
    fixture_file = "external-source-negative-fixtures.csv",
    audits = "companion-store identity, schema, and row counts"),
  `C3 figure claims` = list(
    script = "code/R/26-figure-claim-check.R",
    check_file = "figure-claim-check.csv",
    fixture_file = "figure-claim-negative-fixtures.csv",
    audits = "what each new figure draws against what its caption claims"),
  `C4 release closure` = list(
    script = "code/R/27-release-closure-check.R",
    check_file = "review-025-disposition-check.csv",
    fixture_file = NA_character_,
    audits = "manifest closure and the disposition of every review finding")
)

token_schema <- c("schema_version", "run_token", "started_at_utc")
receipt_schema <- c("schema_version", "run_token", "verifier", "script",
                    "output_file", "sha256", "bytes", "completed_at_utc",
                    "exit_status")

read_csv_fatal <- function(path, label = basename(path)) {
  if (!file.exists(path)) stop(label, " is required but missing")
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
             na.strings = "NA"),
    error = function(e) stop(label, " is unreadable: ", conditionMessage(e)))
}

sha256_file <- function(path) {
  z <- system2("shasum", c("-a", "256", shQuote(path)),
               stdout = TRUE, stderr = TRUE)
  st <- attr(z, "status")
  if ((!is.null(st) && !identical(st, 0L)) || !length(z) ||
      !grepl("^[0-9a-f]{64}[[:space:]]", z[[1]]))
    stop("could not compute SHA-256 for ", path)
  sub("[[:space:]].*$", "", z[[1]])
}

expected_receipts <- do.call(rbind, lapply(names(verifier_contract), function(id) {
  spec <- verifier_contract[[id]]
  data.frame(verifier = id, script = spec$script, output_file = spec$outputs,
             stringsAsFactors = FALSE)
}))
rownames(expected_receipts) <- NULL

validate_current_run <- function(token, receipts, output_dir = verification_dir) {
  if (!identical(names(token), token_schema) || nrow(token) != 1L)
    stop("build-run-token.csv schema/row mismatch")
  if (!identical(as.character(token$schema_version), "1") ||
      is.na(token$run_token) || !nzchar(token$run_token) ||
      is.na(token$started_at_utc) || !nzchar(token$started_at_utc))
    stop("build-run-token.csv contains an invalid current-run token")

  if (!identical(names(receipts), receipt_schema))
    stop("verifier-run-receipts.csv schema mismatch")
  if (nrow(receipts) != nrow(expected_receipts) ||
      anyDuplicated(receipts[c("verifier", "output_file")]))
    stop("verifier-run-receipts.csv does not contain one row per required output")
  if (anyNA(receipts) || any(!nzchar(as.character(receipts$run_token))) ||
      any(!nzchar(receipts$completed_at_utc)) ||
      any(as.character(receipts$schema_version) != "1") ||
      any(as.integer(receipts$exit_status) != 0L))
    stop("verifier-run-receipts.csv contains blank, failed, or invalid rows")
  if (any(receipts$run_token != token$run_token))
    stop("stale verifier receipt: run token differs from current build")
  if (any(basename(receipts$output_file) != receipts$output_file) ||
      any(grepl("[/\\\\]", receipts$output_file)))
    stop("verifier receipt output_file must be a verification-directory basename")

  observed_key <- receipts[order(match(receipts$verifier, paste0("V", 1:9)),
                                  receipts$output_file),
                           c("verifier", "script", "output_file"), drop = FALSE]
  expected_key <- expected_receipts[
    order(match(expected_receipts$verifier, paste0("V", 1:9)),
          expected_receipts$output_file), , drop = FALSE]
  rownames(observed_key) <- rownames(expected_key) <- NULL
  if (!identical(observed_key, expected_key))
    stop("verifier receipt filenames/scripts differ from the V1--V9 contract")

  output_paths <- file.path(output_dir, receipts$output_file)
  if (!all(file.exists(output_paths)))
    stop("required verifier output missing: ",
         paste(receipts$output_file[!file.exists(output_paths)], collapse = ", "))
  info <- file.info(output_paths)
  if (any(is.na(info$isdir)) || any(info$isdir))
    stop("required verifier output is not an ordinary file")
  actual_bytes <- as.numeric(info$size)
  if (any(actual_bytes != as.numeric(receipts$bytes)))
    stop("stale verifier output: byte count differs from current-run receipt")
  actual_hash <- vapply(output_paths, sha256_file, character(1))
  if (any(actual_hash != receipts$sha256))
    stop("stale verifier output: SHA-256 differs from current-run receipt")
  invisible(TRUE)
}

token_file <- file.path(verification_dir, "build-run-token.csv")
receipt_file <- file.path(verification_dir, "verifier-run-receipts.csv")
token <- read_csv_fatal(token_file)
receipts <- read_csv_fatal(receipt_file)
validate_current_run(token, receipts)

rd <- function(f) read_csv_fatal(file.path(verification_dir, f), f)
mf <- function(f) read_csv_fatal(file.path(paths$manifest, f), f)
require_column <- function(d, col, file) {
  if (!col %in% names(d)) stop(file, " lacks required column ", col)
  d[[col]]
}
n_nonblank <- function(f, col) {
  d <- rd(f); z <- require_column(d, col, f)
  sum(!is.na(z) & nzchar(trimws(as.character(z))))
}
n_rows <- function(f) nrow(rd(f))
n_false <- function(f, col) {
  d <- rd(f); z <- require_column(d, col, f)
  if (anyNA(z)) stop(f, " has NA in required logical column ", col)
  sum(!as.logical(z))
}

parse_v2_issues <- function() {
  f <- file.path(verification_dir, "v1-v2-citation-anchor.log")
  x <- readLines(f, warn = FALSE)
  hit <- grep("^V2 text/register anchors: .*; parity issues: [0-9]+$", x,
              value = TRUE)
  if (length(hit) != 1L)
    stop("current V2 log lacks one parseable parity-issues verdict")
  as.integer(sub("^.*parity issues: ([0-9]+)$", "\\1", hit))
}

rr <- mf("result-register.csv")
cx <- mf("corrections.csv")
cb <- mf("cross-book-register.csv")
nt <- mf("notation-register.csv")
cm <- mf("claim-map.csv")
programme_receipts <- mf("programme-artifact-receipts.csv")
narrative_receipts <- mf("narrative-source-receipts.csv")
receipt_sources <- list.files(
  paths$manifest, pattern = "^part-[a-z]+-source-receipts[.]csv$",
  full.names = TRUE)
if (!length(receipt_sources)) stop("no semantic source receipt files found")
rc <- do.call(rbind, lapply(receipt_sources, function(f) {
  z <- read_csv_fatal(f)
  if (ncol(z) < 7L) stop(basename(f), " has fewer than seven receipt columns")
  z[, 1:7, drop = FALSE]
}))

bib <- paste(readLines(file.path(paths$book, "references.bib"), warn = FALSE),
             collapse = "\n")
count_matches <- function(pattern, text) {
  z <- gregexpr(pattern, text, perl = TRUE)[[1]]
  sum(z > 0L)
}
n_bib <- count_matches("\n@[A-Za-z]+\\{", paste0("\n", bib))
n_independent_read <- count_matches("tier = \\{primary-read\\}", bib)
n_programme_read <- count_matches("tier = \\{programme-artifact-read\\}", bib)

dv <- rd("derivation-checks.csv")
dc <- rd("derivation-coverage.csv")

## Cited-source reach.  These public counts still come from the book and its
## registers; only the verifier verdicts depend on current-run receipts.
xr <- c("sec-", "eq-", "fig-", "tbl-", "thm-", "prp-", "def-", "lem-",
        "cor-")
bk <- c(list.files(paths$chapters, pattern = "[.]qmd$", full.names = TRUE),
        list.files(paths$appendices, pattern = "[.]qmd$", full.names = TRUE),
        file.path(paths$book, "index.qmd"))
alltxt <- paste(unlist(lapply(bk, readLines, warn = FALSE)), collapse = "\n")
at <- unique(sub("^@", "", regmatches(
  alltxt, gregexpr("@[A-Za-z0-9_:-]+", alltxt, perl = TRUE))[[1]]))
cited <- at[!vapply(at, function(k) any(startsWith(k, xr)), logical(1))]
tier_of <- function(k) {
  m <- regmatches(
    bib,
    regexpr(paste0("@[A-Za-z]+\\{", k, ",(?:[^@]|\n)*?\n\\}"), bib,
            perl = TRUE))
  if (!length(m)) return(NA_character_)
  t <- regmatches(m, regexpr("tier = \\{[^}]*\\}", m))
  if (!length(t)) NA_character_ else
    sub("tier = \\{([^}]*)\\}", "\\1", t)
}
cited <- cited[!is.na(vapply(cited, tier_of, character(1)))]
ct <- vapply(cited, tier_of, character(1))

T_build <- data.frame(
  Quantity = c(
    "Body chapters", "Appendices", "Numbered results",
    "  of which derived-here", "  of which restated or adapted",
    "Bibliography entries", "  of which primary-read (independent)",
    "  of which programme-artifact-read",
    "Cited sources", "Cited independent sources read",
    "Cited programme artifacts read", "Cited sources held (disclosed)",
    "Semantic source receipts", "Programme-artifact receipts",
    "Narrative-source receipts", "Corrections to the manuscript",
    "  of which carry an author ruling", "Cross-book boundary rows",
    "Notation register rows", "Reviewer claims tracked",
    "  addressed or partially addressed", "Numerical checks (V8)",
    "  of which failed", "Current-run verifier output receipts"),
  Value = c(
    length(list.files(paths$chapters, pattern = "[.]qmd$")),
    length(list.files(paths$appendices, pattern = "[.]qmd$")),
    nrow(rr),
    sum(rr$provenance == "derived-here"),
    sum(rr$provenance %in% c("restated", "adapted")),
    n_bib, n_independent_read, n_programme_read,
    length(cited), sum(ct == "primary-read"),
    sum(ct == "programme-artifact-read"), sum(ct == "primary-held"),
    nrow(rc), nrow(programme_receipts), nrow(narrative_receipts), nrow(cx),
    sum(nzchar(cx$ruled_on)), nrow(cb), nrow(nt), nrow(cm),
    sum(cm$status %in% c("addressed", "partially-addressed")),
    nrow(dv), sum(!as.logical(dv$pass)), nrow(receipts)),
  stringsAsFactors = FALSE)

v1_issues <- sum(
  n_nonblank("citation-check.csv", "problem"),
  n_nonblank("result-check.csv", "problem"),
  n_nonblank("part-vi-source-receipt-check.csv", "issue"),
  n_nonblank("annotation-evidence-check.csv", "issue"),
  n_nonblank("narrative-citation-check.csv", "issue"))
v2_issues <- parse_v2_issues()
v3_issues <- n_rows("notation-lint.csv") + n_rows("notation-collisions.csv")
v7_issues <- n_rows("release-marker-check.csv") +
  n_rows("novelty-claim-check.csv")
v8_issues <- n_false("derivation-checks.csv", "pass") +
  n_false("derivation-coverage.csv", "covered")
v9_cov <- rd("spec-contents-coverage.csv")
low_coverage <- require_column(v9_cov, "low_coverage",
                               "spec-contents-coverage.csv")
if (anyNA(low_coverage) || any(!is.finite(low_coverage)) ||
    any(low_coverage < 0) || any(low_coverage != as.integer(low_coverage)))
  stop("spec-contents-coverage.csv has invalid low_coverage counts")
v9_advisories <- sum(as.integer(low_coverage))

T_verifiers <- data.frame(
  Verifier = c(
    "V1 citations and receipts", "V2 anchor parity", "V3 notation lint",
    "V4 cross-book boundary", "V5 reviewer claim map",
    "V6 render and build check", "V7 release markers and novelty",
    "V8 numerical checks", "V9 specification coverage"),
  `Current output` = c(
    paste(verifier_contract$V1$outputs, collapse = "; "),
    verifier_contract$V2$outputs,
    paste(verifier_contract$V3$outputs, collapse = "; "),
    paste(verifier_contract$V4$outputs, collapse = "; "),
    verifier_contract$V5$outputs,
    paste(verifier_contract$V6$outputs, collapse = "; "),
    paste(verifier_contract$V7$outputs, collapse = "; "),
    paste(verifier_contract$V8$outputs, collapse = "; "),
    paste(verifier_contract$V9$outputs, collapse = "; ")),
  Issues = c(
    v1_issues, v2_issues, v3_issues,
    n_rows("cross-book-check.csv"), n_rows("claim-map-check.csv"),
    n_rows("build-check.csv"), v7_issues, v8_issues,
    n_rows("spec-coverage.csv")),
  Advisories = c(rep(0L, 8L), v9_advisories),
  `Run token` = rep(token$run_token, 9L),
  check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(identical(sub(" .*", "", T_verifiers$Verifier), paste0("V", 1:9)))

## ---- claim-level checkers -------------------------------------------------
## Read each checker's own current output, exactly as the V1--V9 table does.
## A check row that does not pass, or a negative fixture that was not rejected,
## counts as an issue.
claim_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (!length(hit)) stop("claim-check output lacks a verdict column")
  as.logical(df[[hit[[1]]]])
}
claim_rows <- lapply(names(claim_check_contract), function(nm) {
  spec <- claim_check_contract[[nm]]
  chk <- read_csv_fatal(file.path(verification_dir, spec$check_file), spec$check_file)
  verdict <- claim_col(chk, c("pass", "passed", "ok"))
  if (anyNA(verdict)) stop(spec$check_file, " has an unreadable verdict column")
  n_fix <- 0L
  fix_issues <- 0L
  if (!is.na(spec$fixture_file)) {
    fx <- read_csv_fatal(file.path(verification_dir, spec$fixture_file), spec$fixture_file)
    rejected <- claim_col(fx, c("rejected", "pass", "passed"))
    if (anyNA(rejected)) stop(spec$fixture_file, " has an unreadable verdict column")
    n_fix <- nrow(fx)
    fix_issues <- sum(!rejected)
  }
  data.frame(
    Checker = nm,
    `What it audits` = spec$audits,
    `Current output` = if (is.na(spec$fixture_file)) spec$check_file else
      paste(spec$check_file, spec$fixture_file, sep = "; "),
    Checks = nrow(chk),
    `Negative fixtures` = n_fix,
    Issues = sum(!verdict) + fix_issues,
    check.names = FALSE, stringsAsFactors = FALSE)
})
T_claim_checks <- do.call(rbind, claim_rows)

## The evidence-claim register is the honest statement of what is audited and how
## far.  Both counts are read from the register, never typed.
claim_register <- read_csv_fatal(
  file.path(paths$manifest, "evidence-claim-register.csv"),
  "evidence-claim-register.csv")
if (!all(c("assertion_mode", "audit_id") %in% names(claim_register)))
  stop("evidence-claim-register.csv schema mismatch")
n_claims_total <- nrow(claim_register)
n_claims_exec <- sum(claim_register$assertion_mode == "EXECUTABLE")
if (n_claims_exec < 1L || n_claims_exec > n_claims_total)
  stop("evidence-claim-register.csv has an implausible executable count")

T_build <- rbind(T_build, data.frame(
  Quantity = c("Claim-level checkers (beyond V1--V9)",
               "Audited evidence claims",
               "  of which independently recomputed",
               "Negative fixtures rejected"),
  Value = c(nrow(T_claim_checks), n_claims_total, n_claims_exec,
            sum(T_claim_checks$`Negative fixtures`)),
  stringsAsFactors = FALSE))

## Permanent safe mutation fixtures.  The first fixture keeps a real zero-row
## verifier output but gives its receipt an old token.  The second changes only a
## temporary copy of that zero-row output after it was receipted.  The third removes
## a required output only from the temporary copy.  All must be rejected by the same
## validator used for the live report.
is_rejected <- function(expr) {
  tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
}
run_negative_fixtures <- function() {
  stale_receipts <- receipts
  stale_receipts$run_token[stale_receipts$verifier == "V6" &
                             stale_receipts$output_file == "build-check.csv"] <-
    "stale-run-token"
  stale_token_rejected <- is_rejected(
    validate_current_run(token, stale_receipts, verification_dir))

  fixture_dir <- tempfile(pattern = "build-report-current-run-")
  dir.create(fixture_dir, recursive = TRUE)
  on.exit(unlink(fixture_dir, recursive = TRUE, force = TRUE), add = TRUE)
  required_files <- unique(receipts$output_file)
  copied <- file.copy(file.path(verification_dir, required_files), fixture_dir,
                      overwrite = TRUE, copy.mode = TRUE)
  if (!all(copied)) stop("could not stage current-run negative fixtures")

  zero_file <- file.path(fixture_dir, "build-check.csv")
  zero_rows <- read_csv_fatal(zero_file, "fixture build-check.csv")
  if (nrow(zero_rows) != 0L)
    stop("stale-zero fixture requires a passing zero-row build-check.csv")
  writeLines(c(readLines(zero_file, warn = FALSE), "# stale zero fixture"),
             zero_file)
  stale_file_rejected <- is_rejected(
    validate_current_run(token, receipts, fixture_dir))

  unlink(file.path(fixture_dir, "claim-map-check.csv"), force = TRUE)
  missing_file_rejected <- is_rejected(
    validate_current_run(token, receipts, fixture_dir))

  data.frame(
    fixture = c("zero-output-with-stale-run-token",
                "receipted-zero-output-mutated-after-run",
                "required-current-output-missing"),
    rejected = c(stale_token_rejected, stale_file_rejected,
                 missing_file_rejected),
    stringsAsFactors = FALSE)
}

save_tbl <- function(obj, id) {
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
  supplement_dir <- file.path(tables_dir, "supplement")
  dir.create(supplement_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(obj, file.path(tables_dir, paste0(id, ".rds")))
  write.csv(obj, file.path(supplement_dir, paste0(id, ".csv")),
            row.names = FALSE, na = "")
}

if (validate_only) {
  expected_build <- file.path(tables_dir, "T-build-report.rds")
  expected_verifiers <- file.path(tables_dir, "T-verifiers.rds")
  if (!file.exists(expected_build) || !file.exists(expected_verifiers))
    stop("validation-only pass requires frozen Appendix G tables")
  if (!identical(readRDS(expected_build), T_build))
    stop("final current-run T-build-report differs from the rendered table")
  if (!identical(readRDS(expected_verifiers), T_verifiers))
    stop("final current-run T-verifiers differs from the rendered table")
  fixture_file <- file.path(verification_dir,
                            "build-report-negative-fixtures.csv")
  fixture_result <- read_csv_fatal(fixture_file)
  if (!identical(fixture_result$fixture,
                 c("zero-output-with-stale-run-token",
                   "receipted-zero-output-mutated-after-run",
                   "required-current-output-missing")) ||
      any(!as.logical(fixture_result$rejected)))
    stop("build-report negative-fixture receipt is missing or failed")
  cat(sprintf(
    "Appendix G validation-only: token %s; %d/%d current output receipts matched; frozen tables unchanged\n",
    token$run_token, nrow(receipts), nrow(expected_receipts)))
} else {
  fixtures <- run_negative_fixtures()
  if (any(!fixtures$rejected))
    stop("one or more current-run negative fixtures were not rejected")
  write.csv(fixtures,
            file.path(verification_dir, "build-report-negative-fixtures.csv"),
            row.names = FALSE, na = "")
  save_tbl(T_build, "T-build-report")
  save_tbl(T_verifiers, "T-verifiers")
  save_tbl(T_claim_checks, "T-claim-checks")
  writeLines(c(
    paste("Build run token:", token$run_token),
    paste("R version:", getRversion()),
    paste("Platform:", R.version$platform),
    paste("Quarto:", tryCatch(system("quarto --version", intern = TRUE),
                               error = function(e) "unavailable"))),
    file.path(verification_dir, "session-info.txt"))
  cat(sprintf(
    "Appendix G inputs: token %s; %d/%d current output receipts matched; T-build-report %d rows; T-verifiers %d rows; %d/%d stale/missing fixtures rejected\n",
    token$run_token, nrow(receipts), nrow(expected_receipts), nrow(T_build),
    nrow(T_verifiers), sum(fixtures$rejected), nrow(fixtures)))
}
