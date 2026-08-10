## Permanent V1 fixtures for the two annotation-receipt routes introduced in v3.
## Every mutation lives under tempdir(); production bibliography/manifests are read-only.

source("code/R/00-paths.R")

runner <- file.path(paths$root, "code/R/03-citation-check.R")
programme_live <- file.path(paths$manifest, "programme-artifact-receipts.csv")
narrative_live <- file.path(paths$manifest, "narrative-source-receipts.csv")

run_v1 <- function(programme_file, narrative_file) {
  out <- tempfile(pattern = "v1-receipt-output-")
  dir.create(out, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE, force = TRUE), add = TRUE)
  z <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(runner),
    stdout = TRUE, stderr = TRUE,
    env = c(
      paste0("DPMIRT_VERIFICATION_DIR=", shQuote(out)),
      paste0("DPMIRT_PROGRAMME_ARTIFACT_RECEIPTS=", shQuote(programme_file)),
      paste0("DPMIRT_NARRATIVE_SOURCE_RECEIPTS=", shQuote(narrative_file)))))
  status <- attr(z, "status")
  if (is.null(status)) status <- 0L
  issue_file <- file.path(out, "annotation-evidence-check.csv")
  issues <- if (file.exists(issue_file)) {
    d <- read.csv(issue_file, stringsAsFactors = FALSE, check.names = FALSE)
    paste(d$issue[!is.na(d$issue) & nzchar(d$issue)], collapse = "; ")
  } else "annotation-evidence-check.csv missing"
  list(status = as.integer(status), output = paste(z, collapse = "\n"),
       issues = issues)
}

positive <- run_v1(programme_live, narrative_live)

fixture_dir <- tempfile(pattern = "v1-receipt-fixtures-")
dir.create(fixture_dir, recursive = TRUE)
on.exit(unlink(fixture_dir, recursive = TRUE, force = TRUE), add = TRUE)

programme_missing <- read.csv(programme_live, stringsAsFactors = FALSE,
                              check.names = FALSE)
programme_missing <- programme_missing[programme_missing$receipt_id != "PAR-008", ]
programme_missing_file <- file.path(fixture_dir, "programme-missing.csv")
write.csv(programme_missing, programme_missing_file, row.names = FALSE, na = "")
missing <- run_v1(programme_missing_file, narrative_live)

narrative_mismatch <- read.csv(narrative_live, stringsAsFactors = FALSE,
                               check.names = FALSE)
narrative_mismatch$receipt_id[narrative_mismatch$receipt_id == "NSR-001"] <-
  "NSR-MUTATED"
narrative_mismatch_file <- file.path(fixture_dir, "narrative-mismatch.csv")
write.csv(narrative_mismatch, narrative_mismatch_file, row.names = FALSE, na = "")
mismatch <- run_v1(programme_live, narrative_mismatch_file)

receipt <- data.frame(
  fixture = c("positive-current-receipts",
              "missing-programme-artifact-receipt",
              "narrative-receipt-id-not-bound-to-bibliography"),
  expected = c("exit 0", "nonzero and lacks receipt",
               "nonzero and missing from bibliography verified_claims"),
  observed = c(
    paste("exit", positive$status, "issues:", positive$issues,
          substr(positive$output, 1, 250)),
    paste("exit", missing$status, "issues:", missing$issues,
          substr(missing$output, 1, 250)),
    paste("exit", mismatch$status, "issues:", mismatch$issues,
          substr(mismatch$output, 1, 250))),
  passed = c(
    positive$status == 0L,
    missing$status != 0L && grepl("programme-artifact-read key lacks receipt",
                                 missing$issues, fixed = TRUE),
    mismatch$status != 0L && grepl("missing from bibliography verified_claims",
                                   mismatch$issues, fixed = TRUE)),
  stringsAsFactors = FALSE)

if (any(!receipt$passed)) {
  print(receipt, row.names = FALSE)
  stop("V1 annotation-receipt fixture failure")
}
write.csv(receipt, file.path(paths$verification, "v1-receipt-fixtures.csv"),
          row.names = FALSE, na = "")
cat(sprintf("V1 receipt fixtures passed: %d/%d\n",
            sum(receipt$passed), nrow(receipt)))
