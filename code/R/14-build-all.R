## 14-build-all.R — single clean-build entry point
## Order: isolate prior verification output -> generate -> render -> V1--V9 ->
## current-run report -> final render -> final V6 and receipt validation.

source("code/R/00-paths.R")
setwd(paths$root)
dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

## Verification output is generated and disposable.  Start from an empty directory,
## but refuse a broad or recursive deletion: only ordinary entries immediately under
## this repository's verification directory may be removed.
root_abs <- normalizePath(paths$root, mustWork = TRUE)
verification_abs <- normalizePath(paths$verification, mustWork = TRUE)
root_prefix <- paste0(root_abs, .Platform$file.sep)
if (!startsWith(verification_abs, root_prefix) ||
    !identical(basename(verification_abs), "verification"))
  stop("refusing to clear verification outside the project verification directory")
old_verification <- list.files(verification_abs, all.files = TRUE, no.. = TRUE,
                               full.names = TRUE, recursive = FALSE)
if (length(old_verification)) {
  info <- file.info(old_verification)
  if (any(is.na(info$isdir)) || any(info$isdir))
    stop("refusing to recursively clear verification; unexpected directory present")
  unlink(old_verification, recursive = FALSE, force = FALSE)
  if (any(file.exists(old_verification)))
    stop("could not isolate prior verification outputs")
}

run_token <- paste(
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
  Sys.getpid(),
  sub("^.*-", "", basename(tempfile(pattern = "run-token-"))),
  sep = "-")
run_token_file <- file.path(paths$verification, "build-run-token.csv")
write.csv(data.frame(
  schema_version = "1",
  run_token = run_token,
  started_at_utc = utc_now(),
  stringsAsFactors = FALSE),
  run_token_file, row.names = FALSE, na = "")

## Normalized current-run receipt contract.  V1 and V2 share one script.  V1's
## five rows name the files that script actually writes today; V2 has no standalone
## issue CSV, so its current-run output is the captured log containing the explicit
## `parity issues` verdict.  This deliberately excludes the obsolete
## narrative-citation-audit-check.csv and result-anchor-check.csv filenames.
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

sha256_file <- function(path) {
  z <- system2("shasum", c("-a", "256", shQuote(path)),
               stdout = TRUE, stderr = TRUE)
  st <- attr(z, "status")
  if ((!is.null(st) && !identical(st, 0L)) || !length(z) ||
      !grepl("^[0-9a-f]{64}[[:space:]]", z[[1]]))
    stop("could not compute SHA-256 for ", path)
  sub("[[:space:]].*$", "", z[[1]])
}

receipt_file <- file.path(paths$verification, "verifier-run-receipts.csv")
receipts <- data.frame(
  schema_version = character(), run_token = character(), verifier = character(),
  script = character(), output_file = character(), sha256 = character(),
  bytes = numeric(), completed_at_utc = character(), exit_status = integer(),
  stringsAsFactors = FALSE)

write_receipts <- function() {
  tmp <- tempfile(pattern = ".verifier-run-receipts-",
                  tmpdir = paths$verification, fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(receipts, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, receipt_file))
    stop("could not atomically publish verifier-run-receipts.csv")
}

record_receipts <- function(verifiers, script) {
  completed <- utc_now()
  new_rows <- list()
  for (id in verifiers) {
    spec <- verifier_contract[[id]]
    if (is.null(spec) || !identical(spec$script, script))
      stop("verifier contract/script mismatch for ", id)
    output_paths <- file.path(paths$verification, spec$outputs)
    if (!all(file.exists(output_paths)))
      stop(id, " required verifier output missing: ",
           paste(spec$outputs[!file.exists(output_paths)], collapse = ", "))
    output_info <- file.info(output_paths)
    if (any(is.na(output_info$isdir)) || any(output_info$isdir))
      stop(id, " verifier output is not an ordinary file")
    new_rows[[id]] <- data.frame(
      schema_version = "1", run_token = run_token, verifier = id,
      script = script, output_file = spec$outputs,
      sha256 = vapply(output_paths, sha256_file, character(1)),
      bytes = as.numeric(output_info$size), completed_at_utc = completed,
      exit_status = 0L, stringsAsFactors = FALSE)
  }
  receipts <<- receipts[!receipts$verifier %in% verifiers, , drop = FALSE]
  receipts <<- rbind(receipts, do.call(rbind, new_rows))
  verifier_order <- match(receipts$verifier, paste0("V", 1:9))
  receipts <<- receipts[order(verifier_order, receipts$output_file), , drop = FALSE]
  rownames(receipts) <<- NULL
  write_receipts()
}

run_r <- function(script) {
  cat("\n==>", script, "\n")
  status <- system2(file.path(R.home("bin"), "Rscript"), script)
  if (!identical(status, 0L)) stop(script, " failed with exit status ", status)
}

run_verifier <- function(script, verifiers, log_name) {
  log_path <- file.path(paths$verification, log_name)
  cat("\n==>", script, "[", paste(verifiers, collapse = "/"), "]\n")
  status <- system2(file.path(R.home("bin"), "Rscript"), script,
                    stdout = log_path, stderr = log_path)
  if (file.exists(log_path))
    cat(paste(readLines(log_path, warn = FALSE), collapse = "\n"), "\n")
  if (!identical(status, 0L))
    stop(script, " failed with exit status ", status, "; see ", log_path)
  record_receipts(verifiers, script)
}

for (s in c("code/R/01-harvest-bibtex.R", "code/R/02-seed-references.R",
            "code/R/19-citation-context-match.R",
            "code/R/04-reconcile-appendices.R", "code/R/07-tables.R",
            "code/R/20-evidence-tables.R",
            "code/R/25-external-source-check.R",
            "code/R/25-evidence-claim-check.R",
            "code/R/22-figures-v2-concepts.R", "code/R/23-figures-v2-theory.R",
            "code/R/24-figures-v2-evidence.R",
            "code/R/26-figure-claim-check.R",
            "code/R/17-annotated-bibliography.R",
            "code/R/27-release-closure-check.R")) run_r(s)

## F-dpm-fit is the book's one live model fit.  A normal rebuild reuses its
## deterministic cache; a genuinely clean build must be able to create that cache
## rather than silently omitting the figure.
dpm_fit_cache <- file.path(paths$tables, "F-dpm-fit-cache.rds")
if (!file.exists(dpm_fit_cache)) run_r("code/R/16-dpm-fit-artifact.R")
run_r("code/R/09-figures.R")

## T-build-report and T-verifiers can only be finalized after V1--V9 have run,
## while the bootstrap render needs both files in order to give V6 a complete HTML
## tree.  On a clean checkout, create deliberately incomplete bootstrap-only tables.
## code/R/16b-build-report.R replaces them after it validates the current-run
## receipts, and the final render therefore never publishes these placeholders.
bootstrap_build_file <- file.path(paths$tables, "T-build-report.rds")
bootstrap_verifier_file <- file.path(paths$tables, "T-verifiers.rds")
if (!file.exists(bootstrap_build_file) || !file.exists(bootstrap_verifier_file)) {
  tier_file <- file.path(paths$tables, "T-bib-tiers.rds")
  if (!file.exists(tier_file))
    stop("clean-build bootstrap requires T-bib-tiers.rds")
  tier_table <- readRDS(tier_file)
  bibliography_entries <- sum(as.integer(tier_table$Entries))
  if (!file.exists(bootstrap_build_file)) {
    bootstrap_build <- data.frame(
      Quantity = c("Bibliography entries", "Numerical checks (V8)"),
      Value = c(bibliography_entries, 0L), stringsAsFactors = FALSE)
    saveRDS(bootstrap_build, bootstrap_build_file)
    write.csv(bootstrap_build,
              file.path(paths$tables, "supplement", "T-build-report.csv"),
              row.names = FALSE, na = "")
  }
  if (!file.exists(bootstrap_verifier_file)) {
    bootstrap_verifiers <- data.frame(
      Verifier = paste0("V", 1:9),
      `Current output` = rep("bootstrap pending", 9),
      Issues = rep(NA_integer_, 9), Advisories = rep(NA_integer_, 9),
      `Run token` = rep("bootstrap-pending", 9),
      check.names = FALSE, stringsAsFactors = FALSE)
    saveRDS(bootstrap_verifiers, bootstrap_verifier_file)
    write.csv(bootstrap_verifiers,
              file.path(paths$tables, "supplement", "T-verifiers.csv"),
              row.names = FALSE, na = "")
  }
}

quarto <- Sys.which("quarto"); if (!nzchar(quarto)) stop("quarto is required")
render_log <- file.path(paths$verification, "quarto-render.log")
render_book <- function(label) {
  cat("\n==> quarto render book", label, "\n")
  status <- system2(quarto, c("render", "book"),
                    stdout = render_log, stderr = render_log)
  cat(paste(readLines(render_log, warn = FALSE), collapse = "\n"), "\n")
  if (!identical(status, 0L))
    stop("Quarto render failed", label, "; see ", render_log)
}

## Bootstrap render: its sole purpose is to give V6 a complete rendered tree.
## Appendix G is rendered again from current-run receipts below.
Sys.setenv(DPMIRT_APPENDIX_G_BOOTSTRAP = "1")
render_book(" (bootstrap pass)")
Sys.unsetenv("DPMIRT_APPENDIX_G_BOOTSTRAP")

run_verifier("code/R/03-citation-check.R", c("V1", "V2"),
             "v1-v2-citation-anchor.log")
run_verifier("code/R/10-notation-lint.R", "V3", "v3-notation.log")
run_verifier("code/R/11-cross-book-check.R", "V4", "v4-cross-book.log")
run_verifier("code/R/12-claim-map-check.R", "V5", "v5-claim-map.log")
run_verifier("code/R/08-build-check.R", "V6", "v6-build-check.log")
run_verifier("code/R/13-release-marker-check.R", "V7", "v7-release.log")
run_verifier("code/R/15-derivation-checks.R", "V8", "v8-derivation.log")
run_verifier("code/R/18-spec-coverage.R", "V9", "v9-spec.log")

## Only now can Appendix G inputs be generated: 16b rejects a missing, stale,
## wrong-token, or hash-mismatched required verifier output.
run_r("code/R/16b-build-report.R")
render_book(" (current-run report pass)")

## The report pass changes rendered HTML, so V6 must inspect that final tree.  Its
## receipt is replaced under the same run token.  A validation-only report pass then
## proves that the frozen tables still equal the recomputed current-run verdicts
## without touching their mtimes and making the render stale again.
run_verifier("code/R/08-build-check.R", "V6", "v6-build-check.log")
Sys.setenv(DPMIRT_BUILD_REPORT_VALIDATE_ONLY = "1")
run_r("code/R/16b-build-report.R")
Sys.unsetenv("DPMIRT_BUILD_REPORT_VALIDATE_ONLY")

a <- readBin(file.path(paths$refs, "references.bib"), "raw",
             file.info(file.path(paths$refs, "references.bib"))$size)
b <- readBin(file.path(paths$book, "references.bib"), "raw",
             file.info(file.path(paths$book, "references.bib"))$size)
if (!identical(a, b)) stop("refs/references.bib and book/references.bib differ")
cat("\nALL BUILD AND CURRENT-RUN QA CHECKS PASSED\n")
