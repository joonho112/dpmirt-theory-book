## 25-external-source-check.R — bind external evidence labels to exact artifacts.

source("code/R/00-paths.R")
setwd(paths$root)

ledger_path <- file.path(paths$manifest, "external-source-ledger.csv")
stopifnot(file.exists(ledger_path))
ledger <- read.csv(ledger_path, stringsAsFactors = FALSE, check.names = FALSE,
                   na.strings = c("", "NA"))

required <- c("source_id", "role", "cited_edition", "frozen_store_edition",
              "repository_key", "relative_artifact", "artifact_type",
              "required_schema", "physical_rows", "analysis_rows", "sha256",
              "usage_contract")
stopifnot(identical(names(ledger), required), nrow(ledger) == 11L,
          !anyDuplicated(ledger$source_id), !anyNA(ledger$source_id),
          all(nzchar(ledger$usage_contract)))

roots <- list(
  sim_book_v3 = inputs$sim_book_v3,
  case_study_v2 = inputs$case_study_v2,
  case_study_v1 = inputs$case_study,
  project_root = PROJECT_ROOT
)

sha256 <- function(path) {
  out <- system2("shasum", c("-a", "256", shQuote(path)),
                 stdout = TRUE, stderr = TRUE)
  if (!length(out)) return(NA_character_)
  sub("[[:space:]].*$", "", out[[1]])
}

inspect_source <- function(row) {
  root <- roots[[row$repository_key]]
  path <- if (is.null(root)) NA_character_ else file.path(root, row$relative_artifact)
  exists <- !is.na(path) && file.exists(path)
  actual_sha <- if (exists) sha256(path) else NA_character_
  schema_ok <- rows_ok <- analysis_ok <- FALSE
  actual_rows <- actual_analysis <- NA_integer_

  if (exists && identical(row$artifact_type, "CSV")) {
    dat <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    actual_rows <- nrow(dat)
    needed <- strsplit(row$required_schema, "|", fixed = TRUE)[[1]]
    schema_ok <- all(needed %in% names(dat))
    rows_ok <- identical(actual_rows, as.integer(row$physical_rows))
    actual_analysis <- switch(
      row$source_id,
      `SRC-IRW-REL-FULL889` = sum(!is.na(dat$rel_eap_emp)),
      `SRC-IRW-REL-PUBLIC879` = sum(!is.na(dat$rel_eap_emp)),
      `SRC-IRW-SHAPE504` = sum(!is.na(dat$ks_normal)),
      actual_rows
    )
    analysis_ok <- identical(actual_analysis, as.integer(row$analysis_rows))
  } else if (exists && identical(row$artifact_type, "RDS-list")) {
    dat <- readRDS(path)
    actual_rows <- length(dat)
    needed <- strsplit(row$required_schema, "|", fixed = TRUE)[[1]]
    schema_ok <- is.list(dat) && all(needed %in% names(dat))
    rows_ok <- identical(actual_rows, as.integer(row$physical_rows))
    actual_analysis <- NA_integer_
    analysis_ok <- is.na(row$analysis_rows)
  } else if (exists && identical(row$artifact_type, "RDS-data-frame")) {
    dat <- as.data.frame(readRDS(path))
    actual_rows <- nrow(dat)
    needed <- strsplit(row$required_schema, "|", fixed = TRUE)[[1]]
    schema_ok <- all(needed %in% names(dat))
    rows_ok <- identical(actual_rows, as.integer(row$physical_rows))
    actual_analysis <- switch(
      row$source_id,
      `SRC-SIM-COND` = {
        keep <- dat$metric_family == "KS_EDF" & dat$posterior_summary == "PM" &
          dat$prior_arm %in% c("gaussian", "dp_focused")
        nrow(unique(dat[keep, c("condition_key", "prior_arm"), drop = FALSE]))
      },
      `SRC-SIM-EVIDENCE` = sum(dat$contrast_id ==
        "contrast_dp_focused_gr_vs_gaussian_gr"),
      actual_rows
    )
    analysis_ok <- identical(actual_analysis, as.integer(row$analysis_rows))
  } else if (exists && identical(row$artifact_type, "text")) {
    txt <- readLines(path, warn = FALSE)
    needed <- strsplit(row$required_schema, "|", fixed = TRUE)[[1]]
    schema_ok <- all(vapply(needed, function(x) any(grepl(x, txt, fixed = TRUE)), logical(1)))
    rows_ok <- is.na(row$physical_rows)
    analysis_ok <- is.na(row$analysis_rows)
  }

  data.frame(
    source_id = row$source_id,
    exists = exists,
    hash_matches = identical(actual_sha, row$sha256),
    schema_matches = schema_ok,
    physical_rows_match = rows_ok,
    analysis_rows_match = analysis_ok,
    actual_rows = actual_rows,
    actual_analysis_rows = actual_analysis,
    actual_sha256 = actual_sha,
    pass = exists && identical(actual_sha, row$sha256) && schema_ok && rows_ok && analysis_ok,
    stringsAsFactors = FALSE
  )
}

checks <- do.call(rbind, lapply(seq_len(nrow(ledger)), function(i) inspect_source(ledger[i, ])))
write.csv(checks, file.path(paths$verification, "external-source-ledger-check.csv"),
          row.names = FALSE, na = "")
if (!all(checks$pass)) {
  print(checks[!checks$pass, ], row.names = FALSE)
  stop("External-source ledger mismatch")
}

## Safe negative fixtures: mutate only an in-memory ledger row. Each defect must
## be rejected by the same checker used above.
bad_hash <- ledger[1, ]; bad_hash$sha256 <- paste(rep("0", 64), collapse = "")
bad_rows <- ledger[ledger$source_id == "SRC-IRW-REL-FULL889", ]; bad_rows$physical_rows <- 888L
bad_schema <- ledger[ledger$source_id == "SRC-IRW-SHAPE504", ]; bad_schema$required_schema <- paste0(bad_schema$required_schema, "|missing_field")
fixtures <- data.frame(
  fixture = c("source-hash-drift", "reliability-row-drift", "shape-schema-drift"),
  rejected = c(!inspect_source(bad_hash)$pass,
               !inspect_source(bad_rows)$pass,
               !inspect_source(bad_schema)$pass),
  stringsAsFactors = FALSE
)
write.csv(fixtures, file.path(paths$verification, "external-source-negative-fixtures.csv"),
          row.names = FALSE)
stopifnot(all(fixtures$rejected))

cat(sprintf("External-source ledger: %d/%d artifacts pinned; %d/%d negative fixtures rejected.\n",
            sum(checks$pass), nrow(checks), sum(fixtures$rejected), nrow(fixtures)))
