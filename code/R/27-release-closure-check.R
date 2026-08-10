## 27-release-closure-check.R — strict manifest and review-025 disposition gate.

source("code/R/00-paths.R")
setwd(paths$root)
dir.create(paths$verification, recursive = TRUE, showWarnings = FALSE)

manifest_files <- sort(list.files(paths$manifest, pattern = "[.]csv$",
                                  full.names = TRUE))
if (!length(manifest_files)) stop("no manifest CSV files found")

id_names <- c("id", "audit_id", "match_id", "receipt_id", "finding_id",
              "source_id")
rows <- lapply(manifest_files, function(f) {
  raw <- readBin(f, "raw", file.info(f)$size)
  nul <- any(raw == as.raw(0))
  parsed <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
             na.strings = character(), fill = FALSE),
    error = identity)
  if (inherits(parsed, "error"))
    return(data.frame(
      file = sub(paste0("^", paths$root, "/"), "", f), rows = NA_integer_,
      columns = NA_integer_, id_column = "", blank_ids = NA_integer_,
      duplicate_ids = NA_integer_, status = "FAIL",
      detail = conditionMessage(parsed), stringsAsFactors = FALSE))

  header_bad <- !length(names(parsed)) || any(!nzchar(names(parsed))) ||
    anyDuplicated(names(parsed)) > 0L
  id_col <- intersect(names(parsed), id_names)
  id_col <- if (length(id_col)) id_col[[1]] else ""
  blank_ids <- duplicate_ids <- 0L
  if (nzchar(id_col)) {
    ids <- trimws(as.character(parsed[[id_col]]))
    blank_ids <- sum(!nzchar(ids))
    duplicate_ids <- sum(duplicated(ids) & nzchar(ids))
  }
  fail <- nul || header_bad || blank_ids > 0L || duplicate_ids > 0L
  detail <- c(if (nul) "NUL byte", if (header_bad) "invalid header",
              if (blank_ids) paste(blank_ids, "blank IDs"),
              if (duplicate_ids) paste(duplicate_ids, "duplicate IDs"))
  data.frame(
    file = sub(paste0("^", paths$root, "/"), "", f), nrow(parsed), ncol(parsed),
    id_column = id_col, blank_ids = blank_ids, duplicate_ids = duplicate_ids,
    status = if (fail) "FAIL" else "PASS",
    detail = paste(detail, collapse = "; "), stringsAsFactors = FALSE,
    check.names = FALSE)
})
manifest_check <- do.call(rbind, rows)
names(manifest_check)[2:3] <- c("rows", "columns")
write.csv(manifest_check,
          file.path(paths$verification, "manifest-closure.csv"),
          row.names = FALSE, na = "")
if (any(manifest_check$status != "PASS")) {
  print(manifest_check[manifest_check$status != "PASS", ], row.names = FALSE)
  stop("strict manifest closure failed")
}

disposition_file <- file.path(paths$manifest, "review-025-dispositions.csv")
disposition <- read.csv(disposition_file, stringsAsFactors = FALSE,
                        check.names = FALSE, na.strings = character(), fill = FALSE)
expected_ids <- c(
  sprintf("F-L1-%02d", 1:16), sprintf("F-L2-%02d", 1:10),
  sprintf("F-L3-%02d", 1:6), sprintf("F-L4-%02d", 1:2))
required_columns <- c(
  "finding_id", "original_severity", "original_blocking", "disposition",
  "v3_resolution", "evidence_locator", "verification",
  "release_blocking_open")
issues <- character()
if (!identical(names(disposition), required_columns))
  issues <- c(issues, "disposition schema differs from the release contract")
if (!identical(disposition$finding_id, expected_ids))
  issues <- c(issues, "finding IDs are not the exact ordered 34-row inventory")
if (any(!disposition$disposition %in% c("FIXED", "REFRAMED", "ACCEPTED_RISK")))
  issues <- c(issues, "invalid disposition value")
if (any(disposition$original_blocking == "yes" &
        disposition$disposition == "ACCEPTED_RISK"))
  issues <- c(issues, "a release-blocking finding was accepted as risk")
if (sum(disposition$original_blocking == "yes") != 27L)
  issues <- c(issues, "original blocking count is not 27")
if (any(disposition$release_blocking_open != "no"))
  issues <- c(issues, "at least one release-blocking finding remains open")
for (field in c("v3_resolution", "evidence_locator", "verification"))
  if (any(!nzchar(trimws(disposition[[field]]))))
    issues <- c(issues, paste("blank disposition field:", field))

disposition_check <- data.frame(
  check = c("exact inventory", "original blockers", "valid dispositions",
            "blocking accepted risks", "release blockers open",
            "nonblank evidence fields"),
  observed = c(nrow(disposition), sum(disposition$original_blocking == "yes"),
               paste(sort(unique(disposition$disposition)), collapse = "; "),
               sum(disposition$original_blocking == "yes" &
                     disposition$disposition == "ACCEPTED_RISK"),
               sum(disposition$release_blocking_open != "no"),
               sum(!nzchar(trimws(disposition$v3_resolution)) |
                     !nzchar(trimws(disposition$evidence_locator)) |
                     !nzchar(trimws(disposition$verification)))),
  expected = c("34 ordered IDs", "27", "FIXED; REFRAMED; ACCEPTED_RISK",
               "0", "0", "0"),
  passed = c(identical(disposition$finding_id, expected_ids),
             sum(disposition$original_blocking == "yes") == 27L,
             all(disposition$disposition %in%
                   c("FIXED", "REFRAMED", "ACCEPTED_RISK")),
             !any(disposition$original_blocking == "yes" &
                    disposition$disposition == "ACCEPTED_RISK"),
             !any(disposition$release_blocking_open != "no"),
             all(nzchar(trimws(disposition$v3_resolution))) &&
               all(nzchar(trimws(disposition$evidence_locator))) &&
               all(nzchar(trimws(disposition$verification)))),
  stringsAsFactors = FALSE)
write.csv(disposition_check,
          file.path(paths$verification, "review-025-disposition-check.csv"),
          row.names = FALSE, na = "")
if (length(issues)) stop(paste(issues, collapse = "; "))

cat(sprintf(
  "Release closure: %d manifest CSVs strict-parsed; 34/34 review findings dispositioned; 0 release blockers open\n",
  nrow(manifest_check)))
