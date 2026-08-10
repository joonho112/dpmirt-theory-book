## 12-claim-map-check.R — V5 reviewer-claim coverage
source("code/R/00-paths.R")
f <- Sys.getenv("DPMIRT_CLAIM_MAP")
if (!nzchar(f)) f <- file.path(paths$manifest, "claim-map.csv")
inventory_file <- Sys.getenv("DPMIRT_REVIEWER_INVENTORY")
if (!nzchar(inventory_file))
  inventory_file <- file.path(paths$manifest, "reviewer-comment-inventory.csv")
if (!file.exists(f)) stop("missing claim map")
if (!file.exists(inventory_file)) stop("missing independent reviewer-comment inventory")
r <- read.csv(f, stringsAsFactors = FALSE, na.strings = "")
inv <- read.csv(inventory_file, stringsAsFactors = FALSE, na.strings = "")
need <- c("id", "reviewer", "claim", "status", "file", "anchor", "evidence")
if (!all(need %in% names(r))) stop("claim-map schema mismatch")
if (anyDuplicated(r$id)) stop("duplicate claim-map IDs")
if (!all(c("id", "reviewer") %in% names(inv)) || anyDuplicated(inv$id) ||
    any(is.na(inv$id)) || any(!nzchar(inv$id)) ||
    any(is.na(inv$reviewer)) || any(!nzchar(inv$reviewer)))
  stop("invalid reviewer-comment inventory")
allowed <- c("addressed", "partially-addressed", "planned", "out-of-scope")
bad <- character()
for (id in setdiff(inv$id, r$id)) bad <- c(bad, paste(id, "missing from claim map"))
for (id in setdiff(r$id, inv$id)) bad <- c(bad, paste(id, "absent from reviewer inventory"))
common <- intersect(inv$id, r$id)
for (id in common) {
  if (!identical(inv$reviewer[match(id, inv$id)], r$reviewer[match(id, r$id)]))
    bad <- c(bad, paste(id, "reviewer differs from independent inventory"))
}
for (j in seq_len(nrow(r))) {
  if (!r$status[j] %in% allowed) bad <- c(bad, paste(r$id[j], "invalid status"))
  if (r$status[j] %in% c("addressed", "partially-addressed")) {
    q <- file.path(paths$root, r$file[j])
    if (!file.exists(q)) bad <- c(bad, paste(r$id[j], "missing file"))
    else if (!any(grepl(paste0("\\{#", r$anchor[j], "\\}"), readLines(q, warn = FALSE))))
      bad <- c(bad, paste(r$id[j], "missing anchor"))
    if (!nzchar(r$evidence[j])) bad <- c(bad, paste(r$id[j], "missing evidence"))
  }
  if (r$status[j] == "planned" && !is.na(r$file[j]) && nzchar(r$file[j]) &&
      !is.na(r$anchor[j]) && nzchar(r$anchor[j])) {
    q <- file.path(paths$root, r$file[j])
    if (file.exists(q) && any(grepl(paste0("\\{#", r$anchor[j], "\\}"),
                                    readLines(q, warn = FALSE))))
      bad <- c(bad, paste(r$id[j], "stale planned status: target file and anchor already exist"))
  }
}
out <- data.frame(issue = unique(bad), stringsAsFactors = FALSE)
dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)
write.csv(out, file.path(paths$verification, "claim-map-check.csv"), row.names = FALSE)
cat(sprintf("V5 reviewer claims: %d; addressed/partial: %d; issues: %d\n",
            nrow(r), sum(r$status %in% c("addressed", "partially-addressed")), nrow(out)))
if (nrow(out)) { print(out, row.names = FALSE); quit(status = 1L) }
