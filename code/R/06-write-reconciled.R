## 06-write-reconciled.R — T1.5
##
## Produces drafts/appendix-{a..f}-reconciled.md from the raw split by
##   (1) normalising notation to manifest/notation-register.csv, and
##   (2) inserting a provenance marker at each numbered result, drawn from
##       manifest/source-assignments.csv.
##
## The markers are the reconciliation's product. `[SOURCE: key | status]` says
## a candidate source is assigned and the locator is still to be read; a chapter
## may not cite the result until that locator exists (rule R2).

source("code/R/00-paths.R")

asg <- read.csv(file.path(paths$manifest, "source-assignments.csv"),
                stringsAsFactors = FALSE)

## tag -> assignment (a tag belongs to exactly one result group per appendix)
tag_map <- do.call(rbind, lapply(seq_len(nrow(asg)), function(k) {
  tags <- trimws(strsplit(asg$tags[k], ";")[[1]])
  data.frame(appendix = asg$appendix[k], tag = tags,
             group = asg$result_group[k], key = asg$source_key[k],
             tier = asg$tier_now[k], status = asg$locator_status[k],
             dest = asg$book_destination[k], stringsAsFactors = FALSE)
}))

## --- notation substitutions (applied in order) ---------------------------
## Only changes forced by the register. Each is logged.
subs <- list(
  list(from = "\\\\mathcal\\{I\\}", to = "\\\\mathcal{J}",
       why = "one symbol for information (register); the drafts use I, the manuscript and the simulation book use J")
)

report <- list()

for (ap in c("A", "B", "C", "D", "E", "F")) {
  f_in  <- file.path(paths$drafts, sprintf("appendix-%s-raw.md", tolower(ap)))
  x <- readLines(f_in, warn = FALSE)
  n_sub <- integer(length(subs))

  for (s in seq_along(subs)) {
    hits <- sum(vapply(gregexpr(subs[[s]]$from, x), function(m) sum(m > 0), integer(1)))
    n_sub[s] <- hits
    x <- gsub(subs[[s]]$from, subs[[s]]$to, x)
  }

  ## --- insert provenance markers after each tagged display block ---------
  tm <- tag_map[tag_map$appendix == ap, ]
  inserted <- 0L
  if (nrow(tm)) {
    for (k in seq_len(nrow(tm))) {
      pat <- paste0("\\\\tag\\{", gsub("([.])", "\\\\\\1", tm$tag[k]), "\\}")
      ln <- grep(pat, x)
      if (!length(ln)) next
      close <- ln[1] + which(grepl("^\\$\\$", x[(ln[1]):min(ln[1] + 6, length(x))]))[1] - 1L
      if (is.na(close)) close <- ln[1]
      marker <- sprintf(
        "\n> `[SOURCE: %s | %s | %s]` — result group *%s*; destination %s.\n",
        tm$key[k], tm$tier[k], tm$status[k], tm$group[k], tm$dest[k])
      x <- append(x, marker, after = close)
      inserted <- inserted + 1L
    }
  }

  hdr <- c(
    sprintf("<!-- RECONCILED DRAFT — Appendix %s", ap),
    "     Source: 2025-08-12 APM Research Notes, 32_appendices_a-to-f_combined-....md",
    "     Produced by code/R/06-write-reconciled.R. NOT book text: this is staged",
    "     material for the chapters named in the markers below. Rule (blueprint",
    "     sec. authority order): the A-F drafts carry NO citation authority; every",
    "     statement is re-derived or re-verified before it enters a chapter.",
    sprintf("     Notation substitutions applied: %d", sum(n_sub)),
    sprintf("     Provenance markers inserted: %d", inserted),
    "-->", "")

  writeLines(c(hdr, x),
             file.path(paths$drafts, sprintf("appendix-%s-reconciled.md", tolower(ap))))

  report[[length(report) + 1L]] <- data.frame(
    appendix = ap, lines = length(x), notation_subs = sum(n_sub),
    markers = inserted, stringsAsFactors = FALSE)
}

rep <- do.call(rbind, report)
write.csv(rep, file.path(paths$verification, "appendix-reconciliation.csv"),
          row.names = FALSE)
print(rep, row.names = FALSE)
cat(sprintf("\ntotal: %d notation substitutions, %d provenance markers\n",
            sum(rep$notation_subs), sum(rep$markers)))
