## QA V1 + V2: bibliography/citation integrity and result-register parity.

source("code/R/00-paths.R")
source("code/R/00-bibtex-utils.R")

env_or <- function(name, default) {
  z <- Sys.getenv(name); if (nzchar(z)) z else default
}
bib_file <- env_or("DPMIRT_BIB_FILE", file.path(paths$refs, "references.bib"))
result_file <- env_or("DPMIRT_RESULT_REGISTER", file.path(paths$manifest, "result-register.csv"))
## One receipt file per part that carries semantic receipts. Chapter coverage is read
## from each receipt's chapters field; no part or chapter range is hard-coded below.
receipt_files <- env_or("DPMIRT_RECEIPT_FILES",
  paste(sort(list.files(paths$manifest, pattern = "^part-[a-z]+-source-receipts[.]csv$",
                        full.names = TRUE)), collapse = ";"))
receipt_files <- trimws(strsplit(receipt_files, ";", fixed = TRUE)[[1]])
receipt_files <- receipt_files[nzchar(receipt_files)]
required_units_file <- env_or("DPMIRT_SOURCE_RECEIPT_UNITS",
  file.path(paths$manifest, "source-receipt-units.csv"))
chapter_dir <- env_or("DPMIRT_CHAPTERS_DIR", paths$chapters)
out_dir <- env_or("DPMIRT_VERIFICATION_DIR", paths$verification)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

TIERS <- c("primary-read", "programme-artifact-read", "primary-held",
           "secondary", "unverified")
val <- validate_bib_file(bib_file, require_project_fields = TRUE)
if (!val$ok) stop(paste(val$problems, collapse = "\n"))

field <- function(txt, name) {
  pat <- paste0("(?m)^\\s*", name, "\\s*=\\s*\\{([^}]*)\\},?\\s*$")
  m <- regexec(pat, txt, perl = TRUE); z <- regmatches(txt, m)[[1]]
  if (length(z) != 2L) return("")
  z[2]
}

libmap <- setNames(as.list(unname(libraries)), basename(unname(libraries)))
resolve_asset <- function(p) {
  if (!nzchar(p)) return(NA)
  lib <- sub("/.*$", "", p); rest <- sub("^[^/]*/", "", p)
  if (!lib %in% names(libmap)) return(FALSE)
  file.exists(file.path(libmap[[lib]], rest))
}

rec <- do.call(rbind, lapply(seq_along(val$blocks), function(i) {
  b <- val$blocks[i]; key <- val$keys[i]
  tier <- field(b, "tier"); asset <- field(b, "asset")
  verified_on <- field(b, "verified_on"); claims <- field(b, "verified_claims")
  problem <- character()
  if (!tier %in% TIERS) problem <- c(problem, "missing/unknown tier")
  if (tier %in% c("primary-read", "programme-artifact-read") && !nzchar(verified_on))
    problem <- c(problem, paste0(tier, " without verified_on"))
  asset_ok <- resolve_asset(asset)
  if (!is.na(asset_ok) && !asset_ok) problem <- c(problem, "asset path does not resolve")
  data.frame(key=key, tier=tier, asset=asset, verified_on=verified_on,
             verified_claims=claims, asset_ok=asset_ok,
             problem=paste(problem, collapse="; "), stringsAsFactors=FALSE)
}))

## Direct reads of this research programme's own books, manuscripts, packages,
## code, and frozen tables are useful evidence about what those artifacts say and
## do. They are not independent scholarly corroboration. The separate tier and
## receipt register make that authority boundary executable rather than rhetorical.
programme_file <- env_or("DPMIRT_PROGRAMME_ARTIFACT_RECEIPTS",
  file.path(paths$manifest, "programme-artifact-receipts.csv"))
narrative_file <- env_or("DPMIRT_NARRATIVE_SOURCE_RECEIPTS",
  file.path(paths$manifest, "narrative-source-receipts.csv"))
annotation_problem <- character()
read_receipt_register <- function(path, required, label) {
  if (!file.exists(path)) {
    annotation_problem <<- c(annotation_problem, paste("missing", label, "register"))
    return(data.frame())
  }
  d <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                check.names = FALSE)
  if (!all(required %in% names(d))) {
    annotation_problem <<- c(annotation_problem, paste(label, "schema mismatch"))
    return(data.frame())
  }
  d <- d[, required, drop = FALSE]
  if (anyDuplicated(d$receipt_id))
    annotation_problem <<- c(annotation_problem, paste("duplicate receipt_id in", label))
  if (anyDuplicated(d$key))
    annotation_problem <<- c(annotation_problem, paste("duplicate key in", label))
  for (i in seq_len(nrow(d))) {
    if (any(is.na(d[i, ])) || any(!nzchar(trimws(unlist(d[i, ])))))
      annotation_problem <<- c(annotation_problem,
        paste(label, d$receipt_id[i], "has blank required fields"))
  }
  d
}
programme <- read_receipt_register(programme_file,
  c("receipt_id", "key", "artifact_type", "locator", "verified_claim",
    "authority_boundary", "verified_on"), "programme-artifact receipt")
narrative_receipts <- read_receipt_register(narrative_file,
  c("receipt_id", "key", "locator", "verified_claim", "scope_boundary",
    "verified_on"), "narrative-source receipt")

split_claims <- function(z)
  trimws(strsplit(ifelse(is.na(z), "", z), ";", fixed = TRUE)[[1]])
if (nrow(programme)) {
  programme_keys <- rec$key[rec$tier == "programme-artifact-read"]
  for (k in setdiff(programme_keys, programme$key))
    annotation_problem <- c(annotation_problem,
      paste("programme-artifact-read key lacks receipt:", k))
  for (k in setdiff(programme$key, programme_keys))
    annotation_problem <- c(annotation_problem,
      paste("programme-artifact receipt key is not programme-artifact-read:", k))
  for (i in seq_len(nrow(programme))) {
    j <- match(programme$key[i], rec$key)
    if (is.na(j)) {
      annotation_problem <- c(annotation_problem,
        paste("programme-artifact receipt key absent from bibliography:", programme$key[i]))
    } else {
      if (!programme$receipt_id[i] %in% split_claims(rec$verified_claims[j]))
        annotation_problem <- c(annotation_problem,
          paste(programme$receipt_id[i], "missing from bibliography verified_claims"))
      if (!identical(programme$verified_on[i], rec$verified_on[j]))
        annotation_problem <- c(annotation_problem,
          paste(programme$receipt_id[i], "date differs from bibliography"))
    }
  }
}
if (nrow(narrative_receipts)) {
  for (i in seq_len(nrow(narrative_receipts))) {
    j <- match(narrative_receipts$key[i], rec$key)
    if (is.na(j)) {
      annotation_problem <- c(annotation_problem,
        paste("narrative-source receipt key absent from bibliography:", narrative_receipts$key[i]))
    } else {
      if (rec$tier[j] != "primary-read")
        annotation_problem <- c(annotation_problem,
          paste(narrative_receipts$receipt_id[i], "is not primary-read"))
      if (!narrative_receipts$receipt_id[i] %in% split_claims(rec$verified_claims[j]))
        annotation_problem <- c(annotation_problem,
          paste(narrative_receipts$receipt_id[i], "missing from bibliography verified_claims"))
      if (!identical(narrative_receipts$verified_on[i], rec$verified_on[j]))
        annotation_problem <- c(annotation_problem,
          paste(narrative_receipts$receipt_id[i], "date differs from bibliography"))
    }
  }
}

## Appendices carry citations too — Appendix D is an annotated bibliography — so V1
## scans them on the same terms as the chapters.
qmd <- list.files(c(chapter_dir, paths$appendices), pattern = "[.]qmd$",
                  recursive = TRUE, full.names = TRUE)
book_text <- paste(unlist(lapply(qmd, readLines, warn = FALSE)), collapse = "\n")
all_at <- unique(sub("^@", "", regmatches(book_text,
  gregexpr("@[A-Za-z0-9_:-]+", book_text, perl = TRUE))[[1]]))
xref_prefix <- c("sec-","eq-","fig-","tbl-","thm-","prp-","def-","lem-","cor-")
cite_keys <- all_at[!vapply(all_at, function(k) any(startsWith(k, xref_prefix)), logical(1))]
missing_cites <- setdiff(cite_keys, rec$key)
cited_held <- intersect(cite_keys, rec$key[rec$tier == "primary-held"])

rr <- read.csv(result_file, stringsAsFactors = FALSE, na.strings = c("", "NA"),
               check.names = FALSE)
rr_problem <- character(nrow(rr))
for (i in seq_len(nrow(rr))) {
  prov <- rr$provenance[i]; key <- rr$source_key[i]; loc <- rr$source_locator[i]
  p <- character()
  if (prov %in% c("restated", "adapted")) {
    if (is.na(key) || !nzchar(key)) p <- c(p, "sourced provenance without source_key")
    if (is.na(loc) || !nzchar(loc)) p <- c(p, "sourced provenance without locator")
    if (!is.na(key) && nzchar(key)) {
      j <- match(key, rec$key)
      if (is.na(j)) p <- c(p, "source_key absent from bibliography") else {
        if (!rec$tier[j] %in% c("primary-read", "programme-artifact-read", "secondary"))
          p <- c(p, paste0("numbered result supported by ", rec$tier[j], " rather than an honest read/secondary record"))
        claim_ids <- trimws(strsplit(rec$verified_claims[j], ";", fixed = TRUE)[[1]])
        if (rec$tier[j] %in% c("primary-read", "programme-artifact-read") &&
            !rr$id[i] %in% claim_ids)
          p <- c(p, "result id missing from bibliography verified_claims")
      }
    }
  }
  if (prov %in% c("derived-here", "own") && !is.na(key) && nzchar(key))
    p <- c(p, "derived result should not carry a source_key")
  rr_problem[i] <- paste(p, collapse = "; ")
}
rr$problem <- rr_problem

## Semantic locator receipts for the primary works used in completed Part VI.
## V1 cannot prove that prose is a faithful reading, but it can require an auditable
## claim-and-locator receipt and bind every sourced Part VI result to that receipt.
receipt_problem <- character()
receipt_need <- c("receipt_id", "key", "chapters", "locator", "verified_claim",
                  "result_ids", "verified_on")
if (!length(receipt_files) || !all(file.exists(receipt_files))) {
  receipt_problem <- "missing source receipt file"
  receipts <- data.frame()
} else {
  parts <- lapply(receipt_files, read.csv, stringsAsFactors = FALSE, na.strings = "",
                  check.names = FALSE)
  ok_schema <- vapply(parts, function(d) all(receipt_need %in% names(d)), logical(1))
  if (!all(ok_schema)) {
    receipt_problem <- paste("source receipt schema mismatch:",
                             paste(basename(receipt_files[!ok_schema]), collapse = ", "))
    receipts <- data.frame()
  } else {
    receipts <- do.call(rbind, lapply(parts, function(d) d[, receipt_need, drop = FALSE]))
    if (anyDuplicated(receipts$receipt_id))
      receipt_problem <- c(receipt_problem, "duplicate receipt_id")
    for (j in seq_len(nrow(receipts))) {
      if (any(is.na(receipts[j, c("receipt_id", "key", "chapters", "locator",
                                  "verified_claim", "verified_on")])) ||
          any(!nzchar(unlist(receipts[j, c("receipt_id", "key", "chapters", "locator",
                                           "verified_claim", "verified_on")]))) ) {
        receipt_problem <- c(receipt_problem, paste(receipts$receipt_id[j], "has blank required fields"))
        next
      }
      k <- match(receipts$key[j], rec$key)
      if (is.na(k)) receipt_problem <- c(receipt_problem, paste(receipts$receipt_id[j], "key absent from bibliography"))
      else {
        if (!rec$tier[k] %in% c("primary-read", "programme-artifact-read"))
          receipt_problem <- c(receipt_problem,
            paste(receipts$receipt_id[j], "is not a locator-read tier"))
        if (!identical(rec$verified_on[k], receipts$verified_on[j]))
          receipt_problem <- c(receipt_problem, paste(receipts$receipt_id[j], "date differs from bibliography"))
        claim_ids <- trimws(strsplit(rec$verified_claims[k], ";", fixed = TRUE)[[1]])
        if (!receipts$receipt_id[j] %in% claim_ids)
          receipt_problem <- c(receipt_problem, paste(receipts$receipt_id[j], "missing from bibliography verified_claims"))
      }
    }

    ## A receipt covers a citation only if it names that chapter. A receipt written
    ## for one chapter's claim is not evidence that another chapter's claim, at another
    ## locator, was checked -- which is the failure this guard exists to catch.
    ## A receipt's `chapters` field holds body-chapter numbers and/or appendix
    ## tokens like "appB". Appendix D is an annotated bibliography, so appendix
    ## citations need receipts on exactly the same terms as chapter citations.
    receipt_chs <- lapply(receipts$chapters, function(z) {
      tok <- trimws(strsplit(as.character(z), ";", fixed = TRUE)[[1]])
      tok <- tok[nzchar(tok)]
      num <- suppressWarnings(as.integer(tok))
      c(as.character(num[!is.na(num)]), tok[is.na(num) & grepl("^app[A-G]$", tok)])
    })
    if (!file.exists(required_units_file)) stop("missing independent source-receipt unit inventory")
    req_units <- read.csv(required_units_file, stringsAsFactors = FALSE,
                          na.strings = c("", "NA"), check.names = FALSE)
    if (!all(c("unit", "reason") %in% names(req_units)) || anyDuplicated(req_units$unit) ||
        any(is.na(req_units$unit)) || any(!nzchar(req_units$unit)) ||
        any(is.na(req_units$reason)) || any(!nzchar(req_units$reason)))
      stop("invalid source-receipt unit inventory")
    receipted_units <- as.character(req_units$unit)
    named_units <- unique(unlist(receipt_chs, use.names = FALSE))
    for (u in setdiff(receipted_units, named_units))
      receipt_problem <- c(receipt_problem,
        sprintf("required receipted unit %s disappeared from all receipts", u))
    unit_files <- function(u) {
      if (grepl("^app", u))
        qmd[grepl(sprintf("/appendices/%s-[^/]+[.]qmd$", sub("^app", "", u)), qmd, perl = TRUE)]
      else qmd[grepl(sprintf("/%d-[^/]+[.]qmd$", as.integer(u)), qmd, perl = TRUE)]
    }
    receipted_chapters <- suppressWarnings(as.integer(receipted_units))
    receipted_chapters <- sort(receipted_chapters[!is.na(receipted_chapters)])

    ## Sweep every chapter named by any receipt. This makes a future Part VIII
    ## operational by adding its receipt file and chapter files only.
    for (ch in receipted_units) {
      f <- unit_files(ch)
      if (!length(f)) {
        receipt_problem <- c(receipt_problem,
          sprintf("receipt metadata names %s but no such QMD exists", ch))
        next
      }
      txt <- paste(unlist(lapply(f, readLines, warn = FALSE)), collapse = "\n")
      at <- unique(sub("^@", "", regmatches(txt,
        gregexpr("@[A-Za-z0-9_:-]+", txt, perl = TRUE))[[1]]))
      cites <- at[!vapply(at, function(k) any(startsWith(k, xref_prefix)), logical(1))]
      covered <- receipts$key[vapply(receipt_chs, function(z) ch %in% z, logical(1))]
      for (k in setdiff(cites, covered))
        receipt_problem <- c(receipt_problem,
          sprintf("%s cited in %s without a semantic receipt naming it", k, ch))
    }

    ## Bind every sourced/adapted numbered result in a receipted chapter to a receipt
    ## that names both its source and its chapter. A source can have receipts in several
    ## parts, so matching the first source-key occurrence is insufficient.
    sourced_rr <- rr$chapter %in% receipted_chapters &
      rr$provenance %in% c("restated", "adapted")
    for (i in which(sourced_rr)) {
      cand <- which(receipts$key == rr$source_key[i] &
        vapply(receipt_chs, function(z) as.character(rr$chapter[i]) %in% z, logical(1)))
      if (!length(cand)) {
        receipt_problem <- c(receipt_problem,
          sprintf("%s source lacks a receipt naming ch. %d", rr$id[i], rr$chapter[i]))
      } else {
        ids <- unique(unlist(lapply(cand, function(j) {
          if (is.na(receipts$result_ids[j]) || !nzchar(receipts$result_ids[j]))
            return(character())
          trimws(strsplit(receipts$result_ids[j], ";", fixed = TRUE)[[1]])
        }), use.names = FALSE))
        if (!rr$id[i] %in% ids)
          receipt_problem <- c(receipt_problem,
            sprintf("%s missing from result_ids of a receipt naming ch. %d",
                    rr$id[i], rr$chapter[i]))
      }
    }
  }
}

## V2: native Quarto result div anchors must match register labels one-for-one.
anchor_rows <- do.call(rbind, lapply(qmd, function(f) {
  x <- readLines(f, warn = FALSE)
  hit <- grep("^:::\\s*\\{#(def|thm|prp|lem|cor)-[A-Za-z0-9-]+", x, perl = TRUE)
  if (!length(hit)) return(NULL)
  data.frame(file = basename(f), line = hit,
             label = sub("^:::\\s*\\{#([^ }]+).*$", "\\1", x[hit], perl = TRUE),
             stringsAsFactors = FALSE)
}))
if (is.null(anchor_rows)) anchor_rows <- data.frame(file=character(),line=integer(),label=character())
missing_register <- setdiff(anchor_rows$label, rr$label)
missing_anchor <- setdiff(rr$label, anchor_rows$label)
duplicate_anchor <- unique(anchor_rows$label[duplicated(anchor_rows$label)])

## V2 hardening (added by review 021, reconstructed here from its usage sites after a
## bad splice during the tier revert): the register must have unique ids and unique
## labels, and a result's registered chapter must match the file its anchor lives in.
duplicate_register_id <- unique(rr$id[duplicated(rr$id)])
duplicate_register_label <- unique(rr$label[duplicated(rr$label)])
anchor_chapter <- suppressWarnings(as.integer(sub("^([0-9]+)-.*$", "\\1", anchor_rows$file)))
am <- data.frame(label = anchor_rows$label, anchor_file = anchor_rows$file,
                 anchor_chapter = anchor_chapter, stringsAsFactors = FALSE)
am <- merge(am, rr[, c("label", "chapter")], by = "label", all.x = TRUE)
chapter_mismatch <- am[!is.na(am$anchor_chapter) & !is.na(am$chapter) &
                       am$anchor_chapter != as.integer(am$chapter), , drop = FALSE]

write.csv(rec, file.path(out_dir, "citation-check.csv"), row.names = FALSE, na = "")
write.csv(rr, file.path(out_dir, "result-check.csv"), row.names = FALSE, na = "")
write.csv(data.frame(issue = unique(receipt_problem), stringsAsFactors = FALSE),
          file.path(out_dir, "part-vi-source-receipt-check.csv"), row.names = FALSE)
write.csv(data.frame(issue = unique(annotation_problem), stringsAsFactors = FALSE),
          file.path(out_dir, "annotation-evidence-check.csv"), row.names = FALSE)

cat(sprintf("V1 bibliography entries : %d (independent primary-read %d; programme-artifact-read %d)\n",
            nrow(rec), sum(rec$tier == "primary-read"),
            sum(rec$tier == "programme-artifact-read")))
cat(sprintf("V1 citations in chapters: %d; missing keys: %d\n", length(cite_keys), length(missing_cites)))
cat(sprintf("V1 result source issues : %d\n", sum(nzchar(rr$problem))))
cat(sprintf("V1 semantic source receipts: %d; issues: %d\n", nrow(receipts),
            length(unique(receipt_problem))))
cat(sprintf("V1 annotation receipts: programme %d, narrative %d; issues: %d\n",
            nrow(programme), nrow(narrative_receipts),
            length(unique(annotation_problem))))
## Narrative citations of held-but-unread sources.
##
## R1 binds a citation that asserts "this work established this claim". A NUMBERED
## RESULT always makes that assertion, so V1 requires a primary-read source with a
## locator for every one -- enforced above, and unchanged.
##
## An ordinary narrative citation does not always make it. Some place a claim; others
## point a reader at a literature, name a lineage, or credit an idea whose content the
## book takes from a second source it did read. Forbidding those outright would either
## strip the book of context or push it into pretending to reads it has not done. This
## project has now seen the second failure: 60 cited sources were flipped to
## `primary-read` on automated keyword overlap in order to satisfy a blanket ban.
##
## So the rule is disclosure, not prohibition. A held source MAY be cited in narrative;
## what it may not do is appear as though it had been read. V1 therefore checks that
## every cited held source is TAGGED held -- which is what tells the reader the truth --
## and reports the count so the gap stays visible rather than silently accumulating.
cited_held <- intersect(cite_keys, rec$key[rec$tier == "primary-held"])
narrative_problem <- character()

## A held source may not carry a numbered result, an equation/theorem-level locator in
## the register, or a semantic receipt: those are read-claims by construction.
held_with_result <- intersect(cited_held, rr$source_key[!is.na(rr$source_key)])
if (length(held_with_result))
  narrative_problem <- c(narrative_problem,
    paste0("held source carries a numbered result: ", held_with_result))
if (nrow(receipts))
  for (k in intersect(cited_held, receipts$key))
    narrative_problem <- c(narrative_problem,
      paste0("held source carries a semantic receipt: ", k))

## The citation-verification register IS a reading record, and precisely because it is,
## it is the obvious next channel for the same failure. Two guards, both cheap:
##   (a) an ATTRIBUTION SOUND row checked a title and its terms, not a passage, so the key
##       it covers must NOT be primary-read -- a tier promoted on that basis is the review
##       021 defect wearing a better-looking register;
##   (b) a key promoted to primary-read on a verification row must actually carry that row's
##       id in the bibliography's `verified_claims`, so Appendix D cannot annotate a source
##       whose evidence is not bound to it.
cv_file <- file.path(paths$manifest, "citation-verification.csv")
if (file.exists(cv_file)) {
  cv <- read.csv(cv_file, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                 check.names = FALSE)
  if (!all(c("id", "key", "level", "verdict") %in% names(cv)))
    narrative_problem <- c(narrative_problem,
      "citation-verification.csv must carry id, key, level and verdict columns")
  if (anyDuplicated(cv$id))
    narrative_problem <- c(narrative_problem, "duplicate id in citation-verification.csv")
  attr_only <- unique(cv$key[!is.na(cv$level) & cv$level != "locator"])
  attr_only <- setdiff(attr_only, cv$key[!is.na(cv$level) & cv$level == "locator"])
  bad_tier <- intersect(attr_only,
    rec$key[rec$tier %in% c("primary-read", "programme-artifact-read")])
  if (length(bad_tier))
    narrative_problem <- c(narrative_problem,
      paste0("primary-read on attribution-level verification only: ", bad_tier))
  loc_ok <- cv[!is.na(cv$level) & cv$level == "locator" &
                 !is.na(cv$verdict) & grepl("^CONFIRMED", cv$verdict), , drop = FALSE]
  for (i in seq_len(nrow(loc_ok))) {
    j <- match(loc_ok$key[i], rec$key)
    if (is.na(j)) {
      narrative_problem <- c(narrative_problem,
        paste0("citation verification names an unknown key: ", loc_ok$key[i]))
    } else if (rec$tier[j] %in% c("primary-read", "programme-artifact-read") &&
               !grepl(loc_ok$id[i], rec$verified_claims[j], fixed = TRUE)) {
      narrative_problem <- c(narrative_problem,
        paste0("verified_claims does not bind ", loc_ok$id[i], " for ", loc_ok$key[i]))
    }
  }
} else cv <- data.frame()

## The context-match table is NOT a reading record and must not be mistaken for one.
ctx_file <- file.path(paths$manifest, "citation-context-match.csv")
if (file.exists(ctx_file)) {
  ctx <- read.csv(ctx_file, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("is_reading", "method") %in% names(ctx)) || any(ctx$is_reading != "NO"))
    narrative_problem <- c(narrative_problem,
      "citation-context-match.csv must declare is_reading = NO on every row")
} else ctx <- data.frame()

write.csv(data.frame(issue = unique(narrative_problem)),
          file.path(out_dir, "narrative-citation-check.csv"), row.names = FALSE)
cat(sprintf("V1 cited sources: %d independent reads, %d programme-artifact reads, %d held (disclosed), %d secondary; context-match rows: %d; issues: %d\n",
            length(intersect(cite_keys, rec$key[rec$tier == "primary-read"])),
            length(intersect(cite_keys, rec$key[rec$tier == "programme-artifact-read"])),
            length(cited_held),
            length(intersect(cite_keys, rec$key[rec$tier == "secondary"])),
            nrow(ctx), length(unique(narrative_problem))))
if (length(narrative_problem)) print(unique(narrative_problem))
n_bad <- sum(nzchar(rec$problem)) + length(missing_cites) + sum(nzchar(rr$problem)) +
  length(missing_register) + length(missing_anchor) + length(duplicate_anchor) +
  length(duplicate_register_id) + length(duplicate_register_label) +
  nrow(chapter_mismatch) + length(unique(receipt_problem)) +
  length(unique(narrative_problem)) + length(unique(annotation_problem))

cat(sprintf("V2 text/register anchors: %d/%d; parity issues: %d\n", nrow(anchor_rows), nrow(rr),
            length(missing_register)+length(missing_anchor)+length(duplicate_anchor)+
              length(duplicate_register_id)+length(duplicate_register_label)+nrow(chapter_mismatch)))
if (length(missing_cites)) cat("missing citations: ", paste(missing_cites, collapse=", "), "\n", sep="")
if (length(missing_register)) cat("unregistered anchors: ", paste(missing_register, collapse=", "), "\n", sep="")
if (length(missing_anchor)) cat("register rows without anchors: ", paste(missing_anchor, collapse=", "), "\n", sep="")
if (length(duplicate_register_id)) cat("duplicate register ids: ", paste(duplicate_register_id, collapse=", "), "\n", sep="")
if (length(duplicate_register_label)) cat("duplicate register labels: ", paste(duplicate_register_label, collapse=", "), "\n", sep="")
if (nrow(chapter_mismatch)) print(chapter_mismatch, row.names = FALSE)
if (n_bad) quit(status = 1L)
