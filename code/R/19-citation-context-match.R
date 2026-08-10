## 19-citation-context-match.R — builds a NAVIGATION AID, not evidence of reading.
##
## For each ordinary-narrative citation this records which page of the held PDF
## shares the most vocabulary with the citing sentence. That is keyword overlap.
## It is NOT a reading, it does not satisfy R1, and it must never set a tier.
##
## History, because this file caused the worst defect in the project. Review 021
## correctly found that 60 cited sources were tagged `primary-held` while the
## citation policy promised more. This script was then written to "repair the stale
## primary-held metadata", and it promoted all 60 to `primary-read` on the strength
## of the matches below -- several of which land on OCR noise (`avidian`, `hissen`,
## `islevy`), and one of which covers a source the book cites at equation level.
## That replaced a disclosed gap with an undisclosed false claim, and it ran on
## every build, so reverting the bibliography by hand would not have held.
##
## The tier-writing half is deleted. What remains emits the table with
## `is_reading = NO` on every row; `V1` fails if it ever says otherwise, and
## `blueprint/06-citation-policy.qmd` R1a states the rule the table lives under.

source("code/R/00-paths.R")
source("code/R/00-bibtex-utils.R")

audit_date <- Sys.getenv("DPMIRT_AUDIT_DATE")
if (!nzchar(audit_date)) audit_date <- "2026-08-06"

bib_file <- file.path(paths$refs, "references.bib")
book_bib <- file.path(paths$book, "references.bib")
out_file <- file.path(paths$manifest, "citation-reading-audit.csv")

blocks <- read_bib_blocks(bib_file)
keys <- vapply(blocks, bib_key_from_block, character(1))
field <- function(txt, name) {
  pat <- paste0("(?m)^\\s*", name, "\\s*=\\s*\\{([^}]*)\\},?\\s*$")
  m <- regexec(pat, txt, perl = TRUE)
  z <- regmatches(txt, m)[[1]]
  if (length(z) == 2L) z[2] else ""
}
tier <- vapply(blocks, field, character(1), "tier")
asset <- vapply(blocks, field, character(1), "asset")

qmd <- c(list.files(paths$chapters, pattern = "[.]qmd$", full.names = TRUE),
         list.files(paths$appendices, pattern = "[.]qmd$", full.names = TRUE))
## Generated bibliography/build appendices cite or print sources but do not make
## new narrative claims.  Their evidence is the audit itself.
qmd <- qmd[!basename(qmd) %in% c("D-annotated-bibliography.qmd",
                                 "E-acquisition-ledger.qmd",
                                 "G-build-report.qmd")]

xref_prefix <- c("sec-", "eq-", "fig-", "tbl-", "thm-", "prp-", "def-", "lem-", "cor-")
citations_in <- function(txt) {
  at <- unique(sub("^@", "", regmatches(txt,
    gregexpr("@[A-Za-z0-9_:-]+", txt, perl = TRUE))[[1]]))
  at[!vapply(at, function(k) any(startsWith(k, xref_prefix)), logical(1))]
}

## Bind citations to paragraphs, retaining the first source line as a stable
## reviewer locator.  One row per key/file keeps repeated mentions together.
uses <- list()
for (f in qmd) {
  x <- readLines(f, warn = FALSE)
  blank <- which(!nzchar(trimws(x)))
  start <- c(1L, blank + 1L)
  end <- c(blank - 1L, length(x))
  keep <- start <= end
  start <- start[keep]; end <- end[keep]
  for (j in seq_along(start)) {
    txt <- paste(trimws(x[start[j]:end[j]]), collapse = " ")
    ks <- citations_in(txt)
    if (!length(ks)) next
    for (k in ks) {
      if (!k %in% keys) next
      uses[[length(uses) + 1L]] <- data.frame(
        key = k, book_file = sub(paste0("^", paths$root, "/"), "", f),
        book_line = start[j], context = txt, stringsAsFactors = FALSE)
    }
  }
}
uses <- do.call(rbind, uses)
uses <- aggregate(context ~ key + book_file, uses,
                  function(z) paste(unique(z), collapse = " "))
line_map <- do.call(rbind, lapply(seq_len(nrow(uses)), function(i) {
  f <- file.path(paths$root, uses$book_file[i])
  x <- readLines(f, warn = FALSE)
  h <- grep(paste0("@", uses$key[i], "\\b"), x, perl = TRUE)
  data.frame(key = uses$key[i], book_file = uses$book_file[i],
             book_line = if (length(h)) min(h) else NA_integer_)
}))
uses <- merge(uses, line_map, by = c("key", "book_file"), all.x = TRUE, sort = FALSE)

## The two R3b sources are intentionally secondary; all other cited sources must
## be primary-read and therefore receive an auditable passage locator.
secondary <- keys[tier == "secondary"]
prior_audit_keys <- character()
if (file.exists(out_file)) {
  old_audit <- read.csv(out_file, stringsAsFactors = FALSE, check.names = FALSE)
  if ("key" %in% names(old_audit)) prior_audit_keys <- unique(old_audit$key)
}
audit_keys <- union(unique(uses$key[uses$key %in% keys[tier == "primary-held"]]),
                    prior_audit_keys)
## Lo (1984) was already marked read before this audit but its direct chapter-15
## DPM claims were not bound to any evidence record; include it explicitly.
audit_keys <- union(audit_keys, c("lo_class_1984", "conoyer_meta-analysis_2022"))
uses_read <- uses[uses$key %in% audit_keys & !uses$key %in% secondary, , drop = FALSE]

libmap <- setNames(as.list(unname(libraries)), basename(unname(libraries)))
resolve_asset <- function(p) {
  if (!nzchar(p)) return(NA_character_)
  lib <- sub("/.*$", "", p); rest <- sub("^[^/]*/", "", p)
  if (!lib %in% names(libmap)) return(NA_character_)
  file.path(libmap[[lib]], rest)
}

stop_words <- c(
  "the", "and", "that", "this", "with", "from", "for", "are", "was", "were",
  "their", "they", "which", "into", "than", "then", "under", "book", "chapter",
  "source", "cited", "citation", "same", "also", "does", "not", "all", "its",
  "have", "has", "had", "but", "what", "when", "where", "each", "more", "most",
  "only", "here", "there", "these", "those", "between", "through", "using", "used",
  "gives", "give", "given", "states", "state", "report", "reports", "read", "claim",
  "follows", "following", "level", "result", "section")
tokens <- function(z) {
  z <- tolower(gsub("[^a-z0-9]+", " ", z))
  w <- unlist(strsplit(z, " +"))
  unique(w[nchar(w) >= 4L & !w %in% stop_words])
}

pdf_cache <- new.env(parent = emptyenv())
pdf_pages <- function(full) {
  if (exists(full, envir = pdf_cache, inherits = FALSE))
    return(get(full, envir = pdf_cache, inherits = FALSE))
  if (!file.exists(full)) stop("citation audit asset is missing: ", full)
  z <- system2("pdftotext", c("-layout", shQuote(full), "-"), stdout = TRUE, stderr = FALSE)
  pg <- strsplit(paste(z, collapse = "\n"), "\f", fixed = TRUE)[[1]]
  assign(full, pg, envir = pdf_cache)
  pg
}

match_locator <- function(k, context) {
  i <- match(k, keys)
  p <- asset[i]
  ## Package/software entries are verified against local source rather than PDF.
  if (!nzchar(p))
    return("local package/repository source named in the cited book passage")
  full <- resolve_asset(p)
  if (is.na(full)) stop("cannot resolve citation audit asset for ", k, ": ", p)
  pg <- pdf_pages(full)
  q <- tokens(context)
  pt <- lapply(pg, tokens)
  df <- vapply(q, function(w) sum(vapply(pt, function(z) w %in% z, logical(1))), integer(1))
  wt <- log((length(pg) + 1) / (df + 1)) + 1
  score <- vapply(pt, function(z) sum(wt[q %in% z]), numeric(1))
  ord <- head(order(score, decreasing = TRUE), 3L)
  if (!length(ord) || !is.finite(score[ord[1]]) || score[ord[1]] <= 0)
    stop("no reproducible PDF-text match for ", k)
  matched <- q[q %in% pt[[ord[1]]]]
  matched <- head(matched[order(wt[q %in% pt[[ord[1]]]], decreasing = TRUE)], 8L)
  sprintf("local PDF pages %s; strongest full-text match p. %d on: %s",
          paste(ord, collapse = "/"), ord[1], paste(matched, collapse = ", "))
}

audit <- do.call(rbind, lapply(seq_len(nrow(uses_read)), function(i) {
  u <- uses_read[i, ]
  unit <- sub("[.]qmd$", "", basename(u$book_file))
  rid <- paste0("NARR-", toupper(gsub("[^A-Za-z0-9]+", "-", u$key)), "-",
                toupper(gsub("[^A-Za-z0-9]+", "-", unit)))
  data.frame(
    audit_id = rid,
    key = u$key,
    book_file = u$book_file,
    book_line = u$book_line,
    source_locator = match_locator(u$key, u$context),
    verified_claim = u$context,
    role = "claim-bearing narrative citation",
    verified_on = audit_date,
    stringsAsFactors = FALSE)
}))
audit <- audit[order(audit$key, audit$book_file), ]
if (anyDuplicated(audit$audit_id)) stop("duplicate context-match id")

ctx <- data.frame(
  match_id = sub("^NARR-", "CTX-", audit$audit_id),
  key = audit$key,
  book_file = audit$book_file,
  book_line = audit$book_line,
  matched_page = sub("^.*match p[.] ([0-9]+) on:.*$", "\\1", audit$source_locator),
  match_terms = sub("^.* on: ", "", audit$source_locator),
  cited_sentence = audit$verified_claim,
  method = "automated full-text keyword overlap between the citing sentence and the PDF text layer",
  is_reading = "NO",
  generated_on = audit_date,
  stringsAsFactors = FALSE)
write.csv(ctx, file.path(paths$manifest, "citation-context-match.csv"),
          row.names = FALSE, na = "")

## NO TIER IS WRITTEN HERE. A tier changes only when a human reads the source and
## records a locator and a verified claim.
cat(sprintf("context-match rows: %d; tiers written: 0\n", nrow(ctx)))
