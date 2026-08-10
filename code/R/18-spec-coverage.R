## 18-spec-coverage.R — V9: spec coverage audit.
## Four review cycles all looked for OVERCLAIMING. None was asked to look for
## OMISSION, so nothing has ever checked that each chapter delivers what its
## blueprint spec promised. This closes that gap for the parts that can be
## checked mechanically, and REPORTS rather than judges the part that cannot.
##
## Blocking: a named artifact that does not exist or is not embedded where the
## spec puts it, and a reviewer comment a spec assigns to a chapter that the
## claim map does not record against that chapter.
## Advisory: per-item lexical coverage of each spec's Contents list. A lexical
## hit is a maintenance diagnostic only; it is not proof that the chapter has
## delivered the promised argument, evidence, or scope qualification.
source("code/R/00-paths.R")

spec <- readLines(file.path(paths$root, "blueprint", "05-chapter-specifications.qmd"),
                  warn = FALSE)
heads <- grep("^## ([0-9]{2}) — ", spec)
chnum <- as.integer(sub("^## ([0-9]{2}) — .*$", "\\1", spec[heads]))
bounds <- c(heads, length(spec) + 1L)

ch_file <- function(n) {
  f <- list.files(paths$chapters, pattern = sprintf("^%02d-.*[.]qmd$", n), full.names = TRUE)
  if (length(f)) f[1] else NA_character_
}
cm <- read.csv(file.path(paths$manifest, "claim-map.csv"), stringsAsFactors = FALSE)
binding_file <- file.path(paths$manifest, "spec-answer-bindings.csv")
if (!file.exists(binding_file)) stop("missing spec-answer-bindings.csv")
bind <- read.csv(binding_file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
if (!all(c("chapter", "spec_id", "file") %in% names(bind)) ||
    anyDuplicated(paste(bind$chapter, bind$spec_id)) ||
    any(is.na(bind$chapter)) || any(is.na(bind$spec_id)) || any(!nzchar(bind$spec_id)) ||
    any(is.na(bind$file)) || any(!nzchar(bind$file)))
  stop("invalid spec answer binding inventory")

disown_pat <- paste("No Chapter|generated\\s+with Chapter|are generated\\s+with|lives in",
                    "belongs to|contributes to|shared with|Appendix [A-G]", sep = "|")
artifact_requirements <- function(line) {
  clauses <- regmatches(line, gregexpr("[^.;]+(?:[.;]|$)", line, perl = TRUE))[[1]]
  out <- character()
  for (cl in clauses) {
    arts <- gsub("`", "", unique(unlist(regmatches(cl,
      gregexpr("`(T|F)-[A-Za-z0-9-]+`", cl)))))
    if (length(arts) && !grepl(disown_pat, cl, ignore.case = TRUE, perl = TRUE))
      out <- c(out, arts)
  }
  unique(out)
}
artifact_embedded <- function(artifact, text) {
  if (grepl(artifact, text, fixed = TRUE)) return(TRUE)
  if (!startsWith(artifact, "F-")) return(FALSE)
  figure_id <- paste0("fig-", sub("^F-", "", artifact))
  ## Accept source definitions, not a mere @fig-* cross-reference.
  grepl(paste0("#", figure_id, "\\b"), text, perl = TRUE) ||
    grepl(paste0("(?m)^#\\| label:\\s*", figure_id, "\\s*$"), text,
          perl = TRUE)
}
fixture_art <- artifact_requirements("`T-required` is displayed here; `F-elsewhere` lives in Appendix F.")
if (!identical(fixture_art, "T-required"))
  stop("V9 per-artifact disowning regression fixture failed")

field_text <- function(block, field) {
  start <- grep(paste0("^- \\*\\*", field, "[.]?\\*\\*"), block, perl = TRUE)
  if (!length(start)) return(NA_character_)
  j <- start[1]
  while (j + 1L <= length(block) && !grepl("^- \\*\\*[A-Za-z]", block[j + 1L]))
    j <- j + 1L
  out <- paste(block[start[1]:j], collapse = " ")
  trimws(sub(paste0("^- \\*\\*", field, "[.]?\\*\\*\\s*"), "", out,
             perl = TRUE))
}

split_top_level_semicolons <- function(x) {
  if (!nzchar(trimws(x))) return(character())
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  depth <- 0L
  cuts <- integer()
  for (i in seq_along(chars)) {
    if (chars[i] %in% c("(", "[", "{")) depth <- depth + 1L
    if (chars[i] %in% c(")", "]", "}")) depth <- max(0L, depth - 1L)
    if (chars[i] == ";" && depth == 0L) cuts <- c(cuts, i)
  }
  starts <- c(1L, cuts + 1L)
  ends <- c(cuts - 1L, length(chars))
  out <- trimws(vapply(seq_along(starts), function(i)
    paste(chars[starts[i]:ends[i]], collapse = ""), character(1)))
  out[nzchar(out)]
}

content_items <- function(contents, chapter) {
  if (is.na(contents) || !nzchar(trimws(contents)))
    return(data.frame(item_id = character(), text = character(), schema = character()))
  marker_pat <- "\\([0-9]{1,2}[A-Za-z]?\\)"
  loc <- gregexpr(marker_pat, contents, perl = TRUE)[[1]]
  if (loc[1] != -1L) {
    marker <- regmatches(contents, gregexpr(marker_pat, contents, perl = TRUE))[[1]]
    end <- c(loc[-1L] - 1L, nchar(contents))
    item <- trimws(vapply(seq_along(loc), function(i)
      substr(contents, loc[i], end[i]), character(1)))
    suffix <- toupper(gsub("[()]", "", marker))
    number <- as.integer(sub("^([0-9]+).*$", "\\1", suffix))
    letter <- sub("^[0-9]+", "", suffix)
    id <- sprintf("C%02d-%02d%s", chapter, number, letter)
    return(data.frame(item_id = id, text = item, schema = "numbered",
                      stringsAsFactors = FALSE))
  }
  item <- split_top_level_semicolons(contents)
  data.frame(item_id = sprintf("C%02d-%02d", chapter, seq_along(item)),
             text = item, schema = "semicolon", stringsAsFactors = FALSE)
}

## The addendum deliberately uses unnumbered, top-level semicolon items. These
## counts are the ratified v2/v3 schema; changing the specification therefore
## requires an explicit verifier update instead of silently collapsing to zero.
addendum_counts <- c(`24` = 5L, `25` = 5L, `26` = 5L,
                     `27` = 6L, `28` = 8L, `29` = 4L)

## Deliberate departures from the spec, each with a stated reason. The guard stays
## live for anything not listed here.
dev_file <- file.path(paths$manifest, "spec-deviations.csv")
dev <- if (file.exists(dev_file))
  read.csv(dev_file, stringsAsFactors = FALSE) else
  data.frame(chapter = integer(), item = character(), kind = character(),
             reason = character())

issues <- data.frame(kind = character(), chapter = integer(), item = character(),
                     detail = character(), stringsAsFactors = FALSE)
add <- function(k, ch, it, d) {
  if (any(dev$chapter == ch & dev$item == it & dev$kind == k)) return(invisible(NULL))
  issues <<- rbind(issues, data.frame(kind = k, chapter = ch, item = it, detail = d,
                                      stringsAsFactors = FALSE))
}

cover <- list()
content_detail <- list()
expected_bind <- character()
for (i in seq_along(heads)) {
  n <- chnum[i]
  blk <- spec[heads[i]:(bounds[i + 1L] - 1L)]
  f <- ch_file(n)
  if (is.na(f)) { add("missing-chapter", n, "-", "spec has no chapter file"); next }
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  ## --- Artifacts: named T-*/F-* must exist and be embedded here -------------
  ## The Artifacts line often NAMES an artifact in order to say it belongs to
  ## another chapter ("No Chapter-1 figure/table; F-three-goals ... are generated
  ## with Chapter 17"). Requiring embedding there is a parser error, not a defect.
  ai <- grep("^- \\*\\*Artifacts", blk)
  art_line <- if (length(ai)) {
    j <- ai[1]
    while (j + 1L <= length(blk) && !grepl("^- \\*\\*", blk[j + 1L])) j <- j + 1L
    paste(blk[ai[1]:j], collapse = " ")
  } else ""
  ## An Artifacts line names an artifact this chapter must DISPLAY only when it
  ## does not disown it -- "contributes to", "shared with ch. N", "Appendix B of
  ## the book" and the like assign the display elsewhere.
  arts <- artifact_requirements(art_line)
  for (a in arts) {
    embedded_ok <- artifact_embedded(a, txt)
    exists_ok <- file.exists(file.path(paths$tables, paste0(a, ".rds"))) ||
      length(list.files(paths$figures, pattern = paste0("^", a, "[.]"), full.names = TRUE)) > 0 ||
      (startsWith(a, "F-") && embedded_ok)
    if (!exists_ok) add("artifact-missing", n, a, "named by spec; no .rds or figure file")
    else if (!embedded_ok)
      add("artifact-not-embedded", n, a, "artifact exists but the chapter never references it")
  }

  ## --- Answers: reviewer comments this spec assigns to this chapter ---------
  ans_line <- grep("^- \\*\\*Answers", blk, value = TRUE)
  ids <- unique(unlist(regmatches(ans_line,
    gregexpr("(AE|R1|R2)-[0-9]+", ans_line))))
  for (id in ids) {
    expected_bind <- c(expected_bind, paste(n, id, sep = "::"))
    ## The claim map decomposes some comments into sub-items (R2-02A..G), so an
    ## id in the spec is tracked if ANY claim-map row starts with its padded form.
    pad <- sub("^([A-Z0-9]+)-([0-9])$", "\\1-0\\2", id)
    row <- cm[cm$id == id | startsWith(cm$id, pad), , drop = FALSE]
    if (!nrow(row)) add("answer-untracked", n, id, "spec assigns it; absent from claim map")
    else if (!any(grepl("addressed", row$status)))
      add("answer-unaddressed", n, id,
          sprintf("claim map status '%s'", paste(unique(row$status), collapse = "/")))
    target <- sub(paste0(paths$root, "/"), "", f, fixed = TRUE)
    brow <- bind[bind$chapter == n & bind$spec_id == id, , drop = FALSE]
    if (nrow(brow) != 1L)
      add("answer-unbound-target", n, id, "spec answer lacks one exact chapter/file binding")
    else if (!identical(brow$file, target))
      add("answer-wrong-target", n, id,
          sprintf("binding names %s rather than %s", brow$file, target))
  }

  ## --- Contents: blocking schema parse, advisory lexical coverage -----------
  ## Chapters 1--22 use numbered markers; Chapter 23 and the v2/v3 addendum use
  ## prose items separated by top-level semicolons. Parenthesized semicolons
  ## (notably ch. 27's H1--H7 grouping) remain within their parent item.
  cont <- field_text(blk, "Contents")
  parsed <- content_items(cont, n)
  if (is.na(cont)) add("contents-missing", n, "Contents", "spec has no Contents field")
  if (!nrow(parsed))
    add("contents-empty", n, "Contents", "spec Contents field yielded zero expected items")
  expected_n <- unname(addendum_counts[as.character(n)])
  if (!is.na(expected_n) && nrow(parsed) != expected_n)
    add("contents-schema-drift", n, "Contents",
        sprintf("v2/v3 addendum expects %d items; parsed %d", expected_n, nrow(parsed)))

  low <- character()
  per_item <- list()
  for (k in seq_len(nrow(parsed))) {
    it <- parsed$text[k]
    ## Distinctive words: length > 5, not generic. Coverage = share present.
    w <- unique(tolower(unlist(regmatches(it, gregexpr("[A-Za-z]{6,}", it)))))
    w <- setdiff(w, c("chapter", "chapters", "section", "sections", "because", "should",
                      "already", "between", "against", "through", "without", "whether",
                      "result", "results", "chapter's", "instead", "itself", "第"))
    hit <- if (length(w))
      mean(vapply(w, function(z) grepl(z, tolower(txt), fixed = TRUE), logical(1))) else NA_real_
    if (!is.na(hit) && hit < 0.5)
      low <- c(low, sprintf("%s: %s [%.0f%%]", parsed$item_id[k],
                            substr(trimws(it), 1, 60), 100 * hit))
    per_item[[k]] <- data.frame(
      chapter = n, item_id = parsed$item_id[k], schema = parsed$schema[k],
      specification = it, lexical_coverage = hit,
      advisory_low = !is.na(hit) && hit < 0.5,
      stringsAsFactors = FALSE)
  }
  if (length(per_item)) content_detail <- c(content_detail, per_item)
  cover[[length(cover) + 1L]] <- data.frame(
    chapter = n, items = nrow(parsed),
    item_ids = paste(parsed$item_id, collapse = ";"),
    schema = if (nrow(parsed)) paste(unique(parsed$schema), collapse = ";") else "",
    low_coverage = length(low),
    detail = if (length(low)) paste(low, collapse = " | ") else "",
    stringsAsFactors = FALSE)
}
cov <- do.call(rbind, cover)
content_tab <- if (length(content_detail)) do.call(rbind, content_detail) else
  data.frame(chapter = integer(), item_id = character(), schema = character(),
             specification = character(), lexical_coverage = numeric(),
             advisory_low = logical())
actual_bind <- paste(bind$chapter, bind$spec_id, sep = "::")
for (z in setdiff(unique(expected_bind), actual_bind)) {
  p <- strsplit(z, "::", fixed = TRUE)[[1]]
  add("answer-unbound-target", as.integer(p[1]), p[2], "missing from binding inventory")
}
for (z in setdiff(actual_bind, unique(expected_bind))) {
  p <- strsplit(z, "::", fixed = TRUE)[[1]]
  add("stale-answer-binding", as.integer(p[1]), p[2], "binding is not assigned by the specification")
}

## Appendix/front-matter architecture is part of the same specification.  This
## is an existence/anchor check, not a lexical claim that the prose is complete.
app_lines <- grep("^\\| \\*\\*[A-G]\\*\\* \\|", spec, value = TRUE)
app_ids <- sub("^\\| \\*\\*([A-G])\\*\\*.*$", "\\1", app_lines)
for (id in app_ids) {
  af <- list.files(paths$appendices, pattern = paste0("^", id, "-.*[.]qmd$"), full.names = TRUE)
  if (length(af) != 1L) {
    add("missing-appendix", NA_integer_, paste0("Appendix ", id),
        sprintf("expected exactly one %s-*.qmd file", id))
  } else {
    atxt <- paste(readLines(af, warn = FALSE), collapse = "\n")
    if (!grepl(paste0("{#sec-appendix-", tolower(id), "}"), atxt, fixed = TRUE))
      add("appendix-anchor", NA_integer_, paste0("Appendix ", id), "missing canonical appendix anchor")
  }
}
front <- file.path(paths$book, "index.qmd")
if (!file.exists(front)) add("missing-front-matter", NA_integer_, "index.qmd", "book front matter absent")

dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)
write.csv(issues, file.path(paths$verification, "spec-coverage.csv"), row.names = FALSE)
write.csv(cov, file.path(paths$verification, "spec-contents-coverage.csv"), row.names = FALSE)
write.csv(content_tab, file.path(paths$verification, "spec-content-items.csv"), row.names = FALSE)

cat(sprintf("V9 spec coverage: %d chapters plus front matter and %d appendices; blocking issues: %d (%d ratified deviations); contents lexical-diagnostic items: %d; low-coverage diagnostics: %d\n",
            nrow(cov), length(app_ids), nrow(issues), nrow(dev), sum(cov$items), sum(cov$low_coverage)))
if (nrow(issues)) print(issues, row.names = FALSE)
if (sum(cov$low_coverage))
  print(cov[cov$low_coverage > 0, c("chapter", "items", "low_coverage", "detail")],
        row.names = FALSE)
if (nrow(issues)) quit(status = 1L)
