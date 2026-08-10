## 11-cross-book-check.R -- V4 cross-book boundary register.
##
## Detection is deliberately independent of manifest/cross-book-register.csv.
## The register is the object under test, never the source of its vocabulary.

source("code/R/00-paths.R")

env_or <- function(name, default) {
  z <- Sys.getenv(name)
  if (nzchar(z)) z else default
}

regex_escape <- function(z) {
  gsub("([][{}()+*^$|\\?.])", "\\\\\\1", z, perl = TRUE)
}

root_relative <- function(path, root_dir) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- paste0(normalizePath(root_dir, winslash = "/", mustWork = FALSE), "/")
  if (startsWith(p, root)) substring(p, nchar(root) + 1L) else p
}

## Independent canonical source classes. Text patterns are intentionally
## conservative; canonical citation keys provide the strongest detection path.
canonical_external_sources <- function() {
  list(
    simulation = list(
      register_values = c("DPMirt-simulation-study",
                          "DPMirt-simulation-study-v3"),
      citation_keys = "lee_simulation_2026",
      patterns = c(
        dpmirt_repository =
          "\\bdpmirt[[:space:]-]+simulation[[:space:]-]+study(?:[[:space:]-]+v3)?\\b",
        simulation_volume =
          "\\bsimulation[[:space:]-]+volume(?:['’]s)?\\b",
        simulation_book =
          "\\bsimulation[[:space:]-]+book(?:['’]s)?\\b",
        companion_simulation =
          "\\bcompanion[[:space:]-]+simulation(?:[[:space:]-]+(?:study|book|volume))?(?:['’]s)?\\b"
      )
    ),
    case_study = list(
      register_values = "DPMirt-case-study",
      citation_keys = "lee_casestudy_2026",
      patterns = c(
        dpmirt_repository =
          "\\bdpmirt[[:space:]-]+case[[:space:]-]+study\\b",
        case_study_volume =
          "\\bcase[[:space:]-]+study[[:space:]-]+volume(?:['’]s)?\\b",
        case_study_book =
          "\\bcase[[:space:]-]+study[[:space:]-]+book(?:['’]s)?\\b",
        companion_case_study =
          "\\bcompanion[[:space:]-]+case[[:space:]-]+study(?:[[:space:]-]+(?:book|volume))?(?:['’]s)?\\b"
      )
    ),
    irw_reliability = list(
      register_values = "IRW-reliability-study",
      citation_keys = "lee_reliability-distribution_2026",
      patterns = c(
        warehouse_reliability =
          "\\b(?:item[[:space:]-]+response[[:space:]-]+)?warehouse[[:space:]-]+reliability[[:space:]-]+(?:study|analysis|work|census)(?:['’]s)?\\b",
        irw_reliability =
          "\\birw[[:space:]-]+reliability[[:space:]-]+(?:study|analysis|work|census)(?:['’]s)?\\b",
        reliability_corpus =
          "\\b(?:reliability[[:space:]-]+corpus|corpus[[:space:]-]+reliability|reliability[[:space:]-]+census)(?:['’]s)?\\b"
      )
    ),
    irw_shape = list(
      register_values = "IRW-shape-study",
      citation_keys = "lee_theta-nonnormality_2026",
      patterns = c(
        warehouse_shape =
          "\\b(?:item[[:space:]-]+response[[:space:]-]+)?warehouse[[:space:]-]+(?:latent[[:space:]-]+distribution[[:space:]-]+)?shape[[:space:]-]+(?:study|analysis|work|census)(?:['’]s)?\\b",
        irw_shape =
          "\\birw[[:space:]-]+(?:latent[[:space:]-]+distribution[[:space:]-]+)?shape[[:space:]-]+(?:study|analysis|work|census)(?:['’]s)?\\b",
        shape_corpus =
          "\\b(?:shape[[:space:]-]+corpus|corpus[[:space:]-]+shape|shape[[:space:]-]+census)(?:['’]s)?\\b"
      )
    )
  )
}

canonical_vocabulary_table <- function(vocab = canonical_external_sources()) {
  do.call(rbind, lapply(names(vocab), function(k) data.frame(
    source_class = k,
    register_values = paste(vocab[[k]]$register_values, collapse = " | "),
    citation_keys = paste(vocab[[k]]$citation_keys, collapse = " | "),
    text_patterns = paste(names(vocab[[k]]$patterns), collapse = " | "),
    stringsAsFactors = FALSE
  )))
}

register_source_class <- function(z, vocab = canonical_external_sources()) {
  vapply(z, function(value) {
    hit <- names(vocab)[vapply(vocab, function(spec)
      value %in% spec$register_values, logical(1))]
    if (length(hit) == 1L) hit else NA_character_
  }, character(1))
}

## Return fence membership and real ATX heading anchors for a Markdown source.
markdown_structure <- function(x) {
  n <- length(x)
  in_fence <- logical(n)
  anchor <- rep("", n)
  fence_char <- ""
  fence_len <- 0L

  for (i in seq_len(n)) {
    line <- x[i]
    if (nzchar(fence_char)) {
      in_fence[i] <- TRUE
      close_pat <- sprintf("^ {0,3}%s{%d,}[ \\t]*$", fence_char, fence_len)
      if (grepl(close_pat, line, perl = TRUE)) {
        fence_char <- ""
        fence_len <- 0L
      }
      next
    }

    m <- regexec("^ {0,3}(`{3,}|~{3,})", line, perl = TRUE)
    z <- regmatches(line, m)[[1]]
    if (length(z) == 2L) {
      delim <- z[2]
      fence_char <- substr(delim, 1L, 1L)
      fence_len <- nchar(delim)
      in_fence[i] <- TRUE
      next
    }

    hm <- regexec(
      "^ {0,3}#{1,6}[ \\t]+.*\\{#(sec-[A-Za-z0-9_-]+)(?:[ \\t]+[^}]*)?\\}[ \\t]*#*[ \\t]*$",
      line, perl = TRUE)
    hz <- regmatches(line, hm)[[1]]
    if (length(hz) == 2L) anchor[i] <- hz[2]
  }

  list(
    in_fence = in_fence,
    anchor = anchor,
    anchors = anchor[nzchar(anchor)],
    unclosed_fence = nzchar(fence_char)
  )
}

empty_blocks <- function() data.frame(
  file = character(), anchor = character(), origin = character(),
  start_line = integer(), end_line = integer(), host_line = integer(),
  text = character(), stringsAsFactors = FALSE)

## Form reader-facing paragraph blocks. Fenced blocks are excluded. Wrapped
## prose is joined before matching, so line breaks cannot hide a source name.
paragraph_blocks <- function(x, file, initial_anchor = "", origin = file,
                             allow_headings = TRUE, host_line = NA_integer_) {
  s <- markdown_structure(x)
  blocks <- list()
  paragraph <- character()
  paragraph_start <- NA_integer_
  current_anchor <- initial_anchor

  flush_paragraph <- function(end_line) {
    if (!length(paragraph)) return(invisible(NULL))
    txt <- trimws(paste(paragraph, collapse = " "))
    txt <- gsub("[[:space:]]+", " ", txt)
    if (nzchar(txt)) {
      blocks[[length(blocks) + 1L]] <<- data.frame(
        file = file, anchor = current_anchor, origin = origin,
        start_line = paragraph_start, end_line = end_line,
        host_line = host_line, text = txt, stringsAsFactors = FALSE)
    }
    paragraph <<- character()
    paragraph_start <<- NA_integer_
    invisible(NULL)
  }

  for (i in seq_along(x)) {
    if (s$in_fence[i]) {
      flush_paragraph(i - 1L)
      next
    }
    if (allow_headings && nzchar(s$anchor[i])) {
      flush_paragraph(i - 1L)
      current_anchor <- s$anchor[i]
      blocks[[length(blocks) + 1L]] <- data.frame(
        file = file, anchor = current_anchor, origin = origin,
        start_line = i, end_line = i, host_line = host_line,
        text = trimws(x[i]), stringsAsFactors = FALSE)
      next
    }
    if (!nzchar(trimws(x[i]))) {
      flush_paragraph(i - 1L)
      next
    }
    if (!length(paragraph)) paragraph_start <- i
    paragraph <- c(paragraph, x[i])
  }
  flush_paragraph(length(x))
  if (!length(blocks)) empty_blocks() else do.call(rbind, blocks)
}

include_target <- function(line) {
  m <- regexec(
    "^\\s*\\{\\{<\\s*include\\s+([^>]+?)\\s*>\\}\\}\\s*$",
    line, perl = TRUE)
  z <- regmatches(line, m)[[1]]
  if (length(z) != 2L) return(NA_character_)
  gsub("^[\"']|[\"']$", "", trimws(z[2]))
}

## Parse a host QMD and attribute included Markdown paragraphs to the host's
## active section. The include line remains the locator in the host source.
qmd_blocks <- function(path, root_dir) {
  x <- readLines(path, warn = FALSE)
  rel <- root_relative(path, root_dir)
  s <- markdown_structure(x)
  out <- list()
  paragraph <- character()
  paragraph_start <- NA_integer_
  current_anchor <- ""

  flush_paragraph <- function(end_line) {
    if (!length(paragraph)) return(invisible(NULL))
    txt <- trimws(paste(paragraph, collapse = " "))
    txt <- gsub("[[:space:]]+", " ", txt)
    if (nzchar(txt)) {
      out[[length(out) + 1L]] <<- data.frame(
        file = rel, anchor = current_anchor, origin = rel,
        start_line = paragraph_start, end_line = end_line,
        host_line = NA_integer_, text = txt, stringsAsFactors = FALSE)
    }
    paragraph <<- character()
    paragraph_start <<- NA_integer_
    invisible(NULL)
  }

  for (i in seq_along(x)) {
    if (s$in_fence[i]) {
      flush_paragraph(i - 1L)
      next
    }
    if (nzchar(s$anchor[i])) {
      flush_paragraph(i - 1L)
      current_anchor <- s$anchor[i]
      out[[length(out) + 1L]] <- data.frame(
        file = rel, anchor = current_anchor, origin = rel,
        start_line = i, end_line = i, host_line = NA_integer_,
        text = trimws(x[i]), stringsAsFactors = FALSE)
      next
    }

    inc <- include_target(x[i])
    if (!is.na(inc)) {
      flush_paragraph(i - 1L)
      inc_path <- normalizePath(file.path(dirname(path), inc), winslash = "/",
                                mustWork = FALSE)
      if (file.exists(inc_path)) {
        inc_rel <- root_relative(inc_path, root_dir)
        ib <- paragraph_blocks(readLines(inc_path, warn = FALSE), file = rel,
                               initial_anchor = current_anchor,
                               origin = inc_rel, allow_headings = FALSE,
                               host_line = i)
        if (nrow(ib)) out <- c(out, split(ib, seq_len(nrow(ib))))
      }
      next
    }

    if (!nzchar(trimws(x[i]))) {
      flush_paragraph(i - 1L)
      next
    }
    if (!length(paragraph)) paragraph_start <- i
    paragraph <- c(paragraph, x[i])
  }
  flush_paragraph(length(x))
  if (!length(out)) empty_blocks() else do.call(rbind, out)
}

empty_mentions <- function() data.frame(
  file = character(), anchor = character(), origin = character(),
  start_line = integer(), end_line = integer(), host_line = integer(),
  source_class = character(), detector = character(), excerpt = character(),
  stringsAsFactors = FALSE)

detect_external_sources <- function(text,
                                    vocab = canonical_external_sources()) {
  found <- list()
  add <- function(source_class, detector) {
    found[[length(found) + 1L]] <<- data.frame(
      source_class = source_class, detector = detector,
      stringsAsFactors = FALSE)
  }

  for (k in names(vocab)) {
    spec <- vocab[[k]]
    citation_pat <- paste0("@(?:",
                           paste(regex_escape(spec$citation_keys), collapse = "|"),
                           ")(?![A-Za-z0-9_-])")
    if (grepl(citation_pat, text, ignore.case = TRUE, perl = TRUE))
      add(k, paste0("citation:", paste(spec$citation_keys, collapse = "|")))
    hits <- names(spec$patterns)[vapply(spec$patterns, function(p)
      grepl(p, text, ignore.case = TRUE, perl = TRUE), logical(1))]
    if (length(hits)) add(k, paste0("text:", paste(hits, collapse = "|")))
  }

  companion_pair <- grepl(
    "\\b(?:(?:two|both|neither)[[:space:]-]+companion[[:space:]-]+volumes?|companion[[:space:]-]+volumes)(?:['’]s)?\\b",
    text, ignore.case = TRUE, perl = TRUE)
  corpus_pair <- grepl(
    "\\b(?:(?:two[[:space:]-]+)?corpus[[:space:]-]+studies|warehouse[[:space:]-]+studies)(?:['’]s)?\\b",
    text, ignore.case = TRUE, perl = TRUE)
  companion_generic <- grepl(
    "\\bcompanion[[:space:]-]+(?:volume|book)(?:['’]s)?\\b",
    text, ignore.case = TRUE, perl = TRUE)

  if (companion_pair) add("companion_pair", "text:companion-volumes")
  if (corpus_pair) add("irw_pair", "text:corpus-or-warehouse-studies")
  if (companion_generic && !companion_pair)
    add("companion_generic", "text:companion-volume-or-book")

  if (!length(found)) return(data.frame(source_class = character(),
                                        detector = character(),
                                        stringsAsFactors = FALSE))
  z <- do.call(rbind, found)
  aggregate(detector ~ source_class, z, function(v)
    paste(unique(v), collapse = ";"))
}

mentions_from_blocks <- function(blocks,
                                 vocab = canonical_external_sources()) {
  if (!nrow(blocks)) return(empty_mentions())
  out <- list()
  for (i in seq_len(nrow(blocks))) {
    d <- detect_external_sources(blocks$text[i], vocab)
    if (!nrow(d)) next
    excerpt <- substr(blocks$text[i], 1L, 240L)
    for (j in seq_len(nrow(d))) {
      out[[length(out) + 1L]] <- data.frame(
        file = blocks$file[i], anchor = blocks$anchor[i],
        origin = blocks$origin[i], start_line = blocks$start_line[i],
        end_line = blocks$end_line[i], host_line = blocks$host_line[i],
        source_class = d$source_class[j], detector = d$detector[j],
        excerpt = excerpt, stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) empty_mentions() else unique(do.call(rbind, out))
}

empty_issues <- function() data.frame(
  issue = character(), category = character(), file = character(),
  anchor = character(), source_class = character(), locator = character(),
  detail = character(), stringsAsFactors = FALSE)

make_issue <- function(category, detail, file = "", anchor = "",
                       source_class = "", locator = "") {
  data.frame(
    issue = paste(c(file, anchor, source_class, detail)[
      nzchar(c(file, anchor, source_class, detail))], collapse = " "),
    category = category, file = file, anchor = anchor,
    source_class = source_class, locator = locator, detail = detail,
    stringsAsFactors = FALSE)
}

required_classes <- function(source_class) {
  switch(source_class,
         companion_pair = c("simulation", "case_study"),
         irw_pair = c("irw_reliability", "irw_shape"),
         companion_generic = character(),
         source_class)
}

coverage_from_mentions <- function(mentions, register,
                                   vocab = canonical_external_sources()) {
  if (!nrow(mentions)) return(data.frame(
    mentions, required_classes = character(), registered_classes = character(),
    registered_ids = character(), covered = logical(),
    stringsAsFactors = FALSE))
  rclass <- register_source_class(register$external_book, vocab)
  out <- vector("list", nrow(mentions))
  for (i in seq_len(nrow(mentions))) {
    at_section <- register$file == mentions$file[i] &
      register$anchor == mentions$anchor[i]
    got <- unique(na.omit(rclass[at_section]))
    ids <- register$id[at_section]
    req <- required_classes(mentions$source_class[i])
    if (mentions$source_class[i] == "companion_generic") {
      ok <- any(got %in% c("simulation", "case_study"))
      req_label <- "simulation OR case_study"
    } else {
      ok <- all(req %in% got)
      req_label <- paste(req, collapse = " | ")
    }
    out[[i]] <- data.frame(
      mentions[i, , drop = FALSE],
      required_classes = req_label,
      registered_classes = paste(got, collapse = " | "),
      registered_ids = paste(ids, collapse = " | "),
      covered = ok, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

coverage_issues <- function(coverage) {
  if (!nrow(coverage) || all(coverage$covered)) return(empty_issues())
  out <- lapply(which(!coverage$covered), function(i) {
    z <- coverage[i, ]
    loc <- if (!is.na(z$host_line)) {
      sprintf("%s:%s -> %s:%s", z$file, z$host_line,
              z$origin, z$start_line)
    } else {
      sprintf("%s:%s", z$origin, z$start_line)
    }
    make_issue(
      "unregistered-boundary-mention",
      sprintf("requires %s; registered classes here: %s; detector: %s",
              z$required_classes,
              if (nzchar(z$registered_classes)) z$registered_classes else "none",
              z$detector),
      z$file, z$anchor, z$source_class, loc)
  })
  unique(do.call(rbind, out))
}

validate_register <- function(register, root_dir, structure_cache,
                              vocab = canonical_external_sources()) {
  issues <- list()
  add <- function(...) issues[[length(issues) + 1L]] <<- make_issue(...)
  need <- c("id", "file", "anchor", "external_book", "contract", "status")
  if (!all(need %in% names(register))) {
    add("register-schema", "cross-book register schema mismatch")
    return(do.call(rbind, issues))
  }
  if (anyDuplicated(register$id)) add("register-id", "duplicate cross-book IDs")
  expected_ids <- sprintf("CB-%03d", seq_len(nrow(register)))
  if (!identical(register$id, expected_ids))
    add("register-id", "cross-book IDs must be sequential and row-ordered")

  rclass <- register_source_class(register$external_book, vocab)
  for (j in seq_len(nrow(register))) {
    rel <- register$file[j]
    q <- file.path(root_dir, rel)
    if (!file.exists(q)) {
      add("register-file", paste(register$id[j], "missing file"), file = rel)
      next
    }
    s <- structure_cache[[normalizePath(q, winslash = "/")]]
    if (is.null(s)) s <- markdown_structure(readLines(q, warn = FALSE))
    if (!register$anchor[j] %in% s$anchors)
      add("register-anchor", paste(register$id[j], "missing real heading anchor"),
          file = rel, anchor = register$anchor[j])
    if (!nzchar(register$contract[j]) ||
        !register$status[j] %in% c("addressed", "planned"))
      add("register-contract", paste(register$id[j], "invalid contract/status"),
          file = rel, anchor = register$anchor[j])
    if (is.na(rclass[j]))
      add("register-source-class",
          paste(register$id[j], "uses a noncanonical external_book value"),
          file = rel, anchor = register$anchor[j],
          source_class = register$external_book[j])
  }
  if (!length(issues)) empty_issues() else unique(do.call(rbind, issues))
}

run_cross_book_check <- function(
    root_dir = env_or("DPMIRT_ROOT_DIR", paths$root),
    book_dir = env_or("DPMIRT_BOOK_DIR", paths$book),
    register_path = env_or("DPMIRT_CROSS_BOOK_REGISTER",
                           file.path(paths$manifest,
                                     "cross-book-register.csv")),
    out_dir = env_or("DPMIRT_VERIFICATION_DIR", paths$verification)) {
  if (!file.exists(register_path)) stop("missing cross-book register")
  register <- read.csv(register_path, stringsAsFactors = FALSE,
                       na.strings = "")
  vocab <- canonical_external_sources()

  allq <- sort(list.files(book_dir, pattern = "[.]qmd$", recursive = TRUE,
                          full.names = TRUE))
  normalized_q <- vapply(allq, normalizePath, character(1), winslash = "/")
  structure_cache <- setNames(lapply(allq, function(q)
    markdown_structure(readLines(q, warn = FALSE))), normalized_q)

  structural <- list()
  for (i in seq_along(allq)) {
    s <- structure_cache[[normalized_q[i]]]
    rel <- root_relative(allq[i], root_dir)
    if (s$unclosed_fence)
      structural[[length(structural) + 1L]] <- make_issue(
        "markdown-structure", "unclosed fenced block", file = rel)
    if (anyDuplicated(s$anchors))
      structural[[length(structural) + 1L]] <- make_issue(
        "markdown-structure", "duplicate real heading anchors", file = rel)
  }
  structural <- if (length(structural)) unique(do.call(rbind, structural))
                else empty_issues()

  blocks_list <- lapply(allq, qmd_blocks, root_dir = root_dir)
  blocks <- do.call(rbind, blocks_list[vapply(blocks_list, nrow, integer(1)) > 0L])
  if (is.null(blocks)) blocks <- empty_blocks()
  mentions <- mentions_from_blocks(blocks, vocab)
  coverage <- coverage_from_mentions(mentions, register, vocab)

  issues <- unique(rbind(
    structural,
    validate_register(register, root_dir, structure_cache, vocab),
    coverage_issues(coverage)
  ))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(issues, file.path(out_dir, "cross-book-check.csv"),
            row.names = FALSE, na = "")
  write.csv(coverage, file.path(out_dir, "cross-book-coverage.csv"),
            row.names = FALSE, na = "")
  write.csv(canonical_vocabulary_table(vocab),
            file.path(out_dir, "cross-book-canonical-vocabulary.csv"),
            row.names = FALSE, na = "")

  cat(sprintf(paste0("V4 cross-book rows: %d; paragraphs: %d; ",
                     "source mentions: %d; issues: %d\n"),
              nrow(register), nrow(blocks), nrow(mentions), nrow(issues)))
  cat("V4 vocabulary: independent canonical classes; paragraph/citation/include scan\n")
  if (nrow(issues)) {
    print(issues, row.names = FALSE)
    stop("V4 cross-book boundary check failed")
  }
  invisible(list(register = register, blocks = blocks, mentions = mentions,
                 coverage = coverage, issues = issues))
}

if (!identical(Sys.getenv("DPMIRT_CROSS_BOOK_LIBRARY_ONLY"), "1"))
  run_cross_book_check()
