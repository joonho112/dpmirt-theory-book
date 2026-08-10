## Small BibTeX utilities shared by harvest, merge, and citation QA.
## Entries are split with a brace/quote-aware scanner so raw blocks can be
## preserved byte-for-byte; bibtex::read.bib supplies the independent parser.

bib_key_from_block <- function(x) {
  m <- regexec("^\\s*@([[:alpha:]]+)\\s*[\\{(]\\s*([^,[:space:]]+)\\s*,",
               x, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) != 3L) return(NA_character_)
  hit[3]
}

bib_type_from_block <- function(x) {
  m <- regexec("^\\s*@([[:alpha:]]+)\\s*[\\{(]", x, perl = TRUE)
  hit <- regmatches(x, m)[[1]]
  if (length(hit) != 2L) return(NA_character_)
  tolower(hit[2])
}

split_bib_text <- function(txt) {
  if (!nzchar(txt)) return(character())
  ch <- strsplit(txt, "", fixed = TRUE)[[1]]
  n <- length(ch); out <- character(); i <- 1L
  while (i <= n) {
    if (ch[i] != "@") { i <- i + 1L; next }
    start <- i
    open <- which(ch[i:min(n, i + 200L)] %in% c("{", "("))[1]
    if (is.na(open)) stop("BibTeX entry has no opening delimiter near character ", i)
    open <- i + open - 1L
    op <- ch[open]; cl <- if (op == "{") "}" else ")"
    depth <- 1L; quoted <- FALSE; escaped <- FALSE; j <- open + 1L
    while (j <= n && depth > 0L) {
      z <- ch[j]
      if (escaped) {
        escaped <- FALSE
      } else if (z == "\\") {
        escaped <- TRUE
      } else if (z == '"') {
        quoted <- !quoted
      } else if (!quoted && z == op) {
        depth <- depth + 1L
      } else if (!quoted && z == cl) {
        depth <- depth - 1L
      }
      j <- j + 1L
    }
    if (depth != 0L) stop("unbalanced BibTeX entry beginning near character ", start)
    out <- c(out, substr(txt, start, j - 1L))
    i <- j
  }
  out
}

read_bib_blocks <- function(path) {
  if (!file.exists(path)) return(character())
  split_bib_text(paste(readLines(path, warn = FALSE), collapse = "\n"))
}

bib_fields_from_block <- function(x) {
  m <- gregexpr("(?m)^\\s*([[:alpha:]][[:alnum:]_-]*)\\s*=", x, perl = TRUE)
  z <- regmatches(x, m)[[1]]
  if (!length(z)) return(character())
  tolower(sub("\\s*=.*$", "", trimws(z)))
}

md5_text <- function(x) {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f), add = TRUE)
  writeChar(x, f, eos = NULL, useBytes = TRUE)
  unname(tools::md5sum(f))
}

validate_bib_file <- function(path, require_project_fields = FALSE) {
  blocks <- read_bib_blocks(path)
  keys <- vapply(blocks, bib_key_from_block, character(1))
  types <- vapply(blocks, bib_type_from_block, character(1))
  problems <- character()
  if (anyNA(keys)) problems <- c(problems, "one or more entries have no parseable key")
  if (anyDuplicated(keys))
    problems <- c(problems, paste("duplicate keys:", paste(unique(keys[duplicated(keys)]), collapse = ", ")))

  parsed <- tryCatch(
    suppressWarnings(bibtex::read.bib(path)),
    error = function(e) e)
  if (inherits(parsed, "error")) {
    problems <- c(problems, paste("bibtex parser error:", conditionMessage(parsed)))
  } else if (length(parsed) != length(blocks)) {
    missing <- setdiff(keys, names(parsed))
    problems <- c(problems, sprintf("bibtex parser accepted %d/%d entries; rejected: %s",
                                    length(parsed), length(blocks),
                                    paste(missing, collapse = ", ")))
  }

  if (require_project_fields && length(blocks)) {
    req <- c("tier", "asset", "verified_on", "verified_claims")
    miss <- lapply(seq_along(blocks), function(i) setdiff(req, bib_fields_from_block(blocks[i])))
    bad <- which(lengths(miss) > 0L)
    if (length(bad))
      problems <- c(problems, paste("missing project fields:",
                                    paste(sprintf("%s[%s]", keys[bad],
                                                  vapply(miss[bad], paste, collapse = "/", character(1))),
                                          collapse = ", ")))
  }

  list(ok = !length(problems), problems = problems, blocks = blocks,
       keys = keys, types = types, parsed = parsed)
}

replace_or_add_project_fields <- function(block, tier = "primary-held", asset = "") {
  fields <- bib_fields_from_block(block)
  add <- character()
  if (!"tier" %in% fields) add <- c(add, sprintf("\ttier = {%s}", tier))
  if (!"asset" %in% fields) add <- c(add, sprintf("\tasset = {%s}", asset))
  if (!"verified_on" %in% fields) add <- c(add, "\tverified_on = {}")
  if (!"verified_claims" %in% fields) add <- c(add, "\tverified_claims = {}")
  if (!length(add)) return(block)
  body <- sub("[[:space:]]*[})][[:space:]]*$", "", block, perl = TRUE)
  body <- sub(",[[:space:]]*$", "", body)
  paste0(body, ",\n", paste(add, collapse = ",\n"), "\n}")
}
