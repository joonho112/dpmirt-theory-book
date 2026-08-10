## Harvest embedded BibTeX blocks and recursively index every PDF/Markdown asset.
## External libraries are read-only. Nonidentical duplicate keys require an
## explicit preference in refs/bibtex-conflict-resolutions.csv.

source("code/R/00-paths.R")
source("code/R/00-bibtex-utils.R")

BEG <- "BEGIN_REFERENCE_BIBTEX"
END <- "END_REFERENCE_BIBTEX"

extract_bibtex <- function(file) {
  x <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  i <- grep(BEG, x, fixed = TRUE); j <- grep(END, x, fixed = TRUE)
  if (!length(i) || !length(j) || j[1] <= i[1]) return(NA_character_)
  blk <- x[(i[1] + 1L):(j[1] - 1L)]
  blk <- blk[!grepl("^\\s*```", blk)]
  txt <- trimws(paste(blk, collapse = "\n"))
  if (!nzchar(txt)) return(NA_character_)
  parts <- tryCatch(split_bib_text(txt), error = function(e) character())
  if (length(parts) != 1L) return(NA_character_)
  parts[1]
}

rows <- list(); entries <- list(); coverage <- list()
missing_libraries <- names(libraries)[!dir.exists(libraries)]
if (length(missing_libraries)) stop(
  "required reference libraries are missing: ",
  paste(paste0(missing_libraries, "=", libraries[missing_libraries]), collapse = "; "))
for (nm in names(libraries)) {
  d <- libraries[[nm]]
  md <- list.files(d, pattern = "[.]md$", recursive = TRUE, full.names = TRUE)
  pdf <- list.files(d, pattern = "[.]pdf$", recursive = TRUE, full.names = TRUE,
                    ignore.case = TRUE)
  rel_md <- sub(paste0("^", gsub("([][{}()+*^$|\\?.])", "\\\\\\1", d), "/?"), "", md)
  rel_pdf <- sub(paste0("^", gsub("([][{}()+*^$|\\?.])", "\\\\\\1", d), "/?"), "", pdf)
  stems <- sort(unique(c(sub("[.]md$", "", rel_md, ignore.case = TRUE),
                         sub("[.]pdf$", "", rel_pdf, ignore.case = TRUE))))
  for (st in stems) {
    mf <- file.path(d, paste0(st, ".md"))
    ent <- if (file.exists(mf)) extract_bibtex(mf) else NA_character_
    key <- if (is.na(ent)) NA_character_ else bib_key_from_block(ent)
    rows[[length(rows) + 1L]] <- data.frame(
      key = key, library = nm, stem = st,
      pdf = any(tolower(rel_pdf) == tolower(paste0(st, ".pdf"))),
      ocr = file.exists(mf), bibtex = !is.na(ent), stringsAsFactors = FALSE)
    if (!is.na(ent) && !is.na(key))
      entries[[length(entries) + 1L]] <- data.frame(
        key = key, library = nm, stem = st, entry = ent,
        normalized = gsub("\\s+", " ", trimws(ent)), stringsAsFactors = FALSE)
  }
  coverage[[length(coverage) + 1L]] <- data.frame(
    library = nm, n_pdf = length(pdf), n_md = length(md),
    paired = sum(vapply(stems, function(st)
      any(tolower(rel_pdf) == tolower(paste0(st, ".pdf"))) &&
        file.exists(file.path(d, paste0(st, ".md"))), logical(1))),
    stringsAsFactors = FALSE)
}

idx <- do.call(rbind, rows)
ent <- do.call(rbind, entries)
cvg <- do.call(rbind, coverage)
dir.create(paths$verification, recursive = TRUE, showWarnings = FALSE)

res_file <- file.path(paths$refs, "bibtex-conflict-resolutions.csv")
res <- if (file.exists(res_file)) read.csv(res_file, stringsAsFactors = FALSE) else
  data.frame(key = character(), preferred_library = character(), reason = character())

by_key <- split(ent, ent$key)
conflicts <- do.call(rbind, lapply(names(by_key), function(k) {
  z <- by_key[[k]]; variants <- length(unique(z$normalized))
  if (nrow(z) < 2L) return(NULL)
  pref <- res$preferred_library[match(k, res$key)]
  data.frame(key = k, copies = nrow(z), variants = variants,
             libraries = paste(unique(z$library), collapse = ";"),
             preferred_library = ifelse(is.na(pref), "", pref),
             resolved = variants == 1L || (!is.na(pref) && pref %in% z$library),
             stringsAsFactors = FALSE)
}))
if (is.null(conflicts)) conflicts <- data.frame(
  key=character(), copies=integer(), variants=integer(), libraries=character(),
  preferred_library=character(), resolved=logical())
write.csv(conflicts, file.path(paths$verification, "bibtex-conflicts.csv"), row.names = FALSE)

chosen <- lapply(names(by_key), function(k) {
  z <- by_key[[k]]; pref <- res$preferred_library[match(k, res$key)]
  if (!is.na(pref) && pref %in% z$library) z[z$library == pref, , drop = FALSE][1, ] else z[1, ]
})
chosen <- do.call(rbind, chosen)
chosen <- chosen[order(chosen$key), ]

harvest_lines <- unlist(lapply(chosen$entry, function(x) c(x, "")))
if (length(harvest_lines) && !nzchar(tail(harvest_lines, 1L)))
  harvest_lines <- head(harvest_lines, -1L)
writeLines(c(
  "% refs/harvested.bib — generated; raw selected blocks are preserved.",
  sprintf("%% %d unique keys from %d embedded blocks; see verification/bibtex-conflicts.csv.",
          nrow(chosen), nrow(ent)), "",
  harvest_lines),
  file.path(paths$refs, "harvested.bib"))
write.csv(idx, file.path(paths$refs, "asset-index.csv"), row.names = FALSE, na = "")
write.csv(cvg, file.path(paths$verification, "bibtex-coverage.csv"), row.names = FALSE)

unresolved <- conflicts$variants > 1L & !conflicts$resolved
cat(sprintf("assets indexed     : %d (%d PDF, %d Markdown)\n", nrow(idx),
            sum(idx$pdf), sum(idx$ocr)))
cat(sprintf("embedded blocks    : %d; unique keys: %d\n", nrow(ent), nrow(chosen)))
cat(sprintf("duplicate keys     : %d; nonidentical unresolved: %d\n",
            nrow(conflicts), sum(unresolved)))
if (any(unresolved)) {
  print(conflicts[unresolved, ], row.names = FALSE)
  quit(status = 1L)
}
