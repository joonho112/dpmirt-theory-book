## 17-annotated-bibliography.R — generates Appendix D.
## An annotated bibliography is a document made entirely of claims about sources,
## so nothing here is written from memory. Each annotation is assembled from what
## the project has ALREADY verified about that source: a semantic receipt, a
## numbered result sourced to it, a correction it grounds, a bounded narrative
## receipt, or a programme-artifact receipt. Programme-owned artifacts are
## labelled separately so a direct code/table read cannot masquerade as
## independent scholarly corroboration.
source("code/R/00-paths.R")

bib <- paste(readLines(file.path(paths$book, "references.bib"), warn = FALSE),
             collapse = "\n")
blocks <- regmatches(bib, gregexpr("@[A-Za-z]+\\{[^,]+,(?:[^@]|\n)*?\n\\}", bib, perl = TRUE))[[1]]
fld <- function(b, n) {
  m <- regmatches(b, regexpr(paste0(n, " = \\{(.|\n)*?\\}\\s*,?\\s*\n"), b, perl = TRUE))
  if (!length(m)) return("")
  trimws(sub(paste0("^", n, " = \\{"), "", sub("\\}\\s*,?\\s*\n$", "", m)))
}
key  <- sub("^@[A-Za-z]+\\{([^,]+),.*$", "\\1", sub("\n(.|\n)*$", "", blocks))
tier <- vapply(blocks, fld, character(1), "tier", USE.NAMES = FALSE)
auth <- vapply(blocks, fld, character(1), "author", USE.NAMES = FALSE)
titl <- vapply(blocks, fld, character(1), "title", USE.NAMES = FALSE)
year <- vapply(blocks, fld, character(1), "year", USE.NAMES = FALSE)
von  <- vapply(blocks, fld, character(1), "verified_on", USE.NAMES = FALSE)

short <- function(a, y) {
  a1 <- trimws(strsplit(a, " and ", fixed = TRUE)[[1]])
  fam <- sub(",.*$", "", a1)
  lab <- if (length(fam) == 1) fam[1] else
         if (length(fam) == 2) paste(fam[1], "and", fam[2]) else paste0(fam[1], " et al.")
  sprintf("%s (%s)", lab, y)
}

rc <- do.call(rbind, lapply(
  list.files(paths$manifest, pattern = "^part-[a-z]+-source-receipts[.]csv$", full.names = TRUE),
  function(f) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)[, 1:7]))
pr <- read.csv(file.path(paths$manifest, "programme-artifact-receipts.csv"),
               stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
na <- read.csv(file.path(paths$manifest, "narrative-source-receipts.csv"),
               stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
rr <- read.csv(file.path(paths$manifest, "result-register.csv"), stringsAsFactors = FALSE,
               na.strings = c("", "NA"), check.names = FALSE)
cx <- read.csv(file.path(paths$manifest, "corrections.csv"), stringsAsFactors = FALSE,
               na.strings = c("", "NA"), check.names = FALSE)
## The context-match table is a keyword-overlap navigation aid, NOT evidence of
## reading (see blueprint/06-citation-policy.qmd R1a). It is deliberately not used
## as an evidence type here: an annotated bibliography built partly on keyword
## overlap would be the same defect the appendix opens by warning against.
## Citation verifications are the fourth evidence type, admitted under one strict
## condition. A row counts only if it was checked AT A LOCATOR and confirmed there;
## rows whose verdict is "ATTRIBUTION SOUND" compared a title and distinctive terms
## against the citing claim and never opened a passage, so they are refused here for
## exactly the reason the context-match table is refused. The refusal is enforced,
## not documented: those keys also stay `primary-held` and never reach this loop.
cv <- read.csv(file.path(paths$manifest, "citation-verification.csv"),
               stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
cv <- cv[!is.na(cv$level) & cv$level == "locator" &
           !is.na(cv$verdict) & grepl("^CONFIRMED", cv$verdict), , drop = FALSE]

qmd <- c(list.files(paths$chapters, pattern = "[.]qmd$", full.names = TRUE),
         list.files(paths$appendices, pattern = "[.]qmd$", full.names = TRUE))
cited_in <- function(k) {
  hit <- vapply(qmd, function(f)
    grepl(paste0("@", k, "\\b"), paste(readLines(f, warn = FALSE), collapse = "\n"), perl = TRUE),
    logical(1))
  basename(qmd)[hit]
}
## Literature groups follow the blueprint, assigned by the FIRST unit that cites
## the source. Derived from the files, not from recall.
group_of <- function(files) {
  if (!length(files)) return("Not cited in the body")
  n <- suppressWarnings(as.integer(sub("^([0-9]+)-.*$", "\\1", files)))
  if (all(is.na(n))) return("Appendices only")
  n <- min(n, na.rm = TRUE)
  if (n <= 2) "Orientation and assessment context"
  else if (n <= 6) "Rasch, the 2PL, and estimation"
  else if (n <= 9) "Information, error, and reliability"
  else if (n <= 12) "Bayesian hierarchy, shrinkage, and empirical Bayes"
  else if (n <= 14) "Non-normal latent distributions"
  else if (n <= 16) "Bayesian nonparametrics and identification"
  else if (n <= 20) "Posterior summaries and inferential goals"
  else "Positioning and scope"
}

rows <- lapply(which(tier %in% c("primary-read", "programme-artifact-read")), function(i) {
  k <- key[i]
  rcs <- rc[!is.na(rc$key) & rc$key == k, , drop = FALSE]
  nas <- na[!is.na(na$key) & na$key == k, , drop = FALSE]
  prs <- pr[!is.na(pr$key) & pr$key == k, , drop = FALSE]
  ids <- rr$id[!is.na(rr$source_key) & rr$source_key == k]
  ids <- ids[!is.na(ids) & nzchar(ids)]
  ## Corrections bind through the bibliography's own `verified_claims`, which is the
  ## authoritative record of what this source was checked for. The older heuristic
  ## searched the correction's prose locator for the bibtex key and therefore missed
  ## every correction whose locator names the work in words -- C-007 says "Conoyer et
  ## al. (2022, Table 1)", so Conoyer read as unbound despite carrying C-007.
  vc <- trimws(strsplit(fld(blocks[i], "verified_claims"), ";", fixed = TRUE)[[1]])
  vc <- vc[nzchar(vc)]
  cxs <- unique(c(intersect(vc, cx$id),
                  cx$id[!is.na(cx$source_locator) &
                          grepl(k, cx$source_locator, fixed = TRUE)]))
  cxs <- cxs[!is.na(cxs) & nzchar(cxs)]
  ids <- unique(c(ids, intersect(vc, rr$id)))
  cvs <- cv[!is.na(cv$id) & cv$id %in% vc, , drop = FALSE]
  files <- cited_in(k)
  ev <- character()
  if (nrow(prs)) ev <- c(ev, paste0("programme receipt ",
                                    paste(prs$receipt_id, collapse = ", ")))
  if (nrow(rcs)) ev <- c(ev, paste0("receipt ", paste(rcs$receipt_id, collapse = ", ")))
  if (nrow(nas)) ev <- c(ev, paste0("narrative receipt ",
                                    paste(nas$receipt_id, collapse = ", ")))
  if (length(ids)) ev <- c(ev, paste0("result ", paste(ids, collapse = ", ")))
  if (length(cxs)) ev <- c(ev, paste0("correction ", paste(cxs, collapse = ", ")))
  if (nrow(cvs)) ev <- c(ev, paste0("citation verification ", paste(cvs$id, collapse = ", ")))
  what <- if (nrow(prs)) paste0(
            paste(unique(prs$verified_claim), collapse = " "), " ",
            "**Authority boundary:** ",
            paste(unique(prs$authority_boundary), collapse = " "))
          else if (nrow(rcs)) paste(unique(rcs$verified_claim), collapse = " ")
          else if (nrow(nas)) paste(unique(nas$verified_claim), collapse = " ")
          else if (nrow(cvs)) paste(unique(cvs$what_was_checked), collapse = " ")
          else if (length(ids)) paste0(
            "Source for ", paste(ids, collapse = ", "), ": ",
            paste(unique(rr$statement[!is.na(rr$source_key) & rr$source_key == k]), collapse = "; "))
          else if (length(cxs)) paste0(
            "Grounds correction ", paste(cxs, collapse = ", "), ": ",
            paste(unique(cx$why_wrong[!is.na(cx$id) & cx$id %in% cxs]), collapse = " "))
          else paste0("**Read but unbound.** Cited in prose only; no semantic receipt, ",
                      "narrative audit, numbered result or correction binds a specific claim to it. ",
                      "Its use in this book is contextual.")
  locs <- c(if (nrow(prs)) prs$locator else character(),
            if (nrow(rcs)) rcs$locator else character(),
            if (nrow(nas)) nas$locator else character(),
            if (nrow(cvs)) cvs$locator else character(),
            rr$source_locator[!is.na(rr$source_key) & rr$source_key == k])
  locs <- unique(locs[!is.na(locs) & nzchar(locs)])
  loc <- paste(locs, collapse = "; ")
  ## Audited book paragraphs can contain Quarto div fences.  In Appendix D they
  ## are evidence text inside a block quote, not executable document structure.
  what <- gsub(":::", "", what, fixed = TRUE)
  what <- gsub("[[:space:]]+", " ", what)
  data.frame(
    Group = group_of(files),
    Source = short(auth[i], year[i]),
    .key = k,
    Title = titl[i],
    Tier = tier[i],
    Authority = if (tier[i] == "programme-artifact-read")
      "Programme-owned artifact; fidelity evidence, not independent corroboration"
      else "Independent literature read at a locator",
    `Cited in` = if (length(files)) paste(sub("-.*$", "", files), collapse = ", ") else "—",
    `What was verified` = what,
    `At` = if (nzchar(loc)) loc else "—",
    Evidence = if (length(ev)) paste(ev, collapse = "; ") else "none",
    `Read on` = von[i],
    check.names = FALSE, stringsAsFactors = FALSE)
})
T_annot <- do.call(rbind, rows)
## Same-author same-year collisions (this programme's own 2026 works) are
## disambiguated by the citation key, which is what a reader needs to find
## the entry; distinct works must never merge into one display row.
dup <- duplicated(T_annot$Source) | duplicated(T_annot$Source, fromLast = TRUE)
T_annot$Source[dup] <- paste0(T_annot$Source[dup], " [`", T_annot$.key[dup], "`]")
T_annot$.key <- NULL
T_annot <- T_annot[order(T_annot$Group, T_annot$Source), ]
if (any(grepl("(^|[^A-Za-z])NA([^A-Za-z]|$)", unlist(T_annot), perl = TRUE)))
  stop("literal NA token leaked into annotated bibliography")
if (anyDuplicated(T_annot$Source)) stop("duplicate source row in annotated bibliography")
if (any(T_annot$Evidence == "none"))
  stop("locator-read annotation lacks an evidence receipt: ",
       paste(T_annot$Source[T_annot$Evidence == "none"], collapse = ", "))

save_tbl <- function(obj, id) {
  saveRDS(obj, file.path(paths$tables, paste0(id, ".rds")))
  dir.create(file.path(paths$tables, "supplement"), showWarnings = FALSE)
  write.csv(obj, file.path(paths$tables, "supplement", paste0(id, ".csv")), row.names = FALSE)
}
save_tbl(T_annot, "T-annotated-bib")

## The unannotated remainder, by tier. Counts only -- listing 338 entries would
## bulk the appendix without making any of them checkable.
T_tiers <- as.data.frame(table(Tier = tier), stringsAsFactors = FALSE)
names(T_tiers) <- c("Tier", "Entries")
T_tiers$Meaning <- c(
  "primary-held" = "Held in a reference library, not read at a locator for this book",
  "primary-read" = "Independent literature opened at the locator cited; annotated here",
  "programme-artifact-read" = paste(
    "Programme-owned book, manuscript, package, code, or table opened directly;",
    "supports fidelity to that artifact, not independent corroboration"),
  "secondary"    = "Cited as a secondary treatment, and marked as such where used"
)[T_tiers$Tier]
T_tiers$Meaning[is.na(T_tiers$Meaning)] <- "Other"
save_tbl(T_tiers, "T-bib-tiers")

cat(sprintf("Appendix D: %d annotated (%d groups); tiers %s\n",
            nrow(T_annot), length(unique(T_annot$Group)),
            paste(T_tiers$Tier, T_tiers$Entries, sep = "=", collapse = " ")))
