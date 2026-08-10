## 00-paths.R — the ONLY file in this repository containing absolute paths.
## Everything else resolves through here. Override any of these with the
## matching environment variable to relocate the repo or its read-only inputs.
##
## `DPMIRT_PROJECT_ROOT` is the directory that holds this repository alongside
## its read-only inputs: the nine reference libraries whose licensed PDFs back
## the citation audit, the companion simulation and case-study repositories, and
## the two Item Response Warehouse corpus studies. **Those inputs are not part of
## the published release** — they contain licensed third-party material — so the
## full verification pipeline runs only where they are present. Rendering the
## book needs none of them; see the README.

`%||%` <- function(x, y) if (is.null(x) || !nzchar(x)) y else x

## Every script is invoked from the repository root, so the working directory
## identifies the repository without hard-coding either its name or its parent.
BOOK_ROOT_DEFAULT <- normalizePath(getwd(), mustWork = TRUE)

PROJECT_ROOT <- Sys.getenv("DPMIRT_PROJECT_ROOT") %||%
  dirname(BOOK_ROOT_DEFAULT)

## --- this repository (the only tree we write to) -------------------------
BOOK_ROOT <- Sys.getenv("DPMIRT_THEORY_ROOT") %||% BOOK_ROOT_DEFAULT

paths <- list(
  root         = BOOK_ROOT,
  book         = file.path(BOOK_ROOT, "book"),
  chapters     = file.path(BOOK_ROOT, "book", "chapters"),
  appendices   = file.path(BOOK_ROOT, "book", "appendices"),
  figures      = file.path(BOOK_ROOT, "book", "figures"),
  refs         = file.path(BOOK_ROOT, "refs"),
  assets       = file.path(BOOK_ROOT, "refs", "assets"),
  manifest     = file.path(BOOK_ROOT, "manifest"),
  tables       = file.path(BOOK_ROOT, "tables"),
  verification = file.path(BOOK_ROOT, "verification"),
  drafts       = file.path(BOOK_ROOT, "drafts"),
  log          = file.path(BOOK_ROOT, "log")
)

## --- read-only reference libraries (nine) --------------------------------
## Never written to. See DECISIONS.md (b).
IRW <- file.path(PROJECT_ROOT, "Item Response Warehouse Project")
libraries <- c(
  irt_zotero        = file.path(PROJECT_ROOT, "references-IRT-Zotero"),
  epm_references    = file.path(PROJECT_ROOT, "2023-11-28_EPM Writing", "References"),
  psych_methods     = file.path(IRW, "references-Psychological-Methods"),
  nonnormal         = file.path(IRW, "references-nonnormal"),
  irw_reliability   = file.path(IRW, "reference-IRW-Reliability-Paper"),
  reliability_assets= file.path(IRW, "references-01-reliability-assets"),
  dpm_paper         = file.path(PROJECT_ROOT, "references-DPM-Paper"),
  dpmirt            = file.path(PROJECT_ROOT, "references-DPMirt"),
  remaining         = file.path(PROJECT_ROOT, "references-Remaining")
)

## --- other read-only inputs ----------------------------------------------
inputs <- list(
  manuscript   = file.path(PROJECT_ROOT, "2023-11-28_EPM Writing",
                           "2025-04-11_APM에 resubmission and revision"),
  research_notes = file.path(PROJECT_ROOT, "2023-11-28_EPM Writing",
                             "2025-08-12_APM Research Notes for First Revision"),
  sim_book     = file.path(PROJECT_ROOT, "DPMirt-simulation-study"),
  sim_book_v2  = file.path(PROJECT_ROOT, "DPMirt-simulation-study-v2"),
  ## v3 carries the remediation of sim-v2's external review and supersedes the
  ## v2 store as the authority; evidence scripts read v3.
  sim_book_v3  = file.path(PROJECT_ROOT, "DPMirt-simulation-study-v3"),
  ## The case-study SECOND EDITION is the cited volume; its frozen analysis
  ## layer (tables/P*, manifest/, data/) still lives in the first edition's
  ## repository, which the second edition itself reads across the boundary.
  case_study   = file.path(PROJECT_ROOT, "DPMirt-case-study"),
  case_study_v2= file.path(PROJECT_ROOT, "DPMirt-case-study-v2"),
  irw_paper1   = file.path(IRW, "paper-01-reliability-distribution"),
  irw_paper2   = file.path(IRW, "paper-02-theta-nonnormality"),
  pkg_dpmirt   = file.path(PROJECT_ROOT, "DPMirt"),
  pkg_irtsimrel= file.path(PROJECT_ROOT, "IRTsimrel"),
  v3_codebase  = file.path(PROJECT_ROOT, "targeted-DPMirt-simulation-codebase-v3")
)

stopifnot(dir.exists(paths$root))
invisible(NULL)
