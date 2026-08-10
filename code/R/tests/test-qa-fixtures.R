## Negative and positive fixtures for V1--V7 and the BibTeX parser.

source("code/R/00-paths.R")
source("code/R/00-bibtex-utils.R")

td <- tempfile("dpmirt-qa-"); dir.create(td)
on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
## The isolated copy excludes the review record, which is large and is not an
## input to any verifier.
status <- system2(Sys.which("rsync"), c("-a", "--exclude=.git", "--exclude=log/",
  shQuote(paste0(paths$root, "/")), shQuote(paste0(td, "/"))))
if (!identical(status, 0L)) stop("could not create isolated fixture copy")

rscript <- file.path(R.home("bin"), "Rscript")
env <- c(paste0("DPMIRT_THEORY_ROOT=", shQuote(td)),
         paste0("DPMIRT_PROJECT_ROOT=", shQuote(PROJECT_ROOT)))
run <- function(script) {
  z <- suppressWarnings(system2(rscript, script, env = env, stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(z, "status"))) 0L else attr(z, "status"), output = paste(z, collapse = "\n"))
}
results <- data.frame(fixture = character(), expected = character(), observed = character(),
                      passed = logical(), stringsAsFactors = FALSE)
record <- function(name, expected, ok, observed) {
  results <<- rbind(results, data.frame(fixture = name, expected = expected,
    observed = substr(observed, 1, 300), passed = ok, stringsAsFactors = FALSE))
  if (!ok) stop("fixture failed: ", name, "\n", observed)
}
expect_pass <- function(name, script) {
  z <- run(script); record(name, "exit 0", z$status == 0L, paste("exit", z$status, z$output))
}
expect_fail <- function(name, script, pattern) {
  z <- run(script); ok <- z$status != 0L && grepl(pattern, z$output, fixed = TRUE)
  record(name, paste0("nonzero and ", pattern), ok, paste("exit", z$status, z$output))
}
restore <- function(rel) invisible(file.copy(file.path(paths$root, rel), file.path(td, rel), overwrite = TRUE))

## Positive baseline: every independent V-check passes in the isolated copy.
for (s in c("code/R/03-citation-check.R", "code/R/10-notation-lint.R",
            "code/R/11-cross-book-check.R", "code/R/12-claim-map-check.R",
            "code/R/08-build-check.R", "code/R/13-release-marker-check.R"))
  expect_pass(paste0("positive-", basename(s)), s)

ch1 <- file.path(td, "book/chapters/01-the-estimation-problem.qmd")
x <- readLines(ch1, warn = FALSE); writeLines(c(x, "A bad citation [@definitely_missing]."), ch1)
expect_fail("V1-missing-citation", "code/R/03-citation-check.R", "missing citations: definitely_missing")
restore("book/chapters/01-the-estimation-problem.qmd")

idx <- file.path(td, "book/_book/index.html")
x <- readLines(idx, warn = FALSE); x <- sub("</body>", "<span class=\"quarto-unresolved-ref\">?prp-fixture</span></body>", x)
writeLines(x, idx)
expect_fail("V6-unresolved-proposition", "code/R/08-build-check.R", "prp-fixture")
restore("book/_book/index.html")

x <- readLines(ch1, warn = FALSE)
writeLines(c(x, "", "::: {#thm-fixture}", "**Theorem 9.9. Manual duplicate.**", "", "Fixture.", ":::"), ch1)
expect_fail("V6-manual-theorem-number", "code/R/08-build-check.R", "manual-result-number")
restore("book/chapters/01-the-estimation-problem.qmd")

x <- readLines(ch1, warn = FALSE); writeLines(c(x, "Fixture $Z=1$."), ch1)
expect_fail("V3-unregistered-Z", "code/R/10-notation-lint.R", "unregistered")
restore("book/chapters/01-the-estimation-problem.qmd")

x <- readLines(ch1, warn = FALSE)
writeLines(c(x, "Fixture $\\theta_{not_registered}=1$."), ch1)
expect_fail("V3-unregistered-index-family", "code/R/10-notation-lint.R", "not_registered")
restore("book/chapters/01-the-estimation-problem.qmd")

x <- readLines(ch1, warn = FALSE); writeLines(c(x, "Fixture $\\omega=1$."), ch1)
expect_fail("V3-unregistered-Greek", "code/R/10-notation-lint.R", "\\omega")
restore("book/chapters/01-the-estimation-problem.qmd")

nr <- file.path(td, "manifest/notation-register.csv")
x <- readLines(nr, warn = FALSE); writeLines(c(x, x[2]), nr)
expect_fail("V3-duplicate-P", "code/R/10-notation-lint.R", "duplicate definitions: 1")
restore("manifest/notation-register.csv")

side <- file.path(td, "book/figures/F-irf.md"); unlink(side)
expect_fail("V6-missing-caption-sidecar", "code/R/08-build-check.R", "missing-figure-sidecar")
restore("book/figures/F-irf.md")

## The v2/v3 empty-alt + included-sidecar syntax must be covered too.
side <- file.path(td, "book/figures/F-estimand-map.md"); unlink(side)
expect_fail("V6-missing-sidecar-new-syntax", "code/R/08-build-check.R", "missing-figure-sidecar")
restore("book/figures/F-estimand-map.md")

## An inline caption drifting away from its canonical sidecar is rejected.
side <- file.path(td, "book/figures/F-irf.md")
writeLines(c(readLines(side, warn = FALSE), "Fixture drift."), side)
expect_fail("V6-drifted-sidecar", "code/R/08-build-check.R", "figure-caption-mismatch")
restore("book/figures/F-irf.md")

## A sidecar must name an existing generator that itself names the figure.
x <- readLines(side, warn = FALSE)
x <- sub("code/R/09-figures.R", "code/R/22-figures-v2-concepts.R", x, fixed = TRUE)
writeLines(x, side)
expect_fail("V6-wrong-figure-generator", "code/R/08-build-check.R", "figure-generator-binding")
restore("book/figures/F-irf.md")

## An include that names a real but wrong sidecar must not inherit legitimacy
## merely because Quarto can render it.
x <- readLines(ch1, warn = FALSE)
x <- sub("include ../figures/F-estimand-map.md", "include ../figures/F-flexible-tree.md",
         x, fixed = TRUE)
writeLines(x, ch1)
expect_fail("V6-caption-mismatch-new-syntax", "code/R/08-build-check.R", "figure-caption-mismatch")
restore("book/chapters/01-the-estimation-problem.qmd")

Sys.setFileTime(ch1, Sys.time() + 2)
expect_fail("V6-stale-render", "code/R/08-build-check.R", "stale-render")
restore("book/chapters/01-the-estimation-problem.qmd")

bad <- tempfile(fileext = ".bib")
writeLines(c("@article{bad,", "title {balanced but missing equals}", "}"), bad)
v <- validate_bib_file(bad, require_project_fields = FALSE)
record("BibTeX-balanced-malformed", "parser rejection", !v$ok, paste(v$problems, collapse = "; "))

dup <- tempfile(fileext = ".bib")
writeLines(c("@misc{x, title={A}}", "", "@misc{x, title={B}}"), dup)
v <- validate_bib_file(dup, require_project_fields = FALSE)
record("BibTeX-duplicate-key", "duplicate rejection", !v$ok && any(grepl("duplicate", v$problems)),
       paste(v$problems, collapse = "; "))

dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)
write.csv(results, file.path(paths$verification, "qa-fixtures.csv"), row.names = FALSE)
cat(sprintf("QA fixtures passed: %d/%d\n", sum(results$passed), nrow(results)))
