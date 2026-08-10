## 11b-cross-book-fixtures.R -- dedicated positive/negative V4 fixtures.

old_library_only <- Sys.getenv("DPMIRT_CROSS_BOOK_LIBRARY_ONLY", unset = NA)
Sys.setenv(DPMIRT_CROSS_BOOK_LIBRARY_ONLY = "1")
source("code/R/11-cross-book-check.R")
if (is.na(old_library_only)) {
  Sys.unsetenv("DPMIRT_CROSS_BOOK_LIBRARY_ONLY")
} else {
  Sys.setenv(DPMIRT_CROSS_BOOK_LIBRARY_ONLY = old_library_only)
}

out_dir <- env_or("DPMIRT_VERIFICATION_DIR", paths$verification)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

positive <- list()
negative <- list()
record_positive <- function(id, fixture, passed, detail) {
  positive[[length(positive) + 1L]] <<- data.frame(
    fixture_id = id, fixture = fixture, passed = isTRUE(passed),
    detail = detail, stringsAsFactors = FALSE)
}
record_negative <- function(id, mutation, rejected, detail) {
  negative[[length(negative) + 1L]] <<- data.frame(
    fixture_id = id, mutation = mutation, rejected = isTRUE(rejected),
    detail = detail, stringsAsFactors = FALSE)
}

fixture_register <- function(classes = character(),
                             file = "book/fixture.qmd",
                             anchor = "sec-fixture") {
  vocab <- canonical_external_sources()
  if (!length(classes)) return(data.frame(
    id = character(), file = character(), anchor = character(),
    external_book = character(), contract = character(), status = character(),
    stringsAsFactors = FALSE))
  data.frame(
    id = sprintf("CB-%03d", seq_along(classes)), file = file, anchor = anchor,
    external_book = vapply(classes, function(k) vocab[[k]]$register_values[1],
                           character(1)),
    contract = paste("fixture contract for", classes), status = "addressed",
    stringsAsFactors = FALSE)
}

fixture_mentions <- function(lines) {
  b <- paragraph_blocks(lines, file = "book/fixture.qmd")
  mentions_from_blocks(b)
}

fixture_coverage <- function(lines, classes = character()) {
  coverage_from_mentions(fixture_mentions(lines), fixture_register(classes))
}

wrapped_sim <- c(
  "## Fixture {#sec-fixture}", "",
  "The frozen result is read from the simulation", "volume and not recomputed.")
wrapped_case <- c(
  "## Fixture {#sec-fixture}", "",
  "The catalogue belongs to the case-study", "volume's frozen analysis layer.")

## Positive parser/coverage fixtures.
structure_fixture <- c(
  "   ~~~{r}", "# fake {#sec-fake-tilde}", "simulation volume", "   ~~~",
  "````{r}", "```", "# fake {#sec-fake-short-close}", "````",
  "## Real heading {#sec-real}")
ms <- markdown_structure(structure_fixture)
record_positive(
  "VP01", "fenced fake headings and mentions are excluded",
  identical(ms$anchors, "sec-real") &&
    all(ms$in_fence[c(1:4, 5:8)]) && !ms$unclosed_fence,
  "tilde and long-backtick fences preserve only the real heading")

cov <- fixture_coverage(wrapped_sim, "simulation")
record_positive("VP02", "line-wrapped simulation volume is covered",
                nrow(cov) == 1L && cov$source_class == "simulation" &&
                  cov$covered,
                "paragraph join detects the source across a physical newline")

cov <- fixture_coverage(wrapped_case, "case_study")
record_positive("VP03", "line-wrapped case-study volume is covered",
                nrow(cov) == 1L && cov$source_class == "case_study" &&
                  cov$covered,
                "hyphen-plus-newline spelling maps to the case-study class")

both_companion <- c(
  "## Fixture {#sec-fixture}", "",
  "Both companion volumes now report their respective evidence.")
cov <- fixture_coverage(both_companion, c("simulation", "case_study"))
record_positive("VP04", "plural companion reference requires both books",
                nrow(cov) == 1L && cov$source_class == "companion_pair" &&
                  cov$covered,
                "simulation and case-study registrations jointly cover the phrase")

both_irw <- c(
  "## Fixture {#sec-fixture}", "",
  "The two corpus sources are [@lee_reliability-distribution_2026] and",
  "[@lee_theta-nonnormality_2026].")
cov <- fixture_coverage(both_irw, c("irw_reliability", "irw_shape"))
record_positive("VP05", "IRW citation keys map to distinct source classes",
                nrow(cov) == 2L && all(cov$covered) &&
                  setequal(cov$source_class,
                           c("irw_reliability", "irw_shape")),
                "citation detection is independent of prose spelling")

generic_companion <- c(
  "## Fixture {#sec-fixture}", "",
  "The proof remains here; the computation belongs to the companion", "volume.")
cov <- fixture_coverage(generic_companion, "simulation")
record_positive("VP06", "generic singular companion volume is bounded",
                nrow(cov) == 1L &&
                  cov$source_class == "companion_generic" && cov$covered,
                "one explicit companion-book registration is sufficient")

hidden <- c(
  "## Fixture {#sec-fixture}", "", "```text", "simulation volume", "```",
  "", "No external source is mentioned in reader-facing prose.")
record_positive("VP07", "fenced boundary text is not reader-facing",
                nrow(fixture_mentions(hidden)) == 0L,
                "code examples do not create register obligations")

## Negative coverage fixtures: every mutation must be rejected.
cov <- fixture_coverage(c(
  "## Fixture {#sec-fixture}", "",
  "See [@lee_simulation_2026] for the realized grid."))
record_negative("VN01", "omit simulation citation class",
                nrow(coverage_issues(cov)) == 1L,
                "simulation citation without a simulation row is rejected")

cov <- fixture_coverage(c(
  "## Fixture {#sec-fixture}", "",
  "See [@lee_casestudy_2026] for the real-test catalogue."))
record_negative("VN02", "omit case-study citation class",
                nrow(coverage_issues(cov)) == 1L,
                "case-study citation without a case-study row is rejected")

cov <- fixture_coverage(c(
  "## Fixture {#sec-fixture}", "",
  "The reliability census is [@lee_reliability-distribution_2026]."))
record_negative("VN03", "omit IRW reliability source class",
                nrow(coverage_issues(cov)) >= 1L,
                "IRW reliability citation/prose without its row is rejected")

cov <- fixture_coverage(c(
  "## Fixture {#sec-fixture}", "",
  "The shape corpus is [@lee_theta-nonnormality_2026]."))
record_negative("VN04", "omit IRW shape source class",
                nrow(coverage_issues(cov)) >= 1L,
                "IRW shape citation/prose without its row is rejected")

cov <- fixture_coverage(both_companion, "simulation")
record_negative("VN05", "register only one of two companion volumes",
                nrow(coverage_issues(cov)) == 1L,
                "plural companion phrase rejects the missing case-study class")

both_corpora <- c(
  "## Fixture {#sec-fixture}", "",
  "Two corpus studies now bound the transport question.")
cov <- fixture_coverage(both_corpora, "irw_shape")
record_negative("VN06", "register only one of two corpus studies",
                nrow(coverage_issues(cov)) == 1L,
                "corpus pair rejects the missing reliability class")

cov <- fixture_coverage(wrapped_sim)
record_negative("VN07", "omit a line-wrapped simulation-volume mention",
                nrow(coverage_issues(cov)) == 1L,
                "paragraph scanner catches what a line scanner would miss")

cov <- fixture_coverage(wrapped_case)
record_negative("VN08", "omit a line-wrapped case-study-volume mention",
                nrow(coverage_issues(cov)) == 1L,
                "hyphenated wrapped mention remains detectable")

cov <- fixture_coverage(c(
  "## Fixture {#sec-fixture}", "",
  "The corrected frame is [@lee_theta-nonnormality_2026]."),
  "simulation")
record_negative("VN09", "substitute an unrelated registered source class",
                nrow(coverage_issues(cov)) == 1L,
                "a row at the same section cannot cover the wrong source class")

fixture_root <- tempfile("v4-include-")
dir.create(file.path(fixture_root, "book", "figures"), recursive = TRUE)
writeLines(c("## Fixture {#sec-fixture}", "",
             "{{< include figures/caption.md >}}"),
           file.path(fixture_root, "book", "fixture.qmd"))
writeLines(c("The frozen case-study", "volume supplies this caption."),
           file.path(fixture_root, "book", "figures", "caption.md"))
inc_blocks <- qmd_blocks(file.path(fixture_root, "book", "fixture.qmd"),
                         fixture_root)
inc_mentions <- mentions_from_blocks(inc_blocks)
inc_cov <- coverage_from_mentions(inc_mentions, fixture_register())
record_negative("VN10", "omit an included line-wrapped sidecar mention",
                nrow(inc_mentions) == 1L &&
                  inc_mentions$origin == "book/figures/caption.md" &&
                  nrow(coverage_issues(inc_cov)) == 1L,
                "included reader-facing caption is attributed to the host section")
unlink(fixture_root, recursive = TRUE, force = TRUE)

positive_table <- do.call(rbind, positive)
negative_table <- do.call(rbind, negative)
write.csv(positive_table,
          file.path(out_dir, "cross-book-positive-fixtures.csv"),
          row.names = FALSE, na = "")
write.csv(negative_table,
          file.path(out_dir, "cross-book-negative-fixtures.csv"),
          row.names = FALSE, na = "")

cat(sprintf("V4 fixtures: %d/%d positive passed; %d/%d negative rejected\n",
            sum(positive_table$passed), nrow(positive_table),
            sum(negative_table$rejected), nrow(negative_table)))
if (any(!positive_table$passed))
  print(positive_table[!positive_table$passed, ], row.names = FALSE)
if (any(!negative_table$rejected))
  print(negative_table[!negative_table$rejected, ], row.names = FALSE)
if (any(!positive_table$passed) || any(!negative_table$rejected))
  stop("V4 dedicated fixtures failed")
