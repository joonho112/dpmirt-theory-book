## 08-build-check.R — V6 rendered-output, cross-reference and artifact integrity

source("code/R/00-paths.R")
if (!requireNamespace("xml2", quietly = TRUE)) stop("R package xml2 is required")

book_dir <- Sys.getenv("DPMIRT_RENDER_DIR")
if (!nzchar(book_dir)) book_dir <- file.path(paths$book, "_book")
if (!dir.exists(book_dir)) stop("no rendered book at ", book_dir)
html <- list.files(book_dir, pattern = "[.]html$", recursive = TRUE, full.names = TRUE)
if (!length(html)) stop("render directory contains no HTML")

issues <- data.frame(check = character(), object = character(), detail = character(),
                     stringsAsFactors = FALSE)
add_issue <- function(check, object, detail) {
  issues <<- rbind(issues, data.frame(check, object, detail, stringsAsFactors = FALSE))
}
docs <- lapply(html, xml2::read_html); names(docs) <- html
defined <- unique(unlist(lapply(docs, function(d) xml2::xml_attr(xml2::xml_find_all(d, "//*[@id]"), "id"))))

build_status <- Sys.getenv("DPMIRT_BUILD_STATUS")
if (!nzchar(build_status)) build_status <- "release"
if (!build_status %in% c("draft", "release")) stop("DPMIRT_BUILD_STATUS must be draft or release")
planned_forward <- if (build_status == "draft") {
  c(paste0("sec-ch", sprintf("%02d", 7:23)), paste0("sec-appendix-", letters[1:7]))
} else {
  character()
}
expected_warnings <- data.frame(line = character(), target = character(), stringsAsFactors = FALSE)
for (f in names(docs)) {
  d <- docs[[f]]
  nodes <- xml2::xml_find_all(d, ".//*[contains(concat(' ', normalize-space(@class), ' '), ' quarto-unresolved-ref ')]")
  vals <- unique(trimws(xml2::xml_text(nodes)))
  raw <- paste(readLines(f, warn = FALSE), collapse = " ")
  vals <- unique(c(vals, regmatches(raw, gregexpr("\\?(?:sec|fig|tbl|eq|thm|prp|def|lem|cor)-[A-Za-z0-9-]+", raw, perl = TRUE))[[1]]))
  vals <- sub("^\\?", "", vals)
  for (v in vals[nzchar(vals)]) {
    if (v %in% planned_forward && !v %in% defined) next
    add_issue("unresolved-reference", basename(f), v)
  }
  badcite <- xml2::xml_find_all(d, ".//*[contains(concat(' ', normalize-space(@class), ' '), ' citation ')]")
  bt <- trimws(xml2::xml_text(badcite))
  for (v in unique(bt[grepl("^\\?@|\\?\\{", bt)])) add_issue("unresolved-citation", basename(f), v)
}

render_log <- file.path(paths$verification, "quarto-render.log")
if (file.exists(render_log)) {
  w <- grep("^(WARN|WARNING)", readLines(render_log, warn = FALSE), value = TRUE)
  for (line in w) {
    target <- sub(".*Unable to resolve crossref @([A-Za-z0-9-]+).*", "\\1", line)
    if (grepl("Unable to resolve crossref @", line, fixed = TRUE) && target %in% planned_forward) {
      expected_warnings <- rbind(expected_warnings, data.frame(line = line, target = target))
    } else add_issue("unexpected-render-warning", "quarto-render.log", line)
  }
}
if (build_status == "release" && nrow(expected_warnings))
  add_issue("release-forward-whitelist", "DPMIRT_BUILD_STATUS",
            "release builds may not suppress unresolved forward references")

rr <- read.csv(file.path(paths$manifest, "result-register.csv"), stringsAsFactors = FALSE)
for (lab in rr$label[!rr$label %in% defined]) add_issue("missing-result-anchor", lab, "registered anchor absent from HTML")

## A native theorem number must not be repeated manually in its title.
qmd <- list.files(paths$book, pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
for (f in qmd) {
  x <- readLines(f, warn = FALSE)
  hit <- grep("^:::.*#(?:thm|prp|def|lem|cor)-", x, perl = TRUE)
  for (h in hit) {
    z <- x[h:min(length(x), h + 4L)]
    if (any(grepl("\\*\\*(Theorem|Proposition|Definition|Lemma|Corollary)\\s+[0-9]+(?:\\.[0-9]+)+", z, perl = TRUE)))
      add_issue("manual-result-number", basename(f), paste(z, collapse = " "))
  }
}

## Figure co-generation is a reproducibility contract, not a semantic-truth
## certificate. V6 binds each file-backed figure ID to its image, caption
## sidecar, and declared generator, and checks that those sources agree. It
## cannot establish that the caption's substantive interpretation is correct.
##
## Two authoring forms are supported:
##   ![caption](../figures/F-x.png){#fig-x}
## and
##   ::: {#fig-x}
##   ![](../figures/F-x.png)
##   {{< include ../figures/F-x.md >}}
##   :::
## An anchored div may instead carry an ordinary Markdown caption, but that
## caption must still agree with F-x.md. Executable Knitr figures are recorded
## as source-bound: their #| fig-cap is embedded and their QMD is the generator.
norm_caption <- function(x) gsub("[[:space:]]+", " ", trimws(paste(x, collapse = "\n")))
root_abs <- normalizePath(paths$root, mustWork = TRUE)
canonical_path <- function(x) {
  ## normalizePath() need not collapse `..` in a nonexistent leaf on every
  ## platform. Figure fixtures deliberately remove leaves, so canonicalize the
  ## existing parent and then append the basename.
  file.path(normalizePath(dirname(x), mustWork = FALSE), basename(x))
}
root_rel <- function(x) {
  y <- canonical_path(x)
  prefix <- paste0(root_abs, .Platform$file.sep)
  if (startsWith(y, prefix)) substring(y, nchar(prefix) + 1L) else y
}
resolve_from <- function(f, target) {
  target <- trimws(target)
  target <- sub("^[<\"']", "", target)
  target <- sub("[>\"']$", "", target)
  canonical_path(file.path(dirname(f), target))
}
figure_bindings <- data.frame(
  figure_id = character(), qmd = character(), syntax = character(),
  image = character(), sidecar = character(), caption_source = character(),
  generator = character(), stringsAsFactors = FALSE
)
add_binding <- function(id, f, syntax, image = "", sidecar = "",
                        caption_source = "", generator = "") {
  figure_bindings <<- rbind(figure_bindings, data.frame(
    figure_id = id, qmd = root_rel(f), syntax = syntax,
    image = if (nzchar(image)) root_rel(image) else "",
    sidecar = if (nzchar(sidecar)) root_rel(sidecar) else "",
    caption_source = caption_source, generator = generator,
    stringsAsFactors = FALSE
  ))
}
generator_for <- function(id, stem, caption) {
  pat <- "code/R/[A-Za-z0-9._/-]+[.]R"
  generators <- unique(regmatches(caption, gregexpr(pat, caption, perl = TRUE))[[1]])
  generators <- generators[nzchar(generators)]
  if (length(generators) != 1L) {
    add_issue("figure-generator-binding", id,
              sprintf("caption declares %d generator paths", length(generators)))
    return(if (length(generators)) paste(generators, collapse = ";") else "")
  }
  generator <- file.path(root_abs, generators)
  if (!file.exists(generator)) {
    add_issue("missing-figure-generator", id, generators)
  } else {
    source_text <- paste(readLines(generator, warn = FALSE), collapse = "\n")
    if (!grepl(stem, source_text, fixed = TRUE))
      add_issue("figure-generator-binding", id,
                paste(generators, "does not name", stem))
  }
  generators
}
expected_figure_id <- function(stem) paste0("fig-", sub("^F-", "", stem))
check_file_figure <- function(id, f, syntax, src, consuming_caption = "",
                              included_sidecar = "") {
  png <- resolve_from(f, src)
  stem <- tools::file_path_sans_ext(basename(png))
  expected_sidecar <- file.path(dirname(png), paste0(stem, ".md"))
  sidecar <- if (nzchar(included_sidecar)) resolve_from(f, included_sidecar) else expected_sidecar
  caption_source <- if (nzchar(included_sidecar)) "included-sidecar" else
    if (identical(syntax, "inline-image")) "image-alt" else "div-markdown"

  if (!identical(id, expected_figure_id(stem)))
    add_issue("figure-id-image-mismatch", id, paste(id, "does not bind to", basename(png)))
  if (!file.exists(png)) add_issue("missing-figure", basename(png), basename(f))
  if (nzchar(included_sidecar) &&
      !identical(canonical_path(sidecar), canonical_path(expected_sidecar)))
    add_issue("figure-caption-mismatch", id,
              paste("included", root_rel(sidecar), "but image requires", root_rel(expected_sidecar)))
  if (!file.exists(sidecar)) {
    add_issue("missing-figure-sidecar", id, root_rel(sidecar))
    add_binding(id, f, syntax, png, sidecar, caption_source, "")
    return(invisible(NULL))
  }

  side_caption <- norm_caption(readLines(sidecar, warn = FALSE))
  if (nzchar(consuming_caption) &&
      !identical(norm_caption(consuming_caption), side_caption))
    add_issue("figure-caption-mismatch", id,
              paste(caption_source, "differs from", root_rel(sidecar)))
  if (!nzchar(consuming_caption) && !nzchar(included_sidecar))
    add_issue("missing-figure-caption", id,
              "anchored figure needs a Markdown caption or .md include")

  generator <- generator_for(id, stem, side_caption)
  add_binding(id, f, syntax, png, sidecar, caption_source, generator)
  invisible(NULL)
}

inline_pat <- "!\\[(.*?)\\]\\(([^)]+[.]png)\\)\\{#(fig-[A-Za-z0-9-]+)\\}"
image_pat <- "!\\[(.*?)\\]\\(([^)]+[.]png)\\)(?:\\{[^}]*\\})?"
include_pat <- "\\{\\{<\\s*include\\s+[^>]+[.]md\\s*>\\}\\}"
for (f in qmd) {
  x <- readLines(f, warn = FALSE)
  z <- paste(x, collapse = "\n")

  ## Classic inline-caption figures. The lazy caption group permits citations
  ## such as [@key], which the former [^]]* pattern silently skipped.
  hits <- regmatches(z, gregexpr(inline_pat, z, perl = TRUE))[[1]]
  for (hit in hits[nzchar(hits)]) {
    cap <- sub(inline_pat, "\\1", hit, perl = TRUE)
    src <- sub(inline_pat, "\\2", hit, perl = TRUE)
    id <- sub(inline_pat, "\\3", hit, perl = TRUE)
    check_file_figure(id, f, "inline-image", src, cap)
  }

  ## Div-anchored figures allow an empty image alt and an included caption.
  opens <- grep("^\\s*:{3,}\\s*\\{[^}]*#fig-[A-Za-z0-9-]+[^}]*\\}\\s*$", x, perl = TRUE)
  used_include_lines <- integer()
  for (h in opens) {
    id <- sub("^.*#(fig-[A-Za-z0-9-]+).*$", "\\1", x[h], perl = TRUE)
    tail <- seq.int(h + 1L, length(x))
    close <- tail[grepl("^\\s*:{3,}\\s*$", x[tail], perl = TRUE)][1]
    if (is.na(close)) {
      add_issue("unterminated-figure-div", id, basename(f)); next
    }
    block_idx <- if (close > h + 1L) seq.int(h + 1L, close - 1L) else integer()
    block <- x[block_idx]
    image_hits <- regmatches(paste(block, collapse = "\n"),
                             gregexpr(image_pat, paste(block, collapse = "\n"), perl = TRUE))[[1]]
    image_hits <- image_hits[nzchar(image_hits)]
    if (length(image_hits) != 1L) {
      add_issue("figure-image-binding", id,
                sprintf("anchored div contains %d PNG images", length(image_hits)))
      next
    }
    src <- sub(image_pat, "\\2", image_hits[1], perl = TRUE)
    alt <- sub(image_pat, "\\1", image_hits[1], perl = TRUE)
    inc_local <- grep(include_pat, block, perl = TRUE)
    if (length(inc_local) > 1L)
      add_issue("figure-caption-include", id, "anchored div contains multiple .md includes")
    included <- ""
    if (length(inc_local)) {
      used_include_lines <- c(used_include_lines, block_idx[inc_local])
      inc_hit <- regmatches(block[inc_local[1]], regexpr(include_pat, block[inc_local[1]], perl = TRUE))
      included <- sub("^.*\\{\\{<\\s*include\\s+", "", inc_hit, perl = TRUE)
      included <- sub("\\s*>\\}\\}.*$", "", included, perl = TRUE)
      included <- gsub("^[<\"']|[>\"']$", "", trimws(included))
    }
    caption_lines <- block
    caption_lines[grepl(image_pat, caption_lines, perl = TRUE)] <- ""
    caption_lines[grepl(include_pat, caption_lines, perl = TRUE)] <- ""
    manual <- norm_caption(caption_lines)
    consuming <- paste(c(alt[nzchar(trimws(alt))], manual[nzchar(manual)]), collapse = " ")
    check_file_figure(id, f, "anchored-div", src, consuming, included)
  }

  ## A generic included Markdown caption must belong to a parsed figure div;
  ## otherwise it is not bound to any figure ID or image.
  all_includes <- grep(include_pat, x, perl = TRUE)
  for (h in setdiff(all_includes, used_include_lines))
    add_issue("unbound-figure-caption-include", basename(f), trimws(x[h]))

  ## Runtime figures have no file sidecar: the embedded fig-cap and the QMD
  ## chunk itself provide the reproducible source binding.
  chunk_labels <- grep("^#\\| label: fig-[A-Za-z0-9-]+\\s*$", x, perl = TRUE)
  for (h in chunk_labels) {
    id <- sub("^#\\| label:\\s*", "", trimws(x[h]))
    nearby <- x[seq.int(h, min(length(x), h + 8L))]
    cap <- nearby[grepl("^#\\| fig-cap:", nearby)][1]
    if (is.na(cap)) add_issue("missing-figure-caption", id, "Knitr figure lacks #| fig-cap")
    add_binding(id, f, "knitr-chunk", "", "", "embedded-fig-cap", root_rel(f))
  }
}

dupe_figures <- unique(figure_bindings$figure_id[duplicated(figure_bindings$figure_id)])
for (id in dupe_figures)
  add_issue("duplicate-figure-binding", id,
            paste(figure_bindings$qmd[figure_bindings$figure_id == id], collapse = ";"))
declared_figure_ids <- unique(c(
  unlist(lapply(qmd, function(f) {
    z <- paste(readLines(f, warn = FALSE), collapse = "\n")
    sub("^#", "", regmatches(z, gregexpr("#fig-[A-Za-z0-9-]+", z, perl = TRUE))[[1]])
  })),
  figure_bindings$figure_id[figure_bindings$syntax == "knitr-chunk"]
))
declared_figure_ids <- declared_figure_ids[nzchar(declared_figure_ids)]
for (id in setdiff(declared_figure_ids, figure_bindings$figure_id))
  add_issue("unbound-figure-id", id, "source figure ID has no sidecar/generator binding")

## Every executable table has a Source line and an existing, fresh RDS/CSV pair.
for (f in qmd) {
  x <- readLines(f, warn = FALSE)
  labs <- grep("^#\\| label: tbl-", x)
  for (h in labs) {
    id <- sub("^#\\| label: ", "", x[h])
    cap <- x[seq.int(h, min(length(x), h + 4L))]
    cap <- cap[grepl("^#\\| tbl-cap:", cap)]
    if (!length(cap) || !grepl("Source: tables/T-[A-Za-z0-9-]+[.]rds[.]", cap[1])) {
      add_issue("table-source-caption", id, basename(f)); next
    }
    stem <- sub('.*Source: tables/(T-[A-Za-z0-9-]+)[.]rds[.].*', '\\1', cap[1])
    rds <- file.path(paths$tables, paste0(stem, ".rds"))
    csv <- file.path(paths$tables, "supplement", paste0(stem, ".csv"))
    if (!file.exists(rds) || !file.exists(csv)) add_issue("missing-table-artifact", id, stem)
  }
}

## Generated numerical figure claims require paired, matching RDS/CSV data and summaries.
for (stem in c("F-rel-items-summary", "F-stick-crp-summary",
               "F-ensemble-shrink-summary", "F-rank-reliability-summary",
               "F-rank-reliability-convergence", "F-rank-reliability-curves",
               "F-rank-reliability-matched-gaps")) {
  rds <- file.path(paths$tables, paste0(stem, ".rds"))
  csv <- file.path(paths$tables, "supplement", paste0(stem, ".csv"))
  if (!file.exists(rds) || !file.exists(csv)) {
    add_issue("missing-figure-summary", stem, "requires matching RDS and CSV")
  } else {
    zr <- readRDS(rds); zc <- read.csv(csv, stringsAsFactors = FALSE)
    if (!isTRUE(all.equal(zr, zc, check.attributes = FALSE)))
      add_issue("figure-summary-mismatch", stem, "RDS and CSV values differ")
  }
}

## Realized-fit exhibits. Review 012 was right that a single live fit cannot carry
## a PERFORMANCE claim in a theory book, and its first remedy was to delete the
## exhibit outright. The ratified decision (2026-08-04) is narrower: a realized fit
## may appear as a REPRESENTATIONAL-CAPACITY illustration, of the same class as
## F-flexible-g, provided it is declared in the cross-book register and disclaims
## performance in its own caption. This check enforces those two conditions rather
## than banning the exhibit, so the guard now fails on the thing that was actually
## wrong instead of on the file's existence.
fit_fig <- file.path(paths$figures, "F-dpm-fit.md")
if (file.exists(fit_fig)) {
  cap <- paste(readLines(fit_fig, warn = FALSE), collapse = " ")
  if (!grepl("representational capacity", cap, fixed = TRUE))
    add_issue("realized-fit-undisclaimed", "F-dpm-fit.md",
              "caption must name itself a representational-capacity illustration")
  if (!grepl("not how either performs|no performance claim", cap, ignore.case = TRUE))
    add_issue("realized-fit-undisclaimed", "F-dpm-fit.md",
              "caption must disclaim performance")
  if (!grepl("companion simulation (study|book)", cap, ignore.case = TRUE, perl = TRUE))
    add_issue("realized-fit-unassigned", "F-dpm-fit.md",
              "caption must assign performance evaluation to the companion simulation study/book")
  cbr <- read.csv(file.path(paths$manifest, "cross-book-register.csv"),
                  stringsAsFactors = FALSE)
  cb12 <- cbr[cbr$id == "CB-012", , drop = FALSE]
  exact_cb12 <- nrow(cb12) == 1L &&
    identical(cb12$file, "book/chapters/16-dpm-priors.qmd") &&
    identical(cb12$anchor, "sec-ch16-tradeoffs") &&
    identical(cb12$external_book, "DPMirt-simulation-study-v3") &&
    identical(cb12$status, "addressed") &&
    grepl("F-dpm-fit", cb12$contract, fixed = TRUE) &&
    grepl("companion", cb12$contract, ignore.case = TRUE)
  if (!isTRUE(exact_cb12))
    add_issue("realized-fit-undeclared", "cross-book-register.csv",
              "requires exact addressed CB-012 at chapter 16 tradeoffs, assigned to DPMirt-simulation-study-v3, with F-dpm-fit in its companion-book contract")
}

## Public front-matter counts are executable metadata, not hand-maintained prose.
## Reconcile the rendered sentence against the same frozen tables Appendix G uses.
html_dirs <- vapply(dirname(html), normalizePath, character(1), mustWork = TRUE)
index_html <- html[basename(html) == "index.html" & html_dirs == normalizePath(book_dir)]
if (length(index_html) != 1L) {
  add_issue("front-metadata", "index.html", "expected exactly one root index.html")
} else {
  front_text <- gsub("[[:space:]]+", " ", xml2::xml_text(docs[[index_html]]))
  bld_file <- file.path(paths$tables, "T-build-report.rds")
  tier_file <- file.path(paths$tables, "T-bib-tiers.rds")
  ver_file <- file.path(paths$tables, "T-verifiers.rds")
  if (!all(file.exists(c(bld_file, tier_file, ver_file)))) {
    add_issue("front-metadata", "generated tables", "missing build, tier, or verifier table")
  } else {
    fb <- readRDS(bld_file); ft <- readRDS(tier_file); fv <- readRDS(ver_file)
    value <- function(q) fb$Value[fb$Quantity == q]
    tier <- function(q) ft$Entries[ft$Tier == q]
    bib_sentence <- sprintf(
      paste0("bibliography contains %d entries: %d primary-read, ",
             "%d programme-artifact-read, %d primary-held, and %d secondary"),
      value("Bibliography entries"), tier("primary-read"),
      tier("programme-artifact-read"), tier("primary-held"), tier("secondary"))
    verifier_sentence <- sprintf(
      paste0("runs %d verifiers (V1–V9) together with %d claim-level checkers, ",
             "including %d numerical checks in V8"),
      nrow(fv), value("Claim-level checkers (beyond V1--V9)"),
      value("Numerical checks (V8)"))
    ## The front matter also states the evidence-claim contract; those two counts
    ## are the ones a reader uses to size the guarantee, so they are held to the
    ## same parity rule as the verifier and bibliography counts.
    claim_sentence <- sprintf("records %d imported claims", value("Audited evidence claims"))
    claim_exec_sentence <- sprintf("%d of those are recomputed from the",
                                   value("  of which independently recomputed"))
    if (!grepl(bib_sentence, front_text, fixed = TRUE))
      add_issue("front-metadata", "bibliography tiers", "rendered count sentence differs from generated tables")
    if (!grepl(verifier_sentence, front_text, fixed = TRUE))
      add_issue("front-metadata", "verifier counts", "rendered count sentence differs from generated tables")
    if (!grepl(claim_sentence, front_text, fixed = TRUE))
      add_issue("front-metadata", "audited claim count", "rendered count sentence differs from generated tables")
    if (!grepl(claim_exec_sentence, front_text, fixed = TRUE))
      add_issue("front-metadata", "executable claim count", "rendered count sentence differs from generated tables")
  }
}

## A render is stale if a source QMD, bibliography, figure, or table is newer.
render_mtime <- min(file.info(html)$mtime)
## Unnumbered headings cannot be cross-referenced by Quarto, so any @-reference to
## one silently renders as an unresolved link. This has now happened twice, in
## appendices C and F, both times to the front matter's {.unnumbered} anchor. Fail
## on the source pattern rather than waiting for the rendered HTML to show it.
unnum <- unlist(lapply(list.files(paths$book, pattern = "[.]qmd$", recursive = TRUE,
                                  full.names = TRUE), function(f) {
  x <- readLines(f, warn = FALSE)
  h <- grep("^#{1,6} .*\\{#[^}]*\\.unnumbered[^}]*\\}", x, perl = TRUE)
  if (!length(h)) return(NULL)
  sub("^#{1,6} .*\\{#([^ }]+).*$", "\\1", x[h], perl = TRUE)
}))
if (length(unnum)) for (f in list.files(paths$book, pattern = "[.]qmd$", recursive = TRUE,
                                        full.names = TRUE)) {
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  for (u in unique(unnum))
    if (grepl(paste0("@", u, "\\b"), x, perl = TRUE))
      add_issue("xref-to-unnumbered", basename(f), u)
}

sources <- c(qmd, file.path(paths$book, "references.bib"),
             list.files(paths$figures, full.names = TRUE),
             list.files(paths$tables, recursive = TRUE, full.names = TRUE))
sources <- sources[file.exists(sources)]
newer <- sources[file.info(sources)$mtime > render_mtime]
for (f in newer) add_issue("stale-render", basename(f), "source newer than oldest rendered HTML")

dir.create(paths$verification, recursive = TRUE, showWarnings = FALSE)
write.csv(issues, file.path(paths$verification, "build-check.csv"), row.names = FALSE)
write.csv(expected_warnings, file.path(paths$verification, "expected-render-warnings.csv"), row.names = FALSE)
write.csv(figure_bindings, file.path(paths$verification, "figure-bindings.csv"), row.names = FALSE)
cat(sprintf("V6 rendered HTML: %d; anchors: %d; figure bindings: %d; issues: %d\n",
            length(html), length(defined), nrow(figure_bindings), nrow(issues)))
if (nrow(issues)) { print(issues, row.names = FALSE); quit(status = 1L) }
