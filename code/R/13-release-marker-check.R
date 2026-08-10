## 13-release-marker-check.R — V7 release marker guard
source("code/R/00-paths.R")
qmd <- list.files(paths$book, pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
## A marker inside an inline code span is documentation, not a live marker --
## Appendix G describes this very guard. Strip code spans before matching so the
## guard does not fire on its own description, while any real marker still fails.
strip_code <- function(x) gsub("`[^`]*`", "", x)
hits <- do.call(rbind, lapply(qmd, function(f) {
  x <- readLines(f, warn = FALSE)
  h <- grep("\\[VERIFY(?:[^]]*)?\\]", strip_code(x), perl = TRUE)
  if (!length(h)) return(NULL)
  data.frame(file = sub(paste0(paths$root, "/"), "", f, fixed = TRUE), line = h,
             text = trimws(x[h]), stringsAsFactors = FALSE)
}))
if (is.null(hits)) hits <- data.frame(file = character(), line = integer(), text = character())
dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)
write.csv(hits, file.path(paths$verification, "release-marker-check.csv"), row.names = FALSE)
cat(sprintf("V7 VERIFY markers: %d\n", nrow(hits)))

## Novelty-claim guard. The book's convention (index.qmd) is that `derived-here`
## asserts no priority: "proved in this text without claiming that the result is
## new to the literature," and an explicit novelty claim "would require a separate
## prior-art search." The convention only holds if no result's PROSE claims novelty
## anyway. This checks the paragraphs around every numbered result for priority
## language, so the promise is enforced rather than asserted once at T4.6.
rr7 <- read.csv(file.path(paths$manifest, "result-register.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
novel_pat <- paste0("\\b(novel|for the first time|first to (?:show|prove|derive|establish)|",
                    "new to the literature|has not (?:previously )?been (?:shown|proved|derived)|",
                    "we are the first|originates here|our own contribution|",
                    "original (?:method|estimator|result|proof|contribution)|",
                    "we (?:introduce|propose) (?:a )?new)\\b")
novel_fixture <- c("we introduce a new estimator", "an original method", "our own contribution")
if (!all(grepl(novel_pat, novel_fixture, ignore.case = TRUE, perl = TRUE)))
  stop("V7 novelty-language regression fixture failed")
claims <- do.call(rbind, lapply(qmd, function(f) {
  x <- readLines(f, warn = FALSE)
  anchors <- grep("^:::\\s*\\{#(def|thm|prp|lem|cor)-", x, perl = TRUE)
  if (!length(anchors)) return(NULL)
  do.call(rbind, lapply(anchors, function(a) {
    lab <- sub("^:::\\s*\\{#([^ }]+).*$", "\\1", x[a], perl = TRUE)
    win <- x[max(1, a - 6):min(length(x), a + 40)]
    h <- grep(novel_pat, win, perl = TRUE, ignore.case = TRUE)
    if (!length(h)) return(NULL)
    data.frame(file = basename(f), label = lab, text = trimws(win[h]),
               stringsAsFactors = FALSE)
  }))
}))
if (is.null(claims)) claims <- data.frame(file = character(), label = character(),
                                          text = character())
## A novelty claim is a violation only where the register says `derived-here`,
## since a `restated` result's prose may legitimately describe its SOURCE as first.
dh <- rr7$label[rr7$provenance == "derived-here"]
claims <- claims[claims$label %in% dh, , drop = FALSE]
write.csv(claims, file.path(paths$verification, "novelty-claim-check.csv"), row.names = FALSE)
cat(sprintf("V7 derived-here results: %d; prose novelty claims: %d\n",
            length(dh), nrow(claims)))
cat("V7 novelty guard is a lexical screen over derived-here result windows, not a prior-art search\n")
if (nrow(hits) || nrow(claims)) {
  if (nrow(hits)) print(hits, row.names = FALSE)
  if (nrow(claims)) print(claims, row.names = FALSE)
  quit(status = 1L)
}
