## 05-uncited-results.R — T1.5 support
## Lists every numbered result that carries no nearby citation, with the
## sentence that introduces it, so a candidate source can be assigned by hand.

source("code/R/00-paths.R")
args <- commandArgs(trailingOnly = TRUE)
want <- if (length(args)) toupper(args) else c("A", "B", "C", "D", "E", "F")

c1 <- read.csv(file.path(paths$verification, "appendix-claim-audit.csv"),
               stringsAsFactors = FALSE)
c1 <- subset(c1, nzchar(tag))

for (ap in want) {
  s <- subset(c1, appendix == ap)
  if (!nrow(s)) next
  agg <- aggregate(covered ~ tag, s, any)
  un <- agg$tag[!agg$covered]
  src <- readLines(file.path(paths$drafts, sprintf("appendix-%s-raw.md", tolower(ap))),
                   warn = FALSE)
  cat(sprintf("\n########## Appendix %s — %d uncited of %d numbered results\n",
              ap, length(un), nrow(agg)))
  for (t in un) {
    ln <- grep(paste0("\\\\tag\\{", gsub("([.\\\\])", "\\\\\\1", t), "\\}"), src)[1]
    if (is.na(ln)) { cat(sprintf("  %-10s (tag not relocated)\n", t)); next }
    ctx <- rev(src[max(1, ln - 16):ln])
    ctx <- ctx[nzchar(trimws(ctx)) & !grepl("^\\$\\$|^\\\\|^\\s*$", ctx)]
    lead <- if (length(ctx)) substr(gsub("\\s+", " ", ctx[1]), 1, 104) else "(no lead-in)"
    cat(sprintf("  %-10s %s\n", t, lead))
  }
}
