## 04-reconcile-appendices.R — paired-display appendix audit

source("code/R/00-paths.R")

SRC <- Sys.getenv("DPMIRT_APPENDIX_SOURCE")
if (!nzchar(SRC)) SRC <- file.path(inputs$research_notes,
  "32_appendices_a-to-f_combined-eb78c56e-72d3-4aff-8e82-1a77064ea35c.md")
if (!file.exists(SRC)) stop("appendix source does not exist: ", SRC)
x <- readLines(SRC, warn = FALSE)

heads <- c(A = "^## Appendix A\\.", B = "^## Appendix B\\.",
           C = "^## Appendix C\\.", D = "^## Appendix D\\.",
           E = "^## Appendix E\\.", F = "^## Appendix F\\.")
starts <- vapply(heads, function(p) {
  z <- grep(p, x); if (length(z)) z[1] else NA_integer_
}, integer(1))
if (is.na(starts[["A"]])) starts[["A"]] <- 1L
if (anyNA(starts)) stop("could not find every Appendix A--F heading")
if (is.unsorted(starts, strictly = TRUE)) stop("appendix headings are not ordered")
ends <- c(starts[-1] - 1L, length(x)); names(ends) <- names(starts)

dir.create(paths$drafts, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)
for (k in names(starts)) writeLines(
  x[starts[[k]]:ends[[k]]],
  file.path(paths$drafts, sprintf("appendix-%s-raw.md", tolower(k))))

probes <- c(
  "P (persons)" = "\\bP\\b(?![a-zA-Z])",
  "N (persons)" = "N \\\\text\\{ persons\\}|\\bN\\b examinees|p=1, ?\\\\ldots, ?N",
  "U_{pi}" = "U_\\{?pi\\}?", "y_{ip}" = "y_\\{?ip\\}?",
  "u_p vector" = "\\\\mathbf\\{u\\}_p", "J information" = "\\\\mathcal\\{J\\}",
  "I information" = "\\\\mathcal\\{I\\}", "lambda (discrim.)" = "\\\\lambda_\\{?i\\}?",
  "lambda (post. var)" = "\\\\lambda_\\{?p\\}?", "eta_p" = "\\\\eta_\\{?p\\}?",
  "MSEM" = "\\\\mathrm\\{MSEM\\}|MSEM", "delta hyperparam" = "\\\\delta",
  "sigma^2_theta" = "\\\\sigma_?\\{?\\\\theta\\}?\\^?2|\\\\sigma\\^2_\\\\theta",
  "alpha (concentr.)" = "\\\\alpha", "precision (word)" = "precision parameter",
  "concentration (word)" = "concentration parameter")
nota <- do.call(rbind, lapply(names(starts), function(k) {
  seg <- x[starts[[k]]:ends[[k]]]
  data.frame(appendix = k, symbol = names(probes),
    hits = vapply(probes, function(p) sum(grepl(p, seg, perl = TRUE)), integer(1)),
    stringsAsFactors = FALSE)
}))
nota <- nota[nota$hits > 0, ]
write.csv(nota, file.path(paths$verification, "appendix-notation-audit.csv"), row.names = FALSE)

## A citation covers a display only in its bounded eight-line evidence window.
## Pairing the delimiters first prevents the old error of counting both ends.
cite_pat <- paste0(
  "\\([^()]{0,60}[12][0-9]{3}[a-z]?\\)",
  "|[[:alpha:]]{3,}[^()]{0,30}\\([12][0-9]{3}",
  "|cf\\.|see |Eq\\. \\(")
paragraph_bounds <- function(seg, a, b) {
  before <- if (a <= 1L) integer() else {
    blank <- which(!nzchar(trimws(seg[seq_len(a - 1L)])))
    lo <- if (length(blank)) max(blank) + 1L else 1L
    seq.int(lo, a - 1L)
  }
  after <- if (b >= length(seg)) integer() else {
    idx <- seq.int(b + 1L, length(seg)); blank <- idx[!nzchar(trimws(seg[idx]))]
    hi <- if (length(blank)) min(blank) - 1L else length(seg)
    seq.int(b + 1L, hi)
  }
  c(before, after)
}

claims <- do.call(rbind, lapply(names(starts), function(k) {
  seg <- x[starts[[k]]:ends[[k]]]
  marks <- grep("^\\s*\\$\\$\\s*(\\{#[^}]+\\})?\\s*$", seg, perl = TRUE)
  if (length(marks) %% 2L) stop("odd $$ delimiter count in Appendix ", k,
                               " (", length(marks), ")")
  if (!length(marks)) return(NULL)
  pairs <- matrix(marks, ncol = 2L, byrow = TRUE)
  do.call(rbind, lapply(seq_len(nrow(pairs)), function(j) {
    a <- pairs[j, 1]; b <- pairs[j, 2]
    block <- seg[a:b]
    tags <- regmatches(paste(block, collapse = " "),
                       gregexpr("\\\\tag\\{[^}]+\\}", paste(block, collapse = " "), perl = TRUE))[[1]]
    tag <- if (length(tags)) sub("\\}$", "", sub("^\\\\tag\\{", "", tags[1])) else ""
    win <- seq.int(max(1L, a - 8L), min(length(seg), b + 8L))
    cited <- any(grepl(cite_pat, seg[win], perl = TRUE))
    data.frame(appendix = k, block = j, line_in_appendix = a,
      end_line_in_appendix = b, tag = tag,
      evidence_window = paste(range(win), collapse = "-"),
      covered = cited, stringsAsFactors = FALSE)
  }))
}))
write.csv(claims, file.path(paths$verification, "appendix-claim-audit.csv"), row.names = FALSE)

cat("split:\n")
for (k in names(starts)) cat(sprintf("  Appendix %s lines %d-%d (%d lines)\n",
  k, starts[[k]], ends[[k]], ends[[k]] - starts[[k]] + 1L))
cat(sprintf("paired display blocks: %d; numbered: %d; adjacent-citation covered: %d; uncited numbered: %d\n",
  nrow(claims), sum(nzchar(claims$tag)), sum(claims$covered),
  sum(nzchar(claims$tag) & !claims$covered)))
