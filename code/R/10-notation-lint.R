## 10-notation-lint.R — V3 notation coverage and collision audit
## Uses Pandoc's AST, so inline/display math and math in captions are treated alike.

source("code/R/00-paths.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("R package jsonlite is required")
pandoc <- Sys.which("pandoc"); if (!nzchar(pandoc)) stop("pandoc is required")

reg_file <- Sys.getenv("DPMIRT_NOTATION_REGISTER")
if (!nzchar(reg_file)) reg_file <- file.path(paths$manifest, "notation-register.csv")
reg <- read.csv(reg_file, stringsAsFactors = FALSE, na.strings = "")
need <- c("symbol", "meaning", "collides_with")
if (!all(need %in% names(reg))) stop("notation register schema mismatch")
if (any(!nzchar(reg$symbol)) || any(!nzchar(reg$meaning))) stop("blank notation symbol/meaning")

dup <- unique(reg$symbol[duplicated(reg$symbol)])
collision_issues <- data.frame(symbol = dup,
  status = rep("duplicate exact symbol", length(dup)), stringsAsFactors = FALSE)
## `collides_with` may be an explanatory phrase or an exact registered symbol.
## Exact symbolic targets are enforceable: they must exist and cannot point to self.
cw <- which(!is.na(reg$collides_with) & nzchar(reg$collides_with))
exact_cw <- cw[!grepl("[[:space:]]", reg$collides_with[cw])]
for (j in exact_cw) {
  target <- reg$collides_with[j]
  if (identical(target, reg$symbol[j]))
    collision_issues <- rbind(collision_issues,
      data.frame(symbol = reg$symbol[j], status = "collides_with points to self"))
  else if (!target %in% reg$symbol)
    collision_issues <- rbind(collision_issues,
      data.frame(symbol = reg$symbol[j],
                 status = paste("collides_with target is unregistered:", target)))
}

ex_file <- file.path(paths$manifest, "notation-exemptions.csv")
ex <- if (file.exists(ex_file)) read.csv(ex_file, stringsAsFactors = FALSE) else
  data.frame(symbol = character(), reason = character())
if (nrow(ex) && (!all(c("symbol", "reason") %in% names(ex)) || any(!nzchar(ex$reason))))
  stop("notation exemptions require symbol and reason")

records <- data.frame(file = character(), math = character(), stringsAsFactors = FALSE)
walk <- function(z, file) {
  if (!is.list(z)) return()
  if (!is.null(z$t) && identical(z$t, "Math") && length(z$c) >= 2L)
    records <<- rbind(records, data.frame(file = file, math = z$c[[2]], stringsAsFactors = FALSE))
  for (v in z) if (is.list(v)) walk(v, file)
}
sources <- c(list.files(paths$book, pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE),
             list.files(paths$figures, pattern = "[.]md$", full.names = TRUE))
for (f in sources) {
  tmp <- tempfile(fileext = ".json")
  status <- system2(pandoc, c("--from=markdown+tex_math_dollars", "--to=json", "--output", shQuote(tmp), shQuote(f)),
                    stdout = TRUE, stderr = TRUE)
  if (!file.exists(tmp)) stop("Pandoc failed on ", f, ": ", paste(status, collapse = "\n"))
  walk(jsonlite::fromJSON(tmp, simplifyVector = FALSE), sub(paste0(paths$root, "/"), "", f, fixed = TRUE))
  unlink(tmp)
}
csvs <- list.files(file.path(paths$tables, "supplement"), pattern = "[.]csv$", full.names = TRUE)
for (f in csvs) {
  z <- paste(readLines(f, warn = FALSE), collapse = "\n")
  m <- regmatches(z, gregexpr("\\$[^$]+\\$", z, perl = TRUE))[[1]]
  if (length(m)) records <- rbind(records, data.frame(file = sub(paste0(paths$root, "/"), "", f, fixed = TRUE),
    math = substring(m, 2L, nchar(m) - 1L), stringsAsFactors = FALSE))
}

braced_token <- "\\{(?:[^{}]|\\\\[A-Za-z]+\\{[^{}]*\\})+\\}"
suffix_token <- paste0("(?:[_^](?:", braced_token, "|[A-Za-z0-9]))*")
token_pat <- paste0(
  "\\\\(?:mathbf|boldsymbol|mathcal|mathbb|mathrm)\\{[A-Za-z]+\\}", suffix_token,
  "|\\\\(?:alpha|beta|gamma|delta|epsilon|varepsilon|zeta|eta|theta|vartheta|iota|kappa|lambda|mu|nu|xi|omicron|pi|varpi|rho|varrho|sigma|varsigma|tau|upsilon|phi|varphi|chi|psi|omega|Gamma|Delta|Theta|Lambda|Xi|Pi|Sigma|Upsilon|Phi|Psi|Omega)", suffix_token,
  "|\\b[A-Z](?![A-Za-z])", suffix_token,
  "|\\b[a-z](?![A-Za-z])_(?:", braced_token, "|[A-Za-z0-9])")
tokens <- function(z) {
  ## Expose juxtaposed indexed symbols such as P_iQ_i to the matcher.
  z <- gsub("([a-z0-9}])([A-Z])", "\\1 \\2", z, perl = TRUE)
  m <- regmatches(z, gregexpr(token_pat, z, perl = TRUE))[[1]]
  m <- sub("[,;:]$", "", m)
  unique(m[nzchar(m)])
}
allowed <- unique(unlist(lapply(reg$symbol, tokens)))
base <- function(z) sub("(?:_|\\^).*", "", z, perl = TRUE)
allowed_base <- unique(base(allowed))
family_base <- allowed_base
ex_allowed <- unique(ex$symbol)

## Defensive normalization for a token that contains one unmatched trailing brace.
## The structured suffix matcher normally keeps braces balanced; registry symbols are
## never rewritten, so this cannot silently shrink the declared allowed set.
unbalance <- function(m) {
  nopen  <- vapply(gregexpr("\\{", m), function(g) sum(g > 0L), integer(1))
  nclose <- vapply(gregexpr("\\}", m), function(g) sum(g > 0L), integer(1))
  bad <- grepl("\\}$", m) & nclose > nopen
  m[bad] <- sub("\\}$", "", m[bad])
  unique(m)
}
used <- do.call(rbind, lapply(seq_len(nrow(records)), function(j) {
  u <- unbalance(tokens(records$math[j])); if (!length(u)) return(NULL)
  data.frame(file = records$file[j], token = u, math = records$math[j], stringsAsFactors = FALSE)
}))
if (is.null(used)) used <- data.frame(file = character(), token = character(), math = character())
valid_family <- function(z) {
  bz <- base(z)
  if (!bz %in% family_base) return(FALSE)
  has_suffix <- grepl("(?:_|\\^)", z, perl = TRUE)
  ## Bare Greek/styled bases are generic members of a registered family; a bare
  ## Latin token such as Z is not licensed merely because Z_n is registered.
  if (!has_suffix) return(startsWith(z, "\\"))
  ## TD-3 is index-sensitive: the registered item discrimination lambda_i must
  ## not license a person-indexed posterior variance lambda_p. Powers of a
  ## registered lambda stem remain valid (for example lambda_i^2).
  if (identical(bz, "\\lambda")) {
    stem <- sub("\\^.*$", "", z, perl = TRUE)
    if (identical(stem, "\\lambda_p")) return(FALSE)
  }
  tail <- sub("^[^_^]+", "", z)
  tail <- gsub("\\\\(?:mathbf|boldsymbol|mathcal|mathbb|mathrm|text|top|hat|tilde|bar|prime|star)", "", tail, perl = TRUE)
  tail <- gsub("\\\\(?:alpha|beta|gamma|delta|epsilon|varepsilon|zeta|eta|theta|vartheta|iota|kappa|lambda|mu|nu|xi|omicron|pi|varpi|rho|varrho|sigma|varsigma|tau|upsilon|phi|varphi|chi|psi|omega)", "", tail, perl = TRUE)
  atoms <- regmatches(tail, gregexpr("[A-Za-z]+", tail, perl = TRUE))[[1]]
  if (!length(atoms)) return(TRUE)
  declared_words <- c("EAP", "WLE", "ML", "post", "printed", "iid", "CB", "GR", "PL")
  ## Index atoms are short labels (p, pi, CB, PL, ...) or declared annotations,
  ## not arbitrary prose hidden in a subscript.
  all(nchar(atoms) <= 2L | atoms %in% declared_words)
}
if (valid_family("\\lambda_p"))
  stop("V3 self-test failed: registered lambda_i licensed forbidden lambda_p")
if (!valid_family("\\lambda_i^2"))
  stop("V3 self-test failed: a power of registered lambda_i was rejected")
family_ok <- vapply(used$token, valid_family, logical(1))
used$status <- ifelse(used$token %in% allowed | family_ok,
                      "registered", ifelse(used$token %in% ex_allowed, "exempt", "unregistered"))
unknown <- unique(used[used$status == "unregistered", c("file", "token", "math", "status")])

dir.create(paths$verification, showWarnings = FALSE, recursive = TRUE)
write.csv(unknown, file.path(paths$verification, "notation-lint.csv"), row.names = FALSE)
write.csv(collision_issues, file.path(paths$verification, "notation-collisions.csv"), row.names = FALSE)
cat(sprintf("V3 AST math nodes: %d; distinct tokens: %d; unregistered: %d; duplicate definitions: %d\n",
            nrow(records), length(unique(used$token)), nrow(unknown), nrow(collision_issues)))
cat("V3 lexical domain: Greek/styled symbols, bare uppercase Latin, and indexed lowercase Latin; bare local lowercase calculus dummies are outside this lexical screen\n")
if (nrow(unknown)) print(unknown, row.names = FALSE)
if (nrow(collision_issues)) print(collision_issues, row.names = FALSE)
if (nrow(unknown) || nrow(collision_issues)) quit(status = 1L)
