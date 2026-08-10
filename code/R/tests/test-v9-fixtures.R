## Dedicated positive/negative fixtures for V9's specification-content parser.

source("code/R/00-paths.R")

td <- tempfile("dpmirt-v9-")
dir.create(td)
on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
status <- system2(Sys.which("rsync"), c("-a", "--exclude=.git",
  shQuote(paste0(paths$root, "/")), shQuote(paste0(td, "/"))))
if (!identical(status, 0L)) stop("could not create isolated V9 fixture copy")

rscript <- file.path(R.home("bin"), "Rscript")
env <- c(paste0("DPMIRT_THEORY_ROOT=", shQuote(td)),
         paste0("DPMIRT_PROJECT_ROOT=", shQuote(PROJECT_ROOT)))
run_v9 <- function() {
  z <- suppressWarnings(system2(rscript, "code/R/18-spec-coverage.R", env = env,
                                stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(z, "status"))) 0L else attr(z, "status"),
       output = paste(z, collapse = "\n"))
}
results <- data.frame(fixture = character(), expected = character(),
                      observed = character(), passed = logical(),
                      stringsAsFactors = FALSE)
record <- function(name, expected, observed, passed) {
  results <<- rbind(results, data.frame(
    fixture = name, expected = expected, observed = substr(observed, 1L, 500L),
    passed = passed, stringsAsFactors = FALSE))
  if (!passed) stop("V9 fixture failed: ", name, "\n", observed)
}

z <- run_v9()
cov <- read.csv(file.path(td, "verification", "spec-contents-coverage.csv"),
                stringsAsFactors = FALSE)
addendum_items <- sum(cov$items[cov$chapter %in% 24:29])
record("V9-positive-addendum-contents", "exit 0 and 33 addendum items",
       paste("exit", z$status, z$output),
       z$status == 0L && identical(addendum_items, 33L))

spec_file <- file.path(td, "blueprint", "05-chapter-specifications.qmd")
x <- readLines(spec_file, warn = FALSE)
h24 <- grep("^## 24 — ", x)
if (length(h24) != 1L) stop("fixture cannot locate Chapter 24 specification")
rel <- seq.int(h24 + 1L, length(x))
start <- rel[grepl("^- \\*\\*Contents", x[rel])][1]
if (is.na(start)) stop("fixture cannot locate Chapter 24 Contents")
after <- seq.int(start + 1L, length(x))
stop_line <- after[grepl("^- \\*\\*[A-Za-z]", x[after])][1]
if (is.na(stop_line)) stop("fixture cannot isolate Chapter 24 Contents")
x <- c(x[seq_len(start - 1L)], "- **Contents.**", x[stop_line:length(x)])
writeLines(x, spec_file)
z <- run_v9()
record("V9-empty-addendum-contents", "nonzero and contents-empty",
       paste("exit", z$status, z$output),
       z$status != 0L && grepl("contents-empty", z$output, fixed = TRUE))

dir.create(paths$verification, recursive = TRUE, showWarnings = FALSE)
write.csv(results, file.path(paths$verification, "spec-coverage-fixtures.csv"),
          row.names = FALSE)
cat(sprintf("V9 fixtures passed: %d/%d\n", sum(results$passed), nrow(results)))
