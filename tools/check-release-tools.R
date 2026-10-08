root <- commandArgs(TRUE)[1L]
if (is.na(root) || !dir.exists(root)) stop("Pass the repository path.")
launcher_status <- system2(file.path(R.home("bin"), "Rscript"),
  c("--vanilla", shQuote(file.path(root, "tools", "check-launchers.R")), shQuote(root)))
stopifnot(launcher_status == 0L)
comparison_status <- system2(file.path(R.home("bin"), "Rscript"),
  c("--vanilla", shQuote(file.path(root, "tools", "check-benchmark-comparison.R")), shQuote(root)))
stopifnot(comparison_status == 0L)
description <- read.dcf(file.path(root, "DESCRIPTION"))
stopifnot(grepl("testthat (>= 3.2.0)", description[1L, "Suggests"], fixed = TRUE))
workflow <- yaml::read_yaml(file.path(root, ".github/workflows/R-CMD-check.yaml"))
matrix <- workflow$jobs[["R-CMD-check"]]$strategy$matrix$config
keys <- vapply(matrix, function(x) paste(x$os, x$r), character(1))
required <- c("macos-latest release", "ubuntu-latest release", "windows-latest release")
stopifnot(setequal(required, keys), length(keys) == 3L)
stopifnot(identical(workflow$jobs[["R-CMD-check"]]$name,
                    "${{ matrix.config.os }} (${{ matrix.config.r }})"))
extended <- yaml::read_yaml(file.path(root, ".github/workflows/R-CMD-check-extended.yaml"))
extended_job <- extended$jobs[["R-CMD-check"]]
versions <- vapply(extended_job$strategy$matrix$config, function(x) x$r, character(1))
stopifnot(identical(extended_job[["runs-on"]], "ubuntu-latest"),
          setequal(versions, c("devel", "oldrel-1", "4.1.0")), length(versions) == 3L,
          !is.null(extended$on$schedule), "workflow_dispatch" %in% names(extended$on),
          identical(extended$on$push$tags, "v*"),
          identical(extended_job$strategy[["fail-fast"]], FALSE))
for (file in list.files(file.path(root, ".github/workflows"), full.names = TRUE)) {
  content <- readLines(file)
  if (any(grepl("actions/checkout@", content, fixed = TRUE))) {
    stopifnot(!any(grepl("actions/checkout@v[46]", content)))
  }
}
expressions <- parse(file.path(root, "tools/benchmark-exports.R"))
definition <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
                      identical(x[[2L]], as.name("measure")), as.list(expressions))[[1L]]
env <- new.env()
env$capabilities <- function(what) FALSE
env$Rprofmem <- function(...) stop("memory profiling is unavailable")
env$export <- function(book, location) list(file = book)
eval(definition, env)
result <- env$measure(2L)
stopifnot(is.na(result$largest_R_allocation_bytes), identical(result$plots, 4L))
cat("Release matrix, check names, action versions, and unprofiled benchmark: OK\n")
