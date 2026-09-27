root <- commandArgs(TRUE)[1L]
if (is.na(root) || !dir.exists(root)) stop("Pass the repository path.")
description <- read.dcf(file.path(root, "DESCRIPTION"))
stopifnot(grepl("testthat (>= 3.2.0)", description[1L, "Suggests"], fixed = TRUE))
workflow <- yaml::read_yaml(file.path(root, ".github/workflows/R-CMD-check.yaml"))
matrix <- workflow$jobs[["R-CMD-check"]]$strategy$matrix$config
keys <- vapply(matrix, function(x) paste(x$os, x$r), character(1))
required <- c("macos-latest release", "ubuntu-latest release", "windows-latest release",
              "ubuntu-latest devel", "ubuntu-latest oldrel-1", "ubuntu-latest 4.1.0")
stopifnot(all(required %in% keys))
stopifnot(identical(workflow$jobs[["R-CMD-check"]]$name,
                    "${{ matrix.config.os }} (${{ matrix.config.r }})"))
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
