# Compare exact, installed revisions measured on the same host and dependencies.
arguments <- commandArgs(TRUE)
if (!length(arguments) %in% c(3L, 4L)) {
  stop("Usage: Rscript tools/compare-benchmarks.R baseline-dir candidate-dir report.csv [reviewed-exceptions.csv]")
}
if (file.exists(arguments[3L])) stop("The report already exists; choose a new path.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
source(file.path(dirname(script), "benchmark-compare.R"))
result <- compare_benchmark_directories(arguments[1L], arguments[2L])
if (length(arguments) == 4L) {
  result <- review_benchmark_exceptions(result,
    utils::read.csv(arguments[4L], colClasses = "character", check.names = FALSE))
}
utils::write.csv(result, arguments[3L], row.names = FALSE)
print(table(result$status))
if (any(result$status == "review")) stop("Unexplained benchmark changes; review the report before releasing.")
