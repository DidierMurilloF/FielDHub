# Run the fixed benchmark suite against one installed revision, never source-load.
arguments <- commandArgs(TRUE)
if (length(arguments) != 3L || !grepl("^[0-9a-f]{40}$", arguments[3L])) {
  stop("Usage: Rscript tools/run-benchmarks.R output-dir installed-library full-commit-sha")
}
output <- arguments[1L]
library_path <- normalizePath(arguments[2L], mustWork = TRUE)
if (file.exists(output) || !dir.create(output, recursive = TRUE)) stop("Choose a new output directory.")
.libPaths(c(library_path, .libPaths()))
library(FielDHub, lib.loc = library_path)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
source(file.path(dirname(script), "benchmark-compare.R"))
benchmark_scripts <- file.path(dirname(script), paste0("benchmark-", names(benchmark_contracts()), ".R"))
benchmark_hashes <- tools::md5sum(benchmark_scripts)
installed <- installed.packages()
dependencies <- tools::package_dependencies("FielDHub", installed, recursive = TRUE)[[1L]]
dependencies <- sort(unique(dependencies))
versions <- vapply(dependencies, function(package) as.character(utils::packageVersion(package)), character(1))
manifest <- c(Revision = arguments[3L], PackageVersion = as.character(utils::packageVersion("FielDHub")),
  RVersion = as.character(getRversion()), RBuild = R.version.string, Platform = R.version$platform,
  Host = paste(Sys.info()[c("sysname", "release", "nodename", "machine")], collapse = "; "),
  BLAS = unname(extSoftVersion()["BLAS"]),
  BenchmarkCode = paste(paste(basename(benchmark_scripts), unname(benchmark_hashes), sep = "="), collapse = "; "),
  Dependencies = paste(paste(dependencies, versions, sep = "="), collapse = "; "),
  StartedUTC = format(Sys.time(), tz = "UTC", usetz = TRUE))
write.dcf(as.data.frame(as.list(manifest)), file.path(output, "manifest.dcf"))
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
for (family in names(benchmark_contracts())) {
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla",
    shQuote(file.path(dirname(script), paste0("benchmark-", family, ".R"))),
    shQuote(file.path(output, paste0(family, ".csv"))), shQuote(library_path)))
  if (status != 0L) stop("Benchmark failed: ", family)
}
cat("Completed benchmark suite for ", arguments[3L], "\n", sep = "")
