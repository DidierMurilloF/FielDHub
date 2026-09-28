arguments <- commandArgs(TRUE)
if (length(arguments) != 1L) stop("Pass the repository path.")
source(file.path(arguments[1L], "tools", "benchmark-compare.R"))
fails <- function(expression) stopifnot(inherits(tryCatch(force(expression), error = identity), "error"))
baseline <- data.frame(package_version = "1", r_version = "4.5.3", platform = "fixture",
  rows = c(10L, 20L), columns = c(10L, 20L), plots = c(100L, 400L),
  csv_bytes = c(200L, 800L), median_seconds = c(0, 1), largest_R_allocation_bytes = c(100, 400))
candidate <- baseline[2:1, ]
stopifnot(all(compare_benchmark_tables(baseline, candidate, "exports")$status == "pass"))
candidate$median_seconds <- c(1.5, .005)
result <- compare_benchmark_tables(baseline, candidate, "exports")
stopifnot(sum(result$status == "review") == 1L)
candidate$median_seconds[2L] <- .02
stopifnot(sum(compare_benchmark_tables(baseline, candidate, "exports")$status == "review") == 2L)
candidate$largest_R_allocation_bytes[1L] <- NA_real_
stopifnot(sum(compare_benchmark_tables(baseline, candidate, "exports")$status == "review") == 3L)
fails(compare_benchmark_tables(baseline, candidate[1L, ], "exports"))
fails(compare_benchmark_tables(baseline, candidate[c(1L, 1L), ], "exports"))
candidate$csv_bytes[1L] <- 1L
fails(compare_benchmark_tables(baseline, candidate, "exports"))
candidate <- baseline
candidate$platform <- "different"
fails(compare_benchmark_tables(baseline, candidate, "exports"))
candidate <- baseline
candidate$median_seconds[1L] <- Inf
fails(compare_benchmark_tables(baseline, candidate, "exports"))
candidate$median_seconds[1L] <- NA_real_
fails(compare_benchmark_tables(baseline, candidate, "exports"))
optimizer <- transform(baseline, replicated_entries = c(20L, 80L), input_seed = 57L,
  optimizer_seed = 27L, distance_method = "euclidean", iterations_per_threshold = 3L,
  retained_min_distance = 3, stop_reason = "fixture", iterations = 3L, thresholds_attempted = 1L)
candidate <- optimizer
candidate$retained_min_distance[1L] <- 2
candidate$stop_reason[2L] <- "changed"
result <- compare_benchmark_tables(optimizer, candidate, "optimizers")
stopifnot(sum(result$status == "review") == 2L)
result$baseline_revision <- strrep("a", 40L)
result$candidate_revision <- strrep("b", 40L)
exceptions <- result[result$status == "review", ]
exceptions$reason <- "Reviewed fixture explanation."
stopifnot(!any(review_benchmark_exceptions(result, exceptions)$status == "review"))
fails(review_benchmark_exceptions(result, exceptions[c(1L, 1L), ]))
exceptions$candidate_revision <- strrep("c", 40L)
fails(review_benchmark_exceptions(result, exceptions))
exceptions <- result[result$status == "review", ]
exceptions$reason <- " "
fails(review_benchmark_exceptions(result, exceptions))
scratch <- tempfile("fieldhub-comparison-fixture-")
dir.create(scratch)
directories <- file.path(scratch, c("baseline", "candidate"))
for (directory in directories) dir.create(directory)
manifest <- data.frame(Revision = strrep("a", 40L), PackageVersion = "1", RVersion = "4.5.3",
  RBuild = "fixture", Platform = "fixture", Host = "fixture", BLAS = "fixture", Dependencies = "fixture", BenchmarkCode = "fixture")
for (directory in directories) {
  write.dcf(manifest, file.path(directory, "manifest.dcf"))
  for (family in names(benchmark_contracts())) {
    table <- switch(family,
      exports = baseline, optimizers = optimizer,
      dimensions = transform(baseline, factor_pairs = 2L, repetitions = 20L),
      spatial = transform(baseline, correlation_x = .4, correlation_y = .5, nugget = .1, repetitions = 20L))
    utils::write.csv(table, file.path(directory, paste0(family, ".csv")), row.names = FALSE)
  }
}
stopifnot(all(compare_benchmark_directories(directories[1L], directories[2L])$status == "pass"))
manifest$Host <- "different"
write.dcf(manifest, file.path(directories[2L], "manifest.dcf"))
fails(compare_benchmark_directories(directories[1L], directories[2L]))
manifest$Host <- "fixture"
manifest$BenchmarkCode <- "different"
write.dcf(manifest, file.path(directories[2L], "manifest.dcf"))
fails(compare_benchmark_directories(directories[1L], directories[2L]))
manifest$BenchmarkCode <- "fixture"
manifest$PackageVersion <- "different"
write.dcf(manifest, file.path(directories[2L], "manifest.dcf"))
fails(compare_benchmark_directories(directories[1L], directories[2L]))
unlink(scratch, recursive = TRUE)
cat("Benchmark comparison, fail-closed contracts, and scoped explanations: OK\n")
