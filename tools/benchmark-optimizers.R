# Run against the installed candidate release:
# Rscript tools/benchmark-optimizers.R results.csv [package-library]
# Measurements use fixed inputs and RNG settings. Largest R allocation is not
# total allocation or peak resident memory; run on an otherwise idle machine.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop("Usage: Rscript tools/benchmark-optimizers.R results.csv [package-library]")
}
if (file.exists(arguments[1])) stop("The output file already exists; choose a new path.")
if (length(arguments) == 2L) .libPaths(c(normalizePath(arguments[2], mustWork = TRUE), .libPaths()))
library(FielDHub)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

measure <- function(size, method) {
  plots <- size * size
  repeated <- plots %/% 5L
  entries <- c(rep(seq_len(repeated), each = 2L), seq.int(repeated + 1L, plots - repeated))
  set.seed(57)
  input <- matrix(sample(entries), nrow = size)
  run <- function() {
    swap_pairs(input, starting_dist = 3, stop_iter = 3,
               dist_method = method, candidate_sample_size = 4, seed = 27)
  }
  result <- run()
  if (is.null(result$diagnostics)) stop("This benchmark requires optimizer diagnostics.")
  seconds <- replicate(3L, {
    gc()
    system.time(run())[["elapsed"]]
  })
  largest <- NA_real_
  if (isTRUE(capabilities("profmem"))) {
    profile <- tempfile("fieldhub-optimizer-profile-")
    on.exit({Rprofmem(NULL); unlink(profile)}, add = TRUE)
    Rprofmem(profile)
    invisible(run())
    Rprofmem(NULL)
    bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
    largest <- if (all(is.na(bytes))) 0 else max(bytes, na.rm = TRUE)
  }
  data.frame(
    package_version = as.character(utils::packageVersion("FielDHub")),
    r_version = as.character(getRversion()), platform = R.version$platform,
    rows = size, columns = size, plots = plots, replicated_entries = repeated,
    input_seed = 57L, optimizer_seed = 27L, distance_method = method,
    median_seconds = median(seconds), largest_R_allocation_bytes = largest,
    retained_min_distance = result$min_distance,
    stop_reason = result$diagnostics$stop_reason,
    iterations = result$diagnostics$iterations,
    thresholds_attempted = result$diagnostics$thresholds_attempted,
    iterations_per_threshold = result$diagnostics$max_iterations_per_threshold
  )
}

cases <- expand.grid(size = c(10L, 20L, 30L), method = c("euclidean", "manhattan"),
                     stringsAsFactors = FALSE)
results <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  measure(cases$size[i], cases$method[i])
}))
utils::write.csv(results, arguments[1], row.names = FALSE)
print(results, row.names = FALSE)
