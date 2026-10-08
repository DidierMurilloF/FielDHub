# Run against the installed candidate release:
# Rscript tools/benchmark-dimensions.R results.csv [package-library]
# The largest R allocation is not total allocated memory or peak resident RAM.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop("Usage: Rscript tools/benchmark-dimensions.R results.csv [package-library]")
}
if (file.exists(arguments[1])) stop("The output file already exists; choose a new path.")
if (length(arguments) == 2L) .libPaths(c(normalizePath(arguments[2], mustWork = TRUE), .libPaths()))
library(FielDHub)
namespace <- asNamespace("FielDHub")
if (!exists("ordered_factor_pairs", namespace, inherits = FALSE)) {
  stop("This benchmark requires the bounded factor-pair search.")
}
dimensions <- get("factor_subsets", namespace)

largest_allocation <- function(size) {
  if (!isTRUE(capabilities("profmem"))) return(NA_real_)
  profile <- tempfile("fieldhub-dimensions-profile-")
  on.exit({Rprofmem(NULL); unlink(profile)}, add = TRUE)
  Rprofmem(profile)
  invisible(dimensions(size, all_factors = TRUE))
  Rprofmem(NULL)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
  if (all(is.na(bytes))) 0 else max(bytes, na.rm = TRUE)
}

measure <- function(size) {
  result <- dimensions(size, all_factors = TRUE)
  repetitions <- 20L
  elapsed <- replicate(5L, {
    gc()
    system.time(for (i in seq_len(repetitions)) {
      invisible(dimensions(size, all_factors = TRUE))
    })[["elapsed"]] / repetitions
  })
  data.frame(
    package_version = as.character(utils::packageVersion("FielDHub")),
    r_version = as.character(getRversion()), platform = R.version$platform,
    plots = size, factor_pairs = nrow(result$comb_factors),
    repetitions = repetitions, median_seconds = median(elapsed),
    largest_R_allocation_bytes = largest_allocation(size)
  )
}

results <- do.call(rbind, lapply(c(64, 1024, 4096, 2^20, 2^30, 73513440), measure))
utils::write.csv(results, arguments[1], row.names = FALSE)
print(results, row.names = FALSE)
