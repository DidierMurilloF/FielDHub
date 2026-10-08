# Run against the installed candidate release:
# Rscript tools/benchmark-spatial.R results.csv [package-library]
# The largest R allocation is not total allocated memory or peak resident RAM.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop("Usage: Rscript tools/benchmark-spatial.R results.csv [package-library]")
}
if (file.exists(arguments[1])) stop("The output file already exists; choose a new path.")
if (length(arguments) == 2L) .libPaths(c(normalizePath(arguments[2], mustWork = TRUE), .libPaths()))
library(FielDHub)
namespace <- asNamespace("FielDHub")
if (!exists("separable_ar1_patch", namespace, inherits = FALSE)) {
  stop("This benchmark requires the separable spatial simulator.")
}
spatial <- get("ZST", namespace)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

largest_allocation <- function(size) {
  if (!isTRUE(capabilities("profmem"))) return(NA_real_)
  profile <- tempfile("fieldhub-spatial-profile-")
  on.exit({Rprofmem(NULL); unlink(profile)}, add = TRUE)
  Rprofmem(profile)
  invisible(spatial(size, size, 0.4, 0.5, 0.1))
  Rprofmem(NULL)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
  if (all(is.na(bytes))) 0 else max(bytes, na.rm = TRUE)
}

measure <- function(size) {
  repetitions <- if (size <= 100L) 20L else 3L
  invisible(spatial(2, 3, 0.4, 0.5, 0.1))
  elapsed <- replicate(5L, {
    gc()
    set.seed(27)
    system.time(for (i in seq_len(repetitions)) {
      invisible(spatial(size, size, 0.4, 0.5, 0.1))
    })[["elapsed"]] / repetitions
  })
  data.frame(
    package_version = as.character(utils::packageVersion("FielDHub")),
    r_version = as.character(getRversion()), platform = R.version$platform,
    rows = size, columns = size, plots = size * size,
    correlation_x = 0.4, correlation_y = 0.5, nugget = 0.1,
    repetitions = repetitions, median_seconds = median(elapsed),
    largest_R_allocation_bytes = largest_allocation(size)
  )
}

results <- do.call(rbind, lapply(c(40L, 100L, 250L, 500L), measure))
utils::write.csv(results, arguments[1], row.names = FALSE)
print(results, row.names = FALSE)
