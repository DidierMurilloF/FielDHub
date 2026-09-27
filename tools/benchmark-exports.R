# Run against the installed candidate release:
# Rscript tools/benchmark-exports.R results.csv [package-library]
# Measurements include layout assembly and CSV writing to a temporary file.
# The largest R allocation is not total allocations or peak resident memory.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop("Usage: Rscript tools/benchmark-exports.R results.csv [package-library]")
}
if (file.exists(arguments[1])) stop("The output file already exists; choose a new path.")
library(FielDHub, lib.loc = if (length(arguments) == 2L) arguments[2] else NULL)
namespace <- asNamespace("FielDHub")
if (!exists("field_book_export_grid", namespace, inherits = FALSE)) {
  stop("This benchmark requires the indexed layout exporter.")
}
export <- get("export_layout", namespace)

measure <- function(size) {
  book <- expand.grid(ROW = seq_len(size), COLUMN = seq_len(size))
  book$LOCATION <- "A"
  book$ENTRY <- seq_len(nrow(book))
  file <- tempfile("fieldhub-export-", fileext = ".csv")
  profile <- tempfile("fieldhub-export-profile-")
  profiling <- FALSE
  on.exit({
    if (profiling) Rprofmem(NULL)
    unlink(c(file, profile))
  }, add = TRUE)
  write_export <- function() utils::write.csv(export(book, 1)$file, file, row.names = FALSE)
  seconds <- replicate(5L, {
    gc()
    system.time(write_export())[["elapsed"]]
  })
  largest <- NA_real_
  if (isTRUE(capabilities("profmem"))) {
    Rprofmem(profile)
    profiling <- TRUE
    write_export()
    Rprofmem(NULL)
    profiling <- FALSE
    bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
    if (!all(is.na(bytes))) largest <- max(bytes, na.rm = TRUE)
  }
  data.frame(package_version = as.character(utils::packageVersion("FielDHub")),
             r_version = as.character(getRversion()), platform = R.version$platform,
             rows = size, columns = size, plots = nrow(book),
             median_seconds = median(seconds), csv_bytes = file.info(file)$size,
             largest_R_allocation_bytes = largest)
}

results <- do.call(rbind, lapply(c(10L, 50L, 100L, 250L, 500L), measure))
utils::write.csv(results, arguments[1], row.names = FALSE)
print(results, row.names = FALSE)
