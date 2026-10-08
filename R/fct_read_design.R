#' Read a validated recorded design
#'
#' @param file Local filename. Only read serialized files from trusted sources.
#' @param format Explicit format key from \code{design_formats()}; currently
#'   \code{"rds"} is the lossless import format.
#' @details Imports validate the result schema, recorded parameters and known
#'   engine. They do not randomize, reconstruct a design, or execute an R script.
#'   Unknown schema versions are rejected, not silently coerced. Pre-metadata
#'   legacy objects remain usable through \code{readRDS()} but do not contain
#'   the recorded-input contract required by this importer.
#' @return The complete saved result, unchanged. File/format input mistakes
#'   raise \code{fieldhub_input_error}; corrupt or incompatible contents raise
#'   \code{fieldhub_import_error}, with the original condition in \code{parent}.
#' @examples
#' file <- tempfile(fileext = ".rds")
#' x <- RCBD(t = 5, reps = 3, seed = 27)
#' write_design(x, file)
#' identical(read_design(file), x)
#' unlink(file)
#' @export
read_design <- function(file, format = "rds") {
  handler <- design_format_handler(format, fieldhub_importer_registry())
  validate_design_file(file, reading = TRUE)
  problem <- function(e) fieldhub_abort("Could not read the design: ", conditionMessage(e),
    class = "fieldhub_import_error", data = list(parent = e))
  tryCatch({
    x <- handler$read(file)
    reproduction_engine(x)
    x
  }, error = problem, warning = problem)
}
