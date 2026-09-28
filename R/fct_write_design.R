#' Write a recorded design or standalone replay script
#'
#' @param x A validated design, allocation or optimization result with recorded
#'   parameters. App-selected layouts and simulations are separate artifacts.
#' @param file Local output filename; its parent directory must already exist.
#' @param format Explicit exporter key from \code{design_formats()}: \code{"rds"}
#'   saves the exact result and metadata; \code{"r"} saves a standalone script
#'   assigning the reconstructed result to \code{design}.
#' @param overwrite One logical value. Existing files are protected by default.
#' @details Serialization completes in a temporary file before the destination
#'   is copied. This protects an existing file from serialization failures, but
#'   is not an atomic filesystem transaction. Disk/copy failures still require
#'   normal backups. The temporary file is removed on exit.
#'
#'   R scripts embed only portable data and never evaluate recorded arguments
#'   during export. Large uploads or nonportable values require the RDS format.
#'   Reconstruction can depend on matching software versions and platform;
#'   the RDS format preserves the actual saved result independently of replay.
#' @return \code{file}, invisibly. Unsupported formats and overwrite mistakes
#'   raise \code{fieldhub_input_error}; write failures raise
#'   \code{fieldhub_export_error}.
#' @examples
#' file <- tempfile(fileext = ".rds")
#' write_design(CRD(t = 4, reps = 2, seed = 27), file)
#' read_design(file)$metadata
#' unlink(file)
#' @export
write_design <- function(x, file, format = "rds", overwrite = FALSE) {
  handler <- design_format_handler(format, fieldhub_exporter_registry())
  validate_flag(overwrite, "overwrite")
  validate_design_file(file)
  if (file.exists(file) && !overwrite) fieldhub_abort("The output file already exists: ", file)
  reproduction_engine(x)
  temporary <- tempfile(".fieldhub-export-", tmpdir = dirname(file))
  on.exit(unlink(temporary), add = TRUE)
  problem <- function(e) fieldhub_abort("Could not save the design: ", conditionMessage(e),
    class = "fieldhub_export_error", data = list(parent = e))
  tryCatch({
    handler$write(x, temporary)
    if (!file.copy(temporary, file, overwrite = overwrite)) stop("Could not copy the serialized output to its destination.")
  }, error = problem, warning = problem)
  invisible(file)
}
