#' Reviewed importer and exporter declarations
#'
#' Handlers come from package code, never from serialized metadata. Add formats
#' here with a format version and explicit capabilities; the public dispatchers
#' contain no format-specific branches. CSV field books remain tabular workflow
#' exports, not a lossless substitute for a recorded design.
#' @noRd
fieldhub_format_registry <- function() {
  list(
    rds = list(extension = "rds", media_type = "application/octet-stream", version = 1L,
      read = readRDS, write = function(x, file) saveRDS(x, file, version = 2)),
    r = list(extension = "R", media_type = "text/plain", version = 1L, read = NULL,
      write = function(x, file) {
        code <- design_call_code(x)
        if (!length(code)) {
          fieldhub_abort("Standalone source unavailable: ", attr(code, "reason"),
            class = "fieldhub_export_error")
        }
        code[1L] <- paste0("design <- ", code[1L])
        writeLines(c(paste0("# FielDHub version: ", encodeString(x$metadata$package_version)),
          "# Replays recorded inputs; numerical results can depend on software versions.", code), file,
          useBytes = TRUE)
      })
  )
}

#' @noRd
fieldhub_importer_registry <- function() {
  Filter(function(entry) is.function(entry$read), fieldhub_format_registry())
}

#' @noRd
fieldhub_exporter_registry <- function() {
  Filter(function(entry) is.function(entry$write), fieldhub_format_registry())
}

#' Resolve an explicit format from a package-owned registry
#' @noRd
design_format_handler <- function(format, registry) {
  if (!is.character(format) || length(format) != 1L || is.na(format) || !format %in% names(registry)) {
    fieldhub_abort("The requested design format is not supported. Choose: ", paste(names(registry), collapse = ", "),
      data = list(options = names(registry)))
  }
  registry[[format]]
}

#' Validate a local design filename without opening or overwriting it
#' @noRd
validate_design_file <- function(file, reading = FALSE) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(trimws(file)) || dir.exists(file)) {
    fieldhub_abort("`file` must be one local filename, not a directory.")
  }
  if (reading && (!file.exists(file) || file.access(file, 4) != 0L)) {
    fieldhub_abort("The design file does not exist or is not readable: ", file)
  }
  if (!reading && !dir.exists(dirname(file))) fieldhub_abort("The output directory does not exist: ", dirname(file))
  invisible(file)
}
