#' Supported recorded-design file formats
#'
#' @description Lists the version and read/write capabilities of the built-in
#'   design format registry. Formats are selected explicitly, not guessed from
#'   filenames. CSV tables and app workflow ZIPs are separate exports; neither
#'   is silently interpreted as a complete recorded design by these functions.
#' @return A data frame with format, extension, media_type, format_version,
#'   readable and writable columns.
#' @examples
#' design_formats()
#' @export
design_formats <- function() {
  registry <- fieldhub_format_registry()
  data.frame(format = names(registry),
    extension = vapply(registry, `[[`, character(1), "extension"),
    media_type = vapply(registry, `[[`, character(1), "media_type"),
    format_version = vapply(registry, `[[`, integer(1), "version"),
    readable = vapply(registry, function(x) is.function(x$read), logical(1)),
    writable = vapply(registry, function(x) is.function(x$write), logical(1)), row.names = NULL)
}
