#' Validate an exported table without changing its values or column types
#' @noRd
validate_export_table <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L || ncol(data) == 0L ||
      anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data)) > 0L ||
      any(!vapply(data, function(column) is.atomic(column) && is.null(dim(column)), logical(1)))) {
    fieldhub_abort("An export needs a non-empty data frame with unique column names and atomic columns.")
  }
  invisible(data)
}
