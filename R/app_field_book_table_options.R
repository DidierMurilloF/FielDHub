#' Plain table options shared by the app's field-book views
#' @noRd
field_book_table_options <- function(rows, height = 500, collapse = NULL) {
  validate_iteration_budget(rows, "table rows")
  validate_iteration_budget(height, "table height")
  if (!is.null(collapse) &&
      (!is.logical(collapse) || length(collapse) != 1L || is.na(collapse))) {
    fieldhub_abort("Table scroll collapsing must be TRUE, FALSE, or NULL.")
  }
  options <- list(pageLength = rows, autoWidth = FALSE, scrollX = TRUE)
  if (!is.null(collapse)) options$scrollCollapse <- collapse
  options$scrollY <- paste0(height, "px")
  options$columnDefs <- list(list(className = "dt-center", targets = "_all"))
  options
}
