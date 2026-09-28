#' Prepare display factors without moving or coercing other table columns
#' @noRd
field_book_table_data <- function(field_book, factor_columns) {
  validate_archive_table(field_book)
  if (!is.character(factor_columns) || anyNA(factor_columns) ||
      anyDuplicated(factor_columns) > 0L || any(!factor_columns %in% names(field_book))) {
    fieldhub_abort("Table factors must name distinct existing field-book columns.")
  }
  for (name in factor_columns) field_book[[name]] <- as.factor(field_book[[name]])
  field_book
}

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
