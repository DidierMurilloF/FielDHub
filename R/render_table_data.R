#' Prepare display factors without moving or coercing other table columns
#' @noRd
field_book_table_data <- function(field_book, factor_columns) {
  validate_export_table(field_book)
  if (!is.character(factor_columns) || anyNA(factor_columns) ||
      anyDuplicated(factor_columns) > 0L || any(!factor_columns %in% names(field_book))) {
    fieldhub_abort("Table factors must name distinct existing field-book columns.")
  }
  for (name in factor_columns) field_book[[name]] <- as.factor(field_book[[name]])
  field_book
}
