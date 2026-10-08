#' Shared renderer for a field book with explicitly selected display factors
#' @noRd
app_field_book_table <- function(field_book, factor_columns, height = 500, collapse = NULL) {
  data <- field_book_table_data(field_book, factor_columns)
  DT::datatable(data, filter = "top", rownames = FALSE,
                 options = field_book_table_options(nrow(data), height, collapse))
}
