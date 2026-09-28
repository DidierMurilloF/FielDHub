#' Location identifiers in the field book's established appearance order
#' @noRd
field_book_locations <- function(field_book) {
  if (!is.data.frame(field_book) || nrow(field_book) == 0L ||
      anyNA(names(field_book)) || anyDuplicated(names(field_book)) > 0L ||
      !"LOCATION" %in% names(field_book) || !is.atomic(field_book$LOCATION) ||
      !is.null(dim(field_book$LOCATION)) || anyNA(field_book$LOCATION)) {
    fieldhub_abort("The field book must contain rows and nonmissing LOCATION identifiers.")
  }
  unique(as.character(field_book$LOCATION))
}

#' Build per-location maps from named values and coordinates
#' @noRd
field_book_location_grids <- function(field_book, value, reverse_rows = FALSE) {
  locations <- field_book_locations(field_book)
  if (!is.logical(reverse_rows) || length(reverse_rows) != 1L || is.na(reverse_rows)) {
    fieldhub_abort("The row direction must be TRUE or FALSE.")
  }
  lapply(locations, function(location) {
    book <- field_book[as.character(field_book$LOCATION) == location, , drop = FALSE]
    grid <- field_book_export_grid(book, value)
    if (reverse_rows) grid <- grid[rev(seq_len(nrow(grid))), , drop = FALSE]
    grid
  })
}
