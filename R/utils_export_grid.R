#' Rectangular layout values indexed by their named coordinates
#'
#' A complete, uniquely addressed grid is required. Values retain the legacy
#' matrix coercion (including factor codes) used by layout CSV downloads.
#' @noRd
field_book_export_grid <- function(book, type) {
  if (!is.data.frame(book) || nrow(book) == 0L || anyDuplicated(names(book)) > 0L ||
      !all(c("ROW", "COLUMN") %in% names(book))) {
    fieldhub_abort("The layout field book must have rows and unique ROW and COLUMN columns.")
  }
  if (!is.character(type) || length(type) != 1L || is.na(type) || !type %in% names(book)) {
    fieldhub_abort("The layout value must name one field-book column.")
  }
  values <- book[[type]]
  if (!is.atomic(values) || !is.null(dim(values))) {
    fieldhub_abort("The layout value column must be an atomic vector.")
  }
  coordinates <- lapply(book[c("ROW", "COLUMN")], function(x) {
    if (!is.atomic(x) || !is.null(dim(x)) || anyNA(x)) {
      fieldhub_abort("Layout coordinates must be nonmissing atomic vectors.")
    }
    match(as.character(x), as.character(seq_len(length(unique(x)))))
  })
  rows <- coordinates$ROW
  cols <- coordinates$COLUMN
  if (anyNA(rows) || anyNA(cols)) {
    fieldhub_abort("Layout ROW and COLUMN coordinates must be consecutive whole numbers from one.")
  }
  nrows <- max(rows)
  ncols <- max(cols)
  index <- rows + (cols - 1) * nrows
  if (as.double(nrows) * ncols != nrow(book) || anyDuplicated(index) > 0L) {
    fieldhub_abort("The layout must contain exactly one plot at every row and column coordinate.")
  }
  grid <- matrix(NA, nrow = nrows, ncol = ncols)
  grid[index] <- values
  grid
}
