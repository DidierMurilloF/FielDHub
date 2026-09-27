#' Schema-1 field-book extensions, preserving established storage types
#' @noRd
field_book_extension_columns <- function(x) {
  info <- if (is.list(x$infoDesign)) x$infoDesign else list()
  classic <- c("REP", "TREATMENT")
  incomplete <- c("REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT")
  spatial <- c("EXPT", "YEAR", "ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT")
  columns <- switch(x$metadata$design,
    crd = classic,
    rcbd = c(classic, if (!is.null(info$checks)) c("ENTRY", "CHECKS")),
    latin_square = c("SQUARE", "ROW", "COLUMN", "TREATMENT"),
    full_factorial = c("REP", "TRT_COMB", paste0("FACTOR_", info$factors)),
    split_plot = c("REP", "WHOLE_PLOT", "SUB_PLOT", "TRT_COMB"),
    split_split_plot = c("REP", "WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT", "TRT_COMB"),
    strip_plot = c("REP", "HSTRIP", "VSTRIP", "TRT_COMB"),
    incomplete_blocks = incomplete,
    alpha_lattice = incomplete,
    square_lattice = incomplete,
    rectangular_lattice = incomplete,
    row_column = c("REP", "ROW", "COLUMN", "ENTRY", "TREATMENT"),
    diagonal_arrangement = spatial,
    optimized_arrangement = spatial,
    sparse_allocation = spatial,
    rcbd_augmented = c(spatial, "BLOCK"),
    partially_replicated = c(spatial, "REP"),
    multi_location_prep = c(spatial, "REP"),
    character()
  )
  unique(columns)
}

#' Validate design-specific columns without coercing valid field books
#' @noRd
field_book_extension_problems <- function(x) {
  book <- x$fieldBook
  if (!is.data.frame(book)) return(character())
  columns <- field_book_extension_columns(x)
  missing <- setdiff(columns, names(book))
  problems <- if (length(missing)) {
    paste0("has no design-specific field-book columns: ", paste(missing, collapse = ", "))
  } else character()
  for (name in intersect(columns, names(book))) {
    column <- book[[name]]
    missing_allowed <- rep(identical(name, "CHECKS"), nrow(book))
    if (identical(name, "REP") &&
        x$metadata$design %in% c("partially_replicated", "multi_location_prep") &&
        is.numeric(book[["ENTRY"]]) && is.null(dim(book[["ENTRY"]]))) {
      missing_allowed <- !is.na(book[["ENTRY"]]) & book[["ENTRY"]] == 0
    }
    valid <- is.atomic(column) && !is.complex(column) && is.null(dim(column)) &&
      length(column) == nrow(book) && !anyNA(column[!missing_allowed])
    if (valid && is.numeric(column)) {
      valid <- all(is.finite(column) | (missing_allowed & is.na(column)))
    }
    if (!valid) {
      problems <- c(problems, paste0("has an invalid design-specific ", name, " column"))
    }
  }
  problems
}

#' Validate family-split tables, including intentionally empty locations
#' @noRd
family_split_problems <- function(x) {
  counts <- x$rowsEachlist
  entries <- x$data_locations
  if (!allocation_table_valid(counts, c("Location", "n")) ||
      !allocation_counts_valid(counts$n) || anyDuplicated(counts$Location) > 0L) {
    return("has invalid family-split location totals")
  }
  if (!allocation_table_valid(entries, c("ENTRY", "NAME", "FAMILY", "LOCATION"))) {
    return("has no valid family-split entry table")
  }
  indices <- match(entries$LOCATION, counts$Location)
  if (anyNA(indices)) return("has family entries assigned to an unknown location")
  actual <- tabulate(indices, nbins = nrow(counts))
  if (!identical(as.numeric(actual), as.numeric(counts$n))) {
    return("has family-split location totals that disagree with its entries")
  }
  character()
}
