#' Schema-1 field-book extensions, preserving established storage types
#' @noRd
field_book_extension_columns <- function(x) {
  columns <- registered_design_entry(x)$columns
  if (is.null(columns)) return(character())
  if (is.function(columns)) columns <- columns(x)
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

#' Whether an allocation column contains finite nonnegative whole counts
#' @noRd
allocation_counts_valid <- function(x) {
  is.numeric(x) && !is.complex(x) && is.null(dim(x)) &&
    all(is.finite(x)) && all(x >= 0 & x %% 1 == 0)
}

#' Minimum entry-table structure shared by allocation locations
#' @noRd
allocation_table_valid <- function(x, columns) {
  is.data.frame(x) && nrow(x) > 0L && anyDuplicated(names(x)) == 0L &&
    all(columns %in% names(x)) && all(vapply(x[columns], function(column) {
      is.atomic(column) && is.null(dim(column)) && !anyNA(column)
    }, logical(1)))
}
