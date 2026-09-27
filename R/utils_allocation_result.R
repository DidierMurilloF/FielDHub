#' Construct an allocation plan without changing its legacy data or class
#' @noRd
new_fieldhub_allocation <- function(x, parameters) {
  if (!is.list(x) || !is.list(parameters)) {
    fieldhub_abort("Internal error: an allocation and its parameters must be lists.",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  if ((!is.character(parameters$design) && !is.factor(parameters$design)) ||
      length(parameters$design) != 1L || is.na(parameters$design) ||
      !parameters$design %in% c("sparse", "prep")) {
    fieldhub_abort("Internal error: allocation parameters have no supported design.",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  design <- unname(as.character(parameters$design))
  new_fieldhub_result(x, paste0("allocation_", design), parameters$seed, parameters,
                     if (identical(design, "prep")) "MultiPrep" else "Sparse",
                     validate_fieldhub_allocation)
}

#' Validate the allocation-specific result schema
#'
#' Allocations are entry plans, not field books: they have no plot coordinates.
#' Location sizes count test-entry copies, excluding the appended checks.
#' @noRd
validate_fieldhub_allocation <- function(x) {
  fail <- function(message) {
    fieldhub_abort("Internal error: the allocation result ", message, ".",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  if (!is.list(x)) fail("must be a list")
  meta <- x$metadata
  if (!is.list(meta) || !is.character(meta$design) || length(meta$design) != 1L ||
      is.na(meta$design) || !meta$design %in% c("allocation_sparse", "allocation_prep")) {
    fail("has no metadata naming the allocation")
  }
  prep <- identical(meta$design, "allocation_prep")
  if (!inherits(x, if (prep) "MultiPrep" else "Sparse")) fail("has an inconsistent class")
  problems <- fieldhub_metadata_problems(meta)
  if (length(problems) > 0L) fail(paste(problems, collapse = ", "))
  parameters <- meta$parameters
  if (!is.list(parameters) || !identical(names(parameters), names(formals(do_optim))) ||
      (!is.character(parameters$design) && !is.factor(parameters$design)) ||
      !identical(unname(as.character(parameters$design)), if (prep) "prep" else "sparse") ||
      !identical(parameters$seed, meta$seed)) {
    fail("has inconsistent recorded input parameters")
  }

  allocation <- x$allocation
  if (!is.data.frame(allocation) || nrow(allocation) == 0L || ncol(allocation) == 0L ||
      anyDuplicated(names(allocation)) > 0L || anyNA(names(allocation)) ||
      !all(vapply(allocation, allocation_counts_valid, logical(1)))) {
    fail("has no valid allocation count table")
  }
  locations <- names(allocation)
  sizes <- x$size_locations
  if (!allocation_counts_valid(sizes) || !identical(names(sizes), locations) ||
      !identical(as.numeric(sizes), as.numeric(colSums(allocation)))) {
    fail("has location sizes that disagree with its allocation")
  }
  columns <- c("ENTRY", "NAME", if (prep) "REPS")
  lists <- x$list_locs
  if (!is.list(lists) || is.data.frame(lists) || !identical(names(lists), locations) ||
      !all(vapply(lists, allocation_table_valid, logical(1), columns = columns))) {
    fail("has invalid location entry lists")
  }
  if (!allocation_table_valid(x$multi_location_data, c("LOCATION", columns)) ||
      !setequal(x$multi_location_data$LOCATION, locations)) {
    fail("has invalid multi-location entries")
  }
  invisible(x)
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
