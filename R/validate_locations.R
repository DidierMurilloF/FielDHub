#' Validate the common location count before a design allocates its fields
#'
#' @param l Number of locations.
#' @return The unchanged count, invisibly, or a classed input condition.
#' @noRd
validate_locations <- function(l) {
  if (missing(l) || !is.numeric(l) || is.complex(l) || !is.null(dim(l)) || length(l) != 1L || !is.finite(l) ||
      l < 1 || l > .Machine$integer.max || l != trunc(l)) {
    fieldhub_abort("The number of locations 'l' must be one positive whole number ",
                   "in the supported integer range.")
  }
  invisible(l)
}

#' Validate effective location labels without changing legacy length fallbacks
#' @noRd
validate_location_labels <- function(labels, locations) {
  if (is.null(labels) || length(labels) != locations) return(invisible(labels))
  validate_entry_labels(labels, "locationNames")
  if (anyDuplicated(as.character(labels))) {
    fieldhub_abort("`locationNames` must identify distinct locations.",
                   data = list(argument = "locationNames"))
  }
  invisible(labels)
}
