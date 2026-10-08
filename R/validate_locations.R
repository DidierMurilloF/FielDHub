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

#' Coerce a raw "# of Locations"-style app input before validating it
#'
#' @description A cleared Shiny `numericInput` arrives as logical `NA` and a
#'   cleared `textInput` as `""`; a numeric-looking `textInput` arrives as a
#'   character such as `"3"`. This accepts any of those, and only converts
#'   character input, so `validate_locations()` sees the same checks for
#'   every source widget.
#' @param sites Raw locations-count input: numeric, integer, or a
#'   numeric-looking character.
#' @return The validated location count, invisibly, or a classed
#'   `fieldhub_input_error` condition.
#' @noRd
validate_locations_input <- function(sites) {
  if (is.character(sites)) sites <- suppressWarnings(as.numeric(sites))
  validate_locations(sites)
}

#' Choices for a "view location" select input, from a raw locations count
#'
#' @description Several Shiny modules offer a dropdown to pick which
#'   location's layout to view, populated from the "# of Locations" input
#'   with `1:as.numeric(input$...)`. That throws "NA/NaN argument" (ending
#'   the session, since it runs inside an observer) when the input is
#'   cleared, and silently mishandles other invalid values. This validates
#'   the count first, so the observer can call it through
#'   `validate_design()` instead and show the app's usual validation
#'   message.
#' @inheritParams validate_locations_input
#' @return `seq_len(sites)` for one validated positive whole number.
#' @noRd
location_view_choices <- function(sites) {
  seq_len(validate_locations_input(sites))
}

#' Choices for the sparse-allocation "plant reps" select input
#'
#' @description `sparse_allocation()`'s `plant_reps` picks a location count
#'   out of the total, so it is always less than the total number of
#'   locations. With one (valid) location there is nothing to choose from.
#' @inheritParams validate_locations_input
#' @return `seq_len(sites - 1)` for one validated positive whole number,
#'   `integer(0)` when `sites` is 1.
#' @noRd
plant_rep_choices <- function(sites) {
  seq_len(max(validate_locations_input(sites) - 1L, 0L))
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
