#' Layout selector values without drawing a field map
#'
#' Empty options produce integer(), never the phantom sequence 1:0.
#' The application translates input conditions through validate_design().
#' @noRd
layout_choices <- function(x, planter = "serpentine", stacked = "vertical", location = 1) {
  if (!inherits(x, "FielDHub")) {
    fieldhub_abort("'x' must be a design created by FielDHub.")
  }
  if (!is.numeric(location) || length(location) != 1L || !is.finite(location) ||
      location < 1 || location %% 1 != 0) {
    fieldhub_abort("'location' must be a positive whole number.")
  }
  check_layout_arguments(planter, stacked)
  options <- layout_options(with_design_class(x), planter = planter, stacked = stacked)
  if (location > length(options)) {
    fieldhub_abort("Location ", location, " is not available for this design.",
                   data = list(options = seq_along(options)))
  }
  seq_along(options[[location]])
}

#' The layout options of a design, each named by the shape of its field
#'
#' @description The values of \code{layout_choices()}, named by the rows and
#' columns of each option ("4 \u00d7 15"). Options of one shape (the same
#' field with its blocks in another direction) are numbered too.
#' @inheritParams layout_choices
#' @return A named integer vector, for a select.
#' @noRd
layout_choice_labels <- function(x, planter = "serpentine", stacked = "vertical", location = 1) {
  choices <- layout_choices(x, planter = planter, stacked = stacked, location = location)
  if (!length(choices)) return(choices)
  options <- layout_options(with_design_class(x), planter = planter, stacked = stacked)[[location]]
  extent <- function(v) max(as.numeric(as.character(v)))
  shapes <- vapply(options, function(book) {
    if (!is.data.frame(book) || !all(c("ROW", "COLUMN") %in% names(book))) return(NA_character_)
    paste(extent(book$ROW), "\u00d7", extent(book$COLUMN))
  }, character(1))
  if (anyNA(shapes)) return(choices)
  if (anyDuplicated(shapes)) shapes <- paste0(choices, ": ", shapes)
  stats::setNames(choices, shapes)
}
