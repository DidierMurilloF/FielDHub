planter_choices <- c("serpentine", "cartesian")
stacking_choices <- c("vertical", "horizontal", "grid_panel")

#' Check that a planter argument is one of the supported plot movements
#'
#' @description Shared validator for every design and layout function that
#' takes a `planter` (or `planter_mov`, `movement_planter`) argument (AD-08).
#' Raises one classed `fieldhub_input_error`, with `options = c("serpentine",
#' "cartesian")` in the condition data, wherever the movement is not one of
#' the two supported values.
#'
#' @param planter Value to validate.
#' @param call The call reported with the error.
#' @noRd
validate_planter <- function(planter, call = sys.call(-1)) {
  if (!is.character(planter) || length(planter) != 1 || !planter %in% planter_choices) {
    fieldhub_abort("'planter' must be \"serpentine\" or \"cartesian\".",
                   data = list(options = planter_choices), call = call)
  }
  invisible(TRUE)
}

#' Check the planter and the stacking of a layout
#' @noRd
check_layout_arguments <- function(planter, stacked, call = sys.call(-1)) {
  validate_planter(planter, call = call)
  if (!is.character(stacked) || length(stacked) != 1 || !stacked %in% stacking_choices) {
    fieldhub_abort("'stacked' must be \"vertical\", \"horizontal\" or \"grid_panel\".",
                   call = call)
  }
  invisible(TRUE)
}
