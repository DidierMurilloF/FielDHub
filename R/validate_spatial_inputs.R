#' Checks and counts of the spatial design pages
#'
#' @description Plain helpers the spatial design pages
#' (\code{design_app_spec()}) call on Run!, after the controls are read
#' (\code{read_design_controls()}): they check what the controls cannot
#' check on their own (a count against an uploaded list, two lists against
#' each other) and derive the counts the choices of the next step need.
#' Each returns its result or raises a classed \code{fieldhub_input_error}
#' written for the user.
#' @name spatial_inputs
#' @noRd
NULL

#' Check the REPS column of an uploaded entry list
#' @param data A shaped ENTRY/NAME/REPS data frame.
#' @return \code{data}, or a classed input error.
#' @noRd
check_reps_upload <- function(data) {
  if (!is.integer(data$REPS)) fieldhub_abort("'REPS' must be numeric.")
  data
}

#' Total plots of an optimized arrangement typed as counts
#' @param lines Number of entries.
#' @param rep_checks Replicates of each check.
#' @return The number of plots.
#' @noRd
optim_total_plots <- function(lines, rep_checks) {
  if (lines <= sum(rep_checks)) {
    fieldhub_abort("Number of lines should be greater then the number of checks.")
  }
  sum(rep_checks) + lines
}
