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

#' Entries of an augmented RCBD
#' @param lines Number of entries typed (generated path).
#' @param checks Number of checks.
#' @param data The uploaded ENTRY/NAME list (checks first), or \code{NULL}.
#' @return The number of entries, checks excluded.
#' @noRd
augmented_lines <- function(lines, checks, data = NULL) {
  if (!is.null(data)) lines <- nrow(data) - checks
  if (!is.numeric(lines) || length(lines) != 1L || !is.finite(lines) || lines != trunc(lines)) {
    fieldhub_abort("The number of entries must be one whole number.")
  }
  if (lines < 8) fieldhub_abort("At least ten treatments are required!!")
  lines
}

#' What an augmented RCBD with entries left in place does
#' @param random The "Randomize Entries?" checkbox.
#' @return The note shown while it is off, or \code{NULL}.
#' @noRd
augmented_random_note <- function(random) {
  if (isFALSE(random)) "By unchecking this option only the check plots are randomized."
}

#' The check entries of an uploaded list: its first rows
#'
#' @description The diagonal and sparse designs take the checks from the
#' first rows of the list and need their ENTRY numbers to be consecutive.
#' @param data A shaped ENTRY/NAME data frame, checks first.
#' @param checks Number of checks.
#' @return The sorted check ENTRY numbers.
#' @noRd
upload_check_entries <- function(data, checks) {
  entries <- suppressWarnings(sort(as.numeric(data$ENTRY[seq_len(checks)]), na.last = TRUE))
  if (nrow(data) <= checks || anyNA(entries) || any(diff(entries) != 1)) {
    fieldhub_abort("The checks (the first ", checks, " rows of the file) must have consecutive ",
                   "ENTRY numbers, for example 1, 2, 3, 4.")
  }
  entries
}

#' Entries of a single diagonal arrangement
#'
#' @param lines Number of entries typed (generated path).
#' @param checks Number of checks.
#' @param data The uploaded ENTRY/NAME list (checks first), or \code{NULL}.
#' @return A list with \code{checks_entries}, \code{entries} (all entries,
#'   checks included), \code{field_entries} (the count the candidate fields
#'   are searched for) and \code{layout} (the uploaded list, or \code{NULL}).
#' @noRd
diagonal_entries <- function(lines, checks, data = NULL) {
  if (is.null(data)) {
    return(list(checks_entries = seq_len(checks), entries = lines + checks,
                field_entries = lines, layout = NULL))
  }
  list(checks_entries = upload_check_entries(data, checks), entries = nrow(data),
       field_entries = nrow(data), layout = data)
}
