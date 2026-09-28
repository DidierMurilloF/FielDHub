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

#' Entries of a multiple diagonal arrangement
#'
#' @description Checks that the entries per experiment add up to the
#' entries, and lays the entries out with the experiment (BLOCK) of each,
#' checks first, which the field-size and check options read.
#' @inheritParams diagonal_entries
#' @param blocks Entries of each experiment.
#' @param same_entries Whether the experiments repeat the same entries.
#' @return As \code{diagonal_entries()}, with \code{layout} the
#'   ENTRY(/NAME)/BLOCK table.
#' @noRd
multiple_diagonal_entries <- function(lines, blocks, checks, same_entries, data = NULL) {
  if (is.null(data)) {
    if (lines != sum(blocks)) {
      fieldhub_abort("The entries in the blocks must add up to the number of entries.")
    }
    checks_entries <- seq_len(checks)
    layout <- data.frame(ENTRY = seq_len(lines + checks))
  } else {
    checks_entries <- upload_check_entries(data, checks)
    lines <- nrow(data) - checks
    if (sum(blocks) != lines) {
      fieldhub_abort("Number of treatments in blocks does not match with the data input file.")
    }
    layout <- data
  }
  if (lines < 50) fieldhub_abort("Larger field size is recommended for this experiment type")
  if (isTRUE(same_entries) && length(unique(blocks)) > 1L) {
    fieldhub_abort("Blocks should have the same size")
  }
  layout$BLOCK <- c(rep("ALL", checks), rep(seq_along(blocks), times = blocks))
  list(checks_entries = checks_entries, entries = lines + checks,
       field_entries = if (is.null(data)) lines else nrow(data), layout = layout)
}

#' Entries of a sparse allocation
#'
#' @param lines Number of entries (the uploaded list must have as many
#'   after its checks).
#' @param checks Number of checks.
#' @param l Number of locations.
#' @param data The uploaded ENTRY/NAME list (checks first), or \code{NULL}.
#' @return A list with \code{checks_entries} and \code{names} (the uploaded
#'   names of the entries, or \code{NULL}).
#' @noRd
sparse_entries <- function(lines, checks, l, data = NULL) {
  if (l < 3) fieldhub_abort("The system requires at least 3 locations to proceed.")
  if (lines < 60) fieldhub_abort("The system requires at least 60 entries/lines to proceed!")
  if (is.null(data)) return(list(checks_entries = lines + seq_len(checks), names = NULL))
  checks_entries <- upload_check_entries(data, checks)
  if (nrow(data) - checks != lines) {
    fieldhub_abort("Number of entries in file does not match with the input value.")
  }
  list(checks_entries = checks_entries, names = data$NAME[-seq_len(checks)])
}

#' Check the ENTRY column of an uploaded entry list is numeric
#' @param data A shaped ENTRY/NAME data frame.
#' @return \code{data}, or a classed input error.
#' @noRd
check_numeric_entries_upload <- function(data) {
  if (!is.numeric(data$ENTRY)) fieldhub_abort("Column ENTRY should be numeric (integer numbers).")
  data
}

#' Entries of a multi-location p-rep design
#'
#' @param lines Number of entries.
#' @param checks Number of checks, or \code{NULL} without checks.
#' @param l Number of locations.
#' @param data The uploaded ENTRY/NAME list (checks first), or \code{NULL}.
#' @return A list with \code{names} (the uploaded names of the entries, or
#'   \code{NULL}).
#' @noRd
multi_prep_entries <- function(lines, checks, l, data = NULL) {
  if (l < 2) fieldhub_abort("The system requires at least 2 locations to proceed.")
  if (is.null(data)) return(list(names = NULL))
  checks <- if (is.null(checks)) 0L else checks
  if (nrow(data) - checks != lines) {
    fieldhub_abort("Number of entries in file does not match with the input value.")
  }
  list(names = data$NAME[setdiff(seq_len(nrow(data)), seq_len(checks))])
}
