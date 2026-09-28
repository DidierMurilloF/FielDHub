#' Validate a finite whole-number search budget without coercing it
#' @noRd
validate_iteration_budget <- function(value, name = "iterations", minimum = 1) {
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
      length(value) != 1L || !is.finite(value) || value < minimum ||
      value != trunc(value) || value > .Machine$integer.max) {
    fieldhub_abort("`", name, "` must be one finite whole number from ", minimum,
                   " to ", .Machine$integer.max, ".",
                   data = list(argument = name, minimum = minimum, maximum = .Machine$integer.max))
  }
  invisible(value)
}
