#' Validate complete numeric count vectors without changing their attributes
#' @noRd
validate_count_vector <- function(values, argument, minimum = 1) {
  if (!is.numeric(values) || !is.null(dim(values)) || length(values) == 0L ||
      any(!is.finite(values)) || any(values < minimum | values != trunc(values)) ||
      any(values > .Machine$integer.max)) {
    fieldhub_abort("`", argument, "` must contain finite whole numbers from ", minimum,
                   " to ", .Machine$integer.max, ".",
                   data = list(argument = argument, minimum = minimum, maximum = .Machine$integer.max))
  }
  invisible(values)
}
