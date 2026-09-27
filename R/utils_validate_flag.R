#' Validate a Boolean control before randomization or allocation
#' @noRd
validate_flag <- function(value, argument, call = sys.call(-1)) {
  if (!is.logical(value) || length(value) != 1L || !is.null(dim(value)) || is.na(value)) {
    fieldhub_abort("'", argument, "' must be a single TRUE or FALSE.",
                   data = list(argument = argument, value = value, options = c(FALSE, TRUE)),
                   call = call)
  }
  invisible(value)
}
