#' Warn that default starting plots replace the ones supplied
#'
#' @param plotNumber Starting plot numbers supplied by the user, or NULL.
#' @param l Number of locations.
#' @param default Starting plot numbers used instead.
#'
#' @noRd
warn_default_plot_numbers <- function(plotNumber, l, default) {
  if (is.null(plotNumber)) {
    reason <- "'plotNumber' was not supplied"
  } else {
    reason <- paste0("'plotNumber' has ", length(plotNumber), " value(s) for ",
                     l, " location(s)")
  }
  warning(reason, "; using the default starting plots ",
          paste(default, collapse = ", "), ".", call. = FALSE)
}

#' Warn that default location names replace the ones supplied
#'
#' @param locationNames Location names supplied by the user.
#' @param l Number of locations.
#' @param default Location names used instead.
#'
#' @noRd
warn_default_location_names <- function(locationNames, l, default) {
  warning("'locationNames' has ", length(locationNames), " value(s) for ", l,
          " location(s); using the default names ",
          paste(default, collapse = ", "), ".", call. = FALSE)
}
