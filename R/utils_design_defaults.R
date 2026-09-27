#' Signal that a default value replaces what was supplied (DEF-12)
#'
#' @description Shared implementation of the classed `fieldhub_default_warning`
#' condition signalled whenever `plotNumber` or `locationNames` falls back to
#' a default value because nothing, or the wrong number of values, was
#' supplied. The fallback itself is unchanged (1.5.x scripts keep working);
#' only the class and wording of the notice changed for DEF-12. Catch every
#' such fallback the same way with
#' `withCallingHandlers(expr, fieldhub_default_warning = handler)`.
#'
#' @param argument Name of the argument that fell back to a default, such as
#'   `"plotNumber"` or `"locationNames"`.
#' @param supplied Value supplied by the caller (possibly NULL, or the wrong
#'   length for the number of locations).
#' @param used Default value used instead.
#' @param reason Why the default is used, without a trailing period.
#'
#' @noRd
warn_default_values <- function(argument, supplied, used, reason) {
  fieldhub_warn(
    reason, "; using the default ", argument, " ",
    paste(used, collapse = ", "), ".",
    class = "fieldhub_default_warning",
    data = list(argument = argument, supplied = supplied, used = used),
    call = NULL
  )
}

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
  warn_default_values("plotNumber", plotNumber, default, reason)
}

#' Warn that default location names replace the ones supplied
#'
#' @param locationNames Location names supplied by the user.
#' @param l Number of locations.
#' @param default Location names used instead.
#'
#' @noRd
warn_default_location_names <- function(locationNames, l, default) {
  reason <- paste0("'locationNames' has ", length(locationNames), " value(s) for ",
                   l, " location(s)")
  warn_default_values("locationNames", locationNames, default, reason)
}

#' Year recorded in the YEAR column of a field book
#'
#' @param year Year given by the user, or NULL for the current year.
#'
#' @return The year as a character string.
#' @noRd
resolve_year <- function(year) {
  if (is.null(year)) return(format(Sys.Date(), "%Y"))
  if (length(year) != 1 || is.na(year)) {
    fieldhub_abort("'year' must be a single value, such as 2026.", call. = FALSE)
  }
  as.character(year)
}
