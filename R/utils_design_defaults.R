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
#' @description (Ruling R5) A design built with its own default `plotNumber`
#' (the caller never supplied one) falls back to the per-location defaults
#' silently, even when that default does not have one value per location;
#' only a caller-supplied value of the wrong length raises
#' `fieldhub_default_warning`.
#'
#' @param plotNumber Starting plot numbers supplied by the user, or NULL.
#' @param l Number of locations.
#' @param default Starting plot numbers used instead.
#' @param caller_supplied Whether the caller actually supplied `plotNumber`
#'   (typically `!missing(plotNumber)` in the public function), as opposed to
#'   the fallback being reached through the argument's own default value.
#'
#' @noRd
warn_default_plot_numbers <- function(plotNumber, l, default, caller_supplied = TRUE) {
  if (!caller_supplied) return(invisible(NULL))
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
#' @param caller_supplied Whether the caller actually supplied
#'   `locationNames`; see `warn_default_plot_numbers()` (Ruling R5).
#'
#' @noRd
warn_default_location_names <- function(locationNames, l, default, caller_supplied = TRUE) {
  if (!caller_supplied) return(invisible(NULL))
  reason <- paste0("'locationNames' has ", length(locationNames), " value(s) for ",
                   l, " location(s)")
  warn_default_values("locationNames", locationNames, default, reason)
}

#' Default per-location starting plot numbers
#'
#' @description Shared formula behind the two starting-plot-number bases used
#' across engines: `default_plot_starts(l, 1001)` for engines whose first
#' location starts at 1001, and `default_plot_starts(l, 1)` for engines whose
#' first location starts at 1. Output is unchanged from the inline
#' `seq()` calls it replaces.
#'
#' @param l Number of locations.
#' @param base Starting plot number for the first location.
#' @return An integer-like numeric vector of length `l`.
#' @noRd
default_plot_starts <- function(l, base) {
  seq(base, base + 1000 * (l - 1), by = 1000)
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
