#' Validate a check count or consecutive entry identifiers before expansion
#' @noRd
validate_spatial_checks <- function(checks, maximum, sort_entries = FALSE) {
  validate_count_vector(checks, "checks")
  count <- if (length(checks) == 1L) checks else length(checks)
  if (count > maximum) {
    fieldhub_abort("The number of checks cannot exceed the field size.",
                   data = list(argument = "checks", maximum = maximum))
  }
  entries <- if (sort_entries) sort(checks) else checks
  if (length(entries) > 1L && any(diff(entries) != 1)) {
    fieldhub_abort("Check entries must be distinct consecutive whole numbers",
                   if (!sort_entries) " in increasing order" else "", ".",
                   data = list(argument = "checks"))
  }
  invisible(checks)
}
