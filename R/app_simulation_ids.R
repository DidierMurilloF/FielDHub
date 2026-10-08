#' Validate the static input map for a shared simulation dialog
#' @noRd
simulation_control_ids <- function(ids) {
  required <- c("trait", "other", "minimum", "maximum", "submit")
  if (!is.character(ids) || !is.null(dim(ids)) || !identical(names(ids), required) || anyNA(ids) ||
      anyDuplicated(ids) || !all(grepl("^[A-Za-z][A-Za-z0-9_.]*$", ids))) {
    fieldhub_abort("Simulation controls need five distinct, named input identifiers.")
  }
  ids
}
