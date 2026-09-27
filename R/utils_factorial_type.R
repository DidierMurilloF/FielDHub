#' Validate the numeric CRD/RCBD alternative without coercing its representation
#' @noRd
validate_factorial_type <- function(type) {
  if (!is.numeric(type) || !is.null(dim(type)) || length(type) != 1L ||
      !is.finite(type) || !type %in% c(1, 2)) {
    fieldhub_abort("`type` must be one numeric value: 1 (CRD) or 2 (RCBD).",
                   data = list(argument = "type", choices = c(CRD = 1, RCBD = 2)))
  }
  invisible(type)
}
