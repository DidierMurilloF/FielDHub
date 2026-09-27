#' Read an optional app seed without drawing from the caller's RNG stream
#' @noRd
read_app_seed <- function(value) {
  if (is.null(value)) return(NULL)
  if ((!is.numeric(value) && !is.character(value)) || !is.null(dim(value))) {
    fieldhub_abort("The random seed must be one number, or blank for an automatic seed.")
  }
  if (length(value) == 0L) return(NULL)
  if (length(value) != 1L) {
    fieldhub_abort("The random seed must be one number, or blank for an automatic seed.")
  }
  if (is.numeric(value) && is.na(value) && !is.nan(value)) return(NULL)
  if (is.character(value) && !is.na(value) && !nzchar(trimws(value))) return(NULL)
  resolve_seed(suppressWarnings(as.numeric(value)))
}

#' Reuse the accepted design seed when an app simulation has no explicit seed
#' @noRd
workflow_seed <- function(seed, design) {
  if (!is.null(seed) && length(seed) > 0L) return(seed)
  recorded <- design$metadata$seed
  if (!is.numeric(recorded) || length(recorded) != 1L || !is.finite(recorded) ||
      abs(trunc(recorded)) > .Machine$integer.max) {
    fieldhub_abort("The accepted design has no recorded seed for simulation.",
                   class = "fieldhub_internal_error")
  }
  recorded
}
