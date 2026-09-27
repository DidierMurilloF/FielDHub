#' Read an optional app seed without drawing from the caller's RNG stream
#'
#' A cleared Shiny \code{numericInput} is delivered as logical \code{NA}
#' (\code{shiny:::inputHandlers$get("shiny.number")(NULL)}), which counts as
#' blank the same way as \code{NULL}, an empty vector, a numeric/character
#' \code{NA}, or an empty/whitespace string. Any other non-scalar-number
#' value is a classed input error.
#' @noRd
read_app_seed <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.logical(value) && length(value) == 1L && is.na(value)) return(NULL)
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

# Package-private stream for automatic app seeds. Kept separate from the
# caller's/global stream so that a blank app seed box never advances
# process-wide random state: several Shiny sessions share one R process.
app_seed_stream <- new.env(parent = emptyenv())

#' Draw an automatic app seed from the package-private stream
#'
#' Saves the caller's global \code{.Random.seed} (including its absence) and
#' restores it once the draw is done, so the automatic seed never leaks into
#' or out of the caller's/global random-number stream. The private stream
#' itself is seeded from \code{set.seed(NULL)} the first time it is used, and
#' its advanced state is kept in \code{app_seed_stream$state} across calls so
#' consecutive automatic seeds differ.
#' @noRd
private_app_seed <- function() {
  local_rng_state()
  if (is.null(app_seed_stream$state)) {
    set.seed(NULL)
  } else {
    assign(".Random.seed", app_seed_stream$state, envir = globalenv())
  }
  seed <- sample.int(.Machine$integer.max, 1L)
  app_seed_stream$state <- get(".Random.seed", envir = globalenv())
  seed
}

#' Resolve the seed the app uses to build a design
#'
#' An explicit value is only validated (never drawn from any stream); a
#' blank value draws an automatic seed from the private app stream instead
#' of \code{resolve_seed(NULL)}, which would advance the shared process-wide
#' random-number stream.
#' @noRd
app_design_seed <- function(value) {
  seed <- read_app_seed(value)
  if (is.null(seed)) return(private_app_seed())
  resolve_seed(seed)
}
