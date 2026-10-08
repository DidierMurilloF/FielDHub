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
