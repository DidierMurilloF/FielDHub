# Depth of nested design calls, so that only the outermost one restores the
# caller's random-number stream
rng_calls <- new.env(parent = emptyenv())
rng_calls$depth <- 0

#' Restore the caller's random-number state on exit
#'
#' @param frame The frame whose exit restores the state.
#' @noRd
local_rng_state <- function(frame = parent.frame()) {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  old_seed <- if (had_seed) base::get(".Random.seed", envir = global, inherits = FALSE)
  restore <- function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  }
  do.call(base::on.exit, list(as.call(list(restore)), add = TRUE), envir = frame)
}

#' Seed used by a design function
#'
#' @param seed The seed given by the user: NULL or a single number.
#' @param default Function drawing the seed when \code{seed} is NULL.
#'
#' @return The seed. When \code{seed} is NULL, the outermost design call draws
#'   exactly one integer from the caller's random-number stream, without
#'   restoring it: the draw is consumed. A nested call keeps the draw of
#'   \code{default}, so that designs built by other design functions do not
#'   change. Callers that set the design's own random-number state, such as
#'   \code{local_design_seed()}, still restore it to its state right after
#'   this draw, so the design's internal randomization never reaches the
#'   caller's stream. Explicit seeds are only validated: they neither draw
#'   nor advance the caller's stream.
#' @noRd
resolve_seed <- function(seed, default = function() stats::runif(1, min = -50000, max = 50000)) {
  if (is.null(seed)) {
    if (rng_calls$depth == 0) {
      return(sample.int(.Machine$integer.max, 1))
    }
    return(default())
  }
  if (!is.numeric(seed) || is.complex(seed) || length(seed) != 1 || !is.finite(seed)) {
    fieldhub_abort("'seed' must be a single number.", call = sys.call(-1))
  }
  if (abs(trunc(seed)) > .Machine$integer.max) {
    fieldhub_abort("'seed' must fit R's integer seed range after truncation.",
                   call = sys.call(-1))
  }
  seed
}

#' Derive a location seed without overflowing R's supported seed interval
#'
#' Keep the original addition, including its type and names, whenever R can
#' seed from it. Only out-of-range sums wrap around the signed integer interval.
#' @noRd
offset_design_seed <- function(seed, offset) {
  limit <- .Machine$integer.max
  combined <- as.double(seed) + as.double(offset)
  if (abs(trunc(combined)) <= limit) return(seed + offset)
  wrapped <- (trunc(combined) + limit) %% (2 * limit + 1) - limit
  if (is.integer(seed) && is.integer(offset)) as.integer(wrapped) else wrapped
}

#' Set the seed of a design function and restore the caller's stream on exit
#'
#' @description Calls \code{set.seed(seed)}. In the outermost design call, it
#' also saves the caller's random-number state as it stands when called
#' (right after \code{resolve_seed()} drew a seed, when the call had none) and
#' restores it to that state when the calling function exits, so that the
#' design's own internal randomization does not change the random numbers the
#' user draws next: only the one draw of its own seed, when there was one,
#' reaches the caller's stream. Nested design calls leave the stream as they
#' always did.
#'
#' @param seed The seed.
#' @param frame The frame of the design function.
#'
#' @noRd
local_design_seed <- function(seed, frame = parent.frame()) {
  if (rng_calls$depth == 0) local_rng_state(frame)
  restore <- function() {
    rng_calls$depth <- rng_calls$depth - 1
  }
  do.call(base::on.exit, list(as.call(list(restore)), add = TRUE), envir = frame)
  rng_calls$depth <- rng_calls$depth + 1
  set.seed(seed)
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
