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
#'   an integer from the whole range of seeds, and a nested call keeps the
#'   draw of \code{default}, so that designs built by other design functions
#'   do not change.
#' @noRd
resolve_seed <- function(seed, default = function() stats::runif(1, min = -50000, max = 50000)) {
  if (is.null(seed)) {
    if (rng_calls$depth == 0) {
      local_rng_state()
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

#' Set the seed of a design function and restore the caller's stream on exit
#'
#' @description Calls \code{set.seed(seed)}. In the outermost design call, it
#' also saves the caller's random-number state and restores it when the
#' calling function exits, so that creating a design does not change the
#' random numbers the user draws next. Nested design calls leave the stream as
#' they always did.
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
