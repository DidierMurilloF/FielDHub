# Test-only random generators for the augmented row-column engine (research
# M1 specification, section 10.3). Nothing here calls the engine.

# Seed the RNG for the rest of the calling frame and restore the caller's
# RNG state when that frame exits: the previous `.Random.seed` is put back,
# or removed if there was none, and the RNG kinds are restored. withr is not
# available to tests (implementation plan, ground rules), so the exit hook is
# registered with on.exit() evaluated in the caller's frame.
arc_local_seed <- function(seed, envir = parent.frame()) {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  saved_seed <- if (had_seed) {
    get(".Random.seed", envir = global, inherits = FALSE)
  }
  saved_kind <- RNGkind()
  restore <- function() {
    suppressWarnings(RNGkind(saved_kind[1L], saved_kind[2L], saved_kind[3L]))
    if (had_seed) {
      assign(".Random.seed", saved_seed, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  }
  do.call(on.exit, list(as.call(list(restore)), add = TRUE), envir = envir)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  invisible(seed)
}

