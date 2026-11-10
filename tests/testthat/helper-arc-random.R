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

# A random k x s matrix in which label h occurs r[h] times and no column
# repeats a label (column-binary). Random fill, then random swaps of a
# repeated entry with a random cell; after `max_swaps` swaps without success
# the fill restarts, at most `max_restarts` times, and then it stops with an
# error instead of looping without bound. Needs sum(r) = ks and r <= s.
arc_random_column_binary <- function(k, s, r, max_swaps = 2000L,
                                     max_restarts = 50L) {
  stopifnot(sum(r) == k * s, all(r >= 1L), all(r <= s))
  for (restart in seq_len(max_restarts)) {
    m <- matrix(sample(rep(seq_along(r), r)), k, s)
    for (swap in seq_len(max_swaps)) {
      bad <- which(apply(m, 2L, anyDuplicated) > 0L)
      if (length(bad) == 0L) {
        storage.mode(m) <- "integer"
        return(m)
      }
      j <- bad[sample.int(length(bad), 1L)]
      i <- anyDuplicated(m[, j])
      i2 <- sample.int(k, 1L)
      j2 <- sample.int(s, 1L)
      held <- m[i, j]
      m[i, j] <- m[i2, j2]
      m[i2, j2] <- held
    }
  }
  stop("No column-binary matrix found within the retry budget.")
}

# A random positive replication vector of length v with sum `total` and every
# entry at most `max_r`: one occurrence each, the rest spread uniformly at
# random. Rejection on the cap is bounded by `max_tries`.
arc_random_replication <- function(v, total, max_r, max_tries = 1000L) {
  stopifnot(total >= v, total <= v * max_r)
  for (try in seq_len(max_tries)) {
    r <- 1L + tabulate(sample.int(v, total - v, replace = TRUE), nbins = v)
    if (all(r <= max_r)) {
      return(as.integer(r))
    }
  }
  stop("No replication vector found within the retry budget.")
}

# Parameter sets for the random tests. `recolor = TRUE` draws a column-binary
# matrix and makes it row-binary with arc_recolor_rows() (a call made by the
# test, not here); `r = NULL` draws a fresh unbalanced replication for each
# design.
arc_random_sets <- list(
  example1 = list(v = 12L, s = 8L, k = 3L, mode = "restricted",
                  recolor = TRUE, r = rep(2L, 12L)),
  example2 = list(v = 24L, s = 16L, k = 5L, mode = "restricted",
                  recolor = TRUE, r = rep(c(4L, 3L), c(8L, 16L))),
  unequal = list(v = 7L, s = 5L, k = 3L, mode = "column_binary",
                 recolor = FALSE, r = c(3L, 3L, 2L, 2L, 2L, 2L, 1L)),
  unbalanced = list(v = 8L, s = 6L, k = 3L, mode = "column_binary",
                    recolor = FALSE, r = NULL)
)

# One random column-binary matrix for parameter set `p` (before recoloring).
# Fixed replications are assigned to labels in a random order.
arc_random_draw <- function(p) {
  r <- if (is.null(p$r)) {
    arc_random_replication(p$v, p$k * p$s, max_r = p$s)
  } else {
    sample(p$r)
  }
  arc_random_column_binary(p$k, p$s, r)
}
