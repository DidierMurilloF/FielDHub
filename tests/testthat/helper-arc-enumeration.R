# Test-only generator for the fixed-replication 6 x 4 enumeration (research M1
# specification, section 10.3). It reproduces the scheme of
# research/augmented-row-column/m1-verification/exhaust.R: v = 6 pseudo
# treatments, each replicated exactly twice, in a k = 3 by s = 4 contraction.

# TRUE when no pseudo label repeats within any contraction row
arc_is_row_binary <- function(m) {
  all(apply(m, 1, anyDuplicated) == 0L)
}

# The column-binary representatives as a list of 3 x 4 integer matrices
# (85 sorted column multisets x 6^3 orders of columns 2..4 = 18,360). The
# first column of each multiset keeps its generated (ascending) order.
arc_enumerate_6x4 <- function() {
  v <- 6L
  k <- 3L
  s <- 4L
  cols <- utils::combn(v, k, simplify = FALSE) # the 20 column sets

  # Non-decreasing index choices in which every label appears exactly twice
  multisets <- list()
  recurse <- function(start, chosen, counts) {
    if (length(chosen) == s) {
      if (all(counts == 2L)) multisets[[length(multisets) + 1L]] <<- chosen
      return(invisible())
    }
    for (i in start:length(cols)) {
      next_counts <- counts
      next_counts[cols[[i]]] <- next_counts[cols[[i]]] + 1L
      if (all(next_counts <= 2L)) recurse(i, c(chosen, i), next_counts)
    }
  }
  recurse(1L, integer(0), rep(0L, v))

  perms <- list(c(1L, 2L, 3L), c(1L, 3L, 2L), c(2L, 1L, 3L),
                c(2L, 3L, 1L), c(3L, 1L, 2L), c(3L, 2L, 1L))
  out <- list()
  for (chosen in multisets) {
    for (g2 in 1:6) for (g3 in 1:6) for (g4 in 1:6) {
      orders <- c(g2, g3, g4)
      m <- matrix(0L, k, s)
      m[, 1] <- cols[[chosen[1]]]
      for (j in 2:s) m[, j] <- cols[[chosen[j]]][perms[[orders[j - 1L]]]]
      out[[length(out) + 1L]] <- m
    }
  }
  out
}
