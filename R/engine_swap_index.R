# Coordinate bookkeeping for the pair-swap optimizer (swap_pairs_core()).
#
# Every helper reproduces the arithmetic of the full-field reference functions
# (.pair_dists_for_geno(), .score_swap(), the per-entry pairs_distance() loop)
# operation for operation, in the same order and with the same integer/double
# types, so the optimizer's results stay identical() to master 720449e.

#' Duplicate-entry pairs in the order the per-entry reference loop produced them
#'
#' Entries are taken in increasing value (the order of `table()` levels),
#' each entry's cells in increasing linear index (the order of `which()`), and
#' its pairs in `utils::combn()` order. Returns NULL when no entry repeats.
#' @noRd
.duplicate_pairs <- function(X) {
  v <- as.vector(X)
  cells <- which(!is.na(v))
  values <- v[cells]
  o <- order(values)
  sorted_values <- values[o]
  sorted_cells <- cells[o]
  n <- length(sorted_values)
  run_end <- c(which(sorted_values[-1L] != sorted_values[-n]), n)
  run_start <- c(1L, run_end[-length(run_end)] + 1L)
  k <- run_end - run_start + 1L
  dup <- which(k >= 2L)
  if (length(dup) == 0L) return(NULL)
  n_pairs <- (k[dup] * (k[dup] - 1L)) %/% 2L
  group <- rep.int(dup, n_pairs)
  within <- sequence(n_pairs)
  first <- second <- integer(length(group))
  group_k <- k[group]
  for (size in unique(k[dup])) {
    sel <- group_k == size
    template <- utils::combn(size, 2L)
    first[sel] <- template[1L, within[sel]]
    second[sel] <- template[2L, within[sel]]
  }
  start <- run_start[group]
  list(geno = as.integer(sorted_values[start]),
       p1 = sorted_cells[start + first - 1L],
       p2 = sorted_cells[start + second - 1L])
}
