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

#' Pair distances of one entry from its sorted linear cell indices
#'
#' Identical to `.pair_dists_for_geno(X, g)` when `cells` is
#' `which(X == g)`: same pairs, order, types and arithmetic.
#' @noRd
.pair_dists_from_cells <- function(cells, nr, dist_method, templates) {
  k <- length(cells)
  if (k < 2L) return(numeric(0))
  pairs <- templates[[k]]
  if (is.null(pairs)) pairs <- utils::combn(seq_len(k), 2L)
  rows <- ((cells - 1L) %% nr) + 1L
  cols <- ((cells - 1L) %/% nr) + 1L
  dr <- rows[pairs[1L, ]] - rows[pairs[2L, ]]
  dc <- cols[pairs[1L, ]] - cols[pairs[2L, ]]
  if (dist_method == "manhattan") abs(dr) + abs(dc) else sqrt(dr^2 + dc^2)
}

#' Pair templates for copy counts 2..max_k, built once per optimizer run
#' @noRd
.pair_templates <- function(max_k) {
  templates <- vector("list", max(max_k, 1L))
  for (k in seq_len(max_k)[-1L]) templates[[k]] <- utils::combn(seq_len(k), 2L)
  templates
}

#' Score one candidate swap from the cell index (same value as .score_swap())
#'
#' `cells_i` are the sorted cells of the entry at linear cell `a`; `cells_j`
#' those of the entry at candidate cell `b`; `same` is TRUE when both cells
#' hold one entry (the swap is then a no-op and delta is exactly 0).
#' @noRd
.score_swap_cells <- function(cells_i, cells_j, a, b, rj, cj, same,
                              lambda, center, base_sum, n_pairs,
                              nr, dist_method, templates) {
  old_i <- .pair_dists_from_cells(cells_i, nr, dist_method, templates)
  old_j <- if (!same) .pair_dists_from_cells(cells_j, nr, dist_method, templates) else numeric(0)
  old_contrib <- sum(old_i) + sum(old_j)
  if (same) {
    new_i <- old_i
    new_j <- numeric(0)
  } else {
    new_i <- .pair_dists_from_cells(sort.int(c(cells_i[cells_i != a], b)), nr, dist_method, templates)
    new_j <- .pair_dists_from_cells(sort.int(c(cells_j[cells_j != b], a)), nr, dist_method, templates)
  }
  new_contrib <- sum(new_i) + sum(new_j)
  delta <- new_contrib - old_contrib
  adjusted_mean <- (base_sum + delta) / max(n_pairs, 1L)
  candidate_center_dist <- if (dist_method == "manhattan") {
    abs(rj - center[1L]) + abs(cj - center[2L])
  } else sqrt((rj - center[1L])^2 + (cj - center[2L])^2)
  list(score = adjusted_mean - lambda * candidate_center_dist, delta = delta)
}

#' Shared pairs s(g) of one entry: pairs of its cells in one row plus pairs in
#' one column (distinct cells never share both)
#' @noRd
.shared_pairs <- function(cells, nr) {
  if (length(cells) < 2L) return(0L)
  rows <- ((cells - 1L) %% nr) + 1L
  cols <- ((cells - 1L) %/% nr) + 1L
  tr <- tabulate(rows)
  tc <- tabulate(cols)
  as.integer(sum(tr * (tr - 1L)) %/% 2L + sum(tc * (tc - 1L)) %/% 2L)
}

#' Change in S for swapping cells a and b (entries code_a at a, code_b at b)
#'
#' Zero when both cells hold one entry (the swap is the identity; spec 5.5 M1).
#' Only covered entries count.
#' @noRd
.shared_delta <- function(cells_a, cells_b, a, b, same, covered_a, covered_b, nr) {
  if (same) return(0L)
  d <- 0L
  if (covered_a) d <- d + .shared_pairs(sort.int(c(cells_a[cells_a != a], b)), nr) - .shared_pairs(cells_a, nr)
  if (covered_b) d <- d + .shared_pairs(sort.int(c(cells_b[cells_b != b], a)), nr) - .shared_pairs(cells_b, nr)
  d
}

#' Pick the candidate under the row/column rule (spec 5.3 item 2)
#'
#' Finite weight: largest score - weight * dS. Infinite weight (strict): smallest
#' dS, then largest score. Ties go to the first candidate. When `require_gain`
#' is TRUE only candidates with dS < 0 qualify; returns NA if none does.
#' @noRd
.rowcol_pick <- function(scores, dS, weight, require_gain) {
  eligible <- if (require_gain) which(dS < 0L) else seq_along(scores)
  if (length(eligible) == 0L) return(NA_integer_)
  if (is.infinite(weight)) {
    tied <- eligible[dS[eligible] == min(dS[eligible])]
    return(tied[which.max(scores[tied])])
  }
  eligible[which.max(scores[eligible] - weight * dS[eligible])]
}

#' Check the row/column rule controls (spec 5.2); Inf is the strict setting
#' @noRd
validate_rowcol_controls <- function(rowcol_penalty, rowcol_scope) {
  if (!is.numeric(rowcol_penalty) || is.complex(rowcol_penalty) || !is.null(dim(rowcol_penalty)) ||
      length(rowcol_penalty) != 1L || is.na(rowcol_penalty) || rowcol_penalty < 0) {
    fieldhub_abort("`rowcol_penalty` must be one number at least 0, or Inf.")
  }
  if (!is.character(rowcol_scope) || length(rowcol_scope) != 1L || is.na(rowcol_scope) ||
      !rowcol_scope %in% c("pairs", "all")) {
    fieldhub_abort("`rowcol_scope` must be \"pairs\" or \"all\".")
  }
  invisible(NULL)
}

#' Check the row/column rule controls of the p-rep designs (spec-m15 5.2):
#' the rule needs the optimizer, so it conflicts with `spread_reps = FALSE`
#' @noRd
validate_prep_rowcol <- function(rowcol_penalty, rowcol_scope, spread_reps) {
  validate_rowcol_controls(rowcol_penalty, rowcol_scope)
  if (rowcol_penalty > 0 && !isTRUE(spread_reps)) {
    fieldhub_abort("`rowcol_penalty` needs the spreading optimizer; set `spread_reps = TRUE` ",
                   "or `rowcol_penalty = 0`.")
  }
  invisible(NULL)
}
