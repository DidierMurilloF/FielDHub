#' @title Calculate pairwise distances between all elements in a matrix that appears twice or more.
#'
#' @description Given a matrix of integers, this function calculates pairwise
#' distance between all possible pairs of elements in the matrix that appear two or more times.
#' If no element appears two or more times, the function will return an error message.
#'
#'
#' @param X a matrix of integers
#' @param dist_method Coordinate distance: "euclidean" or "manhattan".
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{geno}: the integer value for which the pairwise distances are calculated
#'   \item \code{Pos1}: the row index of the first element in the pair
#'   \item \code{Pos2}: the row index of the second element in the pair
#'   \item \code{DIST}: the Euclidean distance between the two elements in the pair
#'   \item \code{rA}: the row index of the first element in the pair
#'   \item \code{cA}: the column index of the first element in the pair
#'   \item \code{rB}: the row index of the second element in the pair
#'   \item \code{cB}: the column index of the second element in the pair
#' }
#'
#' @author Jean-Marc Montpetit [aut]
#'
#' @noRd
pairs_distance <- function(X, dist_method = "euclidean") {
  validate_swap_matrix(X)
  dist_fn <- swap_distance_function(dist_method)

  nr <- nrow(X)
  # NA cells are inactive field positions and must not enter the distance
  # calculations as an artificial replicated treatment.
  tab <- table(as.vector(X))
  dupsI <- as.integer(names(tab)[tab > 1L])
  if (length(dupsI) == 0L) fieldhub_abort("All elements in X appear only once")

  out_list <- vector("list", length(dupsI))
  for (i in seq_along(dupsI)) {
    g <- dupsI[i]
    id <- which(X == g)
    pairs <- utils::combn(id, 2)
    p1 <- pairs[1L, ]
    p2 <- pairs[2L, ]
    rA <- ((p1 - 1L) %% nr) + 1L
    cA <- ((p1 - 1L) %/% nr) + 1L
    rB <- ((p2 - 1L) %% nr) + 1L
    cB <- ((p2 - 1L) %/% nr) + 1L
    distances <- as.numeric(dist_fn(rA, cA, rB, cB))
    out_list[[i]] <- data.frame(
      geno = rep.int(g, length(distances)),
      Pos1 = p1, Pos2 = p2, DIST = distances,
      rA = rA, cA = cA, rB = rB, cB = cB
    )
  }
  plotDist <- do.call(rbind, out_list)
  plotDist <- plotDist[order(plotDist$DIST), ]
  rownames(plotDist) <- NULL
  plotDist
}

# ============================================================
#  Internal helpers
# ============================================================
#' @noRd
.vec_dist_euclidean <- function(r0, c0, rmat, cmat) {
  sqrt((rmat - r0)^2 + (cmat - c0)^2)
}

#' @noRd
.vec_dist_manhattan <- function(r0, c0, rmat, cmat) {
  abs(rmat - r0) + abs(cmat - c0)
}

#' @noRd
# All pairwise distances for ONE genotype in matrix mat
.pair_dists_for_geno <- function(mat, g, dist_method = "euclidean") {
  pos <- which(mat == g, arr.ind = TRUE)
  if (nrow(pos) < 2L) {
    return(numeric(0))
  }
  pairs <- utils::combn(seq_len(nrow(pos)), 2L)
  dr <- pos[pairs[1L, ], 1L] - pos[pairs[2L, ], 1L]
  dc <- pos[pairs[1L, ], 2L] - pos[pairs[2L, ], 2L]
  # Exponentiation promotes integer coordinates before squaring, avoiding
  # overflow for distant plots while preserving ordinary-field distances.
  if (dist_method == "manhattan") abs(dr) + abs(dc) else sqrt(dr^2 + dc^2)
}

# ---- Score a candidate swap ----------------------------------------------------
#
# Returns a list(score, delta):
#
#   score : adjusted_global_mean - lambda * candidate_center_dist  (maximise)
#   delta : new_contrib - old_contrib
#
# The DELTA is the key optimisation here. After the best swap is applied the
# caller updates base_sum as:
#
#     base_sum <- base_sum + delta
#
# This is pure arithmetic — no pairs_distance() call, no allocations.
# n_pairs never changes (swapping cells doesn't add/remove pairs).
#
# Border penalisation is identical to the previous version:
#   large candidate_center_dist  =>  candidate near border  =>  lower score
#
#' @noRd
.score_swap <- function(X, ri, ci, rj, cj,
                        lambda, center, base_sum, n_pairs,
                        dist_method = "euclidean") {
  g_i <- X[ri, ci]
  g_j <- X[rj, cj]

  # old pairwise-distance sums for the two affected genotypes
  old_i <- .pair_dists_for_geno(X, g_i, dist_method)
  old_j <- if (g_j != g_i) .pair_dists_for_geno(X, g_j, dist_method) else numeric(0)
  old_contrib <- sum(old_i) + sum(old_j)

  # apply swap on a temp copy
  X_tmp <- X
  X_tmp[ri, ci] <- g_j
  X_tmp[rj, cj] <- g_i

  # new pairwise-distance sums for the same genotypes
  new_i <- .pair_dists_for_geno(X_tmp, g_i, dist_method)
  new_j <- if (g_j != g_i) .pair_dists_for_geno(X_tmp, g_j, dist_method) else numeric(0)
  new_contrib <- sum(new_i) + sum(new_j)

  delta <- new_contrib - old_contrib

  # incrementally updated global mean
  adjusted_mean <- (base_sum + delta) / max(n_pairs, 1L)

  # Use the selected metric for the centrality penalty as well.
  candidate_center_dist <- if (dist_method == "manhattan") {
    abs(rj - center[1L]) + abs(cj - center[2L])
  } else sqrt((rj - center[1L])^2 + (cj - center[2L])^2)

  list(
    score = adjusted_mean - lambda * candidate_center_dist,
    delta = delta
  )
}


#' @title Swap pairs in a matrix of integers
#'
#' @description Attempts to separate repeated entries by swapping cells of
#' different entries. Distance thresholds increase from \code{starting_dist}
#' in steps of one. Candidate sampling, mean pairwise distance, and a centrality
#' penalty guide the search. The last successful threshold layout is returned;
#' if none succeeds, the original matrix is returned. A requested minimum
#' distance is not guaranteed.
#'
#' @param X A numeric matrix of whole-number entry identifiers within R's
#' integer range. Missing cells are inactive positions and never move.
#' @param starting_dist First distance threshold; a finite nonnegative number.
#' Default is 3.
#' @param stop_iter Maximum complete swap sweeps per threshold, as a
#' nonnegative whole number within R's integer range. Default is 10.
#' @param lambda Finite nonnegative weight for the centrality penalty.
#' Default is 0.5.
#' @param dist_method Coordinate distance used for candidate filtering,
#' scoring, stopping, and reported distances: "euclidean" (default) or "manhattan".
#' @param candidate_sample_size Maximum candidates evaluated per swap, as a
#' positive whole number within R's integer range. Default is 4.
#' @param seed Optional randomization seed. When omitted, one integer is
#' drawn from the current random-number stream and recorded; the swap's own
#' randomization does not change the caller's stream.
#'
#' @details The finite threshold range is bounded by the field geometry. For
#' Euclidean distance without missing cells, the historical bound is
#' \code{sqrt(nrow(X)^2 + ncol(X)^2)}; with missing cells it is the maximum
#' distance between active positions. For Manhattan distance it is the maximum
#' Manhattan distance between active positions. There are at most
#' \code{floor(bound - starting_dist) + 1} thresholds when the bound is at least
#' \code{starting_dist}, and none otherwise. Each threshold permits at most
#' \code{stop_iter} complete sweeps. Diagnostics distinguish an exhausted
#' threshold budget from completing or skipping the distance range.
#'
#' Standalone calls use the shared seed contract. To reproduce a previous
#' \code{set.seed(s); swap_pairs(X)} call, with \code{X} already constructed,
#' pass \code{seed = s}. Calls without a seed now select an automatic seed,
#' so their layouts can differ from previous versions. Optimization performed
#' inside field-design engines retains its existing draw sequence.
#' Use \code{reproduce_design()} to replay a recorded optimization result.
#'
#' @return A list containing:
#' \item{optim_design}{The modified matrix.}
#' \item{designs}{A list of all intermediate designs, starting from the input matrix.}
#' \item{distances}{A list of all pair distances for each intermediate design.}
#' \item{min_distance}{The minimum distance between pairs of occurrences of the same integer in the final design.}
#' \item{pairwise_distance}{A data frame with the pairwise distances for the final design.}
#' \item{rows_incidence}{Row-repetition counts for retained threshold steps,
#' or for the original matrix when no step succeeds.}
#' \item{diagnostics}{The distance metric, stop reason, total completed sweeps
#' (\code{iterations}), per-threshold budget, number of attempted thresholds,
#' last threshold, last attempted minimum distance, and retained minimum distance.
#' Stop reasons are \code{iteration_limit}, \code{distance_range_complete},
#' and \code{no_distance_thresholds}. A failed attempt is not retained in
#' \code{optim_design}.}
#' \item{metadata}{The design identifier, schema and package versions, RNG
#' settings, resolved seed and complete input parameters, including \code{X}.
#' The result inherits from \code{fieldhub_optimization}.}
#'
#' @examples
#' set.seed(123)
#' X <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
#' B <- swap_pairs(
#'   X,
#'   starting_dist = 3,
#'   stop_iter = 50,
#'   lambda = 0.5,
#'   dist_method = "euclidean",
#'   candidate_sample_size = 3,
#'   seed = 123
#' )
#' B$optim_design
#'
#' @export
swap_pairs <- function(X,
                       starting_dist = 3,
                       stop_iter = 10,
                       lambda = 0.5,
                       dist_method = "euclidean",
                       candidate_sample_size = 4,
                       seed = NULL) {
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  parameters <- list(X = X, starting_dist = starting_dist, stop_iter = stop_iter,
                      lambda = lambda, dist_method = dist_method,
                      candidate_sample_size = candidate_sample_size, seed = seed)
  result <- swap_pairs_core(X, starting_dist, stop_iter, lambda, dist_method, candidate_sample_size)
  new_fieldhub_optimization(result, parameters)
}

#' Pair-swap worker consuming the surrounding design's established RNG stream
#' @noRd
swap_pairs_core <- function(X, starting_dist = 3, stop_iter = 10, lambda = 0.5,
                             dist_method = "euclidean", candidate_sample_size = 4) {
  validate_swap_matrix(X)
  validate_swap_controls(starting_dist, stop_iter, lambda, dist_method, candidate_sample_size)

  input_X <- X
  input_freq <- table(input_X)
  nr <- nrow(X)
  nc <- ncol(X)
  active_pos <- which(!is.na(X), arr.ind = TRUE)
  if (nrow(active_pos) == 0L) fieldhub_abort("X must contain at least one active cell")

  if (anyNA(X)) {
    center <- colMeans(active_pos)
  } else {
    center <- c(nr / 2, nc / 2)
  }
  if (dist_method == "manhattan") {
    # The maximum L1 distance is a range of row + column or row - column.
    minDist <- max(diff(range(active_pos[, 1L] + active_pos[, 2L])),
                   diff(range(active_pos[, 1L] - active_pos[, 2L])))
  } else if (anyNA(X)) {
    minDist <- if (nrow(active_pos) > 1L) max(stats::dist(active_pos)) else 0
  } else {
    # Preserve the historical Euclidean no-filler calculation exactly.
    minDist <- sqrt(nr^2 + nc^2)
  }
  dist_fn <- swap_distance_function(dist_method)

  swap_succeed <- FALSE
  designs <- list(X)
  init_pd <- pairs_distance(X, dist_method)
  distances <- list(init_pd)
  rows_incidence <- numeric()
  genos <- unique(init_pd$geno)
  w <- 2L

  # Guard against a reversed/empty threshold range: on very small fields the
  # field diagonal (minDist) can be shorter than starting_dist, which would make
  # seq(starting_dist, minDist, 1) error with "wrong sign in 'by' argument".
  dist_seq <- if (minDist >= starting_dist) seq(starting_dist, minDist, 1) else numeric(0)
  iterations <- 0
  thresholds_attempted <- 0L
  last_threshold <- NA_real_
  last_attempt_min <- min(init_pd$DIST)
  stop_reason <- if (length(dist_seq)) "distance_range_complete" else "no_distance_thresholds"

  # ------------------------------------------------------------------ #
  #  Main loop over increasing minimum-distance thresholds              #
  # ------------------------------------------------------------------ #
  for (min_dist in dist_seq) {
    # A double counter avoids integer overflow at the largest accepted limit.
    n_iter <- 1
    thresholds_attempted <- thresholds_attempted + 1L
    last_threshold <- min_dist

    while (n_iter <= stop_iter) {
      # ---- (A) pairs_distance() called ONCE per while-iteration ---------
      plotDist <- pairs_distance(X, dist_method)
      LowID <- which(plotDist$DIST < min_dist)
      if (length(LowID) == 0L) {
        break
      }

      low_dist_gens <- unique(plotDist$geno[LowID])

      # Global sum and pair-count for incremental scoring.
      # n_pairs stays constant throughout — swapping never adds/removes pairs.
      base_sum <- sum(plotDist$DIST)
      n_pairs <- nrow(plotDist)

      # ---- (B) Resolve each violating genotype --------------------------
      for (genotype in low_dist_gens) {
        geno_rc <- which(X == genotype, arr.ind = TRUE)

        # Inactive cells are fixed geometry, not swap candidates.
        other_mask <- !is.na(X) & X != genotype
        other_r <- row(X)[other_mask]
        other_c <- col(X)[other_mask]

        for (i in seq_len(nrow(geno_rc))) {
          r0 <- geno_rc[i, 1L]
          c0 <- geno_rc[i, 2L]

          # ---- (C) Vectorised distance to every other cell --------------
          d <- dist_fn(r0, c0, other_r, other_c)
          valid <- d >= min_dist
          if (!any(valid)) next

          v_r <- other_r[valid]
          v_c <- other_c[valid]
          nv <- length(v_r)

          # ---- (D) Sample candidates ------------------------------------
          if (nv > candidate_sample_size) {
            idx <- sample.int(nv, candidate_sample_size)
            v_r <- v_r[idx]
            v_c <- v_c[idx]
            nv <- candidate_sample_size
          }

          # ---- (E) Score candidates — no pairs_distance() call ----------
          scores <- numeric(nv)
          deltas <- numeric(nv)
          for (j in seq_len(nv)) {
            res <- .score_swap(
              X, r0, c0, v_r[j], v_c[j],
              lambda, center, base_sum, n_pairs, dist_method
            )
            scores[j] <- res$score
            deltas[j] <- res$delta
          }

          # ---- (F) Apply best swap --------------------------------------
          best <- which.max(scores)
          rb <- v_r[best]
          cb <- v_c[best]
          tmp <- X[rb, cb]
          X[rb, cb] <- X[r0, c0]
          X[r0, c0] <- tmp

          # ---- (G) Update base_sum — pure arithmetic, zero allocations --
          base_sum <- base_sum + deltas[best]
          # n_pairs is unchanged; no call needed
        }
      }

      n_iter <- n_iter + 1L
      iterations <- iterations + 1
    }

    # ---- Did we satisfy the current min_dist threshold? -----------------
    current_min <- min(pairs_distance(X, dist_method)$DIST)
    last_attempt_min <- current_min
    if (current_min < min_dist) {
      stop_reason <- "iteration_limit"
      break
    } else {
      swap_succeed <- TRUE

      output_freq <- table(X)
      if (!all(input_freq == output_freq)) {
        fieldhub_abort("swap_pairs() changed the frequency of some integers.")
      }

      rows_incidence[w - 1L] <- sum(apply(X, 1L, function(row) {
        any(tabulate(match(row, genos)) >= 2L)
      }))

      designs[[w]] <- X
      distances[[w]] <- pairs_distance(X, dist_method)
      w <- w + 1L
    }
  }

  # ---- Retain the established result fields and append diagnostics ----
  optim_design <- designs[[length(designs)]]
  pairwise_distance <- pairs_distance(optim_design, dist_method)
  min_distance <- min(pairwise_distance$DIST)

  if (!swap_succeed) {
    optim_design <- designs[[1L]]
    pairwise_distance <- pairs_distance(optim_design, dist_method)
    min_distance <- min(pairwise_distance$DIST)
    rows_incidence[1L] <- sum(apply(optim_design, 1L, function(row) {
      any(tabulate(match(row, genos)) >= 2L)
    }))
    distances[[1L]] <- pairwise_distance
  }

  list(
    rows_incidence    = rows_incidence,
    optim_design      = optim_design,
    designs           = designs,
    distances         = distances,
    min_distance      = min_distance,
    pairwise_distance = pairwise_distance,
    diagnostics = list(
      distance_method = dist_method, stop_reason = stop_reason,
      iterations = iterations, max_iterations_per_threshold = stop_iter,
      thresholds_attempted = thresholds_attempted, last_threshold = last_threshold,
      last_attempt_min_distance = last_attempt_min, retained_min_distance = min_distance
    )
  )
}
