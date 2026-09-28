#' Check pair-swap labels before their integer representation is used
#' @noRd
validate_swap_matrix <- function(X) {
  if (!is.matrix(X)) fieldhub_abort("Input must be a matrix")
  if (!is.numeric(X) || is.complex(X)) fieldhub_abort("Matrix elements must be numeric")
  active <- X[!is.na(X)]
  if (length(active) == 0L) fieldhub_abort("X must contain at least one active cell")
  if (any(!is.finite(active)) || any(active %% 1 != 0) ||
      any(abs(active) > .Machine$integer.max)) {
    fieldhub_abort("Active matrix entries must be finite whole numbers within R's integer range.")
  }
  invisible(NULL)
}

#' Select a validated coordinate distance function
#' @noRd
swap_distance_function <- function(dist_method) {
  if (!is.character(dist_method) || length(dist_method) != 1L || is.na(dist_method) ||
      !dist_method %in% c("euclidean", "manhattan")) {
    fieldhub_abort("Invalid dist_method. Use 'euclidean' or 'manhattan'.")
  }
  if (dist_method == "euclidean") .vec_dist_euclidean else .vec_dist_manhattan
}

#' Check finite search controls before evaluating a pair-swap distance range
#' @noRd
validate_swap_controls <- function(starting_dist, stop_iter, lambda,
                                    dist_method, candidate_sample_size) {
  controls <- list(starting_dist = starting_dist, stop_iter = stop_iter,
                   lambda = lambda, candidate_sample_size = candidate_sample_size)
  for (name in names(controls)) {
    value <- controls[[name]]
    minimum <- if (name == "candidate_sample_size") 1 else 0
    if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
        length(value) != 1L || !is.finite(value) || value < minimum) {
      fieldhub_abort("`", name, "` must be one finite number at least ", minimum, ".")
    }
    if (name %in% c("stop_iter", "candidate_sample_size")) {
      validate_iteration_budget(value, name, minimum)
    }
  }
  swap_distance_function(dist_method)
  invisible(NULL)
}

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
