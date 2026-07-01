#' @title Calculate pairwise distances between all elements in a matrix that appears twice or more.
#'
#' @description Given a matrix of integers, this function calculates the pairwise Euclidean
#' distance between all possible pairs of elements in the matrix that appear two or more times.
#' If no element appears two or more times, the function will return an error message.
#'
#'
#' @param X a matrix of integers
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
pairs_distance <- function(X) {
  if (!is.matrix(X)) stop("Input must be a matrix")
  if (!is.numeric(X)) stop("Matrix elements must be numeric")

  nr <- nrow(X)
  tab <- table(as.vector(X))
  dupsI <- as.integer(names(tab)[tab > 1L])
  if (length(dupsI) == 0L) stop("All elements in X appear only once")

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
    dr <- rA - rB
    dc <- cA - cB
    out_list[[i]] <- data.frame(
      geno = rep.int(g, length(dr)),
      Pos1 = p1, Pos2 = p2, DIST = sqrt(dr * dr + dc * dc),
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
.pair_dists_for_geno <- function(mat, g) {
  pos <- which(mat == g, arr.ind = TRUE)
  if (nrow(pos) < 2L) {
    return(numeric(0))
  }
  pairs <- utils::combn(seq_len(nrow(pos)), 2L)
  dr <- pos[pairs[1L, ], 1L] - pos[pairs[2L, ], 1L]
  dc <- pos[pairs[1L, ], 2L] - pos[pairs[2L, ], 2L]
  sqrt(dr * dr + dc * dc)
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
                        lambda, center,
                        base_sum, n_pairs) {
  g_i <- X[ri, ci]
  g_j <- X[rj, cj]

  # old pairwise-distance sums for the two affected genotypes
  old_i <- .pair_dists_for_geno(X, g_i)
  old_j <- if (g_j != g_i) .pair_dists_for_geno(X, g_j) else numeric(0)
  old_contrib <- sum(old_i) + sum(old_j)

  # apply swap on a temp copy
  X_tmp <- X
  X_tmp[ri, ci] <- g_j
  X_tmp[rj, cj] <- g_i

  # new pairwise-distance sums for the same genotypes
  new_i <- .pair_dists_for_geno(X_tmp, g_i)
  new_j <- if (g_j != g_i) .pair_dists_for_geno(X_tmp, g_j) else numeric(0)
  new_contrib <- sum(new_i) + sum(new_j)

  delta <- new_contrib - old_contrib

  # incrementally updated global mean
  adjusted_mean <- (base_sum + delta) / max(n_pairs, 1L)

  # border penalty: penalise far-from-center (= near-border) placement
  candidate_center_dist <- sqrt((rj - center[1L])^2 + (cj - center[2L])^2)

  list(
    score = adjusted_mean - lambda * candidate_center_dist,
    delta = delta
  )
}


#' @title Swap pairs in a matrix of integers
#'
#' @description Modifies the input matrix \code{X} to ensure that the distance between any two occurrences
#' of the same integer is at least a distance \code{d}, by swapping one of the occurrences with a
#' candidate cell of a different integer. The function starts with \code{starting_dist = 3} and increases it
#' by \code{1} until the algorithm no longer converges or \code{stop_iter} iterations have been performed.
#' This version evaluates candidate swaps using both the mean pairwise distance and a centrality penalty,
#' and it uses candidate sampling to reduce computation.
#'
#' @param X A matrix of integers.
#' @param starting_dist The minimum starting distance to enforce between pairs of occurrences of the same integer. Default is 3.
#' @param stop_iter The maximum number of iterations to perform. Default is 50.
#' @param lambda A tuning parameter for the centrality penalty. Default is 0.1.
#' @param dist_method The method used for distance calculation. Options are "euclidean" (default) and "manhattan".
#' @param candidate_sample_size Maximum number of candidate cells to evaluate per swap. Default is 5.
#'
#' @return A list containing:
#' \item{optim_design}{The modified matrix.}
#' \item{designs}{A list of all intermediate designs, starting from the input matrix.}
#' \item{distances}{A list of all pair distances for each intermediate design.}
#' \item{min_distance}{The minimum distance between pairs of occurrences of the same integer in the final design.}
#' \item{pairwise_distance}{A data frame with the pairwise distances for the final design.}
#' \item{rows_incidence}{A vector recording the number of rows with repeated integers for each iteration.}
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
#'   candidate_sample_size = 3
#' )
#' B$optim_design
#'
#' @export
swap_pairs <- function(X,
                       starting_dist = 3,
                       stop_iter = 10,
                       lambda = 0.5,
                       dist_method = "euclidean",
                       candidate_sample_size = 4) {
  if (!is.matrix(X)) stop("Input must be a matrix")
  if (!is.numeric(X)) stop("Matrix elements must be numeric")

  input_X <- X
  input_freq <- table(input_X)
  nr <- nrow(X)
  nc <- ncol(X)
  minDist <- sqrt(nr^2 + nc^2)
  center <- c(nr / 2, nc / 2)

  dist_fn <- if (dist_method == "euclidean") {
    .vec_dist_euclidean
  } else if (dist_method == "manhattan") {
    .vec_dist_manhattan
  } else {
    stop("Invalid dist_method. Use 'euclidean' or 'manhattan'.")
  }

  swap_succeed <- FALSE
  designs <- list(X)
  init_pd <- pairs_distance(X)
  distances <- list(init_pd)
  rows_incidence <- numeric()
  genos <- unique(init_pd$geno)
  w <- 2L

  # Guard against a reversed/empty threshold range: on very small fields the
  # field diagonal (minDist) can be shorter than starting_dist, which would make
  # seq(starting_dist, minDist, 1) error with "wrong sign in 'by' argument".
  dist_seq <- if (minDist >= starting_dist) seq(starting_dist, minDist, 1) else numeric(0)

  # ------------------------------------------------------------------ #
  #  Main loop over increasing minimum-distance thresholds              #
  # ------------------------------------------------------------------ #
  for (min_dist in dist_seq) {
    n_iter <- 1L

    while (n_iter <= stop_iter) {
      # ---- (A) pairs_distance() called ONCE per while-iteration ---------
      plotDist <- pairs_distance(X)
      LowID <- which(plotDist$DIST < min_dist)
      if (length(LowID) == 0L) {
        n_iter <- stop_iter + 1L
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

        other_mask <- X != genotype
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
              lambda, center, base_sum, n_pairs
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
    }

    # ---- Did we satisfy the current min_dist threshold? -----------------
    current_min <- min(pairs_distance(X)$DIST)
    if (current_min < min_dist) {
      break
    } else {
      swap_succeed <- TRUE

      output_freq <- table(X)
      if (!all(input_freq == output_freq)) {
        stop("swap_pairs_fast changed the frequency of some integers.")
      }

      rows_incidence[w - 1L] <- sum(apply(X, 1L, function(row) {
        any(tabulate(match(row, genos)) >= 2L)
      }))

      designs[[w]] <- X
      distances[[w]] <- pairs_distance(X)
      w <- w + 1L
    }
  }

  # ---- Assemble output (identical structure to original swap_pairs) ----
  optim_design <- designs[[length(designs)]]
  pairwise_distance <- pairs_distance(optim_design)
  min_distance <- min(pairwise_distance$DIST)

  if (!swap_succeed) {
    optim_design <- designs[[1L]]
    pairwise_distance <- pairs_distance(optim_design)
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
    pairwise_distance = pairwise_distance
  )
}


#' @title Search Matrix Values
#'
#' @description Search for values in a matrix and return the row number, value, and frequency.
#'
#' @author Jean-Marc Montpetit [aut], Didier Murillo [aut]
#'
#' @param X A matrix.
#' @param values_search A vector of values to search for in the matrix.
#' @return A data frame with three columns: Row (the row number where the value is found),
#' Value (the searched value), and Times (the frequency of the searched value in the row).
#' @examples
#' A <- matrix(c(1, 2, 3, 2, 3, 4, 3, 4, 5), nrow = 3, byrow = TRUE)
#' search_matrix_values(X = A, values_search = c(2, 3, 5))
#' @noRd
search_matrix_values <- function(X, values_search) {
  # Initialize an empty list to store the results
  result <- list()
  # Loop through each row of X
  for (i in 1:nrow(X)) {
    # Get the unique values and their frequency in the current row
    row_vals <- unique(X[i, ])
    row_counts <- tabulate(match(X[i, ], row_vals))
    # Find the values that are in the search list
    search_vals <- row_vals[row_vals %in% values_search]
    # XAd the row number, search values, and their frequency to the result list
    for (val in search_vals) {
      freq <- sum(X[i, ] == val)
      result[[length(result) + 1]] <- c(i, val, freq)
    }
  }
  # Convert the result list to a data frame
  result_df <- do.call(rbind, result)
  colnames(result_df) <- c("Row", "Value", "Times")
  # Return the final data frame
  return(result_df)
}
