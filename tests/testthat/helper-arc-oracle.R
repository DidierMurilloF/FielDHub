# Test-only helpers for the augmented row-column engine (research M1 specification,
# section 10). Nothing here calls the engine in R/engine_augmented_row_column.R:
# the reference evaluator must stay an independent algorithm.

# A design fixture from tests/testthat/fixtures/augmented-row-column/ as an
# integer matrix. Lines starting with "#" carry provenance and are skipped.
arc_fixture <- function(name) {
  path <- test_path("fixtures", "augmented-row-column", name)
  values <- utils::read.csv(path, comment.char = "#", check.names = FALSE)
  matrix <- as.matrix(values)
  storage.mode(matrix) <- "integer"
  dimnames(matrix) <- NULL
  matrix
}

# Relative tolerance for every rank and connectivity decision (specification
# section 5). Decisions use symmetric eigenvalues only, never qr()$rank.
arc_oracle_tol <- 1e-8

# Indicator matrix: one row per element of `values`, one column per level
arc_oracle_dummies <- function(values, levels) {
  out <- outer(values, levels, `==`)
  storage.mode(out) <- "double"
  out
}

# Number of eigenvalues of a symmetric positive semidefinite matrix above
# `tol` times the largest one
arc_oracle_rank <- function(symmetric, tol = arc_oracle_tol) {
  lambda <- eigen(symmetric, symmetric = TRUE, only.values = TRUE)$values
  sum(lambda > tol * max(lambda))
}

# Column rank of a matrix: its singular values, computed directly by svd(),
# above `tol` times the largest one. Cross-product eigenvalues are not used:
# a tolerance on them drops genuine small singular values, and taking their
# square roots lifts the rounding noise of zero eigenvalues to about 1e-8
# relative, so the decision would depend on the BLAS.
arc_oracle_rank_columns <- function(matrix, tol = arc_oracle_tol) {
  singular <- svd(matrix, nu = 0L, nv = 0L)$d
  sum(singular > tol * max(singular))
}

# Moore-Penrose inverse of the information matrix of a connected design. Its
# null space is the constant vector alone, so the smallest eigenvalue is the
# one dropped; every other eigenvalue is inverted.
arc_oracle_pinv <- function(C) {
  decomposition <- eigen(C, symmetric = TRUE)
  keep <- seq_len(ncol(C) - 1) # eigen() sorts eigenvalues in decreasing order
  vectors <- decomposition$vectors[, keep, drop = FALSE]
  vectors %*% (t(vectors) / decomposition$values[keep])
}

# Mean and maximum of the upper-triangle entries of a pairwise block
arc_oracle_pair_summary <- function(pairwise, first, second) {
  block <- pairwise[first, second, drop = FALSE]
  if (identical(first, second)) {
    values <- block[upper.tri(block)]
  } else {
    values <- as.vector(block)
  }
  c(mean = mean(values), max = max(values))
}

# Independent reference evaluator for a field layout (specification section
# 10.3). `codes` is a field-rows x field-columns integer matrix of treatment
# codes. Checks are `check_codes`, or, when NULL, the codes occurring more than
# once; every other code is a test and must occur exactly once.
#
# Algorithm: residualize each plot-level treatment indicator column against
# [1, row dummies, column dummies] with qr.resid(), so C = X'R. This differs
# from the engine's projection formula on purpose. QR computes residuals only.
#
# Returns connected and residual_df_rank always; E_aug, V_tt, V_tc, V_cc,
# max_tt and max_tc only for a connected layout (a disconnected one never gets
# objectives, section 5); and the v* x v* pairwise-variance matrix, named by
# treatment code in increasing order, when v* <= 400.
arc_oracle <- function(codes, check_codes = NULL) {
  stopifnot(is.matrix(codes), is.numeric(codes), !anyNA(codes))
  plots <- as.vector(codes) # column-major: plot p is in row rows[p], column cols[p]
  n <- length(plots)
  rows <- as.vector(row(codes))
  cols <- as.vector(col(codes))

  treatments <- sort(unique(plots))
  counts <- tabulate(match(plots, treatments), nbins = length(treatments))
  if (is.null(check_codes)) {
    check_codes <- treatments[counts > 1]
  }
  is_check <- treatments %in% check_codes
  stopifnot(all(check_codes %in% treatments), all(counts[!is_check] == 1), sum(is_check) >= 2)
  n_treatments <- length(treatments)

  # Plot-level design matrices
  X <- arc_oracle_dummies(plots, treatments)
  B <- cbind(1, arc_oracle_dummies(rows, seq_len(nrow(codes))),
             arc_oracle_dummies(cols, seq_len(ncol(codes))))

  # Information matrix from residualized treatment columns
  R <- qr.resid(qr(B), X)
  C <- crossprod(X, R)
  C <- (C + t(C)) / 2

  # Scaled information matrix and its canonical efficiency factors
  u <- colSums(X)
  scale <- 1 / sqrt(u)
  A_star <- C * outer(scale, scale)
  lambda <- eigen(A_star, symmetric = TRUE, only.values = TRUE)$values
  connected <- sum(lambda < arc_oracle_tol * max(lambda)) == 1

  # Rank of [X B] from its singular values (square roots of the cross-product
  # eigenvalues). Applying the tolerance to the squared values would drop
  # genuine small singular values: in the false-QR fixture one is 2.3e-5 of
  # the largest, so its square falls below 1e-8.
  residual_df_rank <- n - arc_oracle_rank_columns(cbind(X, B))
  result <- list(connected = connected, residual_df_rank = residual_df_rank)
  if (!connected) {
    return(result)
  }

  # The v* - 1 nontrivial eigenvalues (the trivial one, for sqrt(u), is last)
  efficiencies <- lambda[seq_len(n_treatments - 1)]
  E_aug <- (n_treatments - 1) / sum(1 / efficiencies)

  # Var(tau_i - tau_j) = Omega_ii + Omega_jj - 2 Omega_ij
  Omega <- arc_oracle_pinv(C)
  diagonal <- diag(Omega)
  pairwise <- outer(diagonal, diagonal, `+`) - 2 * Omega
  dimnames(pairwise) <- list(treatments, treatments)

  tests <- which(!is_check)
  checks <- which(is_check)
  tt <- arc_oracle_pair_summary(pairwise, tests, tests)
  tc <- arc_oracle_pair_summary(pairwise, tests, checks)
  cc <- arc_oracle_pair_summary(pairwise, checks, checks)

  result <- c(result, list(
    E_aug = E_aug,
    V_tt = unname(tt["mean"]),
    V_tc = unname(tc["mean"]),
    V_cc = unname(cc["mean"]),
    max_tt = unname(tt["max"]),
    max_tc = unname(tc["max"])
  ))
  if (n_treatments <= 400) {
    result$pairwise <- pairwise
  }
  result
}
