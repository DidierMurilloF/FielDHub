# Test-only helpers for the augmented row-column design API (research M3
# specification, revision 2, section 6). They reconstruct placement and scores
# from a returned field book alone, never from the engine or the result's own
# contraction or statistics.
#
# Only base R and stats are used, plus the primitives of helper-arc-oracle.R
# (arc_oracle_tol, arc_oracle_dummies, arc_oracle_rank,
# arc_oracle_rank_columns, arc_oracle_pinv, arc_oracle_pair_summary). The
# field-book oracle is named arc_field_book_oracle(): helper-arc-oracle.R,
# sourced after this file, already defines arc_oracle(codes) for layouts.

# Lines and cross-lines of a field book, from its physical coordinates and
# CHECK_DIRECTION only. With "columns" every check appears once in each
# physical column (the lines) and the physical rows are the cross-lines; with
# "rows" the roles swap.
arc_design_lines <- function(fb) {
  direction <- unique(fb$CHECK_DIRECTION)
  stopifnot(length(direction) == 1L, direction %in% c("columns", "rows"))
  if (identical(direction, "columns")) {
    list(direction = direction, line = fb$COLUMN, cross = fb$ROW)
  } else {
    list(direction = direction, line = fb$ROW, cross = fb$COLUMN)
  }
}

# Reconstruct the check placement of a field book from ROW, COLUMN, CHECKS
# and CHECK_DIRECTION alone. Checks are the non-zero CHECKS identifiers, in
# increasing order. Returns:
# - direction, v (cross-lines), s (lines), k and check_ids;
# - cells: plots per physical cell (nrows x ncols counts);
# - by_line: k x s counts of each check in each line (the check-by-line
#   incidence);
# - by_cross: k x v counts of each check in each cross-line;
# - cross_checks: check plots in each cross-line (length v);
# - contraction: k x s, entry (h, j) the cross-line holding check h in line
#   j, or NA where by_line[h, j] is not exactly 1.
arc_reconstruct <- function(fb) {
  lines <- arc_design_lines(fb)
  nrows <- max(fb$ROW)
  ncols <- max(fb$COLUMN)
  s <- max(lines$line)
  v <- max(lines$cross)
  on_check <- fb$CHECKS != 0
  check_ids <- sort(unique(fb$CHECKS[on_check]))
  k <- length(check_ids)
  h <- match(fb$CHECKS[on_check], check_ids)
  line <- lines$line[on_check]
  cross <- lines$cross[on_check]

  counts <- function(a, b, n_a, n_b) {
    matrix(tabulate(a + (b - 1L) * n_a, nbins = n_a * n_b), n_a, n_b)
  }
  cells <- counts(fb$ROW, fb$COLUMN, nrows, ncols)
  by_line <- counts(h, line, k, s)
  by_cross <- counts(h, cross, k, v)

  contraction <- matrix(NA_integer_, k, s)
  single <- by_line[cbind(h, line)] == 1L
  contraction[cbind(h, line)[single, , drop = FALSE]] <-
    as.integer(cross[single])
  list(direction = lines$direction, v = v, s = s, k = k,
       check_ids = check_ids, cells = cells, by_line = by_line,
       by_cross = by_cross, cross_checks = as.integer(colSums(by_cross)),
       contraction = contraction)
}

# Least-squares oracle from a field book alone (M3 specification section 6,
# "Independent scoring"): the additive model
# model.matrix(~ factor(ROW) + factor(COLUMN) + factor(TREATMENT)) with
# fixed effects and independent errors, row and column factors from the
# physical coordinates and the entry factor from TREATMENT. Checks are the
# treatments on plots with non-zero CHECKS; every other treatment is a test
# and must occur once. No contraction, orientation or engine code is used.
#
# - C = X'(I - P_Z)X, with X the treatment indicators and Z = [1, row and
#   column factors], from qr.resid() residuals (QR computes residuals only,
#   never a rank; M1 section 5).
# - Residual df = n - rank([Z X]), with rank([Z X]) = rank(Z) + rank(C).
#   rank(Z) counts the symmetric eigenvalues of Z'Z above arc_oracle_tol
#   relative to the largest (arc_oracle_rank; Z'Z is well conditioned on a
#   complete grid), and rank(C) counts the eigenvalues of
#   A* = diag(u)^(-1/2) C diag(u)^(-1/2) at or above arc_oracle_tol on their
#   absolute canonical scale [0, 1] (M1 sections 5 and 7.1; a tolerance
#   relative to n on the cross-product of [Z X] would drop genuine small
#   singular values). residual_df_svd is the M1 oracle's route, n minus the
#   svd() rank of the full model matrix, for a cross-check.
# - Connected when exactly one eigenvalue of A* is below arc_oracle_tol.
# - E_aug: harmonic mean of the v* - 1 nontrivial eigenvalues of A*.
# - V_tt, V_tc, V_cc and max_tt, max_tc: means and maxima of
#   Var(tau_a - tau_b) / sigma^2 = Omega_aa + Omega_bb - 2 Omega_ab over
#   test-test, test-check and check-check pairs, with Omega the Moore-Penrose
#   inverse of C (M1 section 6).
arc_field_book_oracle <- function(fb) {
  needed <- c("ROW", "COLUMN", "CHECKS", "TREATMENT")
  stopifnot(is.data.frame(fb), all(needed %in% names(fb)),
            !anyNA(fb[needed]))
  n <- nrow(fb)
  treatments <- unique(fb$TREATMENT)
  model <- data.frame(row = factor(fb$ROW), column = factor(fb$COLUMN),
                      treatment = factor(fb$TREATMENT, levels = treatments))
  Z <- stats::model.matrix(~ row + column, data = model)
  X <- arc_oracle_dummies(fb$TREATMENT, treatments)
  full <- stats::model.matrix(~ row + column + treatment, data = model)

  check_labels <- unique(fb$TREATMENT[fb$CHECKS != 0])
  is_check <- treatments %in% check_labels
  u <- colSums(X)
  stopifnot(all(u[!is_check] == 1), all(u[is_check] > 1), sum(is_check) >= 2)

  R <- qr.resid(qr(Z), X)
  C <- crossprod(X, R)
  C <- (C + t(C)) / 2
  scale <- 1 / sqrt(u)
  lambda <- eigen(C * outer(scale, scale), symmetric = TRUE,
                  only.values = TRUE)$values
  rank_C <- sum(lambda >= arc_oracle_tol)
  rank_Z <- arc_oracle_rank(crossprod(Z))
  result <- list(
    connected = sum(lambda < arc_oracle_tol) == 1L,
    residual_df = n - rank_Z - rank_C,
    residual_df_svd = n - arc_oracle_rank_columns(full)
  )
  if (!result$connected) {
    return(result)
  }

  n_treatments <- length(treatments)
  efficiencies <- lambda[seq_len(n_treatments - 1L)] # trivial one is last
  Omega <- arc_oracle_pinv(C)
  diagonal <- diag(Omega)
  pairwise <- outer(diagonal, diagonal, `+`) - 2 * Omega
  tests <- which(!is_check)
  checks <- which(is_check)
  tt <- arc_oracle_pair_summary(pairwise, tests, tests)
  tc <- arc_oracle_pair_summary(pairwise, tests, checks)
  cc <- arc_oracle_pair_summary(pairwise, checks, checks)
  c(result, list(
    E_aug = (n_treatments - 1) / sum(1 / efficiencies),
    V_tt = unname(tt["mean"]),
    V_tc = unname(tc["mean"]),
    V_cc = unname(cc["mean"]),
    max_tt = unname(tt["max"]),
    max_tc = unname(tc["max"])
  ))
}
