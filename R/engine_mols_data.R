#' Orthogonal Latin square pairs found by FielDHub's own computer search
#'
#' Nested-engine spec section 6.7. No construction in
#' R/engine_lattice_constructions.R gives two orthogonal squares at these
#' orders (not prime powers, 2 mod 4, not 3m + 1), although pairs exist
#' (Bose, Shrikhande & Parker 1960, Canad. J. Math. 12:189-203). Each pair
#' below is the output of `tools/search-mols.R` with the command in its
#' comment, which checks that both squares are Latin and that the q^2 ordered
#' pairs (L[i, j], M[i, j]) are all different. The squares are 0-based and written
#' column by column. `mols()` and `mols_max()` read this list by order.
#' @noRd
stored_mols <- list(
  # Order 14: seed 1, method 1 (transversal exact cover), 1.12 minutes when found
  # (4.45 minutes on a rerun, 2026-11-19); the same pair is found by:
  # Rscript tools/search-mols.R 14 1 60 out.rds
  "14" = list(
    L = matrix(c(
      11L, 0L, 7L, 12L, 9L, 8L, 5L, 3L, 10L, 4L, 13L, 2L, 6L, 1L,
      2L, 13L, 6L, 1L, 11L, 10L, 8L, 12L, 0L, 9L, 7L, 4L, 3L, 5L,
      6L, 4L, 8L, 3L, 5L, 2L, 11L, 7L, 9L, 1L, 12L, 0L, 13L, 10L,
      1L, 5L, 3L, 7L, 4L, 11L, 6L, 0L, 2L, 8L, 9L, 12L, 10L, 13L,
      7L, 12L, 10L, 6L, 2L, 1L, 4L, 8L, 13L, 3L, 0L, 11L, 5L, 9L,
      5L, 2L, 4L, 11L, 13L, 3L, 10L, 1L, 12L, 7L, 6L, 9L, 0L, 8L,
      0L, 6L, 11L, 5L, 10L, 9L, 3L, 13L, 4L, 2L, 1L, 7L, 8L, 12L,
      9L, 10L, 13L, 8L, 12L, 0L, 7L, 2L, 5L, 11L, 3L, 6L, 1L, 4L,
      8L, 3L, 1L, 9L, 7L, 13L, 0L, 4L, 11L, 6L, 10L, 5L, 12L, 2L,
      4L, 8L, 12L, 0L, 3L, 6L, 2L, 10L, 1L, 5L, 11L, 13L, 9L, 7L,
      13L, 1L, 9L, 10L, 6L, 4L, 12L, 5L, 7L, 0L, 8L, 3L, 2L, 11L,
      10L, 9L, 0L, 2L, 8L, 7L, 13L, 11L, 3L, 12L, 5L, 1L, 4L, 6L,
      3L, 7L, 5L, 4L, 1L, 12L, 9L, 6L, 8L, 13L, 2L, 10L, 11L, 0L,
      12L, 11L, 2L, 13L, 0L, 5L, 1L, 9L, 6L, 10L, 4L, 8L, 7L, 3L), 14L, 14L),
    M = matrix(c(
      4L, 10L, 2L, 0L, 1L, 9L, 7L, 3L, 8L, 6L, 12L, 5L, 11L, 13L,
      6L, 3L, 4L, 11L, 2L, 10L, 0L, 5L, 1L, 8L, 13L, 12L, 7L, 9L,
      9L, 1L, 5L, 10L, 0L, 2L, 13L, 8L, 12L, 3L, 11L, 7L, 6L, 4L,
      10L, 2L, 13L, 12L, 4L, 11L, 8L, 0L, 9L, 1L, 6L, 3L, 5L, 7L,
      11L, 6L, 9L, 2L, 7L, 1L, 10L, 4L, 0L, 12L, 3L, 8L, 13L, 5L,
      8L, 12L, 0L, 6L, 10L, 5L, 3L, 2L, 4L, 7L, 1L, 13L, 9L, 11L,
      2L, 5L, 7L, 3L, 12L, 0L, 6L, 9L, 11L, 13L, 8L, 4L, 10L, 1L,
      3L, 13L, 1L, 7L, 8L, 12L, 5L, 11L, 6L, 9L, 4L, 10L, 0L, 2L,
      12L, 11L, 6L, 9L, 3L, 8L, 4L, 13L, 5L, 0L, 7L, 1L, 2L, 10L,
      5L, 8L, 12L, 13L, 9L, 3L, 1L, 6L, 7L, 11L, 10L, 2L, 4L, 0L,
      13L, 4L, 11L, 1L, 6L, 7L, 9L, 12L, 10L, 5L, 2L, 0L, 8L, 3L,
      0L, 7L, 8L, 4L, 13L, 6L, 11L, 1L, 2L, 10L, 5L, 9L, 3L, 12L,
      1L, 9L, 10L, 8L, 5L, 13L, 2L, 7L, 3L, 4L, 0L, 11L, 12L, 6L,
      7L, 0L, 3L, 5L, 11L, 4L, 12L, 10L, 13L, 2L, 9L, 6L, 1L, 8L), 14L, 14L)
  ),
  # Order 18: seed 1, method 3 (pair developed over Z_(q-1) with one fixed point).
  # Forcing method 3 finds this pair in under 1 s:
  # Rscript tools/search-mols.R 18 1 1 out.rds 3
  "18" = list(
    L = matrix(c(
      10L, 9L, 16L, 5L, 0L, 4L, 12L, 17L, 2L, 7L, 15L, 11L, 13L, 8L, 6L, 1L, 3L, 14L,
      4L, 11L, 10L, 0L, 6L, 1L, 5L, 13L, 17L, 3L, 8L, 16L, 12L, 14L, 9L, 7L, 2L, 15L,
      3L, 5L, 12L, 11L, 1L, 7L, 2L, 6L, 14L, 17L, 4L, 9L, 0L, 13L, 15L, 10L, 8L, 16L,
      9L, 4L, 6L, 13L, 12L, 2L, 8L, 3L, 7L, 15L, 17L, 5L, 10L, 1L, 14L, 16L, 11L, 0L,
      12L, 10L, 5L, 7L, 14L, 13L, 3L, 9L, 4L, 8L, 16L, 17L, 6L, 11L, 2L, 15L, 0L, 1L,
      1L, 13L, 11L, 6L, 8L, 15L, 14L, 4L, 10L, 5L, 9L, 0L, 17L, 7L, 12L, 3L, 16L, 2L,
      0L, 2L, 14L, 12L, 7L, 9L, 16L, 15L, 5L, 11L, 6L, 10L, 1L, 17L, 8L, 13L, 4L, 3L,
      5L, 1L, 3L, 15L, 13L, 8L, 10L, 0L, 16L, 6L, 12L, 7L, 11L, 2L, 17L, 9L, 14L, 4L,
      15L, 6L, 2L, 4L, 16L, 14L, 9L, 11L, 1L, 0L, 7L, 13L, 8L, 12L, 3L, 17L, 10L, 5L,
      11L, 16L, 7L, 3L, 5L, 0L, 15L, 10L, 12L, 2L, 1L, 8L, 14L, 9L, 13L, 4L, 17L, 6L,
      17L, 12L, 0L, 8L, 4L, 6L, 1L, 16L, 11L, 13L, 3L, 2L, 9L, 15L, 10L, 14L, 5L, 7L,
      6L, 17L, 13L, 1L, 9L, 5L, 7L, 2L, 0L, 12L, 14L, 4L, 3L, 10L, 16L, 11L, 15L, 8L,
      16L, 7L, 17L, 14L, 2L, 10L, 6L, 8L, 3L, 1L, 13L, 15L, 5L, 4L, 11L, 0L, 12L, 9L,
      13L, 0L, 8L, 17L, 15L, 3L, 11L, 7L, 9L, 4L, 2L, 14L, 16L, 6L, 5L, 12L, 1L, 10L,
      2L, 14L, 1L, 9L, 17L, 16L, 4L, 12L, 8L, 10L, 5L, 3L, 15L, 0L, 7L, 6L, 13L, 11L,
      14L, 3L, 15L, 2L, 10L, 17L, 0L, 5L, 13L, 9L, 11L, 6L, 4L, 16L, 1L, 8L, 7L, 12L,
      8L, 15L, 4L, 16L, 3L, 11L, 17L, 1L, 6L, 14L, 10L, 12L, 7L, 5L, 0L, 2L, 9L, 13L,
      7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 17L), 18L, 18L),
    M = matrix(c(
      5L, 13L, 0L, 14L, 8L, 2L, 12L, 7L, 9L, 1L, 3L, 10L, 15L, 4L, 16L, 17L, 6L, 11L,
      7L, 6L, 14L, 1L, 15L, 9L, 3L, 13L, 8L, 10L, 2L, 4L, 11L, 16L, 5L, 0L, 17L, 12L,
      17L, 8L, 7L, 15L, 2L, 16L, 10L, 4L, 14L, 9L, 11L, 3L, 5L, 12L, 0L, 6L, 1L, 13L,
      2L, 17L, 9L, 8L, 16L, 3L, 0L, 11L, 5L, 15L, 10L, 12L, 4L, 6L, 13L, 1L, 7L, 14L,
      8L, 3L, 17L, 10L, 9L, 0L, 4L, 1L, 12L, 6L, 16L, 11L, 13L, 5L, 7L, 14L, 2L, 15L,
      3L, 9L, 4L, 17L, 11L, 10L, 1L, 5L, 2L, 13L, 7L, 0L, 12L, 14L, 6L, 8L, 15L, 16L,
      16L, 4L, 10L, 5L, 17L, 12L, 11L, 2L, 6L, 3L, 14L, 8L, 1L, 13L, 15L, 7L, 9L, 0L,
      10L, 0L, 5L, 11L, 6L, 17L, 13L, 12L, 3L, 7L, 4L, 15L, 9L, 2L, 14L, 16L, 8L, 1L,
      9L, 11L, 1L, 6L, 12L, 7L, 17L, 14L, 13L, 4L, 8L, 5L, 16L, 10L, 3L, 15L, 0L, 2L,
      1L, 10L, 12L, 2L, 7L, 13L, 8L, 17L, 15L, 14L, 5L, 9L, 6L, 0L, 11L, 4L, 16L, 3L,
      0L, 2L, 11L, 13L, 3L, 8L, 14L, 9L, 17L, 16L, 15L, 6L, 10L, 7L, 1L, 12L, 5L, 4L,
      6L, 1L, 3L, 12L, 14L, 4L, 9L, 15L, 10L, 17L, 0L, 16L, 7L, 11L, 8L, 2L, 13L, 5L,
      14L, 7L, 2L, 4L, 13L, 15L, 5L, 10L, 16L, 11L, 17L, 1L, 0L, 8L, 12L, 9L, 3L, 6L,
      4L, 15L, 8L, 3L, 5L, 14L, 16L, 6L, 11L, 0L, 12L, 17L, 2L, 1L, 9L, 13L, 10L, 7L,
      11L, 5L, 16L, 9L, 4L, 6L, 15L, 0L, 7L, 12L, 1L, 13L, 17L, 3L, 2L, 10L, 14L, 8L,
      15L, 12L, 6L, 0L, 10L, 5L, 7L, 16L, 1L, 8L, 13L, 2L, 14L, 17L, 4L, 3L, 11L, 9L,
      12L, 16L, 13L, 7L, 1L, 11L, 6L, 8L, 0L, 2L, 9L, 14L, 3L, 15L, 17L, 5L, 4L, 10L,
      13L, 14L, 15L, 16L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 17L), 18L, 18L)
  ),
  # Order 26: seed 1, method 3 (pair developed over Z_(q-1) with one fixed point).
  # Forcing method 3 finds this pair in under 1 s:
  # Rscript tools/search-mols.R 26 1 1 out.rds 3
  "26" = list(
    L = matrix(c(
      13L, 11L, 4L, 15L, 21L, 9L, 17L, 5L, 2L, 16L, 25L, 6L, 1L, 12L, 10L, 18L, 7L, 22L, 24L, 20L, 3L, 14L, 19L, 23L, 8L, 0L,
      9L, 14L, 12L, 5L, 16L, 22L, 10L, 18L, 6L, 3L, 17L, 25L, 7L, 2L, 13L, 11L, 19L, 8L, 23L, 0L, 21L, 4L, 15L, 20L, 24L, 1L,
      0L, 10L, 15L, 13L, 6L, 17L, 23L, 11L, 19L, 7L, 4L, 18L, 25L, 8L, 3L, 14L, 12L, 20L, 9L, 24L, 1L, 22L, 5L, 16L, 21L, 2L,
      22L, 1L, 11L, 16L, 14L, 7L, 18L, 24L, 12L, 20L, 8L, 5L, 19L, 25L, 9L, 4L, 15L, 13L, 21L, 10L, 0L, 2L, 23L, 6L, 17L, 3L,
      18L, 23L, 2L, 12L, 17L, 15L, 8L, 19L, 0L, 13L, 21L, 9L, 6L, 20L, 25L, 10L, 5L, 16L, 14L, 22L, 11L, 1L, 3L, 24L, 7L, 4L,
      8L, 19L, 24L, 3L, 13L, 18L, 16L, 9L, 20L, 1L, 14L, 22L, 10L, 7L, 21L, 25L, 11L, 6L, 17L, 15L, 23L, 12L, 2L, 4L, 0L, 5L,
      1L, 9L, 20L, 0L, 4L, 14L, 19L, 17L, 10L, 21L, 2L, 15L, 23L, 11L, 8L, 22L, 25L, 12L, 7L, 18L, 16L, 24L, 13L, 3L, 5L, 6L,
      6L, 2L, 10L, 21L, 1L, 5L, 15L, 20L, 18L, 11L, 22L, 3L, 16L, 24L, 12L, 9L, 23L, 25L, 13L, 8L, 19L, 17L, 0L, 14L, 4L, 7L,
      5L, 7L, 3L, 11L, 22L, 2L, 6L, 16L, 21L, 19L, 12L, 23L, 4L, 17L, 0L, 13L, 10L, 24L, 25L, 14L, 9L, 20L, 18L, 1L, 15L, 8L,
      16L, 6L, 8L, 4L, 12L, 23L, 3L, 7L, 17L, 22L, 20L, 13L, 24L, 5L, 18L, 1L, 14L, 11L, 0L, 25L, 15L, 10L, 21L, 19L, 2L, 9L,
      3L, 17L, 7L, 9L, 5L, 13L, 24L, 4L, 8L, 18L, 23L, 21L, 14L, 0L, 6L, 19L, 2L, 15L, 12L, 1L, 25L, 16L, 11L, 22L, 20L, 10L,
      21L, 4L, 18L, 8L, 10L, 6L, 14L, 0L, 5L, 9L, 19L, 24L, 22L, 15L, 1L, 7L, 20L, 3L, 16L, 13L, 2L, 25L, 17L, 12L, 23L, 11L,
      24L, 22L, 5L, 19L, 9L, 11L, 7L, 15L, 1L, 6L, 10L, 20L, 0L, 23L, 16L, 2L, 8L, 21L, 4L, 17L, 14L, 3L, 25L, 18L, 13L, 12L,
      14L, 0L, 23L, 6L, 20L, 10L, 12L, 8L, 16L, 2L, 7L, 11L, 21L, 1L, 24L, 17L, 3L, 9L, 22L, 5L, 18L, 15L, 4L, 25L, 19L, 13L,
      20L, 15L, 1L, 24L, 7L, 21L, 11L, 13L, 9L, 17L, 3L, 8L, 12L, 22L, 2L, 0L, 18L, 4L, 10L, 23L, 6L, 19L, 16L, 5L, 25L, 14L,
      25L, 21L, 16L, 2L, 0L, 8L, 22L, 12L, 14L, 10L, 18L, 4L, 9L, 13L, 23L, 3L, 1L, 19L, 5L, 11L, 24L, 7L, 20L, 17L, 6L, 15L,
      7L, 25L, 22L, 17L, 3L, 1L, 9L, 23L, 13L, 15L, 11L, 19L, 5L, 10L, 14L, 24L, 4L, 2L, 20L, 6L, 12L, 0L, 8L, 21L, 18L, 16L,
      19L, 8L, 25L, 23L, 18L, 4L, 2L, 10L, 24L, 14L, 16L, 12L, 20L, 6L, 11L, 15L, 0L, 5L, 3L, 21L, 7L, 13L, 1L, 9L, 22L, 17L,
      23L, 20L, 9L, 25L, 24L, 19L, 5L, 3L, 11L, 0L, 15L, 17L, 13L, 21L, 7L, 12L, 16L, 1L, 6L, 4L, 22L, 8L, 14L, 2L, 10L, 18L,
      11L, 24L, 21L, 10L, 25L, 0L, 20L, 6L, 4L, 12L, 1L, 16L, 18L, 14L, 22L, 8L, 13L, 17L, 2L, 7L, 5L, 23L, 9L, 15L, 3L, 19L,
      4L, 12L, 0L, 22L, 11L, 25L, 1L, 21L, 7L, 5L, 13L, 2L, 17L, 19L, 15L, 23L, 9L, 14L, 18L, 3L, 8L, 6L, 24L, 10L, 16L, 20L,
      17L, 5L, 13L, 1L, 23L, 12L, 25L, 2L, 22L, 8L, 6L, 14L, 3L, 18L, 20L, 16L, 24L, 10L, 15L, 19L, 4L, 9L, 7L, 0L, 11L, 21L,
      12L, 18L, 6L, 14L, 2L, 24L, 13L, 25L, 3L, 23L, 9L, 7L, 15L, 4L, 19L, 21L, 17L, 0L, 11L, 16L, 20L, 5L, 10L, 8L, 1L, 22L,
      2L, 13L, 19L, 7L, 15L, 3L, 0L, 14L, 25L, 4L, 24L, 10L, 8L, 16L, 5L, 20L, 22L, 18L, 1L, 12L, 17L, 21L, 6L, 11L, 9L, 23L,
      10L, 3L, 14L, 20L, 8L, 16L, 4L, 1L, 15L, 25L, 5L, 0L, 11L, 9L, 17L, 6L, 21L, 23L, 19L, 2L, 13L, 18L, 22L, 7L, 12L, 24L,
      15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 25L), 26L, 26L),
    M = matrix(c(
      25L, 18L, 20L, 3L, 10L, 1L, 4L, 14L, 24L, 19L, 13L, 5L, 21L, 8L, 11L, 23L, 17L, 16L, 22L, 6L, 9L, 7L, 2L, 0L, 12L, 15L,
      13L, 25L, 19L, 21L, 4L, 11L, 2L, 5L, 15L, 0L, 20L, 14L, 6L, 22L, 9L, 12L, 24L, 18L, 17L, 23L, 7L, 10L, 8L, 3L, 1L, 16L,
      2L, 14L, 25L, 20L, 22L, 5L, 12L, 3L, 6L, 16L, 1L, 21L, 15L, 7L, 23L, 10L, 13L, 0L, 19L, 18L, 24L, 8L, 11L, 9L, 4L, 17L,
      5L, 3L, 15L, 25L, 21L, 23L, 6L, 13L, 4L, 7L, 17L, 2L, 22L, 16L, 8L, 24L, 11L, 14L, 1L, 20L, 19L, 0L, 9L, 12L, 10L, 18L,
      11L, 6L, 4L, 16L, 25L, 22L, 24L, 7L, 14L, 5L, 8L, 18L, 3L, 23L, 17L, 9L, 0L, 12L, 15L, 2L, 21L, 20L, 1L, 10L, 13L, 19L,
      14L, 12L, 7L, 5L, 17L, 25L, 23L, 0L, 8L, 15L, 6L, 9L, 19L, 4L, 24L, 18L, 10L, 1L, 13L, 16L, 3L, 22L, 21L, 2L, 11L, 20L,
      12L, 15L, 13L, 8L, 6L, 18L, 25L, 24L, 1L, 9L, 16L, 7L, 10L, 20L, 5L, 0L, 19L, 11L, 2L, 14L, 17L, 4L, 23L, 22L, 3L, 21L,
      4L, 13L, 16L, 14L, 9L, 7L, 19L, 25L, 0L, 2L, 10L, 17L, 8L, 11L, 21L, 6L, 1L, 20L, 12L, 3L, 15L, 18L, 5L, 24L, 23L, 22L,
      24L, 5L, 14L, 17L, 15L, 10L, 8L, 20L, 25L, 1L, 3L, 11L, 18L, 9L, 12L, 22L, 7L, 2L, 21L, 13L, 4L, 16L, 19L, 6L, 0L, 23L,
      1L, 0L, 6L, 15L, 18L, 16L, 11L, 9L, 21L, 25L, 2L, 4L, 12L, 19L, 10L, 13L, 23L, 8L, 3L, 22L, 14L, 5L, 17L, 20L, 7L, 24L,
      8L, 2L, 1L, 7L, 16L, 19L, 17L, 12L, 10L, 22L, 25L, 3L, 5L, 13L, 20L, 11L, 14L, 24L, 9L, 4L, 23L, 15L, 6L, 18L, 21L, 0L,
      22L, 9L, 3L, 2L, 8L, 17L, 20L, 18L, 13L, 11L, 23L, 25L, 4L, 6L, 14L, 21L, 12L, 15L, 0L, 10L, 5L, 24L, 16L, 7L, 19L, 1L,
      20L, 23L, 10L, 4L, 3L, 9L, 18L, 21L, 19L, 14L, 12L, 24L, 25L, 5L, 7L, 15L, 22L, 13L, 16L, 1L, 11L, 6L, 0L, 17L, 8L, 2L,
      9L, 21L, 24L, 11L, 5L, 4L, 10L, 19L, 22L, 20L, 15L, 13L, 0L, 25L, 6L, 8L, 16L, 23L, 14L, 17L, 2L, 12L, 7L, 1L, 18L, 3L,
      19L, 10L, 22L, 0L, 12L, 6L, 5L, 11L, 20L, 23L, 21L, 16L, 14L, 1L, 25L, 7L, 9L, 17L, 24L, 15L, 18L, 3L, 13L, 8L, 2L, 4L,
      3L, 20L, 11L, 23L, 1L, 13L, 7L, 6L, 12L, 21L, 24L, 22L, 17L, 15L, 2L, 25L, 8L, 10L, 18L, 0L, 16L, 19L, 4L, 14L, 9L, 5L,
      10L, 4L, 21L, 12L, 24L, 2L, 14L, 8L, 7L, 13L, 22L, 0L, 23L, 18L, 16L, 3L, 25L, 9L, 11L, 19L, 1L, 17L, 20L, 5L, 15L, 6L,
      16L, 11L, 5L, 22L, 13L, 0L, 3L, 15L, 9L, 8L, 14L, 23L, 1L, 24L, 19L, 17L, 4L, 25L, 10L, 12L, 20L, 2L, 18L, 21L, 6L, 7L,
      7L, 17L, 12L, 6L, 23L, 14L, 1L, 4L, 16L, 10L, 9L, 15L, 24L, 2L, 0L, 20L, 18L, 5L, 25L, 11L, 13L, 21L, 3L, 19L, 22L, 8L,
      23L, 8L, 18L, 13L, 7L, 24L, 15L, 2L, 5L, 17L, 11L, 10L, 16L, 0L, 3L, 1L, 21L, 19L, 6L, 25L, 12L, 14L, 22L, 4L, 20L, 9L,
      21L, 24L, 9L, 19L, 14L, 8L, 0L, 16L, 3L, 6L, 18L, 12L, 11L, 17L, 1L, 4L, 2L, 22L, 20L, 7L, 25L, 13L, 15L, 23L, 5L, 10L,
      6L, 22L, 0L, 10L, 20L, 15L, 9L, 1L, 17L, 4L, 7L, 19L, 13L, 12L, 18L, 2L, 5L, 3L, 23L, 21L, 8L, 25L, 14L, 16L, 24L, 11L,
      0L, 7L, 23L, 1L, 11L, 21L, 16L, 10L, 2L, 18L, 5L, 8L, 20L, 14L, 13L, 19L, 3L, 6L, 4L, 24L, 22L, 9L, 25L, 15L, 17L, 12L,
      18L, 1L, 8L, 24L, 2L, 12L, 22L, 17L, 11L, 3L, 19L, 6L, 9L, 21L, 15L, 14L, 20L, 4L, 7L, 5L, 0L, 23L, 10L, 25L, 16L, 13L,
      17L, 19L, 2L, 9L, 0L, 3L, 13L, 23L, 18L, 12L, 4L, 20L, 7L, 10L, 22L, 16L, 15L, 21L, 5L, 8L, 6L, 1L, 24L, 11L, 25L, 14L,
      15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 25L), 26L, 26L)
  )
)
