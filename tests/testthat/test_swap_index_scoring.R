# The coordinate-index scorer must equal the full-field oracles exactly.
random_swap_field <- function(nr, nc, k_max = 4L) {
  n <- nr * nc
  values <- sample(c(rep(seq_len(n %/% 3L), sample.int(k_max, n %/% 3L, replace = TRUE)), -(1:n)), n)
  X <- matrix(as.numeric(values), nr)
  if (runif(1) < 0.3) X[sample.int(n, n %/% 10L)] <- NA
  X
}

test_that(".pair_dists_from_cells() equals .pair_dists_for_geno() for every entry", {
  skip_on_cran()
  set.seed(11)
  templates <- .pair_templates(12L)
  for (t in seq_len(200)) {
    X <- random_swap_field(sample(2:12, 1L), sample(2:12, 1L))
    method <- sample(c("euclidean", "manhattan"), 1L)
    for (g in unique(X[!is.na(X)])) {
      expect_identical(unname(.pair_dists_from_cells(which(X == g), nrow(X), method, templates)),
                       unname(.pair_dists_for_geno(X, g, method)))
    }
  }
})

test_that(".score_swap_cells() equals .score_swap() on random swaps, same-entry swaps included", {
  skip_on_cran()
  set.seed(12)
  templates <- .pair_templates(12L)
  same_seen <- 0L
  for (t in seq_len(1000)) {
    X <- random_swap_field(sample(2:12, 1L), sample(2:12, 1L))
    active <- which(!is.na(X))
    if (length(active) < 2L || !anyDuplicated(X[active])) next
    ab <- active[sample.int(length(active), 2L)]
    nr <- nrow(X)
    ri <- (ab[1] - 1L) %% nr + 1L; ci <- (ab[1] - 1L) %/% nr + 1L
    rj <- (ab[2] - 1L) %% nr + 1L; cj <- (ab[2] - 1L) %/% nr + 1L
    method <- sample(c("euclidean", "manhattan"), 1L)
    center <- c(nr / 2, ncol(X) / 2)
    pd <- pairs_distance(X, method)
    same <- X[ab[1]] == X[ab[2]]
    same_seen <- same_seen + same
    expected <- .score_swap(X, ri, ci, rj, cj, 0.5, center, sum(pd$DIST), nrow(pd), method)
    actual <- .score_swap_cells(which(X == X[ab[1]]), which(X == X[ab[2]]), ab[1], ab[2], rj, cj, same,
                                0.5, center, sum(pd$DIST), nrow(pd), nr, method, templates)
    expect_identical(actual, expected)
  }
  expect_gt(same_seen, 0L)
})

test_that("index scoring does not overflow at large coordinates", {
  x <- matrix(NA_real_, 60000, 1)
  x[c(1, 60000)] <- 1
  x[c(2, 59999)] <- 2
  pd <- pairs_distance(x)
  expected <- .score_swap(x, 1L, 1L, 2L, 1L, 0.5, c(30000, 1), sum(pd$DIST), nrow(pd))
  actual <- .score_swap_cells(c(1L, 60000L), c(2L, 59999L), 1L, 2L, 2L, 1L, FALSE, 0.5, c(30000, 1),
                              sum(pd$DIST), nrow(pd), 60000L, "euclidean", .pair_templates(2L))
  expect_identical(actual, expected)
})
