test_that("vectorized pairs_distance() is identical to the per-entry reference", {
  skip_on_cran()
  set.seed(20261105)
  for (t in seq_len(500)) {
    nr <- sample.int(15L, 1L)
    nc <- sample.int(15L, 1L)
    n <- nr * nc
    X <- matrix(as.numeric(sample(c(-3:40, 1e5), n, replace = TRUE)), nr)
    if (runif(1) < 0.4) X[sample.int(n, sample.int(n + 1L, 1L) - 1L)] <- NA
    if (runif(1) < 0.2) storage.mode(X) <- "integer"
    method <- sample(c("euclidean", "manhattan"), 1L)
    expected <- tryCatch(swap_ref_pairs_distance(X, method), error = conditionMessage)
    actual <- tryCatch(pairs_distance(X, method), error = conditionMessage)
    expect_identical(actual, expected, label = paste("case", t))
  }
})

test_that(".duplicate_pairs() lists pairs per entry in increasing value and combn() order", {
  X <- matrix(c(7, 3, 7, 3, 9, 7, 3, 1), 2)
  p <- .duplicate_pairs(X)
  expect_identical(p$geno, c(3L, 3L, 3L, 7L, 7L, 7L))
  expect_identical(p$p1, c(2L, 2L, 4L, 1L, 1L, 3L))
  expect_identical(p$p2, c(4L, 7L, 7L, 3L, 6L, 6L))
  expect_null(.duplicate_pairs(matrix(c(1, 2, NA, 4), 2)))
})
