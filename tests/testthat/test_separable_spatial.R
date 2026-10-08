test_that("separable AR1 factors match an independent dense covariance calculation", {
  for (dims in list(c(1L, 7L), c(7L, 1L), c(4L, 5L), c(5L, 4L))) {
    for (rho in list(c(0, 0), c(0.4, 0.6), c(-0.4, 0.3), c(0.999, -0.999))) {
      n <- dims[1]
      m <- dims[2]
      covariance <- kronecker(rho[2]^abs(outer(seq_len(n), seq_len(n), `-`)),
                               rho[1]^abs(outer(seq_len(m), seq_len(m), `-`)))
      innovations <- sin(seq_len(n * m))
      actual <- separable_ar1_patch(innovations, n, m, rho[1], rho[2])
      reference <- drop(t(chol(covariance)) %*% innovations)
      expect_equal(actual, reference, tolerance = 1e-10)
      basis <- diag(n * m)
      factor <- vapply(seq_len(n * m), function(i) {
        separable_ar1_patch(basis[, i], n, m, rho[1], rho[2])
      }, numeric(n * m))
      expect_equal(tcrossprod(factor), covariance, tolerance = 1e-12)
    }
  }
})

test_that("separable field factors consume no randomness and retain row-major ordering", {
  set.seed(27)
  before <- .Random.seed
  innovations <- as.numeric(1:6)
  expect_identical(separable_ar1_patch(innovations, 2, 3, 0, 0), innovations)
  expect_identical(.Random.seed, before)
  expect_identical(separable_ar1_patch(innovations, 2, 3, 0.4, 0.5),
                   separable_ar1_patch(innovations, 2, 3, 0.4, 0.5))
})

test_that("spatial simulation uses the separable factor without a dense covariance", {
  code <- all.names(body(ZST), functions = TRUE)
  expect_true("separable_ar1_patch" %in% code)
  expect_false("chol" %in% code)
  expect_false("diag" %in% code)
  set.seed(27)
  before <- .Random.seed
  result <- ZST(4, 5, 0.4, 0.5, 0.1)
  after <- .Random.seed
  assign(".Random.seed", before, globalenv())
  covariance <- kronecker(0.5^abs(outer(1:4, 1:4, `-`)),
                           0.4^abs(outer(1:5, 1:5, `-`)))
  patch <- drop(t(chol(covariance)) %*% rnorm(20))
  reference <- (patch - mean(patch)) / sd(patch) * sqrt(0.9) + rnorm(20) * sqrt(0.1)
  expect_identical(.Random.seed, after)
  expect_identical(result$ROW, rep(as.numeric(1:4), each = 5))
  expect_identical(result$COLUMN, rep(as.numeric(1:5), 4))
  expect_equal(result$ZST, reference, tolerance = 1e-12)
})

test_that("spatial factors reject invalid dimensions, correlations and innovations", {
  for (value in list(0, -1, 1.5, NA_real_, Inf, c(2, 3), "2", 2 + 1i)) {
    expect_error(separable_ar1_patch(1:6, value, 3, 0.4, 0.5),
                 class = "fieldhub_input_error")
  }
  for (value in list(-1, 1, NA_real_, Inf, c(0.2, 0.3), "0.2", 0.2 + 1i)) {
    expect_error(separable_ar1_patch(1:6, 2, 3, value, 0.5),
                 class = "fieldhub_input_error")
  }
  for (value in list(1:5, rep(NA_real_, 6), rep(Inf, 6), rep("1", 6), rep(1i, 6))) {
    expect_error(separable_ar1_patch(value, 2, 3, 0.4, 0.5),
                 class = "fieldhub_input_error")
  }
})

test_that("spatial standardization validates its grid and nugget before drawing", {
  set.seed(27)
  before <- .Random.seed
  for (value in list(NA_real_, Inf, -0.1, 1.1, c(0.1, 0.2), "0.1", 1i)) {
    expect_error(ZST(2, 3, 0.4, 0.5, value), class = "fieldhub_input_error")
    expect_identical(.Random.seed, before)
    assign(".Random.seed", before, globalenv())
  }
  expect_error(ZST(1, 1, 0.4, 0.5, 0.1), class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})
