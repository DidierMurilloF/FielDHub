library(FielDHub)

test_that("square_lattice() rejects k that is not sqrt(t)", {
  # Regression test (B3): square_lattice() only checked that t is a square
  # number, not that k == sqrt(t). With t = 64, k = 4 the call silently produced
  # a malformed design (128 rows, wrong lambda) instead of requiring
  # k = sqrt(64) = 8. The guard now rejects k != sqrt(t).
  expect_error(square_lattice(t = 64, k = 4, r = 2, seed = 1), "k to equal")
})

test_that("square_lattice() still accepts k = sqrt(t)", {
  sq <- square_lattice(t = 64, k = 8, r = 2, seed = 1)
  expect_s3_class(sq, "FielDHub")
})
