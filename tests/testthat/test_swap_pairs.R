library(FielDHub)

test_that("swap_pairs() handles fields smaller than the starting distance", {
  # Regression test for the empty/reversed distance-range bug.
  # swap_pairs() looped with
  #   for (min_dist in seq(starting_dist, minDist, 1))
  # where minDist = sqrt(nr^2 + nc^2) is the field diagonal and starting_dist
  # defaults to 3. On a small field (here 2 x 2, diagonal ~2.83 < 3) the call
  # seq(3, 2.83, 1) errored with "wrong sign in 'by' argument". The fix guards
  # the range and returns the input design unchanged when no swap distance is
  # feasible.
  set.seed(1)
  X <- matrix(sample(c(1, 1, 2, 2)), nrow = 2, ncol = 2)
  res <- swap_pairs(X)
  expect_identical(res$optim_design, X)
})
