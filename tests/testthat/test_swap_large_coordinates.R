test_that("incremental pair distances do not overflow for distant plots", {
  for (transposed in c(FALSE, TRUE)) {
    x <- matrix(NA_real_, 60000, 2)
    x[cbind(c(1, 60000), c(1, 2))] <- 1
    if (transposed) x <- t(x)
    expected <- as.numeric(stats::dist(which(x == 1, arr.ind = TRUE)))
    expect_no_warning(actual <- .pair_dists_for_geno(x, 1))
    expect_equal(unname(actual), expected)
  }
})

test_that("large-coordinate swap scores match complete distance recomputation", {
  x <- matrix(NA_real_, 60000, 1)
  x[c(1, 60000)] <- 1
  x[c(2, 59999)] <- 2
  old <- pairs_distance(x)
  expected <- x
  expected[c(1, 2)] <- expected[c(2, 1)]
  revised <- pairs_distance(expected)
  center <- c(30000, 1)
  score <- .score_swap(x, 1L, 1L, 2L, 1L, 0.5, center, sum(old$DIST), nrow(old))
  expect_equal(score$delta, sum(revised$DIST) - sum(old$DIST))
  expect_equal(score$score, mean(revised$DIST) - 0.5 * sqrt(sum((c(2, 1) - center)^2)))
})
