test_that("Manhattan optimization reports distances in the selected metric", {
  x <- matrix(c(1, 2, 3, 1), 2)
  # No swaps are attempted, so this checks measurement, not the random search.
  out <- swap_pairs(x, starting_dist = 99, dist_method = "manhattan")
  expect_identical(out$min_distance, 2)
  expect_identical(out$pairwise_distance$DIST, 2)
  expect_identical(out$distances[[1]]$DIST, 2)
})

test_that("pair distances agree with an independent coordinate calculation", {
  x <- matrix(c(1, 2, 1, 3, 2, 1, NA, 4, 4, 5, 6, 7), 3)
  for (method in c("euclidean", "manhattan")) {
    actual <- pairs_distance(x, dist_method = method)
    expected <- unlist(lapply(c(1, 2, 4), function(entry) {
      coordinates <- which(x == entry, arr.ind = TRUE)
      as.numeric(stats::dist(coordinates, method = method))
    }))
    expect_equal(sort(actual$DIST), sort(expected))
  }
})

test_that("incremental candidate scores match independent full recomputation", {
  x <- matrix(c(1, 2, 1, 3, 2, 1, 4, 4, 5), 3)
  center <- c(1.5, 1.5)
  for (method in c("euclidean", "manhattan")) {
    before <- pairs_distance(x, dist_method = method)
    actual <- .score_swap(x, 1, 1, 2, 1, 0.25, center,
                           sum(before$DIST), nrow(before), dist_method = method)
    swapped <- x
    swapped[1, 1] <- x[2, 1]
    swapped[2, 1] <- x[1, 1]
    after <- pairs_distance(swapped, dist_method = method)
    displacement <- abs(c(2, 1) - center)
    penalty <- if (method == "manhattan") sum(displacement) else sqrt(sum(displacement^2))
    expect_equal(actual$delta, sum(after$DIST) - sum(before$DIST))
    expect_equal(actual$score, mean(after$DIST) - 0.25 * penalty)
  }
})
