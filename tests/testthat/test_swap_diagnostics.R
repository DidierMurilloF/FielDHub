test_that("pair-swap diagnostics explain a budget stop without accepting a failed layout", {
  x <- matrix(c(rep(1:2, 2), 3:10), 3)
  out <- swap_pairs(x, starting_dist = 3, stop_iter = 0)
  expect_identical(out$optim_design, x)
  expect_identical(out$diagnostics$stop_reason, "iteration_limit")
  expect_identical(out$diagnostics$iterations, 0)
  expect_identical(out$diagnostics$thresholds_attempted, 1L)
  expect_identical(out$diagnostics$last_threshold, 3)
  expect_identical(out$diagnostics$retained_min_distance, out$min_distance)
  expect_lt(out$diagnostics$last_attempt_min_distance, out$diagnostics$last_threshold)
})

test_that("pair-swap diagnostics distinguish empty and completed distance ranges", {
  x <- matrix(c(1, 2, 3, 1), 2)
  empty <- swap_pairs(x, starting_dist = 99)
  expect_identical(empty$diagnostics$stop_reason, "no_distance_thresholds")
  expect_identical(empty$diagnostics$iterations, 0)
  expect_identical(empty$diagnostics$thresholds_attempted, 0L)
  expect_true(is.na(empty$diagnostics$last_threshold))
  complete <- swap_pairs(x, starting_dist = 1, stop_iter = 0, dist_method = "manhattan")
  expect_identical(complete$diagnostics$stop_reason, "distance_range_complete")
  expect_identical(complete$diagnostics$distance_method, "manhattan")
  expect_identical(complete$diagnostics$thresholds_attempted, 2L)
  expect_identical(complete$diagnostics$iterations, 0)
})

test_that("pair-swap work stays within the recorded per-threshold budget", {
  x <- matrix(c(rep(1:3, 2), 4:13), 4)
  for (method in c("euclidean", "manhattan")) {
    set.seed(47)
    out <- swap_pairs(x, starting_dist = 2, stop_iter = 3, dist_method = method)
    info <- out$diagnostics
    expect_identical(info$max_iterations_per_threshold, 3)
    expect_lte(info$iterations, info$max_iterations_per_threshold * info$thresholds_attempted)
    expect_true(info$stop_reason %in% c("iteration_limit", "distance_range_complete"))
    expect_identical(sort(out$optim_design), sort(x))
  }
})
