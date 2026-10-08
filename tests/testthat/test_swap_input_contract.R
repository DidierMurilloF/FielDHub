test_that("pair-swap searches require finite bounded controls", {
  # Starting distance 3 exceeds this field's diagonal, so the old code cannot
  # enter its search even when testing an infinite iteration budget.
  x <- matrix(c(1, 2, 3, 1), 2)
  for (value in list(Inf, NA_real_, -1, 1.5, c(1, 2), "10", .Machine$integer.max + 1)) {
    expect_error(swap_pairs(x, stop_iter = value), class = "fieldhub_input_error")
  }
  for (value in list(Inf, NA_real_, -1, c(1, 2), "3")) {
    expect_error(swap_pairs(x, starting_dist = value), class = "fieldhub_input_error")
  }
  for (value in list(Inf, NA_real_, -1, c(1, 2), "0.5")) {
    expect_error(swap_pairs(x, lambda = value), class = "fieldhub_input_error")
  }
  for (value in list(Inf, NA_real_, 0, -1, 1.5, c(1, 2), "4", .Machine$integer.max + 1)) {
    expect_error(swap_pairs(x, candidate_sample_size = value), class = "fieldhub_input_error")
  }
  for (value in list(NULL, NA_character_, c("euclidean", "manhattan"), 1)) {
    expect_error(swap_pairs(x, dist_method = value), class = "fieldhub_input_error")
  }
  expect_error(swap_pairs(matrix(c(1, 1, 2, Inf), 2)), class = "fieldhub_input_error")
  for (value in c(1.5, .Machine$integer.max + 1)) {
    expect_error(swap_pairs(matrix(c(value, value, 1, 2), 2)), class = "fieldhub_input_error")
  }
})

test_that("valid zero-iteration and fractional-distance searches preserve entries", {
  x <- matrix(c(1, 2, 3, 1, 2, NA), 2)
  expect_identical(swap_pairs(x, stop_iter = 0)$optim_design, x)
  out <- swap_pairs(x, starting_dist = 1.5, stop_iter = 1, lambda = 0,
                    candidate_sample_size = 100, dist_method = "manhattan")
  expect_identical(is.na(out$optim_design), is.na(x))
  expect_identical(sort(out$optim_design), sort(x))
})
