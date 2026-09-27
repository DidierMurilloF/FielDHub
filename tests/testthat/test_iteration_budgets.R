test_that("iteration budgets are finite whole scalars with explicit bounds", {
  for (value in list(Inf, -Inf, NaN, NA_real_, 1.5, 1 + 1i, numeric(), c(1, 2),
                     matrix(1), "2", -1, .Machine$integer.max + 1)) {
    expect_error(validate_iteration_budget(value), "iterations", class = "fieldhub_input_error")
  }
  expect_error(validate_iteration_budget(0), class = "fieldhub_input_error")
  expect_identical(validate_iteration_budget(0L, minimum = 0), 0L)
  expect_identical(validate_iteration_budget(17), 17)
})

test_that("row-column optimization rejects invalid budgets before randomization", {
  for (value in list(1.5, 1 + 1i, Inf)) {
    set.seed(19)
    before <- .Random.seed
    expect_error(row_column(t = 12, nrows = 3, reps = 2, seed = 27,
                            method = "twostage", iterations = value),
                 "iterations", class = "fieldhub_input_error")
    expect_identical(.Random.seed, before)
  }
})
