# Validate before the optimizer can disguise malformed labels as an internal error.
test_that("row-column labels and supplied entries fail with input conditions", {
  for (bad in list(c("A", NA, "C", "D"), c("A", " ", "C", "D"), list(1, 2, 3, 4))) {
    expect_error(row_column(t = bad, nrows = 2, reps = 2, seed = 41, iterations = 1),
                 class = "fieldhub_input_error")
  }
  expect_error(row_column(t = 12, nrows = 3, reps = 2, seed = 41, iterations = 1,
                          data = data.frame(ENTRY = 1:12)), class = "fieldhub_input_error")
})
