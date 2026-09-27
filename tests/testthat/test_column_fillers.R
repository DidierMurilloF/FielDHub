library(FielDHub)

test_that("column-split previews reject fillers that cannot fit in the last column", {
  # The 20-percent check map has 41 fillers but only 40 empty cells in its
  # last column. The former repeat loop stepped beyond row 50.
  lines <- 3959
  data <- data.frame(ENTRY = seq_len(lines + 4),
                     BLOCK = c(rep("C", 4), rep(1:2, c(1979, 1980))))
  expect_null(FielDHub:::diagonal_check_options(
    n_rows = 50, n_cols = 100, checks = 1:4, Option_NCD = TRUE,
    kindExpt = "DBUDC", stacked = "By Column", data = data,
    dim_data = lines + 4, dim_data_1 = lines
  ))
})

test_that("infeasible column fillers report a dimension condition from the API", {
  expect_error(diagonal_arrangement(
    nrows = 50, ncols = 100, lines = 3959, checks = 4, kindExpt = "DBUDC",
    splitBy = "column", blocks = c(1979, 1980), seed = 1
  ), class = "fieldhub_dimension_error")
})
