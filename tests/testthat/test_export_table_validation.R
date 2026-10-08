test_that("table validation preserves values and rejects malformed exports", {
  book <- field_layout(RCBD(t = 4, reps = 2, seed = 17))
  expect_identical(validate_export_table(book), book)
  duplicate <- book
  names(duplicate)[2] <- names(duplicate)[1]
  unnamed <- book
  names(unnamed)[1] <- ""
  missing <- book
  names(missing)[1] <- NA_character_
  nested <- book
  nested$BAD <- rep(list(1), nrow(book))
  matrix_column <- book
  matrix_column$BAD <- matrix(1, nrow(book), 2)
  for (bad in list(NULL, list(), book[FALSE, ], book[, FALSE], duplicate,
                  unnamed, missing, nested, matrix_column)) {
    expect_error(validate_export_table(bad), class = "fieldhub_input_error")
  }
})

test_that("layout results record the effective layout parameters", {
  x <- RCBD(t = 4, reps = 2, l = 2, plotNumber = c(101, 1001), seed = 17)
  out <- plot_layout(x, layout = 1, planter = "cartesian", stacked = "horizontal", l = 2)
  expect_identical(out$layout_metadata,
    list(parameters = list(layout = 1, planter = "cartesian", stacked = "horizontal"), selected = 2))
})
