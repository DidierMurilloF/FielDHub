library(FielDHub)

test_that("RCBD size previews share the core block-size rule", {
  expect_identical(FielDHub:::rcbd_size_preview(10, 3, 2, "2,3"),
                   "Block size: 15 plots. Total: 45 plots.")
  expect_identical(FielDHub:::rcbd_size_preview(10, 3, 2, "2"),
                   "Block size: 14 plots. Total: 42 plots.")
  expect_identical(FielDHub:::rcbd_block_size(10, c(2, 3)), 15)
  expect_error(FielDHub:::rcbd_size_preview(10, 3, 2, "1e9"),
               "limit is 10,000", class = "fieldhub_input_error")
  expect_error(FielDHub:::rcbd_block_size(10, c(1e9, 1e9)),
               "limit is 10,000", class = "fieldhub_input_error")
})

test_that("RCBD previews reject malformed counts before formatting", {
  for (value in list(NA_real_, Inf, 1.5, c(1, 2))) {
    expect_error(FielDHub:::rcbd_size_preview(value, 3, 2, "2"),
                 class = "fieldhub_input_error")
    expect_error(FielDHub:::rcbd_size_preview(10, value, 2, "2"),
                 class = "fieldhub_input_error")
  }
  expect_error(FielDHub:::rcbd_size_preview(10, 3, 2, "2,"),
               class = "fieldhub_input_error")
})

test_that("RCBD preview formatting is safe beyond the integer print range", {
  expect_identical(FielDHub:::rcbd_size_preview(10, 1e9, 2, "2"),
                   "Block size: 14 plots. Total: 14000000000 plots.")
  expect_warning(
    expect_error(FielDHub:::rcbd_size_preview(10, 1e308, 2, "2"),
                 "too large", class = "fieldhub_input_error"),
    NA
  )
})
