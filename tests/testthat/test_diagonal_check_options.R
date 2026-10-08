library(FielDHub)

test_that("check-percentage previews retain legacy options without consuming RNG", {
  args <- list(n_rows = 15, n_cols = 20, checks = 1:4, Option_NCD = TRUE,
               kindExpt = "SUDC", dim_data = 274, dim_data_1 = 270)
  set.seed(123)
  before <- .Random.seed
  preview <- do.call(FielDHub:::diagonal_check_options, args)
  expect_identical(.Random.seed, before)
  expect_identical(preview, do.call(FielDHub:::available_percent, args))
})

test_that("check-percentage previews keep empty results", {
  expect_null(FielDHub:::diagonal_check_options(
    n_rows = 4, n_cols = 5, checks = 1:4, Option_NCD = TRUE,
    kindExpt = "SUDC", dim_data = 20, dim_data_1 = 16
  ))
})
