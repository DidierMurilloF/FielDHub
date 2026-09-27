test_that("spatial check counts and entry ranges reject malformed values", {
  specs <- list(
    diagonal_arrangement = list(nrows = 15, ncols = 20, lines = 270, checks = 4),
    optimized_arrangement = list(nrows = 12, ncols = 10, lines = 100, rep_checks = 20, checks = 1:5))
  for (engine in names(specs)) {
    for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, 0, -1, 2.5, "2", TRUE,
                     list(2), matrix(2), c(1, NA), c(1, 1), c(1, 3))) {
      args <- c(specs[[engine]], list(seed = 19, year = 2026))
      args["checks"] <- list(bad)
      set.seed(819)
      before <- .Random.seed
      expect_error(do.call(engine, args), class = "fieldhub_input_error",
                   info = paste(engine, paste(deparse(bad), collapse = " ")))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("optimized check replication is complete and matches the check count", {
  for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, 0, -1, 2.5, "20", TRUE,
                   list(20), matrix(20), c(4, NA, 4, 4, 4), c(10, 10), 4, 5)) {
    expect_error(optimized_arrangement(nrows = 12, ncols = 10, lines = 100,
      checks = 1:5, rep_checks = bad, seed = 19), class = "fieldhub_input_error")
  }
})

test_that("diagonal block sizes reject incomplete and non-count vectors", {
  for (bad in list(NULL, numeric(), NA_real_, Inf, 0, -1, 2.5, "720", TRUE,
                   list(720), matrix(720), c(150, NA), c(150, -1))) {
    expect_error(diagonal_arrangement(nrows = 30, ncols = 26, lines = 720,
      checks = 5, kindExpt = "DBUDC", splitBy = "row", blocks = bad,
      seed = 19, year = 2026), class = "fieldhub_input_error")
  }
})

test_that("check ranges preserve their representations and sorting policy", {
  for (checks in list(4, 4L, c(first = 4), 21:24, c(a = 21L, b = 22L))) {
    expect_identical(validate_spatial_checks(checks, maximum = 300), checks)
  }
  expect_identical(validate_spatial_checks(4:1, 300, sort_entries = TRUE), 4:1)
  expect_error(validate_spatial_checks(4:1, 300), class = "fieldhub_input_error")
  expect_error(validate_spatial_checks(301, 300), class = "fieldhub_input_error")
})
