test_that("spatial engines validate row and column counts before field construction", {
  specifications <- list(
    diagonal_arrangement = list(nrows = 15, ncols = 20, lines = 270, checks = 4),
    optimized_arrangement = list(nrows = 12, ncols = 10, lines = 100, rep_checks = 20, checks = 1:5),
    RCBD_augmented = list(lines = 122, checks = 4, b = 5, nrows = 5, ncols = 29, random = FALSE),
    partially_replicated = list(nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2), spread_reps = FALSE))
  for (engine in names(specifications)) for (parameter in c("nrows", "ncols")) {
    for (bad in list(numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, "2", TRUE, list(2), matrix(2))) {
      args <- c(specifications[[engine]], list(seed = 19, year = 2026))
      args[parameter] <- list(bad)
      set.seed(819)
      before <- .Random.seed
      expect_error(do.call(engine, args), class = "fieldhub_input_error",
                    info = paste(engine, parameter, paste(deparse(bad), collapse = " ")))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("partially replicated per-location dimension vectors are complete", {
  for (parameter in c("nrows", "ncols")) for (bad in list(c(8, NA), c(8, Inf), c(8, 0), c(8, 8.5))) {
    args <- list(nrows = c(8, 8), ncols = c(8, 8), l = 2, repGens = c(50, 7),
                  repUnits = c(1, 2), spread_reps = FALSE, plotNumber = c(101, 1001), seed = 19, year = 2026)
    args[parameter] <- list(bad)
    expect_error(do.call(partially_replicated, args), class = "fieldhub_input_error")
  }
})

test_that("dimension vectors retain their storage while rejecting invalid values", {
  values <- c(first = 8L, second = 9L)
  expect_identical(validate_count_vector(values, "nrows"), values)
  for (bad in list(NULL, numeric(), matrix(8), c(8, NA), c(8, Inf), c(8, .5))) {
    expect_error(validate_count_vector(bad, "nrows"), class = "fieldhub_input_error")
  }
})

test_that("unreplicated entry and block counts reject malformed scalar values", {
  specifications <- list(
    diagonal_arrangement = list(nrows = 15, ncols = 20, lines = 270, checks = 4),
    optimized_arrangement = list(nrows = 12, ncols = 10, lines = 100, rep_checks = 20, checks = 1:5),
    RCBD_augmented = list(lines = 50, checks = 3, b = 5, repsExpt = 1))
  for (engine in names(specifications)) {
    parameters <- if (engine == "RCBD_augmented") c("lines", "checks", "b", "repsExpt") else "lines"
    for (parameter in parameters) for (bad in list(numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, "2", TRUE, matrix(2))) {
      args <- c(specifications[[engine]], list(seed = 19, year = 2026))
      args[parameter] <- list(bad)
      expect_error(do.call(engine, args), class = "fieldhub_input_error",
                    info = paste(engine, parameter, paste(deparse(bad), collapse = " ")))
    }
  }
})
