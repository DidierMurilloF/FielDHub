library(FielDHub)

test_that("invalid arguments raise a fieldhub_input_error", {
  expect_error(CRD(t = 5, reps = 3, plotNumber = 0), class = "fieldhub_input_error")
  expect_error(incomplete_blocks(t = 12, k = 12, r = 2), class = "fieldhub_input_error")
  expect_error(square_lattice(t = 15, k = 4, r = 2), "square number",
               class = "fieldhub_error")
  err <- tryCatch(RCBD(t = 4, reps = 1), error = function(e) e)
  expect_false(inherits(err, "shiny.silent.error"))
})

# The error, and anything printed, from a call that should fail
failure_of <- function(expr) {
  err <- NULL
  printed <- utils::capture.output(err <- tryCatch(expr, error = function(e) e))
  list(error = err, printed = printed)
}

test_that("field dimensions that do not fit raise an error with the valid options", {
  # Regression test: these functions printed a banner and returned NULL, or,
  # for RCBD_augmented(), returned a data frame of options instead of a design
  calls <- list(
    diagonal_arrangement = quote(diagonal_arrangement(nrows = 7, ncols = 7, lines = 100, checks = 4)),
    optimized_arrangement = quote(optimized_arrangement(nrows = 7, ncols = 7, lines = 100,
                                                        amountChecks = 20, checks = 1:5)),
    partially_replicated = quote(partially_replicated(nrows = 7, ncols = 7, repGens = c(50, 7),
                                                      repUnits = c(1, 2))),
    RCBD_augmented = quote(RCBD_augmented(lines = 20, checks = 3, b = 4, nrows = 7, ncols = 7))
  )
  for (fn in names(calls)) {
    result <- failure_of(suppressWarnings(eval(calls[[fn]])))
    expect_s3_class(result$error, "fieldhub_dimension_error")
    expect_match(conditionMessage(result$error), fn, fixed = TRUE, info = fn)
    expect_s3_class(result$error$options, "data.frame")
    expect_gt(nrow(result$error$options), 0)
    expect_length(result$printed, 0)
  }
})

test_that("plot() explains that a layout option is not available", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  expect_warning(
    expect_error(plot(rcbd, layout = 99), class = "fieldhub_error"),
    "Layout option 99 is not available"
  )
})

test_that("the app shows FielDHub errors as validation messages", {
  err <- tryCatch(validate_design(CRD(t = 5, reps = 3, plotNumber = 0)),
                  error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
  expect_match(conditionMessage(err), "plotNumber must be an integer greater than 0")
})
