test_that("design option errors have structured argument and choice details", {
  for (value in list("invalid", NA_character_, "", 1, c("one", "two"), character())) {
    err <- tryCatch(row_column(t = 12, nrows = 3, reps = 2, method = value, seed = 1),
                    error = identity)
    expect_s3_class(err, "fieldhub_input_error")
    expect_identical(err$argument, "method")
    expect_identical(err$choices, c("onestage", "twostage"))
    expect_identical(err$value, value)
    err <- tryCatch(RCBD_augmented(lines = 20, checks = 2, b = 2, repsStack = value, seed = 1),
                    error = identity)
    expect_s3_class(err, "fieldhub_input_error")
    expect_identical(err$argument, "repsStack")
    expect_identical(err$choices, c("vertical", "horizontal"))
  }
})

test_that("shared option matching retains defaults, abbreviations and parent errors", {
  choices <- c("vertical", "horizontal")
  for (value in list(NULL, choices, "ver", "hor", "vertical", "horizontal")) {
    expect_identical(match_design_choice(value, choices, "repsStack"), match.arg(value, choices))
  }
  err <- tryCatch(match_design_choice("invalid", choices, "repsStack"), error = identity)
  expect_s3_class(err$parent, "error")
  expect_identical(conditionMessage(err), conditionMessage(err$parent))
})
