library(FielDHub)

test_that("location_view_choices() validates the raw locations input before seq_len()", {
  expect_identical(FielDHub:::location_view_choices(1), 1L)
  expect_identical(FielDHub:::location_view_choices(3), 1:3)
  expect_identical(FielDHub:::location_view_choices(3L), 1:3)
  expect_identical(FielDHub:::location_view_choices("3"), 1:3)
  for (value in list(NA, NA_real_, "", "abc", NULL, 0, -1, 2.5, c(2, 3))) {
    expect_error(FielDHub:::location_view_choices(value), class = "fieldhub_input_error")
  }
})

test_that("plant_rep_choices() offers fewer than the total validated locations", {
  # sparse_allocation()'s plant_reps picks a count out of the total, so with
  # a single (valid) location there is nothing to choose from.
  expect_identical(FielDHub:::plant_rep_choices(1), integer(0))
  expect_identical(FielDHub:::plant_rep_choices(3), 1:2)
  expect_identical(FielDHub:::plant_rep_choices("4"), 1:3)
  for (value in list(NA, NA_real_, "", "abc", NULL, 0, -1, 2.5, c(2, 3))) {
    expect_error(FielDHub:::plant_rep_choices(value), class = "fieldhub_input_error")
  }
})

test_that("validate_locations_input() coerces numeric-looking text before validating", {
  expect_identical(FielDHub:::validate_locations_input(3), 3)
  expect_identical(FielDHub:::validate_locations_input(3L), 3L)
  expect_identical(FielDHub:::validate_locations_input("3"), 3)
  for (value in list(NA, NA_real_, "", "abc", NULL, 0, -1, 2.5, c(2, 3))) {
    expect_error(FielDHub:::validate_locations_input(value), class = "fieldhub_input_error")
  }
})

test_that("read_n_checks() rejects a cleared or blank checks-count input with a classed error", {
  # Regression test for the RCBD_augmented upload path: init_data()'s
  # owndata_a_rcbd == "Yes" branch (R/mod_RCBD_augmented.R) used to read
  # `checks <- as.numeric(input$checks_a_rcbd)` with no req() guard, unlike
  # its "No" branch. A cleared "Checks per Block" numericInput sends a
  # logical NA, so `lines <- nrow(data_up) - checks` became NA and
  # `if (lines < 8)` threw "missing value where TRUE/FALSE needed" -- inside
  # an observer's eventExpr (list_to_observe() -> observeEvent()), which
  # ends the Shiny session. read_n_checks() (R/validate_input_parsers.R) now
  # validates the raw input first, with a classed fieldhub_input_error a
  # caller can show through validate_design() instead of crashing.
  expect_identical(FielDHub:::read_n_checks(4), 4L)
  for (value in list(NA, NA_real_, "", NULL, 0, -1, 1.5)) {
    err <- tryCatch(FielDHub:::read_n_checks(value), error = function(e) e)
    expect_s3_class(err, "fieldhub_input_error")
  }
})

test_that("no mod_*/app_* function coerces sites with `1:as.numeric(...)`/`1:sum(...)`", {
  # Regression guard for Task 13 ("invalid input never ends a session"):
  # `1:as.numeric(input$...)` and `1:sum(...)` throw "NA/NaN argument" on a
  # cleared/malformed input, which ends the Shiny session when it runs
  # inside an observer (as opposed to a reactive()/render*()). See
  # helper-source.R's fieldhub_colon_as_numeric_calls() and
  # R/validate_locations.R's location_view_choices()/plant_rep_choices(),
  # which observers now use through validate_design() instead.
  functions <- app_functions()
  offenders <- names(Filter(
    function(x) length(fieldhub_colon_as_numeric_calls(body(x))) > 0,
    functions
  ))
  expect_identical(offenders, character(0))
})
