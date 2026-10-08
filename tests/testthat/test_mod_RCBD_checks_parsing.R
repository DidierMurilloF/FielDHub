library(testthat)
library(FielDHub)

# Coverage for the core input parsers `parse_n_checks()` and
# `parse_rep_checks()` (R/validate_input_parsers.R). These are the single source of truth
# for turning the "Input # of Checks" and "Reps per Check" Shiny inputs into
# validated values, shared by rcbd_inputs() and the block-size preview so
# both agree on what is valid. Invalid input is a classed
# fieldhub_input_error the app shows through validate_design().

test_that("parse_rep_checks() recycles a single value across n_checks", {
  expect_equal(parse_rep_checks("2", n_checks = 3), c(2, 2, 2))
  expect_equal(parse_rep_checks(" 5 ", n_checks = 2), c(5, 5))
})

test_that("parse_rep_checks() accepts one value per check", {
  expect_equal(parse_rep_checks("2,3", n_checks = 2), c(2, 3))
})

test_that("parse_rep_checks() strictly rejects an unparsable token", {
  err <- expect_error(parse_rep_checks("2,abc", n_checks = 2), class = "fieldhub_input_error")
  expect_match(conditionMessage(err), "abc", fixed = TRUE)
  expect_match(conditionMessage(err), "could not read", fixed = TRUE)
})

test_that("parse_rep_checks() does not silently drop-and-recycle a bad token", {
  # Historical bug: "2,abc" dropped "abc" and recycled the single surviving
  # value (2) to both checks, silently reinterpreting the user's input as
  # "2,2" instead of failing.
  expect_error(parse_rep_checks("2,abc", n_checks = 2), class = "fieldhub_input_error")
})

test_that("parse_rep_checks() reports the length-mismatch message", {
  err <- expect_error(parse_rep_checks("2,3,4", n_checks = 2), class = "fieldhub_input_error")
  expect_match(conditionMessage(err), "1 value or 2 values", fixed = TRUE)
  expect_match(conditionMessage(err), "got 3", fixed = TRUE)
})

test_that("parse_rep_checks() rejects blank and whitespace-only input", {
  for (text in list("", "   ", NULL)) {
    expect_error(parse_rep_checks(text, n_checks = 2), "blank", class = "fieldhub_input_error")
  }
})

test_that("parse_rep_checks() rejects an empty comma-separated token", {
  expect_error(parse_rep_checks("2,,3", n_checks = 2), "empty value",
               class = "fieldhub_input_error")
})

test_that("parse_rep_checks() accepts implausibly large values (bounds live downstream)", {
  # Parsing succeeds for "1e9" -- it IS a valid number. The block-size cap
  # that rejects it lives in rcbd_resolve_entries(), not the parser.
  expect_equal(parse_rep_checks("1e9", n_checks = 2), c(1e9, 1e9))
})

test_that("parse_n_checks() accepts a whole number of 1 or more", {
  expect_identical(parse_n_checks(2), 2L)
})

test_that("parse_n_checks() rejects blank, NA, zero, negative and fractional input", {
  for (x in list(NA, NULL, 0, -1, 1.5)) {
    expect_error(parse_n_checks(x), class = "fieldhub_input_error")
  }
  expect_error(parse_n_checks(NA), "blank", class = "fieldhub_input_error")
  expect_error(parse_n_checks(-1), "whole number", class = "fieldhub_input_error")
})
