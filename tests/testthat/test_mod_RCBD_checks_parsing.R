library(testthat)
library(FielDHub)

# Coverage for the module-level input parsers `parse_n_checks()` and
# `parse_rep_checks()` (R/mod_RCBD.R). These are the single source of truth
# for turning the "Input # of Checks" and "Reps per Check" Shiny inputs into
# validated values, shared by rcbd_inputs(), the block-size preview, and
# get_data_rcbd() so all three agree on what is valid.

test_that("parse_rep_checks() recycles a single value across n_checks", {
  res <- parse_rep_checks("2", n_checks = 3)
  expect_true(res$ok)
  expect_equal(res$value, c(2, 2, 2))

  res2 <- parse_rep_checks(" 5 ", n_checks = 2)
  expect_true(res2$ok)
  expect_equal(res2$value, c(5, 5))
})

test_that("parse_rep_checks() accepts one value per check", {
  res <- parse_rep_checks("2,3", n_checks = 2)
  expect_true(res$ok)
  expect_equal(res$value, c(2, 3))
})

test_that("parse_rep_checks() strictly rejects an unparsable token", {
  res <- parse_rep_checks("2,abc", n_checks = 2)
  expect_false(res$ok)
  expect_null(res$value)
  expect_match(res$message, "abc", fixed = TRUE)
  expect_match(res$message, "could not read", fixed = TRUE)
})

test_that("parse_rep_checks() does not silently drop-and-recycle a bad token", {
  # Historical bug: "2,abc" dropped "abc" and recycled the single surviving
  # value (2) to both checks, silently reinterpreting the user's input as
  # "2,2" instead of failing.
  res <- parse_rep_checks("2,abc", n_checks = 2)
  expect_false(res$ok)
  expect_false(isTRUE(all.equal(res$value, c(2, 2))))
})

test_that("parse_rep_checks() reports the length-mismatch message", {
  res <- parse_rep_checks("2,3,4", n_checks = 2)
  expect_false(res$ok)
  expect_match(res$message, "1 value or 2 values", fixed = TRUE)
  expect_match(res$message, "got 3", fixed = TRUE)
})

test_that("parse_rep_checks() rejects blank and whitespace-only input", {
  expect_false(parse_rep_checks("", n_checks = 2)$ok)
  expect_false(parse_rep_checks("   ", n_checks = 2)$ok)
  expect_false(parse_rep_checks(NULL, n_checks = 2)$ok)
})

test_that("parse_rep_checks() rejects an empty comma-separated token", {
  res <- parse_rep_checks("2,,3", n_checks = 2)
  expect_false(res$ok)
  expect_match(res$message, "empty value", fixed = TRUE)
})

test_that("parse_rep_checks() accepts implausibly large values (bounds live downstream)", {
  # Parsing succeeds for "1e9" -- it IS a valid number. The block-size cap
  # that rejects it lives in rcbd_resolve_entries(), not the parser.
  res <- parse_rep_checks("1e9", n_checks = 2)
  expect_true(res$ok)
  expect_equal(res$value, c(1e9, 1e9))
})

test_that("parse_n_checks() accepts a whole number of 1 or more", {
  res <- parse_n_checks(2)
  expect_true(res$ok)
  expect_equal(res$value, 2L)
})

test_that("parse_n_checks() rejects blank, NA, zero, negative and fractional input", {
  expect_false(parse_n_checks(NA)$ok)
  expect_false(parse_n_checks(NULL)$ok)
  expect_false(parse_n_checks(0)$ok)
  expect_false(parse_n_checks(-1)$ok)
  expect_false(parse_n_checks(1.5)$ok)

  expect_match(parse_n_checks(NA)$message, "blank", fixed = TRUE)
  expect_match(parse_n_checks(-1)$message, "whole number", fixed = TRUE)
})
