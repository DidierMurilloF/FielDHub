library(FielDHub)

test_that("parse_whole_numbers() reads comma-separated whole numbers", {
  expect_identical(parse_whole_numbers("101", "Plot start")$value, 101)
  out <- parse_whole_numbers(" 2, 2 ,3", "Factors")
  expect_true(out$ok)
  expect_identical(out$value, c(2, 2, 3))
  expect_null(out$message)
})

test_that("parse_whole_numbers() names the value it cannot read", {
  # Regression test: the LSD and factorial modules passed text such as "abc"
  # or "2,x,3" straight to as.numeric(), and the resulting NA surfaced as a
  # raw R error ("missing value where TRUE/FALSE needed", "NA/NaN argument").
  out <- parse_whole_numbers("2,x,3", "Factors")
  expect_false(out$ok)
  expect_null(out$value)
  expect_match(out$message, "Factors could not read \"x\" as a whole number", fixed = TRUE)
  expect_match(parse_whole_numbers("abc", "Plot start")$message, "\"abc\"", fixed = TRUE)
})

test_that("parse_whole_numbers() rejects blank, empty and fractional values", {
  expect_match(parse_whole_numbers("", "Plot start")$message, "cannot be blank")
  expect_match(parse_whole_numbers(NULL, "Plot start")$message, "cannot be blank")
  expect_match(parse_whole_numbers("2,,3", "Factors")$message, "empty value")
  expect_match(parse_whole_numbers("2.5", "Factors")$message,
               "could not read \"2.5\" as a whole number", fixed = TRUE)
  expect_match(parse_whole_numbers("0", "Plot start")$message,
               "could not read \"0\" as a whole number", fixed = TRUE)
})
