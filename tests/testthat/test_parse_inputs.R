library(FielDHub)

test_that("parse_whole_numbers() reads comma-separated whole numbers", {
  expect_identical(parse_whole_numbers("101", "Plot start"), 101)
  expect_identical(parse_whole_numbers(" 2, 2 ,3", "Factors"), c(2, 2, 3))
})

test_that("parse_whole_numbers() names the value it cannot read", {
  # Regression test: the LSD and factorial modules passed text such as "abc"
  # or "2,x,3" straight to as.numeric(), and the resulting NA surfaced as a
  # raw R error ("missing value where TRUE/FALSE needed", "NA/NaN argument").
  expect_error(parse_whole_numbers("2,x,3", "Factors"),
               "Factors could not read \"x\" as a whole number", fixed = TRUE,
               class = "fieldhub_input_error")
  expect_error(parse_whole_numbers("abc", "Plot start"), "\"abc\"", fixed = TRUE,
               class = "fieldhub_input_error")
})

test_that("parse_whole_numbers() rejects blank, empty and fractional values", {
  expect_error(parse_whole_numbers("", "Plot start"), "cannot be blank",
               class = "fieldhub_input_error")
  expect_error(parse_whole_numbers(NULL, "Plot start"), "cannot be blank",
               class = "fieldhub_input_error")
  expect_error(parse_whole_numbers("2,,3", "Factors"), "empty value",
               class = "fieldhub_input_error")
  expect_error(parse_whole_numbers("2.5", "Factors"),
               "could not read \"2.5\" as a whole number", fixed = TRUE,
               class = "fieldhub_input_error")
  expect_error(parse_whole_numbers("0", "Plot start"),
               "could not read \"0\" as a whole number", fixed = TRUE,
               class = "fieldhub_input_error")
})
