library(FielDHub)

test_that("default entry lists have stable identifiers and labels", {
  expect_identical(
    default_entries(3),
    data.frame(ENTRY = 1:3, NAME = c("G-1", "G-2", "G-3"))
  )
  expect_identical(
    default_entries(2, prefix = "Check-", start = 4),
    data.frame(ENTRY = 4:5, NAME = c("Check-4", "Check-5"))
  )
})

test_that("default entry validation follows the FielDHub error contract", {
  for (count in list(0, -1, 2.5, NA_real_, c(2, 3), "2")) {
    expect_error(default_entries(count), class = "fieldhub_input_error")
  }
  expect_error(default_entries(2, prefix = NA_character_), class = "fieldhub_input_error")
  expect_error(default_entries(2, prefix = c("G", "T")), class = "fieldhub_input_error")
  expect_error(default_entries(2, start = 0), class = "fieldhub_input_error")
})
