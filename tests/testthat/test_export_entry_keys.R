library(FielDHub)

test_that("spatial exports join entry labels by ENTRY rather than column position", {
  entries <- matrix(1:4, nrow = 2)
  maps <- list(entries, entries + 100, entries * 0, entries * 0 + 1)
  labels <- data.frame(ENTRY = 1:4, NAME = letters[1:4])
  expected <- FielDHub:::export_design(
    maps, movement_planter = "cartesian", location = "FARGO", Year = "2026",
    data_file = labels
  )
  reordered <- data.frame(NOTE = "extra metadata", NAME = labels$NAME, ENTRY = labels$ENTRY)
  actual <- FielDHub:::export_design(
    maps, movement_planter = "cartesian", location = "FARGO", Year = "2026",
    data_file = reordered
  )
  expect_equal(nrow(actual), 4)
  expect_identical(actual[, names(expected)], expected)
  expect_identical(actual$NOTE, rep("extra metadata", 4))
})
