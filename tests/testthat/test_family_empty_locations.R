test_that("family splits retain empty locations without losing entries", {
  data <- data.frame(ENTRY = 1:3, NAME = paste0("G", 1:3), FAMILY = "A")
  expect_warning(design <- split_families(5, data, seed = 38),
                 "Family A is not in all locations", fixed = TRUE)
  expect_identical(design$rowsEachlist$Location, paste("Location", 1:5))
  expect_identical(sort(design$rowsEachlist$n), c(0, 0, 1, 1, 1))
  expect_identical(sort(design$data_locations$ENTRY), 1:3)
  counts <- table(factor(design$data_locations$LOCATION, levels = paste("Location", 1:5)))
  expect_equal(as.numeric(counts), design$rowsEachlist$n)
  expect_identical(sum(design$rowsEachlist$n), as.double(nrow(data)))
})

test_that("family splits explain when no complete entries remain", {
  data <- data.frame(ENTRY = 1L, NAME = NA_character_, FAMILY = "A")
  for (input in list(data, data[0, ])) {
    expect_error(split_families(3, input, seed = 38), "at least one complete entry",
                 class = "fieldhub_input_error")
  }
})
