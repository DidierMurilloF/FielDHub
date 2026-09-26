library(FielDHub)

test_that("incomplete-block families offer every proper divisor", {
  expected <- c(2L, 3L, 4L, 6L)
  for (design in c("incomplete_blocks", "row_column", "alpha_lattice")) {
    expect_identical(valid_block_sizes(12, design), expected, info = design)
    expect_identical(valid_block_sizes(13, design), integer(0), info = design)
  }
})

test_that("lattice families offer only their feasible block size", {
  expect_identical(valid_block_sizes(64, "square_lattice"), 8L)
  expect_identical(valid_block_sizes(42, "square_lattice"), integer(0))

  expect_identical(valid_block_sizes(42, "rectangular_lattice"), 6L)
  expect_identical(valid_block_sizes(56, "rectangular_lattice"), 7L)
  expect_identical(valid_block_sizes(64, "rectangular_lattice"), integer(0))
})

test_that("block-size validation uses the FielDHub error contract", {
  for (treatments in list(1, 0, -2, 4.5, NA_real_, c(4, 6), "12")) {
    expect_error(
      valid_block_sizes(treatments, "incomplete_blocks"),
      class = "fieldhub_input_error"
    )
  }
  expect_error(valid_block_sizes(12, "unknown"), class = "fieldhub_input_error")
})
