library(FielDHub)
test_that("compute_index_ranges works correctly", {
  expect_equal(FielDHub:::compute_index_ranges(c(2, 3, 1)), list(from = c(1, 3, 6), to = c(2, 5, 6)))
  expect_equal(FielDHub:::compute_index_ranges(list(c(1, 2), c(3), c(4, 5, 6))), list(from = c(1, 3, 4), to = c(2, 3, 6)))
  expect_error(FielDHub:::compute_index_ranges("not a vector or list"))
})

test_that("total_elements counts correctly", {
  expect_equal(FielDHub:::total_elements(list(1, 2, list(3, 4))), 4)
  expect_equal(FielDHub:::total_elements(list()), 0)
  expect_error(FielDHub:::total_elements("not a list"))
})

test_that("split_matrix_into_blocks splits correctly", {
  mat <- matrix(1:12, nrow = 3, byrow = TRUE)
  blocks_by_row <- FielDHub:::split_matrix_into_blocks(mat, c(1, 2), byrow = TRUE)
  expect_equal(length(blocks_by_row), 2)
  expect_equal(blocks_by_row[[1]], matrix(1:4, nrow = 1))
  expect_equal(blocks_by_row[[2]], matrix(5:12, nrow = 2, byrow = TRUE))
  
  blocks_by_col <- FielDHub:::split_matrix_into_blocks(mat, c(2, 2), byrow = FALSE)
  expect_equal(length(blocks_by_col), 2)
  expect_equal(blocks_by_col[[1]], matrix(c(1, 2, 5, 6, 9, 10), ncol = 2, byrow = TRUE))
  expect_equal(blocks_by_col[[2]], matrix(c(3, 4, 7, 8, 11, 12), ncol = 2, byrow = TRUE))
  
  expect_error(FielDHub:::split_matrix_into_blocks(mat, c(1, 1), byrow = TRUE))
  expect_error(FielDHub:::split_matrix_into_blocks(mat, c(2, 2), byrow = TRUE))
})


