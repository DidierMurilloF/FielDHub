library(FielDHub)
# Test pairs_distance function
test_that("pairs_distance returns expected output", {
  # Test with a matrix that contains integers that appear two or more times
  X1 <- matrix(c(1, 2, 3, 4, 2, 2, 6, 1, 7, 8, 5, 9), ncol = 3)
  res1 <- pairs_distance(X1)
  expect_equal(ncol(res1), 8)
  expect_equal(nrow(res1), 4)
  expect_equal(sum(duplicated(res1[, 2:3])), 0)
  
  # Test with a matrix that contains only unique integers
  X2 <- matrix(1:9, ncol = 3)
  expect_error(pairs_distance(X2), "All elements in X appear only once")
})

# create a test for the function
test_that("pairs_distance() correctly calculates the distances between paired points", {
  # create a test input matrix
  X <- matrix(c(1, 2, 3, 4, 2, 5, 6, 1, 7, 8, 2, 9), ncol = 3)
  # call the function and save the output
  output <- pairs_distance(X)
  # check that the output is a data frame
  expect_s3_class(output, "data.frame")
  # check that the output has the correct number of rows and columns
  expect_equal(dim(output), c(4, 8))
  # check that the output is sorted by distance
  expect_true(all(diff(output$DIST) >= 0))
  # check that the output distances are calculated correctly
  expect_equal(round(output$DIST, 4), c(1.4142, 2.2361, 2.2361, 3.1623))
  # check that the output point pairs are correct
  expect_equal(output$Pos1, c(2, 2, 5, 1))
  expect_equal(output$Pos2, c(5, 11, 11, 8))
})

# Test swap_pairs function
test_that("swap_pairs returns expected output", {
  # Test with a matrix that contains integers that appear two or more times
  set.seed(123)
  X1 <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
  res1 <- swap_pairs(X1, starting_dist = 3, stop_iter = 100)
  expect_equal(dim(res1$optim_design), dim(X1))
  expect_equal(length(res1$designs), length(res1$distances))
  expect_equal(min(res1$pairswise_distance$DIST), res1$min_distance)
  expect_true(res1$min_distance >= 3)
  expect_equal(ncol(res1$pairswise_distance), 8)
  
  # Test with a matrix that contains only unique integers
  X2 <- matrix(1:9, ncol = 3)
  expect_error(
    swap_pairs(X2, starting_dist = 3, stop_iter = 100), 
    "All elements in X appear only once"
  )
})
