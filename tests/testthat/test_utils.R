library(FielDHub)
# Testing split_vectors()
x1 <- 1:10
x2 <- 1:15
x3 <- 19:201
test_that("split_vectors is a list of vectors", {
  expect_equal(split_vectors(x1, c(3,5,2)), list(1:3, 4:8, 9:10))
  expect_equal(split_vectors(x2, c(5,6,4)), list(1:5, 6:11, 12:15))
  expect_equal(split_vectors(x3, c(75,80,28)), list(19:93, 94:173, 174:201))
})