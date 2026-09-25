library(FielDHub)

test_that("balance_allocation() gives a smaller location one more copy", {
  # Two locations; LOC2 has one entry fewer, and the last entry is missing
  # from it (0 in a sparse allocation).
  allocation <- cbind(LOC1 = c(1, 1, 1), LOC2 = c(1, 1, 0))
  balanced <- balance_allocation(allocation, key_value = 0, add_value = 1)
  expect_equal(unname(colSums(balanced)), c(3, 3))
  expect_equal(unname(balanced[3, "LOC2"]), 1)
})

test_that("balance_allocation() stops when no entry can be added", {
  # Regression test: the balancing loop in do_optim() walked up the rows
  # with k <- k - 1 and no lower bound. When no row could give the smaller
  # location a copy, k became 0 and then negative, and negative indexing
  # made the loop run forever or write into the wrong rows.
  allocation <- cbind(LOC1 = c(2, 1, 1), LOC2 = c(1, 1, 1))
  expect_warning(
    balanced <- balance_allocation(allocation, key_value = 0, add_value = 1),
    "could not be balanced"
  )
  expect_identical(balanced, allocation)
})
