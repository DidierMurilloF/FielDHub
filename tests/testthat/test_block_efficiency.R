test_that("disconnected block models have zero A- and D-efficiency without warnings", {
  treatment <- factor(c(1, 2, 2))
  block <- factor(c(3, 1, 2))
  expect_no_warning(result <- blockEstEffics(treatment, block))
  expect_identical(result, list(Deffic = 0, Aeffic = 0))
  # Treatment 5 is isolated in blocks 8 and 11. Numerical cancellation in
  # the previous inverse-eigenvalue sum incorrectly reported A-efficiency 0.6.
  treatment <- factor(c(1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 7))
  block <- factor(c(4, 5, 9, 2, 4, 6, 2, 1, 8, 11, 3, 10, 1, 7, 12, 3))
  expect_no_warning(result <- blockEstEffics(treatment, block))
  expect_identical(result, list(Deffic = 0, Aeffic = 0))
})

test_that("efficiency ignores unused factor levels and handles a single block", {
  treatment <- factor(rep(1:4, 3), levels = 1:5)
  block <- factor(rep(1:3, each = 4), levels = 1:4)
  expect_identical(blockEstEffics(treatment, block), list(Deffic = 1, Aeffic = 1))
  expect_identical(blockEstEffics(treatment, factor(rep(1, 12))), list(Deffic = 1, Aeffic = 1))
})

test_that("efficiency reports use named treatment and plot columns", {
  book <- data.frame(Level_1 = factor(rep(1:3, each = 4)), plots = 1:12,
                     treatments = factor(rep(1:4, 3)))
  expected <- data.frame(Level = 1L, Blocks = 3L, `D-Efficiency` = 1,
                         `A-Efficiency` = 1, `A-Bound` = 1, check.names = FALSE)
  expect_identical(BlockEfficiencies(book[c("treatments", "Level_1", "plots")]), expected)
})

test_that("malformed efficiency inputs produce structured errors", {
  for (arguments in list(list(1:3, 1:2), list(c(1, NA), 1:2), list(1:2, c(1, NA)),
                         list(integer(), integer()), list(rep(1, 3), 1:3),
                         list(matrix(1:4, 2), 1:4))) {
    expect_error(do.call(blockEstEffics, arguments), class = "fieldhub_input_error")
  }
  for (book in list(NULL, data.frame(treatments = 1:4),
                    data.frame(plots = 1:4, treatments = 1:4))) {
    expect_error(BlockEfficiencies(book), class = "fieldhub_input_error")
  }
})
