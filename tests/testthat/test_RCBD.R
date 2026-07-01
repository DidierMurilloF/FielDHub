library(FielDHub)

test_that("RCBD() gives an informative error for a single character treatment", {
  # Regression test (C4): a single character treatment (e.g. t = "Wheat")
  # matched no branch in the t-handling (the third `else if` duplicated the
  # second), so nt was never set and RCBD() failed with "object 'nt' not found".
  # The third branch now tests length(t) == 1 and raises the intended message.
  expect_error(RCBD(t = "Wheat", reps = 3), "more than one treatment")
})
