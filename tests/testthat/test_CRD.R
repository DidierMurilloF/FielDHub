library(FielDHub)

test_that("CRD() works with a single experimental unit (N == 1)", {
  # Regression test (C3): PLOT = sample(plotNumber:(plotNumber + N - 1))
  # collapsed to sample(101:101) = sample(101) when N == 1, producing a
  # length-101 permutation instead of a single plot number and erroring when
  # building the field book. The fix uses plotNumber - 1 + sample(seq_len(N)).
  crd <- CRD(t = 1, reps = 1, seed = 1)
  expect_s3_class(crd, "FielDHub")
  expect_equal(nrow(crd$fieldBook), 1)
})

test_that("CRD() gives an informative error for a single character treatment", {
  # Regression test (C4): a single character treatment (e.g. t = "Wheat")
  # matched no branch in the t-handling (the third `else if` duplicated the
  # second), so nt was never set and CRD() failed with "object 'nt' not found".
  # The third branch now tests length(t) == 1 and raises the intended message.
  expect_error(CRD(t = "Wheat", reps = 3), "more than one treatment")
})
