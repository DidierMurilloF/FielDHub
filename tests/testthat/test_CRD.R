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

test_that("CRD() replicates character treatments across every REP", {
  # Regression test: with a character `t`, TRT was the bare treatment vector
  # and was recycled against REP <- rep(1:reps, times = nt), so each treatment
  # was labelled with a single REP (all copies of "A" in REP 1, of "B" in
  # REP 2, ...). Each treatment must appear once in every replicate.
  crd <- CRD(t = c("A", "B", "C"), reps = 3, seed = 1)
  counts <- table(crd$fieldBook$TREATMENT, crd$fieldBook$REP)
  expect_equal(dim(counts), c(3L, 3L))
  expect_true(all(counts == 1))
})
