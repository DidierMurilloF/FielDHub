library(FielDHub)

test_that("field_dimensions() proposes non-prime field sizes for a prime-free range", {
  # Regression test for the isPrime() negative-index bug.
  # field_dimensions() selected the candidate field sizes with
  #   non_primes <- t[-numbers::isPrime(t)]
  # numbers::isPrime() returns a logical vector, so used as a negative index it
  # only ever dropped the first element; for a prime-free size range it even
  # collapsed to t[0] = integer(0). With lines = 105 the size range is
  # t = 115:126 (no primes), so the buggy expression returned zero dimension
  # candidates. The fix uses t[!numbers::isPrime(t)].
  dims <- FielDHub:::field_dimensions(105)
  expect_gt(length(unlist(dims)), 0)
})
