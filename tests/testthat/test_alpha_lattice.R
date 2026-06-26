library(FielDHub)

test_that("alpha_lattice() accepts treatments supplied as a vector (issue #20)", {
  # Regression test for the isPrime() "condition has length > 1" bug.
  # alpha_lattice() guarded against a prime number of treatments with
  #   if (numbers::isPrime(t)) ...
  # but t is the raw treatment argument, which may be a vector (e.g. t = 1:8).
  # numbers::isPrime() then returns a logical vector, so the if() condition had
  # length > 1 and the call errored. A character vector errored at the same line
  # too. The (dead) line dt <- numbers::divisors(t) had the same vector problem.
  # The fix checks the treatment count, numbers::isPrime(nt), instead.
  alpha_num <- alpha_lattice(t = 1:8, k = 2, r = 2, seed = 1)
  expect_s3_class(alpha_num, "FielDHub")
  expect_equal(nrow(alpha_num$fieldBook), 8 * 2)

  alpha_chr <- alpha_lattice(t = LETTERS[1:8], k = 2, r = 2, seed = 1)
  expect_s3_class(alpha_chr, "FielDHub")
})
