library(FielDHub)

test_that("primality is vectorised over positive whole numbers", {
  expect_identical(
    is_prime(c(1, 2, 3, 4, 5, 9, 11, 25, 97)),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  )
})

test_that("integer divisors are complete and sorted", {
  expect_identical(integer_divisors(1), 1L)
  expect_identical(integer_divisors(2), c(1L, 2L))
  expect_identical(integer_divisors(12), c(1L, 2L, 3L, 4L, 6L, 12L))
  expect_identical(integer_divisors(49), c(1L, 7L, 49L))
})

test_that("prime factors preserve multiplicity", {
  expect_identical(prime_factors(1), 1)
  expect_identical(prime_factors(2), 2)
  expect_identical(prime_factors(12), c(2, 2, 3))
  expect_identical(prime_factors(49), c(7, 7))
  expect_equal(prod(prime_factors(2^8 * 3^3)), 2^8 * 3^3)
})

test_that("integer factor helpers reject values outside their contract", {
  invalid <- list(0, -1, 2.5, NA_real_, Inf, c(2, 3), "4")
  for (value in invalid) {
    expect_error(integer_divisors(value), class = "fieldhub_input_error")
    expect_error(prime_factors(value), class = "fieldhub_input_error")
  }

  expect_error(is_prime(c(2, NA_real_)), class = "fieldhub_input_error")
  expect_error(is_prime(c(2, 2.5)), class = "fieldhub_input_error")
})
