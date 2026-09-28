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

test_that("field_dimensions() rejects a malformed entry count with a classed error", {
  # Regression test: typing 0 or a negative "Input # of Entries" and
  # clicking Run reached field_dimensions(lines_within_loc = 0) directly
  # inside an observer-read eventReactive of the diagonal and sparse pages
  # (now diagonal_field_choices(), run by the generic spatial page).
  # field_dimensions(0)/(-1) already raised a classed fieldhub_input_error
  # (via is_prime()'s validator), but field_dimensions(NA) raised an
  # unclassed "NA/NaN argument" from `range[1]:range[2]` before ever
  # reaching that check -- neither is caught by validate_design() alone
  # unless the condition is classed. field_dimensions() now validates
  # `lines_within_loc` up front, so all three are the same classed error.
  for (value in list(0, -1, NA_real_, NA, 2.5, c(2, 3), "abc")) {
    expect_error(FielDHub:::field_dimensions(value), class = "fieldhub_input_error")
  }
})

test_that("validate_design(field_dimensions(0)) is safe inside an observer", {
  skip_if_not_installed("shiny")
  # Proves the wrap used at every observer-read call site (the spatial
  # pages' choice functions run inside validate_design()) turns the classed
  # error into the same silent, session-safe condition req() raises.
  err <- tryCatch(validate_design(FielDHub:::field_dimensions(0)), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
})
