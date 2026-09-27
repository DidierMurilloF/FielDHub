test_that("factor pairs preserve first-appearance order without enumerating subsets", {
  pairs <- ordered_factor_pairs(120)
  expect_identical(pairs[, "row"], c(5, 3, 15, 2, 10, 6, 30, 4, 20, 12, 60, 8, 40, 24))
  expect_identical(pairs[, "col"], 120 / pairs[, "row"])
  expect_identical(ordered_factor_pairs(36)[, "row"], c(3, 9, 2, 6, 18, 4, 12))
  named <- factor_subsets(c(size = 120), all_factors = TRUE)
  expect_identical(names(named$combos[[1]]), c("row.size", "col"))
})

test_that("dimension factor searches handle high multiplicity with bounded work", {
  expect_false("sq" %in% all.names(body(factor_subsets)))
  # Do not run the exponential implementation on these inputs when checking
  # the regression against an older installation.
  if ("sq" %in% all.names(body(factor_subsets))) return(invisible(NULL))
  out <- factor_subsets(2^30, all_factors = TRUE)
  expect_equal(nrow(out$comb_factors), 29L)
  expect_identical(out$comb_factors[, 1], 2^seq_len(29L))
  expect_identical(out$comb_factors[, 1] * out$comb_factors[, 2], rep(2^30, 29L))
  expect_identical(factor_subsets(2^30, all_factors = TRUE), out)
  composite <- 73513440
  out <- factor_subsets(composite, all_factors = TRUE)
  expect_setequal(out$comb_factors[, 1], setdiff(integer_divisors(composite), c(1L, composite)))
  expect_identical(out$comb_factors[, 1] * out$comb_factors[, 2], rep(composite, nrow(out$comb_factors)))
})

test_that("factor-pair search is deterministic, consumes no RNG and validates inputs", {
  set.seed(38)
  before <- .Random.seed
  expect_identical(ordered_factor_pairs(120), ordered_factor_pairs(120))
  expect_identical(.Random.seed, before)
  for (value in list(0, -1, NA_real_, Inf, 1.5, c(2, 3), "12", 1i)) {
    expect_error(ordered_factor_pairs(value), class = "fieldhub_input_error")
  }
})

test_that("mixed factor filters with no feasible rectangle return no choices", {
  expect_null(factor_subsets(4, diagonal = TRUE, all_factors = TRUE))
  expect_null(factor_subsets(4, augmented = TRUE, all_factors = TRUE))
})
