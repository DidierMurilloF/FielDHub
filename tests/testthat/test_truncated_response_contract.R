test_that("truncated response parameters match the stated per-treatment model", {
  book <- data.frame(LOCATION = "A", PLOT = 1:11,
                     TREATMENT = rep(letters[1:4], c(5, 1, 3, 2)))
  model <- truncated_response_spec(0, 90, book)
  expect_identical(model$treatment_column, "TREATMENT")
  expect_identical(model$treatments, factor(book$TREATMENT))
  expect_identical(model$means, c(15, 35, 55, 75))
  expect_identical(model$counts, c(5L, 1L, 3L, 2L))
  expect_equal(model$sd, 4.65)
})

test_that("responses follow named treatment columns and preserve user metadata", {
  book <- data.frame(LOCATION = rep(c("B", "A"), each = 4), PLOT = rep(4:1, 2),
                     TREATMENT = rep(c("x", "y"), 4))
  expected <- norm_trunc(0, 100, book, seed = 4)
  altered <- book[rev(names(book))]
  altered$NOTES <- "preserve this column"
  actual <- norm_trunc(0, 100, altered, seed = 4)
  expect_identical(actual[names(expected)], expected)
  expect_identical(actual$NOTES, rep("preserve this column", 8))
  names(book)[names(book) == "TREATMENT"] <- "TRT_COMB"
  expect_identical(norm_trunc(0, 100, book, seed = 4)$RESP, expected$RESP)
})

test_that("malformed response simulations fail before drawing random numbers", {
  book <- data.frame(LOCATION = "A", PLOT = 1:6, TREATMENT = rep(letters[1:3], 2))
  bad_bounds <- list(NULL, NA_real_, Inf, "0", c(0, 1), 1 + 1i)
  set.seed(772)
  before <- .Random.seed
  for (value in bad_bounds) {
    expect_error(norm_trunc(value, 100, book), class = "fieldhub_input_error")
    expect_error(norm_trunc(0, value, book), class = "fieldhub_input_error")
  }
  expect_error(norm_trunc(10, 1, book), class = "fieldhub_input_error")
  expect_error(norm_trunc(-1e308, 1e308, book), class = "fieldhub_input_error")
  cases <- list(book[FALSE, ], book[names(book) != "LOCATION"],
                book[names(book) != "PLOT"], book[names(book) != "TREATMENT"],
                transform(book, TREATMENT = "one"),
                transform(book, TREATMENT = c(NA, letters[1:5])),
                transform(book, LOCATION = NA_character_),
                transform(book, PLOT = NA_real_),
                transform(book, RESP = 99),
                transform(book, TRT_COMB = TREATMENT))
  for (bad in cases) expect_error(norm_trunc(0, 100, bad), class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})
