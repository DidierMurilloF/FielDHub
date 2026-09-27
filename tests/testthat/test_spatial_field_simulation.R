library(FielDHub)

spatial_test_book <- function() {
  first <- data.frame(
    ID = 1:20, LOCATION = "Z", ROW = rep(1:4, each = 5),
    COLUMN = rep(1:5, times = 4), ENTRY = c(rep(1:2, 3), 3:16),
    EXTRA = "keep"
  )
  second <- first
  second$LOCATION <- "A"
  second$ROW <- rep(1:5, each = 4)
  second$COLUMN <- rep(1:4, times = 5)
  rbind(first, second)
}

simulate_spatial_test <- function(book = spatial_test_book(), ...) {
  FielDHub:::simulate_spatial_field_book(
    field_book = book, nrows = c(4, 5), ncols = c(5, 4),
    correlation_x = 0.4, correlation_y = 0.5,
    min_value = 1, max_value = 10, response_name = "YIELD", ...
  )
}

test_that("spatial simulations preserve metadata and location order", {
  out <- simulate_spatial_test(seed = 27)
  expect_named(out, c("field_book", "simulations", "seed", "input_field_book", "metadata"))
  expect_identical(out$field_book[names(spatial_test_book())], spatial_test_book())
  expect_identical(unique(out$field_book$LOCATION), c("Z", "A"))
  expect_length(out$simulations, 2)
  expect_identical(out$field_book$YIELD,
                   round(unlist(lapply(out$simulations, `[[`, "YIELD")), 2))
  expect_identical(simulate_spatial_test(seed = 27), out)
  expect_false(identical(simulate_spatial_test(seed = 28)$field_book$YIELD,
                         out$field_book$YIELD))
})

test_that("spatial simulations select columns by name and align rows by ID", {
  book <- spatial_test_book()
  original <- simulate_spatial_test(seed = 27)$field_book
  reordered <- book[c(20:1, 40:21), rev(names(book))]
  rownames(reordered) <- NULL
  out <- simulate_spatial_test(reordered, seed = 27)$field_book
  key <- function(x) paste(x$LOCATION, x$ID)
  expect_identical(out$YIELD[match(key(original), key(out))], original$YIELD)
  expect_identical(out[names(reordered)], reordered)
  expect_error(simulate_spatial_test(transform(book, YIELD = 0), seed = 27),
               "already has", class = "fieldhub_input_error")
})

test_that("spatial simulations preserve the caller's RNG state", {
  set.seed(123)
  before <- .Random.seed
  simulate_spatial_test(seed = 27)
  expect_identical(.Random.seed, before)
  simulate_spatial_test()
  expect_identical(.Random.seed, before)
  out <- simulate_spatial_test()
  expect_identical(simulate_spatial_test(seed = out$seed), out)
})

test_that("spatial simulations validate field books and dimensions", {
  book <- spatial_test_book()
  expect_error(simulate_spatial_test(book[-1, ], seed = 27),
               "dimensions", class = "fieldhub_input_error")
  expect_error(simulate_spatial_test(book[c(1, 1, 3:40), ], seed = 27),
               "ID", class = "fieldhub_input_error")
  expect_error(simulate_spatial_test(book[, names(book) != "ENTRY"], seed = 27),
               "ENTRY", class = "fieldhub_input_error")
  expect_error(FielDHub:::simulate_spatial_field_book(
    book, nrows = c(4, 5, 6), ncols = 5, correlation_x = 0.4,
    correlation_y = 0.5, min_value = 1, max_value = 10,
    response_name = "YIELD", seed = 27
  ), "nrows", class = "fieldhub_input_error")
})

test_that("spatial responses cannot shadow internal simulation columns", {
  for (response in c("ZST", "genot", "text")) {
    expect_error(FielDHub:::simulate_spatial_field_book(
      spatial_test_book(), nrows = c(4, 5), ncols = c(5, 4),
      correlation_x = 0.4, correlation_y = 0.5, min_value = 1, max_value = 10,
      response_name = response, seed = 27
    ), "reserved", class = "fieldhub_input_error")
  }
})

test_that("explicit simulation seeds do not leak into the caller's stream", {
  set.seed(123)
  before <- .Random.seed
  AR1xAR1_simulation(
    nrows = 4, ncols = 5, ROX = 0.4, ROY = 0.5,
    minValue = 1, maxValue = 10,
    fieldbook = spatial_test_book()[1:20, c("ID", "ROW", "COLUMN", "ENTRY")],
    trail = "YIELD", seed = 27
  )
  expect_identical(.Random.seed, before)
  norm_trunc(a = 1, b = 10,
             data = data.frame(LOCATION = 1, PLOT = 1:6, TREATMENT = rep(1:3, 2)),
             seed = 27)
  expect_identical(.Random.seed, before)
})
