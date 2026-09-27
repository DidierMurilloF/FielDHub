test_that("dimension queries reuse exact arguments and isolate returned values", {
  calls <- 0L
  cached <- new_dimension_cache(function(x, extra = 0.1) {
    calls <<- calls + 1L
    list(value = x, extra = extra)
  })
  first <- cached(120, extra = 0.1)
  expect_identical(cached(120, extra = 0.1), first)
  expect_identical(calls, 1L)
  first$value <- 0
  expect_identical(cached(120, extra = 0.1)$value, 120)
  cached(120L, extra = 0.1)
  cached(c(size = 120), extra = 0.1)
  cached(120, extra = 0.11)
  expect_identical(calls, 4L)
})

test_that("dimension caches evict the least recently used query", {
  calls <- 0L
  cached <- new_dimension_cache(function(x) { calls <<- calls + 1L; x }, max_entries = 2)
  cached(1); cached(2); cached(1); cached(3)
  expect_identical(calls, 3L)
  cached(1)
  expect_identical(calls, 3L)
  cached(2)
  expect_identical(calls, 4L)
})

test_that("dimension cache memory is bounded and oversized results are not retained", {
  size <- as.numeric(object.size(list(key = list(1), value = raw(1000))))
  calls <- 0L
  cached <- new_dimension_cache(function(x) {
    calls <<- calls + 1L
    raw(1000)
  }, max_bytes = size + 1)
  cached(1); cached(2); cached(2); cached(1)
  expect_identical(calls, 3L)
  oversized <- new_dimension_cache(function(x) {
    calls <<- calls + 1L
    raw(2000)
  }, max_bytes = size)
  oversized(1); oversized(1)
  expect_identical(calls, 5L)
})

test_that("failed or warning-producing searches are never cached", {
  calls <- 0L
  cached <- new_dimension_cache(function(x) {
    calls <<- calls + 1L
    if (x == 1) fieldhub_abort("invalid query")
    warning("partial result")
    NULL
  })
  for (i in 1:2) expect_error(cached(1), class = "fieldhub_input_error")
  for (i in 1:2) expect_warning(cached(2), "partial result")
  expect_identical(calls, 4L)
  empty <- new_dimension_cache(function() { calls <<- calls + 1L; NULL })
  expect_null(empty())
  expect_null(empty())
  expect_identical(calls, 5L)
})

test_that("cache construction validates its function and bounds", {
  expect_error(new_dimension_cache(1), class = "fieldhub_input_error")
  for (value in list(NULL, NA_real_, Inf, 0, -1, "2", c(1, 2), 1 + 1i)) {
    expect_error(new_dimension_cache(identity, max_entries = value), class = "fieldhub_input_error")
    expect_error(new_dimension_cache(identity, max_bytes = value), class = "fieldhub_input_error")
  }
  expect_error(new_dimension_cache(identity, max_entries = 1.5), class = "fieldhub_input_error")
})

test_that("field-dimension candidates use the bounded query cache", {
  expect_true(exists("cached_field_dimensions", asNamespace("FielDHub"), inherits = FALSE))
  expect_match(paste(deparse(body(field_dimensions)), collapse = " "), "cached_field_dimensions", fixed = TRUE)
})
