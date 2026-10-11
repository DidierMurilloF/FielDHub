option_key <- function(percent = 10, seed = 1) {
  list(engine = "diagonal_arrangement", args = list(checksPercent = percent, seed = seed),
       rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"))
}
option_job <- function(value) list(ok = TRUE, value = value, warnings = list())

test_that("option caches isolate complete requests and enforce retention bounds", {
  cache <- new_design_job_cache(max_entries = 2L)
  first <- option_key()
  expect_null(cache$get(first))
  expect_true(cache$put(first, option_job("first")))
  expect_identical(cache$get(first), option_job("first"))
  expect_null(cache$get(option_key(seed = 2)))
  changed <- first
  changed$args$dist_method <- "manhattan"
  expect_null(cache$get(changed))
  changed <- first
  changed$rng_kind[1L] <- "L'Ecuyer-CMRG"
  expect_null(cache$get(changed))
  changed <- first
  changed$args$data <- data.frame(ENTRY = 1, NAME = "A")
  expect_null(cache$get(changed))
  cache$put(option_key(12), option_job("second"))
  cache$get(first)
  cache$put(option_key(14), option_job("third"))
  expect_null(cache$get(option_key(12)))
  expect_identical(cache$get(first), option_job("first"))
  expect_false(cache$put(first, list(ok = FALSE, condition = simpleError("failed"))))
  expect_false(cache$put(option_key(seed = NULL), option_job("unseeded")))
  small <- new_design_job_cache(max_bytes = 1L)
  expect_false(small$put(first, option_job("too large")))
  expect_null(small$get(first))
  for (bound in list(0, NA, Inf, 1.5)) {
    expect_error(new_design_job_cache(max_entries = bound), class = "fieldhub_input_error")
  }
})

test_that("selected options take priority and older jobs cannot replace them", {
  queue <- new_design_option_queue()
  a <- option_key(10)
  b <- option_key(12)
  c <- option_key(14)
  queue$select(a, list(b, c))
  first <- queue$next_job()
  expect_identical(first$key, a)
  expect_null(queue$next_job())
  queue$select(c, list(a, b))
  expect_null(queue$result())
  queue$finish(first, option_job("A"))
  expect_null(queue$result())
  selected <- queue$next_job()
  expect_identical(selected$key, c)
  queue$finish(selected, option_job("C"))
  expect_identical(queue$result(), option_job("C"))
  background <- queue$next_job()
  expect_identical(background$key, b)
  queue$select(a, list(b, c))
  expect_identical(queue$result(), option_job("A"))
  queue$finish(background, option_job("B"))
  expect_identical(queue$result(), option_job("A"))
  expect_null(queue$next_job())
  expect_false(queue$finish(first, option_job("obsolete completion")))
})

test_that("invalidated requests, failures and synchronous execution do not loop", {
  queue <- new_design_option_queue()
  key <- option_key()
  queue$select(key, list(option_key(12)))
  active <- queue$next_job()
  queue$clear()
  queue$finish(active, option_job("old result"))
  expect_null(queue$result())
  expect_null(queue$next_job())
  queue$select(key, list(option_key(12)))
  expect_identical(queue$result(), option_job("old result"))
  expect_null(queue$next_job(allow_prefetch = FALSE))
  background <- queue$next_job()
  failure <- list(ok = FALSE, condition = simpleError("failed"), warnings = list())
  queue$select(option_key(12))
  queue$finish(background, failure)
  expect_identical(queue$result(), failure)
  expect_null(queue$next_job())
  queue$select(option_key(12))
  expect_false(is.null(queue$next_job()))
})

test_that("percentage alternatives retain the complete seeded API arguments", {
  args <- list(nrows = 10, ncols = 32, lines = 287, checks = 4, seed = 3,
               spread_checks = TRUE, dist_method = "manhattan", checksPercent = 10)
  alternatives <- diagonal_option_requests(args, c(8, 10, 12))
  expect_length(alternatives, 3L)
  for (i in seq_along(alternatives)) {
    expect_identical(alternatives[[i]][setdiff(names(args), "checksPercent")],
                     args[setdiff(names(args), "checksPercent")])
  }
  expect_equal(vapply(alternatives, `[[`, numeric(1), "checksPercent"), c(8, 10, 12))
})

test_that("the diagonal UI passes the optimization options to the API", {
  spec <- design_app_spec("Diagonal")
  ids <- vapply(spec$controls, `[[`, "", "id")
  expect_true(all(c("spread_checks", "dist_method") %in% ids))
  values <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1,
                 spread_checks = TRUE, dist_method = "manhattan")
  args <- spec$args(values)
  expect_true(args$spread_checks)
  expect_identical(args$dist_method, "manhattan")
  expect_identical(do.call(spec$engine, args), do.call(diagonal_arrangement, args))
})
