test_that("Latin-square searches stop at their per-square iteration limit", {
  set.seed(1)
  err <- tryCatch(lsq(len = 4, max_iterations = 1L), error = identity)
  expect_s3_class(err, "fieldhub_search_error")
  expect_match(conditionMessage(err), "placement-iteration limit of 1")
  expect_identical(err$design, "latin_square")
  expect_identical(err$square, 1L)
  expect_identical(err$iterations, 1L)
  expect_identical(err$max_iterations, 1L)
})

test_that("the Latin-square search budget must be a positive finite integer", {
  for (limit in list(NULL, NA, Inf, 0, -1, 1.5, "10", c(1, 2), 1 + 1i)) {
    expect_error(lsq(len = 4, max_iterations = limit),
                 class = "fieldhub_input_error")
  }
})

test_that("a failed Latin-square search still restores the caller's RNG", {
  bounded_lsq <- lsq
  testthat::local_mocked_bindings(
    lsq = function(len, reps, ...) bounded_lsq(len, reps, max_iterations = 1L, ...)
  )
  set.seed(42)
  previous <- .Random.seed
  expect_error(latin_square(t = 4, seed = 1), class = "fieldhub_search_error")
  expect_identical(.Random.seed, previous)
})

test_that("a failed search identifies the square requested by the public API", {
  bounded_lsq <- lsq
  calls <- 0L
  testthat::local_mocked_bindings(
    lsq = function(len, reps, ...) {
      calls <<- calls + 1L
      limit <- if (calls == 2L) 1L else 100000L
      bounded_lsq(len, reps, max_iterations = limit, ...)
    }
  )
  err <- tryCatch(latin_square(t = 4, reps = 2, seed = 1), error = identity)
  expect_s3_class(err, "fieldhub_search_error")
  expect_identical(err$square, 2L)
  expect_match(conditionMessage(err), "square 2")
})

test_that("each square receives its own complete search budget", {
  set.seed(1)
  squares <- lsq(len = 2, reps = 2, max_iterations = 2L)
  expect_identical(dim(squares), c(4L, 2L))
  expect_false(anyNA(squares))
})
