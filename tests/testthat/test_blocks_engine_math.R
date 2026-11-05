test_that("engine draws reproduce sample.int under the same seed", {
  set.seed(1)
  x <- engine_draw_indices_cpp(10L, 1000L)
  set.seed(1)
  expect_identical(x, sample.int(10L, 1000L, replace = TRUE))
})

test_that("engine draws follow a non-default sample.kind", {
  old <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old[1], old[2], old[3])), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(2)
  x <- engine_draw_indices_cpp(7L, 500L)
  set.seed(2)
  expect_identical(x, sample.int(7L, 500L, replace = TRUE))
})
