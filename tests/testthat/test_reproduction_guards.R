test_that("reconstruction restores the caller after invalid RNG metadata", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  x$metadata$rng_kind <- c("invalid-generator", "Inversion", "Rejection")
  set.seed(73)
  before <- .Random.seed
  kind <- RNGkind()
  expect_error(reproduce_design(x), "Cannot restore", class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
})

test_that("reconstruction only resolves supported public design engines", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  x$metadata$design <- "system"
  class(x) <- c("fieldhub_system", "FielDHub")
  expect_error(reproduce_design(x), "not supported", class = "fieldhub_input_error")
})
