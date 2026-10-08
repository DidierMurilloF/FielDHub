test_that("a seedless standalone pair swap consumes exactly one draw from the caller's stream", {
  x <- matrix(c(rep(1:3, 2), 4:13), 4)
  set.seed(99)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(99)
  out <- swap_pairs(x, starting_dist = 2, stop_iter = 3, candidate_sample_size = 1)
  expect_true(identical(.Random.seed, after_one_draw))
  expect_s3_class(out, "fieldhub_optimization")
  expect_identical(out$metadata$design, "pair_swap")
  expect_true(is.integer(out$metadata$seed))
  expect_identical(out$metadata$seed, expected)
  expect_identical(out$metadata$parameters$X, x)
  expect_identical(reproduce_design(out), out)
  expect_true(identical(.Random.seed, after_one_draw))
})

test_that("explicit pair-swap seeds reproduce independently of caller state", {
  x <- matrix(c(rep(1:4, 2), 5:11, NA), 4)
  for (method in c("euclidean", "manhattan")) {
    set.seed(21)
    before <- .Random.seed
    first <- swap_pairs(x, starting_dist = 2, stop_iter = 2, dist_method = method, seed = 18)
    expect_true(identical(.Random.seed, before))
    set.seed(94)
    before <- .Random.seed
    expect_identical(swap_pairs(x, starting_dist = 2, stop_iter = 2, dist_method = method, seed = 18), first)
    expect_identical(reproduce_design(first), first)
    expect_true(identical(.Random.seed, before))
    expect_identical(first$metadata$parameters$seed, 18)
    expect_identical(first$metadata$rng_kind, RNGkind())
  }
})

test_that("pair-swap errors and unseeded callers leave no RNG side effects", {
  local_rng_state()
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  out <- swap_pairs(matrix(c(1, 2, 1, 2), 2), seed = 18)
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_identical(reproduce_design(out), out)
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_error(swap_pairs(matrix(1:4, 2), seed = 18), class = "fieldhub_input_error")
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_error(swap_pairs(matrix(c(1, 2, 1, 2), 2), seed = "18"), class = "fieldhub_input_error")
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
})

test_that("pair-swap result validation detects corrupted optimization records", {
  x <- swap_pairs(matrix(c(1, 2, 1, 2), 2), seed = 18)
  expect_identical(validate_fieldhub_optimization(x), x)
  bad <- x
  bad$metadata$parameters$seed <- 19
  expect_error(reproduce_design(bad), class = "fieldhub_internal_error")
  bad <- x
  bad$optim_design[1] <- 99
  expect_error(validate_fieldhub_optimization(bad), class = "fieldhub_internal_error")
  bad <- x
  bad$optim_design <- t(bad$optim_design)[, 1, drop = FALSE]
  expect_error(validate_fieldhub_optimization(bad), class = "fieldhub_internal_error")
})
