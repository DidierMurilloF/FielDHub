test_that("reproduce_design rebuilds every recorded catalogue result", {
  for (name in setdiff(names(catalogue), "swap_pairs")) {
    x <- catalogue_design(name)
    replay <- suppressWarnings(reproduce_design(x))
    expect_identical(replay, x, info = name)
  }
})

test_that("reconstruction uses recorded RNG settings and restores the caller", {
  (function() {
    local_rng_state()
    old_kind <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE, after = FALSE)
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    x <- RCBD(t = 5, reps = 3, seed = 38)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(73)
    before <- .Random.seed
    kind <- RNGkind()
    expect_identical(reproduce_design(x), x)
    expect_identical(RNGkind(), kind)
    expect_identical(.Random.seed, before)
    x$metadata$parameters$t <- NULL
    expect_error(reproduce_design(x), class = "fieldhub_error")
    expect_identical(RNGkind(), kind)
    expect_identical(.Random.seed, before)
  })()
})

test_that("reconstruction preserves an initially absent random seed", {
  (function() {
    local_rng_state()
    x <- RCBD(t = 4, reps = 2, seed = 38)
    if (exists(".Random.seed", globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    before <- RNGkind()
    expect_identical(reproduce_design(x), x)
    expect_identical(RNGkind(), before)
    expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
  })()
})

test_that("reconstruction explains missing parameters and unsupported designs", {
  expect_error(reproduce_design(1), class = "fieldhub_input_error")
  x <- RCBD(t = 4, reps = 2, seed = 38)
  x$metadata$parameters <- NULL
  expect_error(reproduce_design(x), "recorded parameters", class = "fieldhub_input_error")
  x$metadata <- NULL
  expect_error(reproduce_design(x), "recorded parameters", class = "fieldhub_input_error")
})

test_that("reconstruction warns about a different package version", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  x$metadata$package_version <- "0.0.0"
  expect_warning(replay <- reproduce_design(x), class = "fieldhub_reproduction_warning")
  expect_identical(replay$fieldBook, x$fieldBook)
})

test_that("recorded argument expressions are values, not executable code", {
  x <- CRD(t = 4, reps = 2, seed = 38)
  x$metadata$parameters$data <- quote(stop("must not execute"))
  expect_error(reproduce_design(x), "Data must be a data frame", class = "fieldhub_input_error")
})
