test_that("recorded RNG arguments support old and new R settings", {
  old <- c("Mersenne-Twister", "Inversion", "Rejection")
  three <- c("kind", "normal.kind", "sample.kind")
  four <- c(three, "binom.kind")
  expect_identical(recorded_rng_arguments(old, three), setNames(as.list(old), three))
  expect_identical(recorded_rng_arguments(old, four),
                   c(setNames(as.list(old), three), list(binom.kind = "Buggy BTPE")))
  new <- c(old, "BTPE")
  expect_identical(recorded_rng_arguments(new, four), setNames(as.list(new), four))
  expect_error(recorded_rng_arguments(new, three), "requires R with binom.kind support")
  for (bad in list(character(), old[1:2], c(old, NA_character_),
                   c(old, ""), c(old, "BTPE", "extra"), matrix(old, nrow = 1))) {
    expect_error(recorded_rng_arguments(bad, four), "Incomplete")
  }
})

test_that("metadata accepts both recorded RNG formats without losing settings", {
  x <- CRD(t = 4, reps = 2, seed = 38)
  three <- c("Mersenne-Twister", "Inversion", "Rejection")
  for (kind in list(three, c(three, "BTPE"))) {
    x$metadata$rng_kind <- kind
    expect_length(fieldhub_metadata_problems(x$metadata), 0L)
  }
  x$metadata$rng_kind <- c(three, "")
  expect_match(fieldhub_metadata_problems(x$metadata), "random-number settings")
})

test_that("four-setting replay either restores all settings or fails without changing the caller", {
  local_rng_state()
  x <- CRD(t = 4, reps = 2, seed = 38)
  x$metadata$rng_kind <- c("Mersenne-Twister", "Inversion", "Rejection", "BTPE")
  set.seed(721)
  before <- .Random.seed
  kind <- RNGkind()
  if ("binom.kind" %in% names(formals(RNGkind))) {
    expect_identical(reproduce_design(x)$metadata$rng_kind, x$metadata$rng_kind)
  } else {
    expect_error(reproduce_design(x), "Cannot restore.*binomial RNG",
                 class = "fieldhub_input_error")
  }
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  code <- design_call_code(x)
  if ("binom.kind" %in% names(formals(RNGkind))) {
    expect_identical(eval(parse(text = code))$metadata$rng_kind, x$metadata$rng_kind)
  } else {
    expect_error(eval(parse(text = code)), "recorded binomial RNG requires R >= 4.7")
  }
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
})

test_that("three-setting records use the historical binomial algorithm on newer R", {
  skip_if_not("binom.kind" %in% names(formals(RNGkind)))
  local_rng_state()
  previous <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous)), add = TRUE)
  x <- CRD(t = 4, reps = 2, seed = 38)
  x$metadata$rng_kind <- x$metadata$rng_kind[1:3]
  RNGkind(binom.kind = "BTPE")
  expect_identical(tail(reproduce_design(x)$metadata$rng_kind, 1L), "Buggy BTPE")
  expect_identical(tail(eval(parse(text = design_call_code(x)))$metadata$rng_kind, 1L),
                   "Buggy BTPE")
  expect_identical(tail(RNGkind(), 1L), "BTPE")
})
