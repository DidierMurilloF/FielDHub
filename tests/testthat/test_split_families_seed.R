family_seed_data <- function() {
  data.frame(ENTRY = 1:18, NAME = paste0("G", 1:18), FAMILY = rep(1:3, each = 6))
}

test_that("split_families records explicit seeds and restores caller RNG", {
  set.seed(42)
  before <- .Random.seed
  design <- split_families(l = 3, data = family_seed_data(), seed = 38)
  expect_identical(.Random.seed, before)
  expect_identical(design$infoDesign$seed, 38)
  expect_identical(design$metadata$seed, 38)
  set.seed(99)
  expect_identical(split_families(3, family_seed_data(), seed = 38), design)
})

test_that("split_families consumes exactly one draw for automatic seeds", {
  set.seed(42)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(42)
  design <- split_families(l = 3, data = family_seed_data())
  expect_identical(.Random.seed, after_one_draw)
  expect_type(design$metadata$seed, "integer")
  expect_length(design$metadata$seed, 1L)
  expect_identical(design$metadata$seed, expected)
  expect_identical(split_families(3, family_seed_data(), seed = design$metadata$seed),
                   design)
})

test_that("split_families creates .Random.seed when the caller session had none", {
  local_rng_state()
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  design <- split_families(3, family_seed_data())
  expect_true(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_type(design$metadata$seed, "integer")
})

test_that("split_families validates seeds and restores RNG when inputs fail", {
  set.seed(42)
  before <- .Random.seed
  expect_error(split_families(3, "invalid data", seed = 38),
               class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
  for (seed in list("38", NA, Inf, c(1, 2), 2^31, 1 + 1i)) {
    expect_error(split_families(3, family_seed_data(), seed = seed),
                 class = "fieldhub_input_error")
  }
})
