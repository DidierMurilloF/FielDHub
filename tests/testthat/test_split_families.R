# split_families(): entries of each family split across locations.

family_seed_data <- function() {
  data.frame(ENTRY = 1:18, NAME = paste0("G", 1:18), FAMILY = rep(1:3, each = 6))
}

# ---- Allocation of the entries ----

test_that("split_families() assigns every entry of valid three-column data", {
  # Regression: gen.list was created after stop() inside the column-count
  # guard, so valid three-column input failed with "object 'gen.list' not found".
  gen.list <- data.frame(ENTRY = 1:12, NAME = paste0("SB-", 1:12), FAMILY = rep(1:3, each = 4))
  out <- split_families(l = 2, data = gen.list)
  expect_s3_class(out, "FielDHub")
  expect_setequal(out$data_locations$ENTRY, 1:12)
})

test_that("family splits keep empty locations without losing entries", {
  data <- data.frame(ENTRY = 1:3, NAME = paste0("G", 1:3), FAMILY = "A")
  expect_warning(design <- split_families(5, data, seed = 38), class = "fieldhub_design_warning")
  counts <- table(factor(design$data_locations$LOCATION, levels = paste("Location", 1:5)))
  expect_equal(
    list(locations = design$rowsEachlist$Location, n = sort(design$rowsEachlist$n),
         entries = sort(design$data_locations$ENTRY), counts_agree = as.numeric(counts)),
    list(locations = paste("Location", 1:5), n = c(0, 0, 1, 1, 1), entries = 1:3,
         counts_agree = design$rowsEachlist$n))
})

# ---- Input errors ----

test_that("split_families() rejects data without complete entries and malformed seeds", {
  data <- data.frame(ENTRY = 1L, NAME = NA_character_, FAMILY = "A")
  cases <- list(
    no_complete_entry = list(l = 3, data = data, seed = 38),
    no_rows = list(l = 3, data = data[0, ], seed = 38),
    not_a_data_frame = list(l = 3, data = "invalid data", seed = 38),
    seed_text = list(l = 3, data = family_seed_data(), seed = "38"),
    seed_na = list(l = 3, data = family_seed_data(), seed = NA),
    seed_infinite = list(l = 3, data = family_seed_data(), seed = Inf),
    seed_vector = list(l = 3, data = family_seed_data(), seed = c(1, 2)),
    seed_too_large = list(l = 3, data = family_seed_data(), seed = 2^31),
    seed_complex = list(l = 3, data = family_seed_data(), seed = 1 + 1i)
  )
  for (nm in names(cases)) {
    set.seed(42)
    before <- .Random.seed
    expect_error(do.call(split_families, cases[[nm]]), class = "fieldhub_input_error", info = nm)
    expect_identical(.Random.seed, before, info = nm)
  }
})

# ---- Seeds and reproduction ----

test_that("split_families() records explicit seeds and leaves the caller's stream", {
  set.seed(42)
  before <- .Random.seed
  design <- split_families(l = 3, data = family_seed_data(), seed = 38)
  expect_identical(.Random.seed, before)
  expect_equal(c(design$infoDesign$seed, design$metadata$seed), c(38, 38))
  set.seed(99)
  expect_identical(split_families(3, family_seed_data(), seed = 38), design)
})

test_that("split_families() consumes exactly one draw for automatic seeds, and the seed replays", {
  set.seed(42)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(42)
  design <- split_families(l = 3, data = family_seed_data())
  expect_identical(.Random.seed, after_one_draw)
  expect_identical(design$metadata$seed, expected)
  expect_identical(split_families(3, family_seed_data(), seed = design$metadata$seed), design)
})

test_that("split_families() creates .Random.seed when the caller session had none", {
  local_rng_state()
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  design <- split_families(3, family_seed_data())
  expect_true(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_type(design$metadata$seed, "integer")
})
