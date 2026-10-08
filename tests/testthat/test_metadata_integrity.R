metadata_integrity_results <- function() list(
  field = RCBD(t = 4, reps = 2, seed = 38),
  allocation = do_optim(design = "sparse", lines = 12, l = 3,
                        copies_per_entry = 2, checks = 2, add_checks = TRUE, seed = 38)
)

test_that("field designs and allocations reject incomplete reproducibility metadata", {
  results <- metadata_integrity_results()
  validators <- list(field = validate_fieldhub_design, allocation = validate_fieldhub_allocation)
  invalid_rng <- list(NULL, "Mersenne-Twister", c("", "Inversion", "Rejection"),
                      c(NA_character_, "Inversion", "Rejection"), c(" ", "Inversion", "Rejection"))
  invalid_version <- list(NULL, "", " ", c("1.5.0", "1.5.1"), 1.5)
  for (kind in names(results)) {
    for (rng in invalid_rng) {
      bad <- results[[kind]]
      bad$metadata$rng_kind <- rng
      expect_error(validators[[kind]](bad), class = "fieldhub_internal_error")
    }
    for (version in invalid_version) {
      bad <- results[[kind]]
      bad$metadata$package_version <- version
      expect_error(validators[[kind]](bad), class = "fieldhub_internal_error")
    }
  }
})

test_that("recorded seeds must be usable even if parameter metadata agrees", {
  results <- metadata_integrity_results()
  validators <- list(field = validate_fieldhub_design, allocation = validate_fieldhub_allocation)
  invalid_seed <- list(NULL, NA_real_, Inf, "38", numeric(), c(38, 39),
                       1 + 1i, .Machine$integer.max + 1)
  for (kind in names(results)) for (seed in invalid_seed) {
    bad <- results[[kind]]
    bad$metadata$seed <- seed
    bad$metadata$parameters$seed <- seed
    if (kind == "field") bad$infoDesign$seed <- seed
    expect_error(validators[[kind]](bad), class = "fieldhub_internal_error")
  }
})

test_that("field metadata agrees with its design seed and remains backward compatible", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  bad <- x
  bad$infoDesign$seed <- 39
  expect_error(validate_fieldhub_design(bad), class = "fieldhub_internal_error")
  x$metadata$parameters <- NULL
  expect_identical(validate_fieldhub_design(x), x)
})

test_that("duplicate reproducibility fields are rejected", {
  results <- metadata_integrity_results()
  validators <- list(field = validate_fieldhub_design, allocation = validate_fieldhub_allocation)
  for (kind in names(results)) {
    bad <- results[[kind]]
    bad$metadata <- c(bad$metadata, list(seed = 38))
    expect_error(validators[[kind]](bad), class = "fieldhub_internal_error")
  }
})
