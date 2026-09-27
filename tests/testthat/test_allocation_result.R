allocation_example <- function(design = "sparse", seed = 38) {
  do_optim(design = design, lines = 12, l = 4,
             copies_per_entry = if (design == "sparse") 3 else 5,
             add_checks = TRUE, checks = 2, rep_checks = c(2, 2), seed = seed)
}

test_that("allocation results record a reproducible input contract", {
  for (design in c("sparse", "prep")) {
    out <- allocation_example(design)
    expect_identical(class(out), if (design == "sparse") "Sparse" else "MultiPrep")
    expect_identical(out$metadata$design, paste0("allocation_", design))
    expect_identical(out$metadata$schema_version, fieldhub_schema_version)
    expect_identical(out$metadata$seed, 38)
    expect_identical(out$metadata$rng_kind, RNGkind())
    expect_identical(out$metadata$package_version,
                       as.character(utils::packageVersion("FielDHub")))
    expect_named(out$metadata$parameters, names(formals(do_optim)))
    expect_identical(do.call(do_optim, out$metadata$parameters), out)
    expect_identical(validate_fieldhub_allocation(out), out)
    expect_true(withVisible(allocation_example(design))$visible)
  }
})

test_that("automatic allocation seeds consume exactly one draw from the caller's stream", {
  set.seed(91)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(91)
  out <- allocation_example(seed = NULL)
  expect_identical(.Random.seed, after_one_draw)
  expect_type(out$metadata$seed, "integer")
  expect_identical(out$metadata$seed, expected)
  expect_identical(do.call(do_optim, out$metadata$parameters), out)
})

test_that("allocation result validation reports structural failures", {
  out <- allocation_example()
  bad_objects <- list(1, unclass(out))
  for (name in c("metadata", "allocation", "size_locations", "list_locs",
                  "multi_location_data")) {
    bad <- out
    bad[[name]] <- NULL
    bad_objects[[length(bad_objects) + 1L]] <- bad
  }
  bad_counts <- out
  bad_counts$allocation[1, 1] <- -1
  bad_totals <- out
  bad_totals$size_locations[1] <- bad_totals$size_locations[1] + 1
  bad_seed <- out
  bad_seed$metadata$parameters$seed <- 99
  for (bad in c(bad_objects, list(bad_counts, bad_totals, bad_seed))) {
    expect_error(validate_fieldhub_allocation(bad), class = "fieldhub_internal_error")
  }
})

test_that("allocation metadata requires RNG and version information", {
  out <- allocation_example()
  for (field in c("rng_kind", "package_version")) {
    bad <- out
    bad$metadata[[field]] <- NULL
    expect_error(validate_fieldhub_allocation(bad), class = "fieldhub_internal_error")
  }
  bad <- out
  bad$metadata$schema_version <- 99L
  expect_error(validate_fieldhub_allocation(bad), "unknown schema version",
                 class = "fieldhub_internal_error")
})

test_that("malformed allocation constructor inputs use internal conditions", {
  out <- allocation_example()
  parameters <- out$metadata$parameters
  parameters$design <- new.env(parent = emptyenv())
  expect_error(new_fieldhub_allocation(out, parameters), class = "fieldhub_internal_error")
  expect_error(new_fieldhub_allocation(1, list()), class = "fieldhub_internal_error")
})
