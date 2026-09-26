library(FielDHub)

# Design name of the results of each design function
result_designs <- c(
  CRD = "crd", RCBD = "rcbd", latin_square = "latin_square",
  full_factorial = "full_factorial", split_plot = "split_plot",
  split_split_plot = "split_split_plot", strip_plot = "strip_plot",
  incomplete_blocks = "incomplete_blocks", row_column = "row_column",
  square_lattice = "square_lattice", rectangular_lattice = "rectangular_lattice",
  alpha_lattice = "alpha_lattice", partially_replicated = "partially_replicated",
  multi_location_prep = "multi_location_prep", RCBD_augmented = "rcbd_augmented",
  diagonal_arrangement = "diagonal_arrangement", sparse_allocation = "sparse_allocation",
  optimized_arrangement = "optimized_arrangement", split_families = "split_families"
)

test_that("every design result has the class of its design", {
  for (name in names(catalogue)) {
    fun <- catalogue[[name]]$fun
    if (!fun %in% names(result_designs)) next
    expect_identical(
      class(catalogue_design(name)),
      c(paste0("fieldhub_", result_designs[[fun]]), "FielDHub"),
      info = name
    )
  }
})

test_that("every design result records how it was built", {
  version <- as.character(utils::packageVersion("FielDHub"))
  for (name in names(catalogue)) {
    fun <- catalogue[[name]]$fun
    if (!fun %in% names(result_designs)) next
    design <- catalogue_design(name)
    expect_identical(
      design$metadata,
      list(design = result_designs[[fun]], schema_version = 1L,
           seed = design$infoDesign$seed, rng_kind = RNGkind(),
           package_version = version),
      info = name
    )
  }
})

test_that("the seed in the metadata rebuilds the design", {
  design <- RCBD(t = 5, reps = 3)
  again <- RCBD(t = 5, reps = 3, seed = design$metadata$seed)
  expect_identical(again$fieldBook, design$fieldBook)
})

test_that("the metadata leaves the elements of 1.5.0 results in place", {
  design <- RCBD(t = 4, reps = 3, seed = 89076)
  expect_identical(
    names(design),
    c("infoDesign", "layoutRandom", "plotNumber", "fieldBook", "metadata")
  )
})

test_that("the validator accepts design results", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  expect_identical(validate_fieldhub_design(design), design)
})

test_that("the validator rejects results that break the contract", {
  design <- RCBD(t = 4, reps = 2, seed = 1)

  bad <- design
  bad$metadata$design <- "crd"
  expect_error(validate_fieldhub_design(bad), "class that does not match its design",
               class = "fieldhub_internal_error")

  bad <- design
  bad$metadata$schema_version <- 2L
  expect_error(validate_fieldhub_design(bad), "unknown schema version")

  bad <- design
  bad$metadata <- NULL
  expect_error(validate_fieldhub_design(bad), "no metadata naming the design")

  bad <- design
  bad$infoDesign$id_design <- NULL
  expect_error(validate_fieldhub_design(bad), "no infoDesign with id_design")

  bad <- design
  bad$fieldBook <- bad$fieldBook[0, ]
  expect_error(validate_fieldhub_design(bad), "no field book")

  expect_error(validate_fieldhub_design(unclass(design)), "not a FielDHub list")
})
