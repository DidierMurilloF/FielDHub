# Shared public input contract, independent of application controls.
test_that("block engines reject malformed scalar controls on generated and supplied paths", {
  engines <- list(incomplete_blocks = c(t = 12, k = 3), alpha_lattice = c(t = 12, k = 3),
                   square_lattice = c(t = 16, k = 4), rectangular_lattice = c(t = 12, k = 3))
  invalid <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, -1, 0, .5, "2", TRUE, c(2, 3), matrix(2))
  for (name in names(engines)) for (supplied in c(FALSE, TRUE)) {
    args <- c(as.list(engines[[name]]), list(reps = 2, seed = 19))
    if (supplied) args$data <- data.frame(ENTRY = seq_len(args$t), TREATMENT = paste0("T", seq_len(args$t)))
    for (parameter in c("k", "reps")) for (bad in invalid) {
      candidate <- args
      candidate[parameter] <- list(bad)
      set.seed(918)
      before <- .Random.seed
      expect_error(do.call(name, candidate), class = "fieldhub_input_error",
                    info = paste(name, parameter, supplied, deparse(bad)))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("block-size and field-book size bounds are checked before allocation", {
  expect_error(incomplete_blocks(t = 12, k = 1, reps = 2, seed = 19),
                class = "fieldhub_input_error")
  expect_error(validate_block_design_inputs(12, 3, .Machine$integer.max, 1, NULL),
                class = "fieldhub_input_error")
  expect_error(validate_block_design_inputs(12, 3, 2, .Machine$integer.max, NULL),
                class = "fieldhub_input_error")
})

test_that("block engines validate treatment counts and labels before constructing entries", {
  for (name in c("incomplete_blocks", "alpha_lattice", "square_lattice", "rectangular_lattice")) {
    for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, TRUE,
                     list(1, 2), matrix(1:4), c("A", NA, "C", "D"), c("A", " ", "C", "D"))) {
      expect_error(do.call(name, list(t = bad, k = 2, reps = 2, seed = 19)),
                    class = "fieldhub_input_error", info = paste(name, deparse(bad)))
    }
  }
})

test_that("lattice entry tables must have two columns before they are indexed", {
  for (name in c("square_lattice", "rectangular_lattice")) {
    args <- if (name == "square_lattice") list(t = 16, k = 4) else list(t = 12, k = 3)
    args <- c(args, list(reps = 2, seed = 19, data = data.frame(ENTRY = seq_len(args$t))))
    expect_error(do.call(name, args), class = "fieldhub_input_error")
  }
})
