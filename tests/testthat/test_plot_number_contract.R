# Plot identifiers have a shared value contract and design-specific numbering.
test_that("physical design engines reject malformed plot starts with input conditions", {
  cases <- c("RCBD_two_locations", "latin_square", "full_factorial_rcbd",
             "split_plot_rcbd", "split_split_plot_rcbd", "strip_plot",
             "incomplete_blocks", "alpha_lattice", "square_lattice",
             "rectangular_lattice", "row_column", "diagonal_single",
             "optimized_arrangement", "RCBD_augmented", "partially_replicated",
             "multi_location_prep", "sparse_allocation")
  for (name in cases) {
    args <- catalogue_design(name)$metadata$parameters
    sites <- if (is.null(args[["l"]])) 1L else args[["l"]]
    invalid <- list(rep(NA_real_, sites), rep(NaN, sites), rep(Inf, sites),
                    rep(-Inf, sites), matrix(rep(101, sites)),
                    rep(101 + 1i, sites), rep("101", sites), rep(TRUE, sites))
    for (bad in invalid) {
      args["plotNumber"] <- list(bad)
      set.seed(1941)
      before <- .Random.seed
      expect_error(suppressWarnings(do.call(catalogue[[name]]$fun, args)),
                   class = "fieldhub_input_error", info = paste(name, deparse(bad)))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("diagonal experiment-specific plot starts are validated before coercion", {
  args <- catalogue_design("diagonal_blocks_row")$metadata$parameters
  for (bad in list(NA_real_, NaN, Inf, -Inf, "101", TRUE, 101 + 1i, matrix(101))) {
    values <- rep(bad[1L], length(args$blocks))
    if (is.matrix(bad)) dim(values) <- c(1L, length(values))
    args$plotNumber <- list(values)
    expect_error(diagonal_arrangement(
      nrows = args$nrows, ncols = args$ncols, lines = args$lines,
      checks = args$checks, kindExpt = args$kindExpt, splitBy = args$splitBy,
      blocks = args$blocks, plotNumber = args$plotNumber, seed = args$seed,
      year = args$year), class = "fieldhub_input_error", info = deparse(bad))
  }
})

test_that("plot-number helpers reject malformed vectors before arithmetic", {
  for (bad in list(NA_real_, NaN, Inf, -Inf, "101", TRUE, 101 + 1i, matrix(101))) {
    for (fun in list(ibd_plot_numbers, seriePlot.numbers, plot_number_splits)) {
      args <- list(plot.number = bad, l = 1)
      if (identical(fun, ibd_plot_numbers)) args <- c(args, list(nt = 4, r = 2))
      else args <- c(args, list(t = 4, reps = 2))
      expect_error(do.call(fun, args), class = "fieldhub_input_error")
    }
  }
})
