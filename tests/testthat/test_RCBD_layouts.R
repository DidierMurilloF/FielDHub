library(testthat)
library(FielDHub)
library(dplyr)

# Create an example RCBD design with 4 treatments and 3 replicates.
example_design <- RCBD(t = 4, reps = 3, seed = 89076)

# Use the fieldBook from the example design.
NewBook <- example_design$fieldBook
plots <- NewBook$PLOT
n_TrtGen <- 4
n_Reps <- 3
planter <- "serpentine"

test_that("generate_vertical_layout produces correct basic vertical layout", {
  layouts <- generate_vertical_layout(NewBook, plots, n_TrtGen, n_Reps, planter)

  # Verify that the basic vertical layout exists.
  expect_true("basic_vertical" %in% names(layouts))

  basic_df <- layouts[["basic_vertical"]]
  # Check that the number of rows equals treatments x replicates.
  expect_equal(nrow(basic_df), n_TrtGen * n_Reps)
  # For basic vertical layout:
  # ROW should be rep(1:n_Reps, each = n_TrtGen)
  expect_equal(basic_df$ROW, rep(1:n_Reps, each = n_TrtGen))
  # COLUMN should be rep(1:n_TrtGen, times = n_Reps)
  expect_equal(basic_df$COLUMN, rep(1:n_TrtGen, times = n_Reps))
})

test_that("generate_vertical_layout produces a single-column layout", {
  layouts <- generate_vertical_layout(NewBook, plots, n_TrtGen, n_Reps, planter)

  expect_true("single_column" %in% names(layouts))
  single_df <- layouts[["single_column"]]
  # In a single-column layout, COLUMN should be 1 for all rows.
  expect_equal(single_df$COLUMN, rep(1, n_TrtGen * n_Reps))
})

test_that("generate_vertical_layout produces extended layouts when prime factors exist", {
  layouts <- generate_vertical_layout(NewBook, plots, n_TrtGen, n_Reps, planter)

  # For n_TrtGen = 4, numbers::primeFactors(4) may return c(2,2) (or similar),
  # so we expect at least one extended vertical layout.
  if (length(numbers::primeFactors(n_TrtGen)) >= 2) {
    expect_true(any(grepl("vertical_ext", names(layouts))))
    # Check that the extended layout has a PLOT column computed.
    ext_name <- grep("vertical_ext", names(layouts), value = TRUE)[1]
    ext_layout <- layouts[[ext_name]]
    expect_true("PLOT" %in% colnames(ext_layout))
  }
})

test_that("generate_horizontal_layout produces correct basic horizontal layout", {
  layouts <- generate_horizontal_layout(NewBook, plots, n_TrtGen, n_Reps, planter)

  # Verify that the basic horizontal layout exists.
  expect_true("basic_horizontal" %in% names(layouts))

  basic_df <- layouts[["basic_horizontal"]]
  expect_equal(nrow(basic_df), n_TrtGen * n_Reps)
  # For basic horizontal layout:
  # ROW should be rep(1:n_TrtGen, times = n_Reps)
  expect_equal(basic_df$ROW, rep(1:n_TrtGen, times = n_Reps))
  # COLUMN should be rep(1:n_Reps, each = n_TrtGen)
  expect_equal(basic_df$COLUMN, rep(1:n_Reps, each = n_TrtGen))
})

test_that("generate_horizontal_layout produces extended layouts when available", {
  layouts <- generate_horizontal_layout(NewBook, plots, n_TrtGen, n_Reps, planter)

  # Check if factor-based combinations are available.
  factor_combos <- as.data.frame(FielDHub:::factor_subsets(n_TrtGen, all_factors = TRUE)$comb_factors)
  if (nrow(factor_combos) > 0) {
    expect_true(any(grepl("horizontal_ext", names(layouts))))
    # Check that an extended horizontal layout has a computed PLOT column.
    ext_name <- grep("horizontal_ext", names(layouts), value = TRUE)[1]
    ext_layout <- layouts[[ext_name]]
    expect_true("PLOT" %in% colnames(ext_layout))
  }
})
