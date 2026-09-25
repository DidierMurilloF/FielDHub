library(FielDHub)

# Regression tests: when `plotNumber` or `locationNames` did not have one
# value per location, the design functions replaced them with defaults
# silently, or printed "Since plotNumber was missing" to stdout even when
# plotNumber had been supplied. They now raise a warning that says why.

test_that("design functions warn when plotNumber has the wrong length", {
  calls <- list(
    RCBD = function() RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1),
    full_factorial = function() full_factorial(setfactors = c(2, 2), reps = 2, l = 2,
                                               plotNumber = 101, seed = 1),
    incomplete_blocks = function() incomplete_blocks(t = 12, k = 4, r = 2, l = 2,
                                                     plotNumber = 101, seed = 1),
    strip_plot = function() strip_plot(Hplots = 3, Vplots = 2, b = 2, l = 2,
                                       plotNumber = 101, seed = 1),
    RCBD_augmented = function() RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2,
                                               plotNumber = c(1, 2, 3), seed = 1),
    optimized_arrangement = function() optimized_arrangement(
      nrows = 12, ncols = 10, lines = 100, amountChecks = 20, checks = 1:5,
      l = 2, plotNumber = 101, seed = 1
    ),
    partially_replicated = function() partially_replicated(
      nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2),
      l = 2, plotNumber = 101, seed = 1
    ),
    sparse_allocation = function() sparse_allocation(lines = 120, l = 4,
                                                     copies_per_entry = 3, checks = 4,
                                                     plotNumber = c(1, 1001), seed = 1234)
  )
  for (fn in names(calls)) {
    expect_warning(
      utils::capture.output(calls[[fn]]()),
      "'plotNumber' has [0-9]+ value\\(s\\) for [0-9]+ location\\(s\\)",
      info = fn
    )
  }
})

test_that("RCBD() reports default plot numbers as a warning, not on stdout", {
  expect_silent(suppressWarnings(RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1)))
})

test_that("design functions warn when locationNames has the wrong length", {
  names3 <- c("A", "B", "C")
  calls <- list(
    RCBD = function() RCBD(t = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                           locationNames = names3, seed = 1),
    full_factorial = function() full_factorial(setfactors = c(2, 2), reps = 2, l = 2,
                                               plotNumber = c(1, 101),
                                               locationNames = names3, seed = 1),
    incomplete_blocks = function() incomplete_blocks(t = 12, k = 4, r = 2, l = 2,
                                                     plotNumber = c(1, 101),
                                                     locationNames = names3, seed = 1),
    alpha_lattice = function() alpha_lattice(t = 12, k = 4, r = 2, l = 2,
                                             plotNumber = c(1, 101),
                                             locationNames = names3, seed = 1),
    square_lattice = function() square_lattice(t = 16, k = 4, r = 2, l = 2,
                                               plotNumber = c(1, 101),
                                               locationNames = names3, seed = 1),
    rectangular_lattice = function() rectangular_lattice(t = 12, k = 3, r = 2, l = 2,
                                                         plotNumber = c(1, 101),
                                                         locationNames = names3, seed = 1),
    row_column = function() row_column(t = 12, nrows = 3, r = 2, l = 2,
                                       plotNumber = c(1, 101),
                                       locationNames = names3, seed = 1),
    strip_plot = function() strip_plot(Hplots = 3, Vplots = 2, b = 2, l = 2,
                                       plotNumber = c(1, 101),
                                       locationNames = "A", seed = 1),
    diagonal_arrangement = function() diagonal_arrangement(
      nrows = 15, ncols = 20, lines = 270, checks = 4, l = 2,
      plotNumber = c(1, 1001), locationNames = names3, seed = 1
    ),
    RCBD_augmented = function() RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2,
                                               plotNumber = c(1, 101),
                                               locationNames = names3, seed = 1),
    sparse_allocation = function() sparse_allocation(lines = 120, l = 4,
                                                     copies_per_entry = 3, checks = 4,
                                                     locationNames = paste0("L", 1:5),
                                                     seed = 1234)
  )
  for (fn in names(calls)) {
    expect_warning(
      utils::capture.output(calls[[fn]]()),
      "'locationNames' has [0-9]+ value\\(s\\) for [0-9]+ location\\(s\\)",
      info = fn
    )
  }
})
