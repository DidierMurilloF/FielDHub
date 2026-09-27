library(FielDHub)

test_that("every planter check shares one classed error", {
  e <- expect_error(FielDHub:::validate_planter("zigzag"), class = "fieldhub_input_error")
  expect_identical(e$options, c("serpentine", "cartesian"))

  # Internal callers of the shared validator: each used to have its own
  # copy of the "serpentine" or "cartesian" check.
  internal_calls <- list(
    function() FielDHub:::ARCBD_name(planter = "zigzag"),
    function() FielDHub:::ARCBD_plot_number(planter = "zigzag"),
    function() FielDHub:::export_design(movement_planter = "zigzag"),
    function() FielDHub:::get_random(planter_mov = "zigzag")
  )
  for (f in internal_calls) {
    e <- expect_error(f(), class = "fieldhub_input_error")
    expect_identical(e$options, c("serpentine", "cartesian"))
  }
})

test_that("every exported planter check shares the same classed error", {
  calls <- list(
    function() RCBD(t = 4, reps = 2, planter = "zigzag", seed = 1),
    function() full_factorial(setfactors = c(2, 2), planter = "zigzag", seed = 1),
    function() partially_replicated(nrows = 8, ncols = 5, repGens = c(10, 20), repUnits = c(2, 1),
                                    planter = "zigzag", seed = 1),
    function() diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                    planter = "zigzag", seed = 1),
    function() latin_square(t = 4, planter = "zigzag", seed = 1),
    function() RCBD_augmented(lines = 50, checks = 3, b = 6, planter = "zigzag", seed = 1),
    function() optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                                     rep_checks = c(10, 10, 10, 10), planter = "zigzag", seed = 1)
  )
  for (f in calls) {
    e <- expect_error(f(), class = "fieldhub_input_error")
    expect_identical(e$options, c("serpentine", "cartesian"))
  }
})

test_that("optimized_arrangement accepts rep_checks and deprecates amountChecks", {
  a <- optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                             rep_checks = c(10, 10, 10, 10), seed = 1)
  expect_warning(
    b <- optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                               amountChecks = c(10, 10, 10, 10), seed = 1),
    class = "fieldhub_deprecated_warning"
  )
  expect_identical(a$fieldBook, b$fieldBook)
  expect_error(
    optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                          rep_checks = c(10, 10, 10, 10), amountChecks = c(10, 10, 10, 10), seed = 1),
    class = "fieldhub_input_error"
  )
})

test_that("optimized_arrangement records rep_checks, not amountChecks", {
  a <- optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                             rep_checks = c(10, 10, 10, 10), seed = 1)
  expect_identical(a$metadata$parameters$rep_checks, c(10, 10, 10, 10))
  expect_null(a$metadata$parameters$amountChecks)
})

test_that("default_plot_starts() reproduces the two inline formulas it replaces", {
  expect_identical(FielDHub:::default_plot_starts(1, 1001), 1001)
  expect_identical(FielDHub:::default_plot_starts(3, 1001), c(1001, 2001, 3001))
  expect_identical(FielDHub:::default_plot_starts(1, 1), 1)
  expect_identical(FielDHub:::default_plot_starts(3, 1), c(1, 1001, 2001))
})

test_that("(R5) a design called with its own default plotNumber builds defaults silently", {
  expect_no_warning(RCBD(t = 4, reps = 2, l = 2, seed = 1))
  expect_no_warning(split_plot(wp = 3, sp = 2, reps = 2, l = 2, seed = 1))
  expect_no_warning(strip_plot(Hplots = 3, Vplots = 2, reps = 2, l = 2, seed = 1))
  expect_no_warning(incomplete_blocks(t = 12, k = 4, reps = 2, l = 2, seed = 1))
  expect_no_warning(row_column(t = 12, nrows = 3, reps = 2, l = 2, seed = 1, iterations = 20))
  expect_no_warning(alpha_lattice(t = 12, k = 4, reps = 2, l = 2, seed = 1))
})

test_that("(R5) a caller-supplied wrong-length plotNumber still warns", {
  w <- expect_warning(RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1),
                      class = "fieldhub_default_warning")
  expect_identical(w$argument, "plotNumber")
  w <- expect_warning(row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = 101, seed = 1,
                                 iterations = 20),
                      class = "fieldhub_default_warning")
  expect_identical(w$argument, "plotNumber")
})

test_that("augmented RCBD lists exactly the block counts it accepts", {
  # This is a fieldhub_dimension_error (block counts are field-size feasibility,
  # not an argument-shape problem), a more specific fieldhub_error than the
  # brief's fieldhub_input_error placeholder class.
  e <- expect_error(RCBD_augmented(lines = 20, checks = 4, b = 2, seed = 1), class = "fieldhub_error")
  accepted <- Filter(function(b) !inherits(try(RCBD_augmented(lines = 20, checks = 4, b = b, seed = 1), silent = TRUE), "try-error"), 1:10)
  for (b in accepted) expect_true(grepl(paste0("\\b", b, "\\b"), conditionMessage(e)))
})

test_that("valid_block_sizes() backs the block-size checks in incomplete_blocks() and row_column()", {
  e1 <- expect_error(incomplete_blocks(t = 12, k = 5, reps = 2, seed = 1), class = "fieldhub_input_error")
  expect_match(conditionMessage(e1), "incomplete_blocks", fixed = TRUE)
  e2 <- expect_error(row_column(t = 12, nrows = 5, reps = 2, seed = 1), class = "fieldhub_input_error")
  expect_match(conditionMessage(e2), "row_column", fixed = TRUE)
  expect_false(grepl("incomplete_blocks", conditionMessage(e2), fixed = TRUE))
})

test_that("(R6) incomplete_blocks() keeps its 1.5 signature without an internal caller argument", {
  expect_false("caller" %in% names(formals(incomplete_blocks)))
  expect_identical(
    names(formals(incomplete_blocks)),
    c("t", "k", "r", "l", "plotNumber", "locationNames", "seed", "data", "reps")
  )
})

test_that("(R6) row_column() names itself, not incomplete_blocks(), in its block-size error", {
  e <- expect_error(row_column(t = 12, nrows = 5, reps = 2, seed = 1), class = "fieldhub_input_error")
  expect_match(conditionMessage(e), "row_column", fixed = TRUE)
})
