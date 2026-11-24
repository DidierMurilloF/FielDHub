test_that("the default simulator covers the 21 field designs", {
  cases <- list(
    Diagonal = diagonal_arrangement(15, 20, 270, 4, seed = 1),
    diagonal_multiple = diagonal_arrangement(19, 18, 300, 4, kindExpt = "DBUDC",
      blocks = c(100, 120, 80), seed = 3),
    Optim = optimized_arrangement(12, 10, 100, rep_checks = 20, checks = 1:5, seed = 1),
    RCBD_augmented = RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 1),
    Augmented_RowCol = suppressWarnings(augmented_row_column(10, 8, 4, "columns", budget = 300, seed = 4)),
    sparse_allocation = sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234),
    pREPS = partially_replicated(8, 8, repGens = c(50, 7), repUnits = c(1, 2), seed = 1),
    multi_loc_preps = multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
      rep_checks = c(4, 4), allow_fillers = TRUE, seed = 1),
    Square_Lattice = square_lattice(t = 9, k = 3, reps = 3, seed = 1),
    Rectangular_Lattice = rectangular_lattice(t = 12, k = 3, reps = 3, seed = 1),
    Alpha_Lattice = alpha_lattice(t = 12, k = 3, reps = 3, seed = 1),
    CRD = CRD(6, 3, seed = 1),
    RCBD = RCBD(6, 3, seed = 1),
    LSD = latin_square(5, reps = 2, seed = 1),
    Latin_Rectangle = latin_rectangle(6, seed = 1),
    FD = full_factorial(c(2, 3), reps = 3, seed = 1),
    SPD = split_plot(3, 2, reps = 3, seed = 1),
    SSPD = split_split_plot(3, 2, 2, reps = 3, seed = 1),
    STRIPD = strip_plot(3, 2, reps = 3, seed = 1),
    IBD = incomplete_blocks(12, 3, reps = 3, seed = 1),
    RowCol = row_column(12, nrows = 3, reps = 3, seed = 1)
  )
  expected <- list(
    Diagonal = NULL, diagonal_multiple = "decision_block", Optim = NULL,
    RCBD_augmented = "block", Augmented_RowCol = c("row", "column"), sparse_allocation = NULL,
    pREPS = NULL, multi_loc_preps = NULL,
    Square_Lattice = c("replicate", "incomplete_block"),
    Rectangular_Lattice = c("replicate", "incomplete_block"),
    Alpha_Lattice = c("replicate", "incomplete_block"),
    CRD = NULL, RCBD = "block", LSD = c("square", "row", "column"),
    Latin_Rectangle = c("row", "column"), FD = "block", SPD = c("block", "whole_plot"),
    SSPD = c("block", "whole_plot", "subplot"),
    STRIPD = c("replicate", "horizontal_strip", "vertical_strip"),
    IBD = c("replicate", "incomplete_block"), RowCol = c("replicate", "row", "column")
  )
  expect_setequal(names(cases), vapply(fieldhub_app_registry(), `[[`, character(1), "workflow"))
  for (name in names(cases)) {
    x <- cases[[name]]
    result <- stats::simulate(x, seed = 10)
    expect_s3_class(result, "fieldhub_simulation")
    expect_identical(result$input_field_book, x$fieldBook)
    expect_identical(result$field_book[names(x$fieldBook)], x$fieldBook)
    expect_identical(names(result$metadata$parameters$terms$design), expected[[name]])
    planted <- result$metadata$parameters$terms$planted
    expect_true(all(is.finite(result$field_book$YIELD[planted])), info = name)
    expect_true(all(is.na(result$field_book$YIELD[!planted])), info = name)
    expect_identical(reproduce_simulation(result), result)
  }
})

test_that("treatment effects follow entry identity while location and block follow their units", {
  x <- suppressWarnings(RCBD(t = c("A", "B", "Control"), reps = 3, l = 2,
                             checks = "Control", rep_checks = 2, seed = 5))
  locations <- unique(as.character(x$fieldBook$LOCATION))
  means <- stats::setNames(c(8, 6), rev(locations))
  deviations <- stats::setNames(c(1, 0), rev(locations))
  result <- stats::simulate(x, nsim = 2, seed = 19, mean = means, residual_sd = deviations)
  book <- result$field_book
  draw <- result$effects[[1L]]
  check <- book$TREATMENT == "Control"
  expect_length(unique(draw$treatment[check]), 1)
  expect_true(all(draw$residual[book$LOCATION == locations[1L]] == 0))
  expect_true(all(draw$location == 0))
  expect_true(all(vapply(split(draw$block, list(book$LOCATION, book$REP)),
    function(block) length(unique(block)) == 1L, logical(1))))
  expect_equal(book$YIELD_1, unname(means[as.character(book$LOCATION)]) +
    draw$treatment + draw$block + draw$residual)
  expect_false(identical(result$responses[[1L]], result$responses[[2L]]))
  expect_identical(reproduce_simulation(result), result)
})

test_that("physical coordinates control only the optional spatial component", {
  x <- RCBD(t = 6, reps = 3, seed = 1)
  expect_error(stats::simulate(x, spatial = TRUE), "physical ROW and COLUMN")
  x <- RCBD(t = 6, reps = 3, nrows = 3, seed = 1)
  result <- stats::simulate(x, seed = 9, spatial = TRUE, residual_sd = 0,
                            treatment_sd = 0, design_sd = 0)
  expect_equal(result$field_book$YIELD, 100 + result$effects[[1L]]$spatial)
  expect_identical(names(result$metadata$parameters$terms$design), "block")
  expect_identical(reproduce_simulation(result), result)
  kept <- x$fieldBook[-2, ]
  x$fieldBook <- kept
  expect_identical(stats::simulate(x, spatial = TRUE)$input_field_book, kept)
  x$fieldBook <- rbind(kept, kept[1, ])
  expect_error(stats::simulate(x, spatial = TRUE), "unique physical")
})

test_that("a seeded simulation preserves caller randomness and validates overrides", {
  x <- CRD(4, 2, seed = 1)
  set.seed(22)
  previous <- .Random.seed
  zero <- stats::simulate(x, seed = 4, mean = -2, treatment_sd = 0, residual_sd = 0)
  expect_identical(zero$field_book$YIELD, rep(-2, nrow(x$fieldBook)))
  expect_error(stats::simulate(x, seed = 4, design_sd = c(block = 0)), "design effects")
  expect_error(stats::simulate(x, residual_sd = -1), "nonnegative")
  expect_error(stats::simulate(x, trait = "ID"), "already present")
  expect_error(stats::simulate(x, nsim = 0), "nsim")
  expect_identical(.Random.seed, previous)
})

test_that("the app uses package responses for the displayed field book", {
  x <- RCBD(6, 3, nrows = 3, seed = 1)
  book <- x$fieldBook
  settings <- list(trait = "HEIGHT", mean = 6, residual_sd = 0.4, spatial = TRUE)
  expected <- do.call(stats::simulate, c(list(object = x, seed = 7), settings))
  actual <- app_trait_simulation(x, book, settings, seed = 7, renumber_display = TRUE)
  expect_identical(actual$simulation, expected)
  expect_identical(actual$df$HEIGHT, expected$field_book$HEIGHT)

  rc <- row_column(12, nrows = 3, reps = 3, seed = 1)
  layout <- field_layout(rc, stacked = "horizontal")
  result <- app_trait_simulation(rc, layout, settings, seed = 7)
  source <- rc$fieldBook[match(layout$ID, rc$fieldBook$ID), ]
  expect_identical(result$df[names(layout)], layout)
  expect_identical(result$simulation$input_field_book$ROW_REP, source$ROW)
  expect_identical(result$simulation$input_field_book$ROW, layout$ROW)
  expect_true("spatial" %in% names(result$simulation$effects[[1L]]))
})

test_that("row-column effects follow the realized nested or shared axes", {
  nested <- row_column(24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                       method = "onestage")
  shared <- row_column(24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                       method = "onestage", latinize = TRUE)
  nr <- stats::simulate(nested, seed = 2)$metadata$parameters$terms$design
  sr <- stats::simulate(shared, seed = 2)$metadata$parameters$terms$design
  expect_length(unique(nr$row), 12)
  expect_length(unique(nr$column), 8)
  expect_length(unique(sr$row), 6)
  expect_length(unique(sr$column), 4)
})

test_that("changing the residual scale rescales the default trait effects", {
  x <- RCBD(t = 6, reps = 3, nrows = 3, seed = 1)
  standard <- stats::simulate(x, seed = 2, mean = 0, spatial = TRUE)
  smaller <- stats::simulate(x, seed = 2, mean = 0, residual_sd = 0.4, spatial = TRUE)
  expect_equal(smaller$field_book$YIELD, standard$field_book$YIELD * 0.05)
  expect_identical(reproduce_simulation(smaller), smaller)
})

test_that("spatial correlation retains grid distances across unplanted gaps", {
  book <- data.frame(LOCATION = "A", ROW = c(1, 1, 7, 7), COLUMN = c(1, 3, 1, 3))
  spec <- list(sd = 1, rho_row = .8, rho_col = -.7)
  patch <- simulation_spatial_patches(book, rep(1L, 4), spec)[[1L]]
  set.seed(104)
  draws <- replicate(4000, simulation_spatial_draw(patch, spec))
  expected <- spec$rho_row ^ abs(outer(book$ROW, book$ROW, "-")) *
    spec$rho_col ^ abs(outer(book$COLUMN, book$COLUMN, "-"))
  expect_equal(unname(stats::cov(t(draws))), expected, tolerance = .06)
})

test_that("augmented RCBD blocks and entries use their realized identities", {
  entries <- data.frame(ENTRY = 1:53, NAME = c("A", "B", "C", "A", paste0("G", 5:53)))
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 2L,
                      l = 2, data = entries, seed = 3)
  out <- stats::simulate(x, seed = 9)
  effects <- out$effects[[1L]]
  book <- out$field_book
  units <- unique(data.frame(book[c("LOCATION", "EXPT", "BLOCK")], effect = effects$block))
  expect_identical(nrow(units), 20L)
  expect_length(unique(units$effect), 20L)
  expect_length(unique(effects$treatment[book$ENTRY == 1]), 1L)
  expect_length(unique(effects$treatment[book$ENTRY == 4]), 1L)
  expect_false(identical(effects$treatment[match(1, book$ENTRY)],
                         effects$treatment[match(4, book$ENTRY)]))
})

test_that("row-column field layouts preserve shared experimental axes in the API and app", {
  x <- row_column(24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                  method = "onestage", latinize = TRUE)
  layout <- field_layout(x, stacked = "horizontal")
  indexes <- match(layout$ID, x$fieldBook$ID)
  expect_identical(layout$ROW_REP, x$fieldBook$ROW[indexes])
  expect_identical(layout$COLUMN_REP, x$fieldBook$COLUMN[indexes])
  expect_error(stats::simulate(x, spatial = TRUE), "field_layout")
  settings <- list(trait = "YIELD", mean = 100, residual_sd = 8, spatial = TRUE)
  app <- app_trait_simulation(x, layout, settings, seed = 42)
  x$fieldBook <- layout
  sim <- stats::simulate(x, spatial = TRUE, seed = 42)
  expect_length(unique(sim$effects[[1L]]$column), 4L)
  expect_identical(app$df$YIELD, sim$field_book$YIELD)
  expect_error(simulation_request(100, 8, "Other", "ROW_REP", field_book = layout),
               "already present")
})

test_that("numeric Latin-square experimental labels require a physical layout", {
  data <- data.frame(Period = c("101", "205", "414"), Cow = c("605", "902", "987"),
                     Treatment = c("A1", "A2", "A3"))
  x <- latin_square(reps = 1, seed = 2, data = data)
  expect_error(stats::simulate(x, spatial = TRUE), "field_layout")
  x$fieldBook <- field_layout(x)
  result <- stats::simulate(x, spatial = TRUE, seed = 2)
  expect_identical(sort(unique(result$field_book$COLUMN)), 1:3)
  expect_setequal(as.character(result$field_book$COLUMN_SQ), data$Cow)
  expect_true("spatial" %in% names(result$effects[[1L]]))
})

test_that("diagonal fillers create no decision-block term", {
  x <- diagonal_arrangement(10, 32, 287, 4, planter = "cartesian", seed = 25)
  result <- stats::simulate(x, seed = 25)
  filler <- x$fieldBook$ENTRY == 0
  expect_true(any(filler))
  expect_true(all(is.na(result$field_book$YIELD[filler])))
  expect_false("decision_block" %in% names(result$effects[[1L]]))
  expect_error(stats::simulate(x, design_sd = c(decision_block = 1)), "design effects")
})

test_that("simulation and its replay preserve the caller's Box-Muller cache", {
  x <- RCBD(t = 6, reps = 3, nrows = 3, seed = 2)
  local_rng_state()
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE, after = FALSE)
  RNGkind(normal.kind = "Box-Muller")
  set.seed(21)
  expected <- stats::rnorm(2)
  set.seed(21)
  expect_identical(stats::rnorm(1), expected[1L])
  before <- .Random.seed
  result <- stats::simulate(x, nsim = 2, seed = 6, spatial = TRUE)
  expect_identical(result, reproduce_simulation(result))
  expect_identical(.Random.seed, before)
  expect_identical(stats::rnorm(1), expected[2L])
  RNGkind(normal.kind = "Inversion")
  expect_identical(result, reproduce_simulation(result))
  expect_identical(RNGkind()[2L], "Inversion")
})

test_that("unambiguous labels share an effect when entry numbers are location-local", {
  names <- paste0("G", seq_len(12))
  entries <- data.frame(
    LOCATION = rep(c("A", "B"), each = 12L),
    ENTRY = rep(seq_len(12), 2L),
    NAME = c(names, rev(names)),
    REPS = rep(c(rep(1, 8L), rep(2, 4L)), 2L)
  )
  x <- partially_replicated(4, 4, l = 2, locationNames = c("A", "B"),
                            multiLocationData = TRUE, data = entries, seed = 1)
  result <- stats::simulate(x, seed = 19)
  shared <- result$field_book$TREATMENT == "G1"
  expect_length(unique(result$field_book$ENTRY[shared]), 2L)
  expect_length(unique(result$effects[[1L]]$treatment[shared]), 1L)
})
