# Trait simulation: stats::simulate() on a design, the app's classic
# (truncated-normal) and spatial (AR1 x AR1) simulators, their replay with
# reproduce_simulation(), and the random-number state around them.

# Names of the failed entries of a named logical vector, for messages.
failed_checks <- function(ok) paste(names(ok)[!ok], collapse = ", ")

# The default simulator: stats::simulate() on a design ----------------------

test_that("the default simulator covers every field design of the app", {
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
  # The design terms each family must contribute
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
    terms <- result$metadata$parameters$terms
    planted <- terms$planted
    yield <- result$field_book$YIELD
    groups <- c(terms$treatment, terms$design)
    # Every unit of a term (an entry, a block, ...) gets one effect
    one_effect_per_unit <- vapply(names(groups), function(term) {
      unit <- groups[[term]] > 0L
      all(tapply(result$effects[[1L]][[term]][unit], groups[[term]][unit],
                 function(effect) length(unique(effect)) == 1L))
    }, logical(1))
    ok <- c(
      class = inherits(result, "fieldhub_simulation"),
      input_kept = identical(result$input_field_book, x$fieldBook),
      columns_kept = identical(result$field_book[names(x$fieldBook)], x$fieldBook),
      design_terms = identical(names(terms$design), expected[[name]]),
      planted_finite = all(is.finite(yield[planted])),
      fillers_missing = all(is.na(yield[!planted])),
      additive = isTRUE(all.equal(yield[planted], (100 + Reduce(`+`, result$effects[[1L]]))[planted])),
      one_effect_per_unit = all(one_effect_per_unit),
      replays = identical(reproduce_simulation(result), result)
    )
    expect(all(ok), sprintf("case %s: %s", name, failed_checks(ok)))
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
  ok <- c(
    check_shares_effect = length(unique(draw$treatment[book$TREATMENT == "Control"])) == 1L,
    zero_sd_location_has_no_error = all(draw$residual[book$LOCATION == locations[1L]] == 0),
    named_means_have_no_shift = all(draw$location == 0),
    one_block_effect_per_rep = all(vapply(split(draw$block, list(book$LOCATION, book$REP)),
      function(block) length(unique(block)) == 1L, logical(1))),
    draws_differ = !identical(result$responses[[1L]], result$responses[[2L]])
  )
  expect(all(ok), failed_checks(ok))
  expect_equal(book$YIELD_1, unname(means[as.character(book$LOCATION)]) +
    draw$treatment + draw$block + draw$residual)
  expect_identical(reproduce_simulation(result), result)
  printed <- capture.output(print(result))
  expect_true(length(printed) < 20L && any(grepl("Design effects: block", printed)),
              info = paste(printed, collapse = "\n"))
})

test_that("physical coordinates control only the optional spatial component", {
  x <- RCBD(t = 6, reps = 3, seed = 1)
  # No physical ROW and COLUMN in the field book
  expect_error(stats::simulate(x, spatial = TRUE), class = "fieldhub_input_error")
  x <- RCBD(t = 6, reps = 3, nrows = 3, seed = 1)
  result <- stats::simulate(x, seed = 9, spatial = TRUE, residual_sd = 0,
                            treatment_sd = 0, design_sd = 0)
  expect_equal(result$field_book$YIELD, 100 + result$effects[[1L]]$spatial)
  expect_identical(list(names(result$metadata$parameters$terms$design),
                        identical(reproduce_simulation(result), result)),
                   list("block", TRUE))
  kept <- x$fieldBook[-2, ]
  x$fieldBook <- kept
  expect_identical(stats::simulate(x, spatial = TRUE)$input_field_book, kept)
  # A new plot (its own ID and PLOT) at the coordinates of another
  extra <- kept[1, ]
  extra$ID <- max(kept$ID) + 1
  extra$PLOT <- max(kept$PLOT) + 1
  x$fieldBook <- rbind(kept, extra)
  expect_error(stats::simulate(x, spatial = TRUE), class = "fieldhub_input_error")
})

test_that("a seeded simulation preserves caller randomness and validates overrides", {
  x <- CRD(4, 2, seed = 1)
  set.seed(22)
  previous <- .Random.seed
  zero <- stats::simulate(x, seed = 4, mean = -2, treatment_sd = 0, residual_sd = 0)
  grid <- RCBD(6, 3, nrows = 3, seed = 1)
  expect_identical(zero$field_book$YIELD, rep(-2, nrow(x$fieldBook)))
  bad <- list(
    `design_sd for a term the design lacks` = function() stats::simulate(x, seed = 4, design_sd = c(block = 0)),
    `negative residual_sd` = function() stats::simulate(x, residual_sd = -1),
    `trait already in the field book` = function() stats::simulate(x, trait = "ID"),
    `nsim = 0` = function() stats::simulate(x, nsim = 0),
    `named treatment_sd` = function() stats::simulate(x, treatment_sd = c(a = 1)),
    # On a design with field coordinates, so only the correlation can be refused
    `correlation range given high to low` = function() stats::simulate(grid, spatial = list(rho_row = c(.8, .2))),
    `three correlation values` = function() stats::simulate(grid, spatial = list(rho_col = c(.1, .2, .3))),
    `correlation range reaching 1` = function() stats::simulate(grid, spatial = list(rho_row = c(.5, 1)))
  )
  for (name in names(bad)) expect_error(bad[[name]](), class = "fieldhub_input_error", info = name)
  expect_identical(.Random.seed, previous)
})

test_that("row-column effects follow the realized nested or shared axes", {
  nested <- row_column(24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                       method = "onestage")
  shared <- row_column(24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                       method = "onestage", latinize = TRUE)
  levels_of <- function(x) {
    terms <- stats::simulate(x, seed = 2)$metadata$parameters$terms$design
    c(row = length(unique(terms$row)), column = length(unique(terms$column)))
  }
  # Nested axes: one row per replicate row; shared axes span the replicates
  expect_identical(list(nested = levels_of(nested), shared = levels_of(shared)),
                   list(nested = c(row = 12L, column = 8L), shared = c(row = 6L, column = 4L)))
})

test_that("changing the residual scale rescales the default trait effects", {
  x <- RCBD(t = 6, reps = 3, nrows = 3, seed = 1)
  standard <- stats::simulate(x, seed = 2, mean = 0, spatial = TRUE)
  smaller <- stats::simulate(x, seed = 2, mean = 0, residual_sd = 0.4, spatial = TRUE)
  # Default residual_sd is 8: every default scale shrinks by 0.4 / 8
  expect_equal(smaller$field_book$YIELD, standard$field_book$YIELD * 0.05)
  expect_identical(reproduce_simulation(smaller), smaller)
})

test_that("spatial correlation retains grid distances across unplanted gaps", {
  x <- CRD(2, 2, seed = 1)
  x$fieldBook$ROW <- c(1, 1, 7, 7)
  x$fieldBook$COLUMN <- c(1, 4, 1, 4)
  spec <- list(sd = 1, rho_row = .8, rho_col = -.7)
  result <- stats::simulate(x, nsim = 4000, seed = 104, mean = 0, residual_sd = 0,
                            treatment_sd = 0, spatial = spec)
  expected <- spec$rho_row ^ abs(outer(x$fieldBook$ROW, x$fieldBook$ROW, "-")) *
    spec$rho_col ^ abs(outer(x$fieldBook$COLUMN, x$fieldBook$COLUMN, "-"))
  # Sample covariances of 4000 unit-variance draws have standard error about
  # 1 / sqrt(4000) = 0.016; 0.06 is under four standard errors.
  expect_equal(unname(stats::cov(t(as.matrix(result$responses)))), expected, tolerance = .06)
})

test_that("spatial correlations are drawn for each draw and location and reported", {
  x <- RCBD(t = 6, reps = 3, l = 2, nrows = 3, ncols = 6, seed = 1)
  cases <- list(
    default = list(spatial = TRUE, row = c(.15, .9), col = c(.15, .9)),
    range_and_fixed = list(spatial = list(rho_row = c(.3, .4), rho_col = -.2),
                           row = c(.3, .4), col = c(-.2, -.2))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    d <- stats::simulate(x, nsim = 100, seed = 8, spatial = case$spatial)$spatial_draws
    within <- function(v, range) all(v >= range[1L] & v <= range[2L])
    distinct <- function(v, range) length(unique(v)) == if (range[1L] < range[2L]) 200L else 1L
    # 200 uniform draws all miss the outer 5% at one end with probability 0.95^200 < 1e-4
    spans <- function(v, range) {
      edge <- 0.05 * (range[2L] - range[1L])
      range[1L] == range[2L] || (min(v) < range[1L] + edge && max(v) > range[2L] - edge)
    }
    ok <- c(
      one_row_per_draw_and_location = identical(paste(d$draw, d$LOCATION),
                                                paste(rep(1:100, each = 2), c("loc1", "loc2"))),
      rho_row_in_range = within(d$rho_row, case$row), rho_col_in_range = within(d$rho_col, case$col),
      rho_row_spans_range = spans(d$rho_row, case$row), rho_col_spans_range = spans(d$rho_col, case$col),
      # A range gives every draw and location its own value; a fixed value is kept
      rho_row_drawn_or_fixed = distinct(d$rho_row, case$row),
      rho_col_drawn_or_fixed = distinct(d$rho_col, case$col)
    )
    expect(all(ok), paste0(name, ": ", failed_checks(ok)))
  }
})

test_that("the reported drawn correlation is the one behind the spatial field", {
  # One long line of plots, down a column or along a row: the lag-1
  # autocorrelation of its spatial effects estimates the correlation used
  # along that line in each draw
  x <- CRD(4, 100, seed = 1)
  cases <- list(
    rho_row = list(row = seq_len(400), col = rep(1, 400), along = "ROW",
                   spatial = list(sd = 1, rho_row = c(.1, .9), rho_col = .5)),
    rho_col = list(row = rep(1, 400), col = seq_len(400), along = "COLUMN",
                   spatial = list(sd = 1, rho_row = .5, rho_col = c(.1, .9)))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    x$fieldBook$ROW <- case$row
    x$fieldBook$COLUMN <- case$col
    sim <- stats::simulate(x, nsim = 10, seed = 5, mean = 0, residual_sd = 0, treatment_sd = 0,
                           spatial = case$spatial)
    estimate <- vapply(sim$effects, function(e) {
      s <- e$spatial[order(sim$field_book[[case$along]])]
      stats::cor(s[-1L], s[-400L])
    }, numeric(1))
    # The estimate's standard error is about sqrt((1 - rho^2) / 400) <= 0.05;
    # 0.2 is four standard errors
    reported <- sim$spatial_draws[[name]]
    error <- abs(estimate - reported)
    expect(max(error) < .2, sprintf("%s, draw %d: estimated %.3f, reported %.3f", name,
                                    which.max(error), estimate[which.max(error)], reported[which.max(error)]))
  }
})

test_that("the simulation summary shows the drawn correlations by location", {
  x <- RCBD(6, 3, l = 2, nrows = 3, seed = 1)
  sim <- stats::simulate(x, nsim = 2, seed = 8, spatial = TRUE)
  printed <- trimws(capture.output(print(sim)))
  draws <- sim$spatial_draws
  expected <- sprintf("draw %d, %s: rho_row = %s, rho_col = %s",
                      draws$draw[1L], draws$LOCATION[1L],
                      format(draws$rho_row[1L], digits = 3),
                      format(draws$rho_col[1L], digits = 3))
  expect_true(expected %in% printed)
  expect_true("... 1 more" %in% printed)
})

test_that("fixed correlations give the values of FielDHub 1.7.5", {
  # Values from FielDHub 1.7.5 (master 8feaab5) for this call: fixed
  # correlations must consume no extra random numbers
  x <- RCBD(6, 3, nrows = 3, seed = 1)
  sim <- stats::simulate(x, nsim = 2, seed = 11, spatial = list(rho_row = .5, rho_col = .3))
  expect_equal(unname(unlist(sim$responses[c(1, 9, 18), ])),
               c(89.2420240921, 94.1475130999, 97.6314915472, 90.1732757525, 105.1410087041,
                 90.0072121455), tolerance = 1e-9)
  # Values taken from a named vector, accepted by 1.7.5, give the same draws
  pars <- c(rho_row = .5, rho_col = .3)
  named <- stats::simulate(x, nsim = 2, seed = 11,
                           spatial = list(rho_row = pars["rho_row"], rho_col = pars["rho_col"]))
  expect_identical(named$responses, sim$responses)
})

test_that("layouts simulated with one seed share their drawn spatial fields", {
  x <- RCBD(6, 3, nrows = 3, seed = 1)
  # The same design with treatments rearranged within blocks and the field
  # book rows in another order
  y <- x
  book <- y$fieldBook
  book$TREATMENT <- unsplit(lapply(split(book$TREATMENT, book$REP), rev), book$REP)
  y$fieldBook <- book[c(18:1), ]
  # Another design on the same grid
  z <- CRD(6, 3, seed = 2)
  z$fieldBook$ROW <- x$fieldBook$ROW
  z$fieldBook$COLUMN <- x$fieldBook$COLUMN
  run <- function(d) stats::simulate(d, nsim = 3, seed = 21, spatial = TRUE)
  sx <- run(x)
  sy <- run(y)
  sz <- run(z)
  by_plot <- function(s, part) unlist(lapply(s$effects, function(e) e[[part]][order(s$field_book$ID)]))
  rho <- function(s) s$spatial_draws[c("rho_row", "rho_col")]
  ok <- c(
    same_design_same_correlations = identical(rho(sx), rho(sy)),
    same_design_same_spatial_field = identical(by_plot(sx, "spatial"), by_plot(sy, "spatial")),
    same_design_same_plot_errors = identical(by_plot(sx, "residual"), by_plot(sy, "residual")),
    other_design_same_correlations = identical(rho(sx), rho(sz))
  )
  expect(all(ok), failed_checks(ok))
})

test_that("augmented RCBD blocks and entries use their realized identities", {
  entries <- data.frame(ENTRY = 1:53, NAME = c("A", "B", "C", "A", paste0("G", 5:53)))
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 2L,
                      l = 2, data = entries, seed = 3)
  out <- stats::simulate(x, seed = 9)
  effects <- out$effects[[1L]]
  book <- out$field_book
  units <- unique(data.frame(book[c("LOCATION", "EXPT", "BLOCK")], effect = effects$block))
  ok <- c(
    blocks_per_location_and_experiment = nrow(units) == 20L && length(unique(units$effect)) == 20L,
    entry_1_one_effect = length(unique(effects$treatment[book$ENTRY == 1])) == 1L,
    entry_4_one_effect = length(unique(effects$treatment[book$ENTRY == 4])) == 1L,
    # Entries 1 and 4 share the name "A" but are different entries
    same_name_entries_differ = !identical(effects$treatment[match(1, book$ENTRY)],
                                          effects$treatment[match(4, book$ENTRY)])
  )
  expect(all(ok), failed_checks(ok))
})

test_that("row-column field layouts preserve shared experimental axes", {
  x <- row_column(24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                  method = "onestage", latinize = TRUE)
  layout <- field_layout(x, stacked = "horizontal")
  indexes <- match(layout$ID, x$fieldBook$ID)
  expect_identical(layout[c("ROW_REP", "COLUMN_REP")],
                   data.frame(ROW_REP = x$fieldBook$ROW[indexes],
                              COLUMN_REP = x$fieldBook$COLUMN[indexes]))
  # Experimental rows and columns are not physical: field_layout() is needed
  expect_error(stats::simulate(x, spatial = TRUE), class = "fieldhub_input_error")
  x$fieldBook <- layout
  sim <- stats::simulate(x, spatial = TRUE, seed = 42)
  expect_identical(list(length(unique(sim$effects[[1L]]$column)), "spatial" %in% names(sim$effects[[1L]])),
                   list(4L, TRUE))
})

test_that("numeric Latin-square experimental labels require a physical layout", {
  data <- data.frame(Period = c("101", "205", "414"), Cow = c("605", "902", "987"),
                     Treatment = c("A1", "A2", "A3"))
  x <- latin_square(reps = 1, seed = 2, data = data)
  expect_error(stats::simulate(x, spatial = TRUE), class = "fieldhub_input_error")
  x$fieldBook <- field_layout(x)
  result <- stats::simulate(x, spatial = TRUE, seed = 2)
  ok <- c(physical_columns = identical(sort(unique(result$field_book$COLUMN)), 1:3),
          experimental_columns_kept = setequal(as.character(result$field_book$COLUMN_SQ), data$Cow),
          spatial_term = "spatial" %in% names(result$effects[[1L]]))
  expect(all(ok), failed_checks(ok))
})

test_that("diagonal fillers create no decision-block term", {
  x <- diagonal_arrangement(10, 32, 287, 4, planter = "cartesian", seed = 25)
  result <- stats::simulate(x, seed = 25)
  filler <- x$fieldBook$ENTRY == 0
  expect(any(filler) && all(is.na(result$field_book$YIELD[filler])) &&
           !"decision_block" %in% names(result$effects[[1L]]),
         "fillers must exist, have missing responses and add no decision_block term")
  expect_error(stats::simulate(x, design_sd = c(decision_block = 1)), class = "fieldhub_input_error")
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
  expect_identical(c(entries = length(unique(result$field_book$ENTRY[shared])),
                     effects = length(unique(result$effects[[1L]]$treatment[shared]))),
                   c(entries = 2L, effects = 1L))
})

# Random-number state around stats::simulate() -------------------------------

test_that("an explicit simulation seed leaves the caller's stream untouched and replays", {
  x <- CRD(t = 3, reps = 2, seed = 1)
  set.seed(52)
  before <- .Random.seed
  # Fractional and negative seeds follow set.seed(), which truncates
  for (seed in list(19, 19.75, -8)) {
    result <- stats::simulate(x, seed = seed)
    expect(identical(.Random.seed, before) && is.integer(result$metadata$seed) &&
             identical(reproduce_simulation(result), result) && identical(.Random.seed, before),
           sprintf("seed %s: the caller's stream changed or the record does not replay", seed))
  }
})

test_that("a seedless simulation consumes exactly one draw from the caller's stream", {
  x <- CRD(t = 3, reps = 2, seed = 1)
  set.seed(52)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(52)
  result <- stats::simulate(x)
  expect_identical(list(stream = .Random.seed, seed = result$metadata$seed),
                   list(stream = after_one_draw, seed = expected))
  expect_identical(reproduce_simulation(result), result)
})

test_that("simulated trait names must be one new, non-reserved column name", {
  x <- CRD(t = 3, reps = 2, seed = 1)
  for (trait in list(NULL, "", "  ", NA_character_, c("a", "b"), "PLOT", "text")) {
    expect_error(stats::simulate(x, seed = 7, trait = trait), class = "fieldhub_input_error",
                 info = deparse(trait))
  }
  named <- stats::simulate(x, seed = 7, trait = "Dry yield (kg)")
  expect_true("Dry yield (kg)" %in% names(named$field_book))
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
  first <- stats::rnorm(1)
  before <- .Random.seed
  result <- stats::simulate(x, nsim = 2, seed = 6, spatial = TRUE)
  expect_identical(result, reproduce_simulation(result))
  # The second Box-Muller normal is still cached for the caller
  expect_identical(list(first, .Random.seed, stats::rnorm(1)), list(expected[1L], before, expected[2L]))
  RNGkind(normal.kind = "Inversion")
  expect_identical(list(identical(result, reproduce_simulation(result)), RNGkind()[2L]),
                   list(TRUE, "Inversion"))
})

# Classic truncated-normal responses (norm_trunc) ----------------------------

# A field book with one treatment replicated ten times and 19 unreplicated
sim_input <- function() {
  data.frame(LOCATION = 1, PLOT = 1:29, TREATMENT = c(rep("A", 10), paste0("E", 1:19)))
}

test_that("each treatment's truncated responses come from the stated distribution", {
  # Treatment means are evenly spaced over the middle two thirds of [a, b]
  # (15, 35, 55, 75 for [0, 90] and four treatments) and assigned to the
  # treatments at random; the SD is spacing / max(reps) + 0.13 * max(reps)
  # for more than three reps (20 / 30 + 3.9 = 4.567 here).
  counts <- c(a = 30, b = 12, c = 21, d = 30)
  book <- data.frame(LOCATION = "A", PLOT = seq_len(sum(counts)),
                     TREATMENT = rep(names(counts), counts))
  sim <- norm_trunc(a = 0, b = 90, data = book, seed = 11)
  means <- c(15, 35, 55, 75)
  sd <- 20 / 30 + 0.13 * 30
  # Truncated-normal means on [0, 90] (the bounds are over 3 SD away)
  alpha <- (0 - means) / sd
  beta <- (90 - means) / sd
  truncated <- means + sd * (dnorm(alpha) - dnorm(beta)) / (pnorm(beta) - pnorm(alpha))
  observed <- tapply(sim$RESP, sim$TREATMENT, mean)[names(counts)]
  # Means are 20 apart and their standard errors at most 4.57 / sqrt(12) =
  # 1.32, so ranking the observed means recovers the random assignment
  rank_order <- order(observed)
  error <- abs(observed[rank_order] - truncated) / (sd / sqrt(counts[rank_order]))
  expect(all(error < 4), sprintf("treatment %s mean %.2f is %.1f standard errors from %.2f",
                                 names(error)[which.max(error)], observed[rank_order][which.max(error)],
                                 max(error), truncated[which.max(error)]))
  # Pooled within-treatment SD with 89 degrees of freedom: standard error
  # about sd / sqrt(2 * 89) = 0.34; allow four.
  pooled <- sqrt(sum(tapply(sim$RESP, sim$TREATMENT, function(y) sum((y - mean(y))^2))) /
                   (sum(counts) - length(counts)))
  expect_equal(pooled, sd, tolerance = 4 * 0.34 / sd)
})

test_that("norm_trunc() draws each treatment's responses from one distribution", {
  # Regression: responses were drawn in blocks sized by the counts of the
  # sorted treatment levels but labelled with shuffled treatments, so under
  # unequal replication one treatment's responses spanned several means.
  sim <- norm_trunc(a = 0, b = 100, data = sim_input(), seed = 2)
  expect_identical(sum(sim$TREATMENT == "A"), 10L)
  expect_lt(diff(range(sim$RESP[sim$TREATMENT == "A"])), 15)
})

test_that("norm_trunc() keeps the caller's random-number stream when seed is NULL", {
  # Regression: seed = NULL called set.seed(NULL), discarding the caller's seed
  set.seed(5)
  first <- norm_trunc(a = 0, b = 100, data = sim_input())
  set.seed(5)
  expect_identical(norm_trunc(a = 0, b = 100, data = sim_input()), first)
})

test_that("responses follow named treatment columns and preserve user metadata", {
  book <- data.frame(LOCATION = rep(c("B", "A"), each = 4), PLOT = rep(4:1, 2),
                     TREATMENT = rep(c("x", "y"), 4))
  expected <- norm_trunc(0, 100, book, seed = 4)
  altered <- book[rev(names(book))]
  altered$NOTES <- "preserve this column"
  actual <- norm_trunc(0, 100, altered, seed = 4)
  expect_identical(actual[names(expected)], expected)
  expect_identical(actual$NOTES, rep("preserve this column", 8))
  names(book)[names(book) == "TREATMENT"] <- "TRT_COMB"
  expect_identical(norm_trunc(0, 100, book, seed = 4)$RESP, expected$RESP)
})

test_that("malformed response simulations fail before drawing random numbers", {
  book <- data.frame(LOCATION = "A", PLOT = 1:6, TREATMENT = rep(letters[1:3], 2))
  bad_bounds <- list(NULL, NA_real_, Inf, "0", c(0, 1), 1 + 1i)
  cases <- c(
    lapply(bad_bounds, function(value) list(value, 100, book)),
    lapply(bad_bounds, function(value) list(0, value, book)),
    list(list(10, 1, book), list(-1e308, 1e308, book)),
    lapply(list(book[FALSE, ], book[names(book) != "LOCATION"],
                book[names(book) != "PLOT"], book[names(book) != "TREATMENT"],
                transform(book, TREATMENT = "one"),
                transform(book, TREATMENT = c(NA, letters[1:5])),
                transform(book, LOCATION = NA_character_),
                transform(book, PLOT = NA_real_),
                transform(book, RESP = 99),
                transform(book, TRT_COMB = TREATMENT)),
           function(bad) list(0, 100, bad))
  )
  set.seed(772)
  before <- .Random.seed
  for (i in seq_along(cases)) {
    expect_error(do.call(norm_trunc, cases[[i]]), class = "fieldhub_input_error", info = i)
  }
  expect_identical(.Random.seed, before)
})

# Spatial AR1 x AR1 responses -------------------------------------------------

# A 4 x 5 field with one entry per plot
field_input <- function() {
  data.frame(ID = 1:20, ROW = rep(1:4, each = 5), COLUMN = rep(1:5, times = 4), ENTRY = 1:20)
}

test_that("AR1xAR1_simulation() with seed = NULL follows the caller's seed", {
  # Regression: seed = NULL called set.seed(runif(1)), which truncates to 0
  simulate_field <- function() {
    AR1xAR1_simulation(nrows = 4, ncols = 5, ROX = 0.4, ROY = 0.4, minValue = 1,
                       maxValue = 10, fieldbook = field_input(), trail = "YIELD")$outOrder
  }
  set.seed(1)
  first <- simulate_field()
  set.seed(2)
  second <- simulate_field()
  set.seed(1)
  expect_identical(list(differs = !isTRUE(all.equal(first$YIELD, second$YIELD)),
                        repeats = identical(simulate_field(), first)),
                   list(differs = TRUE, repeats = TRUE))
})

test_that("explicit simulation seeds do not leak into the caller's stream", {
  set.seed(123)
  before <- .Random.seed
  AR1xAR1_simulation(nrows = 4, ncols = 5, ROX = 0.4, ROY = 0.5, minValue = 1, maxValue = 10,
                     fieldbook = field_input(), trail = "YIELD", seed = 27)
  norm_trunc(a = 1, b = 10,
             data = data.frame(LOCATION = 1, PLOT = 1:6, TREATMENT = rep(1:3, 2)), seed = 27)
  expect_identical(.Random.seed, before)
})

test_that("the spatial field is the AR1 x AR1 Cholesky factor applied to normal draws", {
  # Independent reference: ZST() draws N normals for the patch and N for the
  # nugget; the patch is the lower Cholesky factor of the dense separable
  # covariance kronecker(Ry, Rx) applied to the first N draws, standardized.
  # Both sides compute the same unique factor; rounding is about N * eps per
  # element, amplified by 1 / sd(patch) (under 30 at rho = 0.999), so a
  # relative tolerance of 1e-10 leaves a wide margin.
  nugget <- 0.1
  cases <- expand.grid(dims = list(c(1L, 7L), c(7L, 1L), c(4L, 5L), c(5L, 4L)),
                       rho = list(c(0, 0), c(0.4, 0.6), c(-0.4, 0.3), c(0.999, -0.999)))
  for (i in seq_len(nrow(cases))) {
    n <- cases$dims[[i]][1]
    m <- cases$dims[[i]][2]
    rho <- cases$rho[[i]]
    set.seed(27 + i)
    before <- .Random.seed
    result <- ZST(n, m, rho[1], rho[2], nugget)
    after <- .Random.seed
    assign(".Random.seed", before, globalenv())
    covariance <- kronecker(rho[2]^abs(outer(seq_len(n), seq_len(n), `-`)),
                            rho[1]^abs(outer(seq_len(m), seq_len(m), `-`)))
    patch <- drop(t(chol(covariance)) %*% rnorm(n * m))
    reference <- (patch - mean(patch)) / sd(patch) * sqrt(1 - nugget) + rnorm(n * m) * sqrt(nugget)
    expect_equal(result$ZST, reference, tolerance = 1e-10,
                 info = sprintf("%d x %d, rho = (%g, %g)", n, m, rho[1], rho[2]))
    if (n == 4L && m == 5L && identical(rho, c(0.4, 0.6))) {
      # Exactly 2N normal draws, laid out in row-major order
      expect_identical(list(after, result$ROW, result$COLUMN),
                       list(.Random.seed, rep(as.numeric(1:4), each = 5), rep(as.numeric(1:5), 4)))
    }
  }
})

test_that("spatial fields validate their grid, correlations and nugget before drawing", {
  bad <- c(
    lapply(list(0, -1, 1.5, NA_real_, Inf, c(2, 3), "2", 2 + 1i),
           function(value) list(value, 3, 0.4, 0.5, 0.1)),
    lapply(list(-1, 1, NA_real_, Inf, c(0.2, 0.3), "0.2", 0.2 + 1i),
           function(value) list(2, 3, value, 0.5, 0.1)),
    lapply(list(NA_real_, Inf, -0.1, 1.1, c(0.1, 0.2), "0.1", 1i),
           function(value) list(2, 3, 0.4, 0.5, value)),
    # A single plot cannot be standardized
    list(list(1, 1, 0.4, 0.5, 0.1))
  )
  set.seed(27)
  before <- .Random.seed
  for (i in seq_along(bad)) {
    expect_error(do.call(ZST, bad[[i]]), class = "fieldhub_input_error", info = deparse(bad[[i]]))
  }
  expect_identical(.Random.seed, before)
})

# Spatial simulation of a multi-location field book ---------------------------

spatial_test_book <- function() {
  first <- data.frame(
    ID = 1:20, LOCATION = "Z", ROW = rep(1:4, each = 5),
    COLUMN = rep(1:5, times = 4), ENTRY = c(rep(1:2, 3), 3:16),
    EXTRA = "keep"
  )
  second <- first
  second$LOCATION <- "A"
  second$ROW <- rep(1:5, each = 4)
  second$COLUMN <- rep(1:4, times = 5)
  rbind(first, second)
}

simulate_spatial_test <- function(book = spatial_test_book(), ...) {
  simulate_spatial_field_book(
    field_book = book, nrows = c(4, 5), ncols = c(5, 4),
    correlation_x = 0.4, correlation_y = 0.5,
    min_value = 1, max_value = 10, response_name = "YIELD", ...
  )
}

test_that("spatial simulations preserve metadata and location order", {
  out <- simulate_spatial_test(seed = 27)
  expect_named(out, c("field_book", "simulations", "seed", "input_field_book", "metadata"))
  ok <- c(
    input_columns_kept = identical(out$field_book[names(spatial_test_book())], spatial_test_book()),
    location_order = identical(unique(out$field_book$LOCATION), c("Z", "A")),
    one_simulation_per_location = length(out$simulations) == 2L,
    rounded_response = identical(out$field_book$YIELD,
                                 round(unlist(lapply(out$simulations, `[[`, "YIELD")), 2)),
    same_seed_same_result = identical(simulate_spatial_test(seed = 27), out),
    other_seed_differs = !identical(simulate_spatial_test(seed = 28)$field_book$YIELD,
                                    out$field_book$YIELD)
  )
  expect(all(ok), failed_checks(ok))
})

test_that("spatial simulations select columns by name and align rows by ID", {
  book <- spatial_test_book()
  original <- simulate_spatial_test(seed = 27)$field_book
  reordered <- book[c(20:1, 40:21), rev(names(book))]
  rownames(reordered) <- NULL
  out <- simulate_spatial_test(reordered, seed = 27)$field_book
  key <- function(x) paste(x$LOCATION, x$ID)
  expect_identical(out$YIELD[match(key(original), key(out))], original$YIELD)
  expect_identical(out[names(reordered)], reordered)
})

test_that("spatial simulations preserve the caller's RNG state for explicit seeds", {
  set.seed(123)
  before <- .Random.seed
  simulate_spatial_test(seed = 27)
  expect_identical(.Random.seed, before)
})

test_that("a seedless spatial simulation consumes exactly one draw from the caller's stream", {
  set.seed(123)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(123)
  out <- simulate_spatial_test()
  expect_identical(list(stream = .Random.seed, seed = out$seed),
                   list(stream = after_one_draw, seed = expected))
  expect_identical(simulate_spatial_test(seed = out$seed), out)
})

test_that("spatial simulations validate field books, dimensions and response names", {
  book <- spatial_test_book()
  cases <- list(
    `book smaller than the field` = list(field_book = book[-1, ]),
    `duplicated ID` = list(field_book = book[c(1, 1, 3:40), ]),
    `no ENTRY` = list(field_book = book[, names(book) != "ENTRY"]),
    `response already present` = list(field_book = transform(book, YIELD = 0)),
    `too many nrows` = list(nrows = c(4, 5, 6), ncols = 5),
    `reserved name ZST` = list(response_name = "ZST"),
    `reserved name genot` = list(response_name = "genot"),
    `reserved name text` = list(response_name = "text")
  )
  defaults <- list(field_book = book, nrows = c(4, 5), ncols = c(5, 4),
                   correlation_x = 0.4, correlation_y = 0.5, min_value = 1,
                   max_value = 10, response_name = "YIELD", seed = 27)
  for (name in names(cases)) {
    args <- defaults
    args[names(cases[[name]])] <- cases[[name]]
    expect_error(do.call(simulate_spatial_field_book, args), class = "fieldhub_input_error",
                 info = name)
  }
})

test_that("a simulated response is appended by name without replacing metadata", {
  field_book <- data.frame(ID = 1:3, REP = c(1L, 2L, 1L), ENTRY = 11:13)
  simulation <- data.frame(ID = 1:3, YIELD = c(1.234, 5.678, 9.101))
  out <- append_simulated_response(field_book, simulation, "YIELD")
  expect_named(out, c("ID", "REP", "ENTRY", "YIELD"))
  expect_identical(out$REP, field_book$REP)
  # Rounded to two decimals
  expect_equal(out$YIELD, c(1.23, 5.68, 9.1))
  # The simulation has a REP column too, so only the clash with the field
  # book's REP can reject it
  expect_error(append_simulated_response(field_book, transform(simulation, REP = 9), "REP"),
               class = "fieldhub_input_error")
})

# Recorded simulations and reproduce_simulation() ------------------------------

recorded_spatial_test <- function(seed = 17) {
  book <- data.frame(ID = 1:6, LOCATION = "A", ROW = rep(1:2, each = 3),
                     COLUMN = rep(1:3, 2), ENTRY = c(1, 2, 1, 2, 3, 0))
  simulate_spatial_field_book(book, 2, 3, 0.4, 0.5, 10, 20, "HEIGHT", seed)
}

recorded_classic_test <- function(seed = 17) {
  book <- data.frame(ID = 1:6, LOCATION = "B", PLOT = 101:106,
                     TREATMENT = rep(letters[1:3], 2))
  simulate_classic_field_book(book, 10, 20, "HEIGHT", seed)
}

test_that("spatial simulations retain the exact input and effective parameters", {
  x <- recorded_spatial_test()
  expect_identical(
    x$metadata[c("model", "schema_version", "seed", "rng_kind", "package_version")],
    list(model = "ar1xar1", schema_version = 1L, seed = 17L, rng_kind = RNGkind(),
         package_version = as.character(utils::packageVersion("FielDHub")))
  )
  expect_named(x$metadata$parameters, c("nrows", "ncols", "correlation_x", "correlation_y",
                                        "min_value", "max_value", "response_name", "seed"))
  expect_identical(list(x$metadata$parameters[c("seed", "response_name")],
                        x$field_book[names(x$input_field_book)]),
                   list(list(seed = x$seed, response_name = "HEIGHT"), x$input_field_book))
})

test_that("both recorded simulation models replay without advancing the caller stream", {
  # classic_by_id: a record of an app workflow that lists the plots by ID
  classic_by_id <- function(seed) {
    book <- data.frame(ID = c(4L, 1L, 6L, 2L, 5L, 3L), LOCATION = "B", PLOT = 101:106,
                       TREATMENT = rep(letters[1:3], 2))
    simulate_classic_field_book(book, 10, 20, "HEIGHT", seed, order_by_id = TRUE)
  }
  builds <- list(classic = recorded_classic_test, spatial = recorded_spatial_test,
                 classic_by_id = classic_by_id)
  seeds <- list(17, NULL, -1, 17.9)
  failed <- character(0)
  for (model in names(builds)) for (seed in seeds) {
    x <- builds[[model]](seed)
    set.seed(39)
    before <- .Random.seed
    replay <- reproduce_simulation(x)
    if (!identical(replay, x) || !identical(.Random.seed, before) ||
        (model == "classic_by_id" && !identical(replay$field_book$ID, 1:6))) {
      failed <- c(failed, paste(model, deparse(seed)))
    }
  }
  expect_identical(failed, character(0))
})

test_that("simulation reconstruction validates metadata and never evaluates input expressions", {
  x <- recorded_spatial_test()
  classic <- recorded_classic_test()
  by_id <- within(classic, metadata$parameters$order_by_id <- TRUE)
  bad <- list(NULL, list(), within(x, rm(metadata)),
              within(x, metadata$model <- "system"),
              within(x, metadata$schema_version <- 99L),
              within(x, metadata$parameters$seed <- 2L),
              within(x, metadata$parameters$unknown <- TRUE),
              # Classic records: one new response name, a flag, and finite IDs to order by
              within(classic, metadata$parameters$response_name <- " "),
              within(classic, metadata$parameters$response_name <- "PLOT"),
              within(classic, metadata$parameters$order_by_id <- NA),
              within(by_id, input_field_book$ID[2] <- NA))
  for (i in seq_along(bad)) {
    expect_error(reproduce_simulation(bad[[i]]), class = "fieldhub_input_error", info = paste("record", i))
  }
  ran <- FALSE
  x$metadata$parameters$min_value <- quote({ran <<- TRUE; 10})
  expect_error(reproduce_simulation(x), class = "fieldhub_input_error")
  expect_false(ran)
})

test_that("simulation reconstruction restores RNG settings on success and failure", {
  x <- recorded_spatial_test()
  previous <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(67)
  caller <- list(seed = .Random.seed, kind = "L'Ecuyer-CMRG")
  expect_identical(reproduce_simulation(x), x)
  expect_identical(list(seed = .Random.seed, kind = RNGkind()[1]), caller)
  x$metadata$rng_kind[1] <- "invalid"
  expect_error(reproduce_simulation(x), class = "fieldhub_input_error")
  expect_identical(list(seed = .Random.seed, kind = RNGkind()[1]), caller)
})

test_that("simulation reconstruction warns about changed package versions", {
  x <- recorded_classic_test()
  x$metadata$package_version <- "0.0.0"
  expect_warning(reproduce_simulation(x), class = "fieldhub_reproduction_warning")
})
