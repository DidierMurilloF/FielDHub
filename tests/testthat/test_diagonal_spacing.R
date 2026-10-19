test_that("spacing measures agree with independent known coordinates", {
  local_rng_state()
  layout <- matrix(99, 3, 3)
  layout[cbind(c(1, 3), c(1, 3))] <- 1
  layout[cbind(c(1, 3), c(3, 1))] <- 2
  layout[2, 2] <- 3
  entries <- data.frame(ENTRY = 1:3, NAME = c("Check A!", "Check B", "Singleton"))
  set.seed(17)
  rng <- .Random.seed
  euclidean <- diagonal_spacing_metrics(layout, 1:3, entries, radius = 3)
  expect_equal(euclidean$occurrences, c(2, 2, 1))
  expect_equal(euclidean$min_distance, c(sqrt(8), sqrt(8), NA))
  expect_equal(euclidean$mean_nearest_distance, c(sqrt(8), sqrt(8), NA))
  expect_equal(euclidean$mean_pairwise_distance, c(sqrt(8), sqrt(8), NA))
  expect_equal(euclidean$neighboring_occurrences, c(2, 2, 0))
  expect_equal(euclidean$close_pairs, c(1, 1, 0))
  expect_identical(euclidean$TREATMENT, entries$NAME)
  expect_equal(unname(diagonal_spacing_totals(euclidean)), c(sqrt(8), sqrt(8), sqrt(8), 80))
  manhattan <- diagonal_spacing_metrics(layout, 1:3, entries, "manhattan", radius = 3)
  expect_equal(manhattan$min_distance, c(4, 4, NA))
  expect_equal(manhattan$neighboring_occurrences, c(0, 0, 0))
  boundary <- diagonal_spacing_metrics(layout, 1:3, entries, "manhattan", radius = 4)
  expect_equal(boundary$neighboring_occurrences, c(2, 2, 0))
  expect_identical(.Random.seed, rng)
})

test_that("overall distance averages weight checks equally and neighbors by occurrences", {
  metrics <- data.frame(min_distance = c(2, 4), mean_nearest_distance = c(3, 5),
    mean_pairwise_distance = c(4, 8), occurrences = c(2, 8), neighboring_occurrences = c(2, 0))
  expect_equal(unname(diagonal_spacing_totals(metrics)), c(2, 4, 6, 20))
})

test_that("off and on record the same starting assignment and replay exactly", {
  args <- list(nrows = 20, ncols = 20, lines = 350, checks = 4, seed = 7, year = 2026)
  off <- do.call(diagonal_arrangement, args)
  on <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE)))
  expect_identical(off$check_spacing[[1]]$before, on$check_spacing[[1]]$before)
  expect_null(off$check_spacing[[1]]$after)
  expect_equal(diagonal_spacing_totals(off$check_spacing[[1]]$before)[["min_distance"]], sqrt(8))
  expect_equal(diagonal_spacing_totals(on$check_spacing[[1]]$after)[["min_distance"]], sqrt(10))
  expect_identical(reproduce_design(off), off)
  expect_identical(reproduce_design(on), on)
  printed <- paste(capture.output(print(on, n = 2)), collapse = "\n")
  expect_match(printed, "Information on the design parameters")
  expect_match(printed, "Before.*After")
  expect_match(printed, "Candidate evaluations: 12,181", fixed = TRUE)
  expect_match(printed, "field book")
  overview <- paste(capture.output(print(off)), collapse = "\n")
  expect_match(overview, "Optimization: OFF", fixed = TRUE)
  expect_match(overview, "Value", fixed = TRUE)
  expect_false(grepl("Candidate evaluations", overview, fixed = TRUE))
  detailed <- paste(capture.output(summary(on)), collapse = "\n")
  for (heading in c("2. Layout randomization", "3. Plot number layout", "4. Structure",
                    "5. Structure", "6. Check spacing", "Per-check spacing", "Check-3")) {
    expect_match(detailed, heading, fixed = TRUE)
  }
  legacy <- off
  legacy$check_spacing <- NULL
  expect_false(grepl("Check spacing", paste(capture.output(print(legacy)), collapse = "\n")))
})

test_that("spacing reports use each location and the existing generic app page", {
  design <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
    seed = 8, l = 2, locationNames = c("North", "South"), dist_method = "manhattan")
  expect_length(design$check_spacing, 2)
  for (site in 1:2) {
    expect_identical(design$check_spacing[[site]]$before,
      diagonal_spacing_metrics(design$layoutRandom[[site]], 1:4,
        design$data_entry[[site]], "manhattan"))
  }
  spec <- design_app_spec("Diagonal")
  ui <- as.character(mod_design_ui("spacing_test", spec))
  expect_match(ui, "spacing_test-design_report", fixed = TRUE)
  report <- paste(capture.output(spec$report(design, 2)), collapse = "\n")
  expect_match(report, "Location 2", fixed = TRUE)
  expect_false(grepl("Location 1", report, fixed = TRUE))
  expect_match(report, "Manhattan", fixed = TRUE)
  expect_null(design_app_spec("diagonal_multiple")$report)
})
