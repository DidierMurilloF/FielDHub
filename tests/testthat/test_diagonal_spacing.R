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
  expect_identical(spec$report, "check_spacing")
  # the page shows the report as a view built from the record: one section
  # for each location, the selected one showing
  report <- as.character(app_check_spacing(design, 2))
  expect_match(report, '<option value="2" selected>SOUTH</option>', fixed = TRUE)
  expect_match(report, 'data-fh-section="1" hidden', fixed = TRUE)
  expect_false(grepl('data-fh-section="2" hidden', report, fixed = TRUE))
  expect_match(report, "Manhattan", fixed = TRUE)
  expect_false(grepl("style=", report, fixed = TRUE))
  single <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 8)
  expect_false(grepl("<select", as.character(app_check_spacing(single)), fixed = TRUE))
  for (module in c("diagonal_multiple", "sparse_allocation")) {
    expect_identical(design_app_spec(module)$report, "check_spacing")
  }
})

test_that("the p-rep pages report the spacing of their replicated entries, and Field setup holds the rest", {
  design <- partially_replicated(nrows = 15, ncols = 20, repGens = c(75, 150), repUnits = c(2, 1),
                                 l = 2, locationNames = c("North", "South"), seed = 7,
                                 rowcol_penalty = Inf)
  for (site in 1:2) {
    pairs <- design$pairsDistance[[site]]
    table <- app_replicate_spacing_table(pairs)
    expect_identical(names(table), c("Measure", "Value"))
    expect_identical(table$Value[1:3], c(sprintf("%.2f", min(pairs$DIST)), sprintf("%.2f", mean(pairs$DIST)),
                                         as.character(nrow(pairs))))
    # the engine's own minimum, and its rule of distinct rows and columns
    expect_equal(min(pairs$DIST), design$infoDesign$min_distance[[site]])
    expect_identical(table$Value[4], sprintf("%.1f%%", 100 * design$infoDesign$shared_pairs[[site]] / nrow(pairs)))
    facts <- app_replicate_spacing_facts(design, site)
    values <- stats::setNames(vapply(facts, `[[`, "", "value"), vapply(facts, `[[`, "", "label"))
    # the spacing alone: the field and its entries are on Field setup
    expect_identical(values, c(Optimization = "On", Distance = "Euclidean, in grid steps",
                               `Distinct rows and columns` = "Required", Seed = "7"))
  }
  report <- as.character(app_design_report("replicate_spacing", design, 2))
  expect_match(report, "Replicate spacing", fixed = TRUE)
  expect_match(report, '<option value="2" selected>South</option>', fixed = TRUE)
  expect_match(report, 'data-fh-section="1" hidden', fixed = TRUE)
  expect_false(grepl("style=", report, fixed = TRUE))
  off <- partially_replicated(nrows = 15, ncols = 20, repGens = c(75, 150), repUnits = c(2, 1),
                              seed = 7, spread_reps = FALSE, dist_method = "manhattan")
  facts <- app_replicate_spacing_facts(off, 1L)
  expect_identical(vapply(facts[1:3], `[[`, "", "value"),
                   c("Off", "Manhattan, in grid steps", "Not required"))
  expect_false(grepl("<select", as.character(app_replicate_spacing(off)), fixed = TRUE))
  # the same view as the check spacing of the diagonal pages
  checks <- as.character(app_design_report("check_spacing",
    diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 8)))
  expect_match(checks, "fh-spacing-head", fixed = TRUE)
  expect_match(report, "fh-spacing-head", fixed = TRUE)
  for (module in c("pREPS", "multi_loc_preps")) {
    spec <- design_app_spec(module)
    expect_identical(spec$report, "replicate_spacing")
    ui <- as.character(mod_design_ui("prep_test", spec))
    expect_match(ui, "prep_test-design_report", fixed = TRUE)
  }
  # Field setup: the run and the field in the select under the steps of one
  # page, the allocation under the steps of the other
  setup <- design_app_spec("pREPS")$setup
  expect_identical(setup$type, "none")
  facts <- function(values, data, size, view = setup$facts) {
    items <- view(values, data, if (is.null(size)) list(NULL) else if (is.list(size)) size else list(size))
    stats::setNames(vapply(items, `[[`, "", "value"), vapply(items, `[[`, "", "label"))
  }
  values <- list(repGens = c(75, 150), repUnits = c(2, 1), plots = 300, l = 1)
  expect_identical(facts(values, NULL, c(15, 20)),
                   c(Entries = "225", Replicated = "75 \u00d7 2 copies", Unreplicated = "150 \u00d7 1 copy",
                     Plots = "300", Field = "15 \u00d7 20", Fillers = "0"))
  expect_identical(facts(values, NULL, c(16, 19))[c("Field", "Fillers")],
                   c(Field = "16 \u00d7 19", Fillers = "4"))
  # a select that does not hold a field of the run yet shows none
  expect_identical(names(facts(values, NULL, NULL)), c("Entries", "Replicated", "Unreplicated", "Plots"))
  expect_false("Field" %in% names(facts(values, NULL, c(10, 10))))
  # one cell for each group of copies, and the locations where there are several
  several <- list(repGens = c(20, 30, 100), repUnits = c(3, 2, 1), plots = 220, l = 3)
  expect_identical(facts(several, NULL, c(11, 20)),
                   c(Entries = "150", Replicated = "20 \u00d7 3 copies", Replicated = "30 \u00d7 2 copies",
                     Unreplicated = "100 \u00d7 1 copy", `Plots per location` = "220",
                     Field = "11 \u00d7 20", Fillers = "0", Locations = "3"))
  # an uploaded list is counted from its copies
  upload <- data.frame(ENTRY = 1:6, NAME = letters[1:6], REPS = c(2, 2, 1, 1, 1, 1))
  expect_identical(facts(list(plots = 8, l = 1), upload, c(2, 4))[1:3],
                   c(Entries = "6", Replicated = "2 \u00d7 2 copies", Unreplicated = "4 \u00d7 1 copy"))
  block <- as.character(app_design_facts(setup$facts(values, NULL, list(c(15, 20)))))
  expect_match(block, "fh-stats", fixed = TRUE)
  expect_false(grepl("style=", block, fixed = TRUE))
  ui <- as.character(mod_design_ui("prep_test", design_app_spec("pREPS")))
  expect_gt(regexpr('id="prep_test-design_facts"', ui, fixed = TRUE), regexpr("fh-setup-steps", ui, fixed = TRUE))
  # on every spatial page the steps come first, the summary or the table under them
  for (page in Filter(function(page) identical(page$kind, "spatial"), fieldhub_design_specs())) {
    html <- as.character(mod_design_ui("order_test", page))
    for (id in c("setup", "design_facts")) {
      view <- regexpr(sprintf('id="order_test-%s"', id), html, fixed = TRUE)
      if (view > 0) expect_gt(view, regexpr("fh-setup-steps", html, fixed = TRUE), label = page$module)
    }
  }
  multi <- design_app_spec("multi_loc_preps")
  expect_identical(multi$setup$type, "table")
  expect_false(is.null(multi$setup$tip))
  ui <- as.character(mod_design_ui("prep_test", multi))
  expect_false(grepl('data-value="Allocation"', ui, fixed = TRUE))
  # one structure on both pages: the steps, the Design block, the page's own table under its heading
  steps <- regexpr("fh-setup-steps", ui, fixed = TRUE)
  block <- regexpr('id="prep_test-design_facts"', ui, fixed = TRUE)
  heading <- regexpr("<h3>Allocation</h3>", ui, fixed = TRUE)
  table <- regexpr('id="prep_test-setup"', ui, fixed = TRUE)
  expect_true(steps > 0 && steps < block && block < heading && heading < table)
  values <- list(lines = 312, rep_checks = NULL, l = 6, copies_per_entry = "8",
                 optim_list = list(size_locations = rep(416, 6)))
  expect_identical(facts(values, NULL, c(26, 16), multi$setup$facts),
                   c(Entries = "312", `Copies per entry` = "8", Locations = "6", `Plots per location` = "416",
                     Field = "26 \u00d7 16", Fillers = "0"))
  # a field for each location: alike they read as one, otherwise by location, each in the tooltip
  same <- rep(list(c(16, 26)), 6)
  expect_identical(facts(values, NULL, same, multi$setup$facts)[["Field"]], "16 \u00d7 26")
  mixed <- c(list(c(21, 20)), rep(list(c(26, 16)), 5))
  items <- multi$setup$facts(values, NULL, mixed)
  by_label <- stats::setNames(items, vapply(items, `[[`, "", "label"))
  expect_identical(by_label$Field$value, "By location")
  expect_match(by_label$Field$tip, "Location 1: 21 \u00d7 20, Location 2: 26 \u00d7 16", fixed = TRUE)
  expect_identical(by_label$Fillers$value, "0\u20134")
  expect_match(by_label$Fillers$tip, "Location 1: 4, Location 2: 0", fixed = TRUE)
  # a location still without a field shows none
  expect_false("Field" %in% names(facts(values, NULL, c(same[1:5], list(NULL)), multi$setup$facts)))
  with_checks <- utils::modifyList(values, list(rep_checks = c(8, 8, 8)))
  expect_identical(facts(with_checks, NULL, c(22, 20), multi$setup$facts)[c("Checks", "Plots per location", "Fillers")],
                   c(Checks = "3", `Plots per location` = "440", Fillers = "0"))
})
