diagonal_check_distances <- function(layout, checks, method = "euclidean") {
  sort(unlist(lapply(checks, function(check) {
    positions <- which(layout == as.character(check), arr.ind = TRUE)
    if (nrow(positions) < 2L) return(numeric())
    as.numeric(stats::dist(positions, method = method))
  })))
}

test_that("check optimization preserves entries, fillers and both field-book orientations", {
  local_rng_state()
  set.seed(781)
  rng <- .Random.seed
  for (planter in c("serpentine", "cartesian")) {
    for (fillers in c(FALSE, TRUE)) {
      args <- list(nrows = if (fillers) 10 else 15, ncols = if (fillers) 32 else 20,
                   lines = if (fillers) 287 else 270, checks = 4, planter = planter,
                   seed = 1, year = 2026)
      before <- do.call(diagonal_arrangement, args)
      for (method in c("euclidean", "manhattan")) {
        after <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE, dist_method = method)))
        checks <- before$infoDesign$entry_checks[[1L]]
        x <- before$layoutRandom[[1L]]
        y <- after$layoutRandom[[1L]]
        slots <- matrix(x %in% as.character(checks), nrow(x))
        expect_identical(x[!slots], y[!slots])
        expect_identical(table(x[slots]), table(y[slots]))
        expect_identical(matrix(y %in% as.character(checks), nrow(y)), slots)
        expect_identical(x == "Filler", y == "Filler")
        expect_identical(typeof(x), typeof(y))
        expect_gt(min(diagonal_check_distances(y, checks, method)),
                  min(diagonal_check_distances(x, checks, method)))
        fb <- before$fieldBook
        optimized <- after$fieldBook
        expect_identical(fb[fb$CHECKS == 0, ], optimized[fb$CHECKS == 0, ])
        fixed <- setdiff(names(fb), c("ENTRY", "CHECKS", "TREATMENT"))
        expect_identical(fb[fixed], optimized[fixed])
        coordinates <- cbind(nrow(y) + 1L - optimized$ROW, optimized$COLUMN)
        rows <- optimized$CHECKS != 0
        expect_equal(as.numeric(y[coordinates[rows, , drop = FALSE]]), optimized$ENTRY[rows])
        expect_equal(as.numeric(optimized$CHECKS[rows]), optimized$ENTRY[rows])
        expect_identical(after$infoDesign$rep_checks, before$infoDesign$rep_checks)
        expect_identical(after$plotsNumber, before$plotsNumber)
        expect_identical(reproduce_design(after), after)
        expect_identical(.Random.seed, rng)
      }
      expect_identical(do.call(diagonal_arrangement, c(args, spread_checks = FALSE)), before)
    }
  }
})

test_that("multiple locations and supplied check names remain consistent", {
  data <- data.frame(ENTRY = c(101:104, 1001:1287),
                     NAME = c("Check A", "Check B", "Check C", "Check D", paste0("Line ", 1001:1287)))
  args <- list(nrows = 10, ncols = 32, checks = 4, data = data, l = 2,
               locationNames = c("North", "South"), plotNumber = c(101, 1001), seed = 2)
  before <- do.call(diagonal_arrangement, args)
  after <- do.call(diagonal_arrangement, c(args, spread_checks = TRUE))
  rows <- before$fieldBook$CHECKS == 0
  expect_identical(before$fieldBook[rows, ], after$fieldBook[rows, ])
  expect_identical(after$fieldBook$TREATMENT[!rows],
                   data$NAME[match(after$fieldBook$ENTRY[!rows], data$ENTRY)])
  expect_length(after$infoDesign$check_optimization, 2L)
  expect_identical(reproduce_design(after), after)
})

test_that("the default still selects the least-filler option before optimization", {
  args <- list(nrows = 10, ncols = 32, lines = 287, checks = 4, seed = 1)
  choices <- diagonal_percent_choices(10, 32, 1:4, 291)
  options <- lapply(choices$choices, function(p) {
    do.call(diagonal_arrangement, c(args, list(checksPercent = p, spread_checks = TRUE)))
  })
  default <- do.call(diagonal_arrangement, c(args, spread_checks = TRUE))
  expect_identical(default, options[[length(options)]])
  fillers <- vapply(options, function(x) sum(x$fieldBook$ENTRY == 0), integer(1))
  expect_equal(sum(default$fieldBook$ENTRY == 0), min(fillers))
})

test_that("the optimizer handles no-op cases and both search limits", {
  x <- matrix(c(1, 2, 3, 4, 1, 2, 3, 4, 99, 98, 97, 96), 3)
  expect_identical(optimize_diagonal_checks(x, 1)$layout, x)
  expect_identical(optimize_diagonal_checks(x, 99:100)$layout, x)
  expect_identical(optimize_diagonal_checks(x, 101:102)$layout, x)
  none <- optimize_diagonal_checks(x, 1:4, max_evaluations = 0L)
  expect_identical(none$layout, x)
  expect_identical(none$diagnostics$stop_reason, "evaluation_limit")
  limited <- optimize_diagonal_checks(x, 1:4, max_swaps = 0L)
  expect_identical(limited$layout, x)
  expect_identical(limited$diagnostics$stop_reason, "swap_limit")
  short <- optimize_diagonal_checks(x, 1:4, max_evaluations = 1L)
  expect_lte(short$diagnostics$evaluations, 1)
  expect_gte(min(diagonal_check_distances(short$layout, 1:4)), min(diagonal_check_distances(x, 1:4)))
})

test_that("the first exchange agrees with independent full-distance enumeration", {
  x <- matrix(c(1, 1, 2, 2, 3, 3, 99, 98, 1, 2, 3, 97), 3)
  slots <- which(x %in% 1:3)
  best <- x
  best_distances <- diagonal_check_distances(x, 1:3)
  pairs <- utils::combn(slots, 2L)
  for (i in seq_len(ncol(pairs))) {
    candidate <- x
    candidate[pairs[, i]] <- rev(candidate[pairs[, i]])
    distances <- diagonal_check_distances(candidate, 1:3)
    different <- which(distances != best_distances)
    if (length(different) && distances[different[1L]] > best_distances[different[1L]]) {
      best <- candidate
      best_distances <- distances
    }
  }
  actual <- optimize_diagonal_checks(x, 1:3, max_swaps = 1L)
  expect_equal(diagonal_check_distances(actual$layout, 1:3), best_distances)
  expect_equal(actual$diagnostics$after_min, min(best_distances))
  expect_identical(table(actual$layout, dnn = NULL), table(best, dnn = NULL))
})

test_that("optimization options are validated before layout generation", {
  args <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1)
  for (value in list(NA, NULL, 1, "TRUE", c(TRUE, FALSE))) {
    expect_error(do.call(diagonal_arrangement, c(args, list(spread_checks = value))),
                  class = "fieldhub_input_error")
  }
  expect_error(do.call(diagonal_arrangement, c(args, list(dist_method = "bad"))),
                class = "fieldhub_input_error")
  expect_error(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1,
                                 spread_checks = "yes"), class = "fieldhub_input_error")
  expect_error(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1,
                                 dist_method = "bad"), class = "fieldhub_input_error")
})

# Multiple Diagonal Arrangement (DBUDC) and Sparse Allocation use the same
# exchange of check identities, in each location on its own.
check_exchange_invariants <- function(before, after, method = "euclidean") {
  for (site in seq_along(before$layoutRandom)) {
    checks <- before$infoDesign$entry_checks[[site]]
    x <- before$layoutRandom[[site]]
    y <- after$layoutRandom[[site]]
    slots <- matrix(x %in% as.character(checks), nrow(x))
    # test entries and fillers stay; the check plots are the same plots
    expect_identical(x[!slots], y[!slots])
    expect_identical(matrix(y %in% as.character(checks), nrow(y)), slots)
    expect_identical(table(x[slots]), table(y[slots]))
    expect_identical(typeof(x), typeof(y))
    # the smallest same-check distance never gets worse
    expect_gte(min(diagonal_check_distances(y, checks, method)),
               min(diagonal_check_distances(x, checks, method)))
    diagnostics <- after$infoDesign$check_optimization[[site]]
    expect_gte(diagnostics$after_min, diagnostics$before_min)
    expect_identical(after$check_spacing[[site]]$before,
      diagonal_spacing_metrics(x, checks, before$data_entry[[site]], method))
    expect_identical(after$check_spacing[[site]]$after,
      diagonal_spacing_metrics(y, checks, after$data_entry[[site]], method))
  }
  fb <- before$fieldBook
  optimized <- after$fieldBook
  # only the identity of a check plot changes in the field book
  fixed <- setdiff(names(fb), c("ENTRY", "CHECKS", "TREATMENT"))
  expect_identical(fb[fixed], optimized[fixed])
  expect_identical(fb[fb$CHECKS == 0, ], optimized[fb$CHECKS == 0, ])
  expect_identical(fb$CHECKS != 0, optimized$CHECKS != 0)
  rows <- optimized$CHECKS != 0
  expect_equal(as.numeric(optimized$CHECKS[rows]), as.numeric(optimized$ENTRY[rows]))
  # a location keeps the number of plots of each check, and the field book agrees with the layout
  expect_identical(table(fb$LOCATION[rows], fb$CHECKS[rows]),
                   table(optimized$LOCATION[rows], optimized$CHECKS[rows]))
  locations <- unique(optimized$LOCATION)
  for (site in seq_along(locations)) {
    book <- optimized[optimized$LOCATION == locations[site] & optimized$CHECKS != 0, ]
    y <- after$layoutRandom[[site]]
    expect_equal(as.numeric(y[cbind(nrow(y) + 1L - book$ROW, book$COLUMN)]), as.numeric(book$ENTRY))
    names <- after$data_entry[[site]]
    expect_identical(as.character(book$TREATMENT),
                     as.character(names$NAME[match(book$ENTRY, names$ENTRY)]))
  }
  expect_identical(after$infoDesign$rep_checks, before$infoDesign$rep_checks)
  expect_identical(after$plotsNumber, before$plotsNumber)
}

test_that("check optimization of a multiple diagonal arrangement keeps entries, experiments and check plots", {
  local_rng_state()
  set.seed(99)
  rng <- .Random.seed
  cases <- list(
    list(nrows = 20, ncols = 17, lines = 300, checks = 4, blocks = c(100, 120, 80), splitBy = "row"),
    list(nrows = 20, ncols = 17, lines = 300, checks = 4, blocks = c(100, 120, 80), splitBy = "column",
         planter = "cartesian"),
    list(nrows = 30, ncols = 26, lines = 720, checks = 5, blocks = c(150, 155, 95, 200, 120), l = 2,
         plotNumber = c(1001, 2001), locationNames = c("North", "South")),
    list(nrows = 12, ncols = 18, lines = 200, checks = 4, blocks = rep(40, 5), sameEntries = TRUE)
  )
  improved <- 0L
  for (case in cases) {
    args <- c(case, list(kindExpt = "DBUDC", seed = 5, year = 2026))
    before <- do.call(diagonal_arrangement, args)
    for (method in c("euclidean", "manhattan")) {
      after <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE, dist_method = method)))
      check_exchange_invariants(before, after, method)
      # the experiment of every plot is the one the randomization gave it
      expect_identical(after$fieldBook$EXPT, before$fieldBook$EXPT)
      improved <- improved + (after$infoDesign$check_optimization[[1L]]$swaps > 0L)
      # the same seed gives the same optimized design, and it replays
      expect_identical(do.call(diagonal_arrangement,
        c(args, list(spread_checks = TRUE, dist_method = method))), after)
      expect_identical(reproduce_design(after), after)
      expect_identical(.Random.seed, rng)
    }
    # the default is the randomization as it was
    expect_identical(do.call(diagonal_arrangement, c(args, spread_checks = FALSE)), before)
    expect_null(before$infoDesign$check_optimization)
    expect_null(before$check_spacing[[1L]]$after)
    expect_false("spread_checks" %in% names(before$metadata$parameters))
  }
  expect_gt(improved, 0L)
})

test_that("check optimization of a sparse allocation keeps the allocation and each location's checks", {
  local_rng_state()
  set.seed(7)
  rng <- .Random.seed
  args <- list(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234, year = 2026,
               locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"))
  before <- do.call(sparse_allocation, args)
  expect_length(before$check_spacing, 4L)
  expect_null(before$check_spacing[[1L]]$after)
  expect_false("spread_checks" %in% names(before$metadata$parameters))
  for (method in c("euclidean", "manhattan")) {
    after <- do.call(sparse_allocation, c(args, list(spread_checks = TRUE, dist_method = method)))
    check_exchange_invariants(before, after, method)
    expect_identical(after$allocation, before$allocation)
    expect_identical(after$list_locs, before$list_locs)
    expect_identical(after$size_locations, before$size_locations)
    expect_length(after$infoDesign$check_optimization, 4L)
    expect_true(any(vapply(after$infoDesign$check_optimization, `[[`, 1L, "swaps") > 0L))
    expect_identical(do.call(sparse_allocation,
      c(args, list(spread_checks = TRUE, dist_method = method))), after)
    expect_identical(reproduce_design(after), after)
    expect_identical(.Random.seed, rng)
  }
  expect_identical(do.call(sparse_allocation, c(args, spread_checks = FALSE)), before)
  # the report of the page reads a sparse allocation and a multiple arrangement alike
  report <- as.character(app_check_spacing(after, 3))
  expect_match(report, '<option value="3" selected>LOC3</option>', fixed = TRUE)
  expect_match(report, "Before", fixed = TRUE)
  expect_output(print(after), "Check spacing")
})

test_that("the pages with check spacing share its controls, values and arguments", {
  for (module in c("Diagonal", "diagonal_multiple", "sparse_allocation")) {
    spec <- design_app_spec(module)
    ids <- vapply(spec$controls, `[[`, "", "id")
    expect_true(all(c("spread_checks", "dist_method") %in% ids), info = module)
    flag <- spec$controls[[which(ids == "spread_checks")]]
    expect_true(flag$value, info = module)   # the page optimizes unless turned off
    expect_identical(spec$report, "check_spacing")
  }
  expect_identical(check_spacing_values(list(spread_checks = FALSE, dist_method = "manhattan")),
                   list(spread_checks = FALSE, dist_method = "euclidean"))
  expect_identical(check_spacing_args(list()), list(spread_checks = FALSE, dist_method = "euclidean"))
  on <- list(spread_checks = TRUE, dist_method = "manhattan")
  blocks <- c(100, 120, 80)
  values <- c(list(nrows = 19, ncols = 18, lines = 300, checks = 4, l = 1, plot_start = 1,
                   stacked = "By Row", seed = 3, blocks = blocks, sameEntries = FALSE), on)
  via_app <- do.call(diagonal_arrangement, design_args_diagonal_multiple(values))
  direct <- diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, plotNumber = 1,
    kindExpt = "DBUDC", seed = 3, blocks = blocks, spread_checks = TRUE, dist_method = "manhattan")
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$check_spacing, direct$check_spacing)
  allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                         add_checks = TRUE, checks = 4, seed = 5)
  values <- c(list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 5, nrows = 10,
                   ncols = 10, plot_start = c(1, 1001, 2001), sparse_list = allocation), on)
  via_app <- do.call(sparse_allocation, design_args_sparse_allocation(values))
  direct <- sparse_allocation(lines = 135, nrows = 10, ncols = 10, l = 3, plotNumber = c(1, 1001, 2001),
    copies_per_entry = 2, checks = 4, sparse_list = allocation, seed = 5,
    spread_checks = TRUE, dist_method = "manhattan")
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
})
