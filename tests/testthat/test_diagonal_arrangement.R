# diagonal_arrangement() (single and multiple, DBUDC), its check spreading and
# check-spacing report, and the check options it shares with sparse_allocation().

# All same-check distances of a layout, sorted, by brute force
diagonal_check_distances <- function(layout, checks, method = "euclidean") {
  sort(unlist(lapply(checks, function(check) {
    positions <- which(layout == as.character(check), arr.ind = TRUE)
    if (nrow(positions) < 2L) return(numeric())
    as.numeric(stats::dist(positions, method = method))
  })))
}

# Maps each test entry of a DBUDC field book to the block it was assigned to
# in `blocks`, given entries numbered consecutively after the checks.
entry_block <- function(entry, checks, blocks) {
  findInterval(entry - checks - 1, cumsum(c(0, blocks))[-(length(blocks) + 1)])
}

# Percentages of checks offered for a field, read from the error listing them
available_checks_percent <- function(fun = diagonal_arrangement, ...) {
  msg <- tryCatch(fun(..., checksPercent = 0.001), error = function(e) conditionMessage(e))
  as.numeric(strsplit(sub(".*available for this field: ", "", sub("\\.$", "", msg)), ", ")[[1]])
}

# TRUE when the call raises `class` and leaves the caller's stream untouched
signals_without_draw <- function(call, class) {
  set.seed(819)
  before <- .Random.seed
  err <- tryCatch({ eval(call); NULL }, error = identity)
  inherits(err, class) && identical(.Random.seed, before)
}

expect_properties <- function(ok, case) {
  expect(all(ok), sprintf("case %s: property %s fails", case, paste(names(ok)[!ok], collapse = ", ")))
}

# What a check exchange must keep, for every location: a named logical vector
check_exchange_properties <- function(before, after, method = "euclidean") {
  site_ok <- vapply(seq_along(before$layoutRandom), function(site) {
    checks <- before$infoDesign$entry_checks[[site]]
    x <- before$layoutRandom[[site]]
    y <- after$layoutRandom[[site]]
    slots <- matrix(x %in% as.character(checks), nrow(x))
    diagnostics <- after$infoDesign$check_optimization[[site]]
    all(identical(x[!slots], y[!slots]),                              # entries and fillers stay
        identical(matrix(y %in% as.character(checks), nrow(y)), slots),  # the same plots hold checks
        identical(table(x[slots]), table(y[slots])),                   # each check keeps its plots
        identical(typeof(x), typeof(y)),
        min(diagonal_check_distances(y, checks, method)) >= min(diagonal_check_distances(x, checks, method)),
        diagnostics$after_min >= diagnostics$before_min,
        isTRUE(all.equal(after$check_spacing[[site]]$before,
                         diagonal_spacing_metrics(x, checks, before$data_entry[[site]], method))),
        isTRUE(all.equal(after$check_spacing[[site]]$after,
                         diagonal_spacing_metrics(y, checks, after$data_entry[[site]], method))))
  }, logical(1))
  fb <- before$fieldBook
  optimized <- after$fieldBook
  rows <- optimized$CHECKS != 0
  fixed <- setdiff(names(fb), c("ENTRY", "CHECKS", "TREATMENT"))
  locations <- unique(optimized$LOCATION)
  book_ok <- vapply(seq_along(locations), function(site) {
    book <- optimized[optimized$LOCATION == locations[site] & optimized$CHECKS != 0, ]
    y <- after$layoutRandom[[site]]
    names <- after$data_entry[[site]]
    isTRUE(all.equal(as.numeric(y[cbind(nrow(y) + 1L - book$ROW, book$COLUMN)]), as.numeric(book$ENTRY))) &&
      identical(as.character(book$TREATMENT), as.character(names$NAME[match(book$ENTRY, names$ENTRY)]))
  }, logical(1))
  c(layouts_per_location = all(site_ok),
    only_check_identity_changes = identical(fb[fixed], optimized[fixed]) &&
      identical(fb[fb$CHECKS == 0, ], optimized[fb$CHECKS == 0, ]) && identical(fb$CHECKS != 0, rows),
    checks_column_is_entry = isTRUE(all.equal(as.numeric(optimized$CHECKS[rows]), as.numeric(optimized$ENTRY[rows]))),
    check_plots_per_location = identical(table(fb$LOCATION[rows], fb$CHECKS[rows]),
                                         table(optimized$LOCATION[rows], optimized$CHECKS[rows])),
    field_book_matches_layout = all(book_ok),
    replication_kept = identical(after$infoDesign$rep_checks, before$infoDesign$rep_checks) &&
      identical(after$plotsNumber, before$plotsNumber))
}

# ---- DBUDC: blocks, experiments and plot numbers ----

test_that("DBUDC keeps each entry in its own block, with ten or more blocks", {
  # Regression: block labels B1, B10, B2, ... were matched alphabetically
  # against entry groups in numeric order.
  cases <- list(equal = rep(40, 10), unequal = c(30, 50, rep(40, 8)))
  for (nm in names(cases)) {
    blocks <- cases[[nm]]
    fb <- diagonal_arrangement(nrows = 20, ncols = 25, lines = 400, checks = 4, kindExpt = "DBUDC",
                               splitBy = "column", blocks = blocks, plotNumber = 1, seed = 1)$fieldBook
    tests <- fb[fb$CHECKS == 0, ]
    expect_equal(tests$EXPT, paste0("Block", entry_block(tests$ENTRY, 4, blocks)), info = nm)
  }
})

test_that("each DBUDC experiment is one run of consecutive plots, in block order", {
  # Regression: plot_number() paired the experiment sizes, sorted by name,
  # with the experiments in the given order.
  cases <- list(
    row_serpentine = list(splitBy = "row", planter = "serpentine", exptName = c("Z", "Y", "X", "W", "V")),
    row_cartesian = list(splitBy = "row", planter = "cartesian"),
    column_serpentine = list(splitBy = "column", planter = "serpentine"),
    column_cartesian = list(splitBy = "column", planter = "cartesian")
  )
  blocks <- c(150, 155, 95, 200, 120)
  for (nm in names(cases)) {
    case <- cases[[nm]]
    names <- if (is.null(case$exptName)) paste0("Block", 1:5) else case$exptName
    fb <- do.call(diagonal_arrangement, c(case, list(nrows = 30, ncols = 26, lines = 720, checks = 5,
                                                     kindExpt = "DBUDC", blocks = blocks, plotNumber = 1,
                                                     seed = 1987)))$fieldBook
    fb <- fb[fb$EXPT != "Filler", ]
    fb <- fb[order(fb$PLOT), ]
    tests <- table(factor(fb$EXPT[fb$CHECKS == 0], levels = names))
    expect_equal(list(runs = rle(fb$EXPT)$values, entries = as.vector(tests), plots_distinct = anyDuplicated(fb$PLOT)),
                 list(runs = names, entries = blocks, plots_distinct = 0L), info = nm)
  }
})

test_that("DBUDC randomizes a block that holds a single entry", {
  # Regression: sample(5) permutes 1:5 instead of returning the one entry 5.
  for (split in c("row", "column")) {
    fb <- diagonal_arrangement(nrows = 20, ncols = 25, lines = 400, checks = 4, kindExpt = "DBUDC",
                               splitBy = split, blocks = c(1, 399), plotNumber = 1, seed = 1)$fieldBook
    tests <- fb[fb$CHECKS == 0, ]
    expect_equal(list(single = tests$ENTRY[tests$EXPT == "Block1"], all = sort(tests$ENTRY)),
                 list(single = 5, all = 5:404), info = split)
  }
})

test_that("diagonal_arrangement() marks only the filler plots as fillers with a cartesian planter", {
  # Regression: names_layout() was called without the planter, so a check and
  # an entry were labelled fillers and the real fillers got plot numbers.
  fb <- diagonal_arrangement(nrows = 10, ncols = 32, lines = 287, checks = 4, planter = "cartesian",
                             seed = 17)$fieldBook
  is_filler <- fb$TREATMENT == "Filler"
  expect_equal(list(n = sum(is_filler), filler_expt = fb$EXPT == "Filler", filler_plot = fb$PLOT == 0),
               list(n = 2L, filler_expt = is_filler, filler_plot = is_filler))
})

# ---- Same entries in every block ----

test_that("DBUDC with sameEntries repeats the same entries in every block", {
  blocks_data <- data.frame(ENTRY = c(1:4, rep(5:44, times = 10)),
                            NAME = c(paste0("CK", 1:4), rep(paste0("L", 5:44), times = 10)))
  cases <- list(
    row = list(nrows = 30, ncols = 26, lines = 720, checks = 5, blocks = rep(180, 4), splitBy = "row"),
    column = list(nrows = 20, ncols = 25, lines = 400, checks = 4, blocks = rep(40, 10), splitBy = "column"),
    uploaded = list(nrows = 20, ncols = 25, checks = 4, blocks = rep(40, 10), splitBy = "column",
                    data = blocks_data)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    fb <- do.call(diagonal_arrangement, c(case, list(kindExpt = "DBUDC", sameEntries = TRUE, plotNumber = 1,
                                                     seed = 1)))$fieldBook
    tests <- fb[fb$CHECKS == 0 & fb$ENTRY > 0, ]
    want <- (case$checks + 1):(case$checks + case$blocks[1])
    each_block <- vapply(split(tests$ENTRY, tests$EXPT), function(e) identical(sort(as.numeric(e)), as.numeric(want)),
                         logical(1))
    ok <- c(field_size = nrow(fb) == case$nrows * case$ncols, every_block_has_each_entry_once = all(each_block))
    if (!is.null(case$data)) ok["uploaded_names"] <- identical(unique(tests$TREATMENT[tests$ENTRY == 5]), "L5")
    expect_properties(ok, nm)
  }
})

test_that("sameEntries needs a DBUDC design with equal blocks", {
  expect_error(diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, sameEntries = TRUE, seed = 1),
               class = "fieldhub_input_error")
  expect_error(diagonal_arrangement(nrows = 30, ncols = 26, lines = 720, checks = 5, kindExpt = "DBUDC",
                                    blocks = c(150, 155, 95, 200, 120), sameEntries = TRUE, seed = 1),
               class = "fieldhub_input_error")
})

# ---- Percentage of checks ----

test_that("diagonal_arrangement() uses the last percentage of checks by default and places the one requested", {
  args <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1)
  options <- do.call(available_checks_percent, args)
  expect_gt(length(options), 1)
  default <- do.call(diagonal_arrangement, args)
  last <- do.call(diagonal_arrangement, c(args, checksPercent = options[length(options)]))
  first <- do.call(diagonal_arrangement, c(args, checksPercent = options[1]))$fieldBook
  expect_equal(last, default)
  expect_equal(list(fewer_checks = sum(first$CHECKS != 0) != sum(default$fieldBook$CHECKS != 0),
                    entries = sort(unique(first$ENTRY[first$CHECKS == 0 & first$ENTRY > 0]))),
               list(fewer_checks = TRUE, entries = 5:274))
  expect_error(do.call(diagonal_arrangement, c(args, checksPercent = 0.001)), class = "fieldhub_input_error")
})

test_that("sparse_allocation() passes checksPercent to every location", {
  args <- list(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234)
  options <- do.call(available_checks_percent, c(list(fun = sparse_allocation), args))
  default <- do.call(sparse_allocation, args)
  last <- do.call(sparse_allocation, c(args, checksPercent = options[length(options)]))
  expect_equal(last$fieldBook, default$fieldBook)
})

test_that("the default selects the least-filler option, with check spreading too", {
  args <- list(nrows = 10, ncols = 32, lines = 287, checks = 4, seed = 1)
  choices <- diagonal_percent_choices(10, 32, 1:4, 291)
  options <- lapply(choices$choices, function(p) {
    do.call(diagonal_arrangement, c(args, list(checksPercent = p, spread_checks = TRUE)))
  })
  default <- do.call(diagonal_arrangement, c(args, spread_checks = TRUE))
  fillers <- vapply(options, function(x) sum(x$fieldBook$ENTRY == 0), integer(1))
  expect_equal(default, options[[length(options)]])
  expect_equal(sum(default$fieldBook$ENTRY == 0), min(fillers))
})

# ---- Fillers in the last column ----

test_that("fillers that cannot fit in the last column are refused", {
  # The 20-percent check map has 41 fillers but only 40 empty cells in its
  # last column; the app preview looped past row 50.
  lines <- 3959
  data <- data.frame(ENTRY = seq_len(lines + 4), BLOCK = c(rep("C", 4), rep(1:2, c(1979, 1980))))
  expect_null(diagonal_check_options(n_rows = 50, n_cols = 100, checks = 1:4, Option_NCD = TRUE,
                                     kindExpt = "DBUDC", stacked = "By Column", data = data,
                                     dim_data = lines + 4, dim_data_1 = lines))
  expect_error(diagonal_arrangement(nrows = 50, ncols = 100, lines = 3959, checks = 4, kindExpt = "DBUDC",
                                    splitBy = "column", blocks = c(1979, 1980), seed = 1),
               class = "fieldhub_dimension_error")
})

# ---- Input errors ----

test_that("spatial designs reject malformed checks, block sizes and options before any draw", {
  bad_checks <- list(NULL, numeric(), NA_real_, NaN, Inf, 0, -1, 2.5, "2", TRUE, list(2), matrix(2),
                     c(1, NA), c(1, 1), c(1, 3))
  bad_rep_checks <- list(NULL, numeric(), NA_real_, NaN, Inf, 0, -1, 2.5, "20", TRUE, list(20), matrix(20),
                         c(4, NA, 4, 4, 4), c(10, 10), 4, 5)
  bad_blocks <- list(NULL, numeric(), NA_real_, Inf, 0, -1, 2.5, "720", TRUE, list(720), matrix(720),
                     c(150, NA), c(150, -1))
  diag <- function(...) bquote(diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 19,
                                                    year = 2026, ..(list(...))), splice = TRUE)
  cases <- list(
    diagonal_checks = lapply(bad_checks, function(b) bquote(diagonal_arrangement(nrows = 15, ncols = 20,
      lines = 270, checks = .(b), seed = 19, year = 2026))),
    diagonal_checks_unsorted = list(quote(diagonal_arrangement(nrows = 15, ncols = 20, lines = 270,
      checks = 4:1, seed = 19))),
    diagonal_checks_exceed_field = list(quote(diagonal_arrangement(nrows = 15, ncols = 20, lines = 270,
      checks = 301, seed = 19))),
    optimized_checks = lapply(bad_checks, function(b) bquote(optimized_arrangement(nrows = 12, ncols = 10,
      lines = 100, rep_checks = 20, checks = .(b), seed = 19, year = 2026))),
    optimized_rep_checks = lapply(bad_rep_checks, function(b) bquote(optimized_arrangement(nrows = 12,
      ncols = 10, lines = 100, checks = 1:5, rep_checks = .(b), seed = 19))),
    diagonal_blocks = lapply(bad_blocks, function(b) bquote(diagonal_arrangement(nrows = 30, ncols = 26,
      lines = 720, checks = 5, kindExpt = "DBUDC", splitBy = "row", blocks = .(b), seed = 19, year = 2026))),
    spread_checks = lapply(list(NA, NULL, 1, "TRUE", c(TRUE, FALSE)), function(v) diag(spread_checks = v)),
    dist_method = list(diag(dist_method = "bad")),
    sparse_options = list(
      quote(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1, spread_checks = "yes")),
      quote(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1, dist_method = "bad")))
  )
  for (nm in names(cases)) {
    ok <- vapply(cases[[nm]], signals_without_draw, logical(1), class = "fieldhub_input_error")
    expect(all(ok), sprintf("%s: %s gives no fieldhub_input_error, or the caller's stream moved", nm,
                            paste(deparse(cases[[nm]][[which(!ok)[1]]]), collapse = " ")))
  }
})

test_that("optimized_arrangement() accepts check entries in any order", {
  design <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100, rep_checks = rep(5, 4), checks = 4:1,
                                  seed = 19)
  expect_setequal(design$fieldBook$ENTRY[design$fieldBook$CHECKS != 0], 1:4)
})

# ---- Check spreading ----

test_that("check spreading keeps entries, fillers and the field book, and spreads the checks", {
  local_rng_state()
  set.seed(781)
  rng <- .Random.seed
  for (planter in c("serpentine", "cartesian")) {
    for (fillers in c(FALSE, TRUE)) {
      args <- list(nrows = if (fillers) 10 else 15, ncols = if (fillers) 32 else 20,
                   lines = if (fillers) 287 else 270, checks = 4, planter = planter, seed = 1, year = 2026)
      before <- do.call(diagonal_arrangement, args)
      off_is_default <- identical(do.call(diagonal_arrangement, c(args, spread_checks = FALSE)), before)
      for (method in c("euclidean", "manhattan")) {
        after <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE, dist_method = method)))
        checks <- before$infoDesign$entry_checks[[1L]]
        x <- before$layoutRandom[[1L]]
        y <- after$layoutRandom[[1L]]
        ok <- c(check_exchange_properties(before, after, method),
                fillers_stay = identical(x == "Filler", y == "Filler"),
                strictly_spread = min(diagonal_check_distances(y, checks, method)) >
                  min(diagonal_check_distances(x, checks, method)),
                replays = identical(reproduce_design(after), after),
                off_is_default = off_is_default,
                caller_stream_kept = identical(.Random.seed, rng))
        expect_properties(ok, paste(planter, if (fillers) "fillers" else "no fillers", method))
      }
    }
  }
})

test_that("no single exchange of two check plots improves the spread layout", {
  # The spreading compares the sorted same-check distances, smallest first;
  # its result must be a local optimum of that order under one exchange.
  for (seed in 1:3) {
    design <- diagonal_arrangement(nrows = 8, ncols = 10, lines = 70, checks = 3, seed = seed, spread_checks = TRUE)
    y <- design$layoutRandom[[1]]
    checks <- design$infoDesign$entry_checks[[1]]
    best <- diagonal_check_distances(y, checks)
    slots <- which(y %in% as.character(checks))
    pairs <- utils::combn(slots, 2L)
    improves <- vapply(seq_len(ncol(pairs)), function(i) {
      candidate <- y
      candidate[pairs[, i]] <- rev(candidate[pairs[, i]])
      d <- diagonal_check_distances(candidate, checks)
      k <- which(d != best)
      length(k) > 0 && d[k[1]] > best[k[1]]
    }, logical(1))
    expect(!any(improves), sprintf("seed %d: exchanging plots %s improves the layout", seed,
                                   paste(pairs[, which(improves)[1]], collapse = " and ")))
  }
})

test_that("multiple locations and supplied check names stay consistent under check spreading", {
  data <- data.frame(ENTRY = c(101:104, 1001:1287),
                     NAME = c("Check A", "Check B", "Check C", "Check D", paste0("Line ", 1001:1287)))
  args <- list(nrows = 10, ncols = 32, checks = 4, data = data, l = 2,
               locationNames = c("North", "South"), plotNumber = c(101, 1001), seed = 2)
  before <- do.call(diagonal_arrangement, args)
  after <- do.call(diagonal_arrangement, c(args, spread_checks = TRUE))
  rows <- before$fieldBook$CHECKS == 0
  ok <- c(tests_unchanged = identical(before$fieldBook[rows, ], after$fieldBook[rows, ]),
          check_names = identical(after$fieldBook$TREATMENT[!rows],
                                  data$NAME[match(after$fieldBook$ENTRY[!rows], data$ENTRY)]),
          per_location = length(after$infoDesign$check_optimization) == 2L,
          replays = identical(reproduce_design(after), after))
  expect_properties(ok, "two locations")
})

test_that("check spreading of a multiple diagonal arrangement keeps entries, experiments and check plots", {
  skip_on_cran()
  local_rng_state()
  set.seed(99)
  rng <- .Random.seed
  cases <- list(
    by_row = list(nrows = 20, ncols = 17, lines = 300, checks = 4, blocks = c(100, 120, 80), splitBy = "row"),
    by_column_cartesian = list(nrows = 20, ncols = 17, lines = 300, checks = 4, blocks = c(100, 120, 80),
                               splitBy = "column", planter = "cartesian"),
    two_locations = list(nrows = 30, ncols = 26, lines = 720, checks = 5, blocks = c(150, 155, 95, 200, 120),
                         l = 2, plotNumber = c(1001, 2001), locationNames = c("North", "South")),
    same_entries = list(nrows = 12, ncols = 18, lines = 200, checks = 4, blocks = rep(40, 5), sameEntries = TRUE)
  )
  improved <- 0L
  for (nm in names(cases)) {
    args <- c(cases[[nm]], list(kindExpt = "DBUDC", seed = 5, year = 2026))
    before <- do.call(diagonal_arrangement, args)
    off <- c(off_is_default = identical(do.call(diagonal_arrangement, c(args, spread_checks = FALSE)), before),
             off_reports_no_optimization = is.null(before$infoDesign$check_optimization) &&
               is.null(before$check_spacing[[1L]]$after),
             off_records_no_option = !"spread_checks" %in% names(before$metadata$parameters))
    for (method in c("euclidean", "manhattan")) {
      after <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE, dist_method = method)))
      ok <- c(check_exchange_properties(before, after, method), off,
              experiments_kept = identical(after$fieldBook$EXPT, before$fieldBook$EXPT),
              replays = identical(reproduce_design(after), after),
              caller_stream_kept = identical(.Random.seed, rng))
      expect_properties(ok, paste(nm, method))
      improved <- improved + (after$infoDesign$check_optimization[[1L]]$swaps > 0L)
    }
  }
  expect_gt(improved, 0L)
})

test_that("check spreading of a sparse allocation keeps the allocation and each location's checks", {
  local_rng_state()
  set.seed(7)
  rng <- .Random.seed
  args <- list(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234, year = 2026,
               locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"))
  before <- do.call(sparse_allocation, args)
  off <- c(off_is_default = identical(do.call(sparse_allocation, c(args, spread_checks = FALSE)), before),
           spacing_per_location = length(before$check_spacing) == 4L,
           off_reports_no_after = is.null(before$check_spacing[[1L]]$after),
           off_records_no_option = !"spread_checks" %in% names(before$metadata$parameters))
  for (method in c("euclidean", "manhattan")) {
    after <- do.call(sparse_allocation, c(args, list(spread_checks = TRUE, dist_method = method)))
    ok <- c(check_exchange_properties(before, after, method), off,
            allocation_kept = identical(after[c("allocation", "list_locs", "size_locations")],
                                        before[c("allocation", "list_locs", "size_locations")]),
            each_location_optimized = length(after$infoDesign$check_optimization) == 4L,
            some_swap = any(vapply(after$infoDesign$check_optimization, `[[`, 1L, "swaps") > 0L),
            replays = identical(reproduce_design(after), after),
            caller_stream_kept = identical(.Random.seed, rng))
    expect_properties(ok, method)
  }
  expect_match(as.character(app_check_spacing(after, 3)), "Before", fixed = TRUE)
  expect_output(print(after), "Check spacing")
})

test_that("the pages with check spacing spread checks by default and build the same design as a direct call", {
  on_by_default <- vapply(c("Diagonal", "diagonal_multiple", "sparse_allocation"), function(module) {
    controls <- design_app_spec(module)$controls
    ids <- vapply(controls, `[[`, "", "id")
    all(c("spread_checks", "dist_method") %in% ids) && isTRUE(controls[[which(ids == "spread_checks")]]$value)
  }, logical(1))
  expect(all(on_by_default), sprintf("page %s does not offer check spreading on by default",
                                     names(on_by_default)[!on_by_default][1]))
  on <- list(spread_checks = TRUE, dist_method = "manhattan")
  blocks <- c(100, 120, 80)
  values <- c(list(nrows = 19, ncols = 18, lines = 300, checks = 4, l = 1, plot_start = 1,
                   stacked = "By Row", seed = 3, blocks = blocks, sameEntries = FALSE), on)
  via_app <- do.call(diagonal_arrangement, design_args_diagonal_multiple(values))
  direct <- diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, plotNumber = 1,
    kindExpt = "DBUDC", seed = 3, blocks = blocks, spread_checks = TRUE, dist_method = "manhattan")
  expect_equal(via_app[c("fieldBook", "check_spacing")], direct[c("fieldBook", "check_spacing")])
  allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                         add_checks = TRUE, checks = 4, seed = 5)
  values <- c(list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 5, nrows = 10,
                   ncols = 10, plot_start = c(1, 1001, 2001), sparse_list = allocation), on)
  via_app <- do.call(sparse_allocation, design_args_sparse_allocation(values))
  direct <- sparse_allocation(lines = 135, nrows = 10, ncols = 10, l = 3, plotNumber = c(1, 1001, 2001),
    copies_per_entry = 2, checks = 4, sparse_list = allocation, seed = 5,
    spread_checks = TRUE, dist_method = "manhattan")
  expect_equal(list(via_app$fieldBook, via_app$metadata$parameters),
                   list(direct$fieldBook, direct$metadata$parameters))
})

# ---- Check-spacing report ----

test_that("spacing measures agree with independent known coordinates", {
  local_rng_state()
  # Checks 1 and 2 on opposite corners of a 3 x 3 field, 3 alone in the centre
  layout <- matrix(99, 3, 3)
  layout[cbind(c(1, 3), c(1, 3))] <- 1
  layout[cbind(c(1, 3), c(3, 1))] <- 2
  layout[2, 2] <- 3
  entries <- data.frame(ENTRY = 1:3, NAME = c("Check A!", "Check B", "Singleton"))
  set.seed(17)
  rng <- .Random.seed
  euclidean <- diagonal_spacing_metrics(layout, 1:3, entries, radius = 3)
  measures <- c("occurrences", "min_distance", "mean_nearest_distance", "mean_pairwise_distance",
                "neighboring_occurrences", "close_pairs")
  expect_equal(as.list(euclidean[measures]),
               list(occurrences = c(2, 2, 1), min_distance = c(sqrt(8), sqrt(8), NA),
                    mean_nearest_distance = c(sqrt(8), sqrt(8), NA), mean_pairwise_distance = c(sqrt(8), sqrt(8), NA),
                    neighboring_occurrences = c(2, 2, 0), close_pairs = c(1, 1, 0)))
  expect_identical(euclidean$TREATMENT, entries$NAME)
  expect_equal(unname(diagonal_spacing_totals(euclidean)), c(sqrt(8), sqrt(8), sqrt(8), 80))
  manhattan <- diagonal_spacing_metrics(layout, 1:3, entries, "manhattan", radius = 3)
  boundary <- diagonal_spacing_metrics(layout, 1:3, entries, "manhattan", radius = 4)
  expect_equal(list(min = manhattan$min_distance, neighbors = manhattan$neighboring_occurrences,
                    boundary_neighbors = boundary$neighboring_occurrences),
               list(min = c(4, 4, NA), neighbors = c(0, 0, 0), boundary_neighbors = c(2, 2, 0)))
  expect_identical(.Random.seed, rng)
  # Overall distances average the checks equally; neighbors weigh by occurrences
  metrics <- data.frame(min_distance = c(2, 4), mean_nearest_distance = c(3, 5),
    mean_pairwise_distance = c(4, 8), occurrences = c(2, 8), neighboring_occurrences = c(2, 0))
  expect_equal(unname(diagonal_spacing_totals(metrics)), c(2, 4, 6, 20))
})

test_that("check spacing is reported for each location, before and after spreading", {
  design <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 8, l = 2,
                                 locationNames = c("North", "South"), dist_method = "manhattan")
  per_site <- vapply(1:2, function(site) {
    isTRUE(all.equal(design$check_spacing[[site]]$before,
                     diagonal_spacing_metrics(design$layoutRandom[[site]], 1:4, design$data_entry[[site]],
                                              "manhattan")))
  }, logical(1))
  expect_equal(list(locations = length(design$check_spacing), per_site = per_site),
               list(locations = 2L, per_site = c(TRUE, TRUE)))
  args <- list(nrows = 20, ncols = 20, lines = 350, checks = 4, seed = 7, year = 2026)
  off <- do.call(diagonal_arrangement, args)
  on <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE)))
  before_min <- diagonal_spacing_totals(off$check_spacing[[1]]$before)[["min_distance"]]
  ok <- c(same_start = identical(off$check_spacing[[1]]$before, on$check_spacing[[1]]$before),
          off_has_no_after = is.null(off$check_spacing[[1]]$after),
          before_is_brute_force = isTRUE(all.equal(before_min, min(diagonal_check_distances(off$layoutRandom[[1]], 1:4)))),
          after_not_worse = diagonal_spacing_totals(on$check_spacing[[1]]$after)[["min_distance"]] >= before_min,
          off_replays = identical(reproduce_design(off), off),
          on_replays = identical(reproduce_design(on), on))
  expect_properties(ok, "off and on")
})

test_that("printed designs and summaries show the check-spacing report", {
  args <- list(nrows = 20, ncols = 20, lines = 350, checks = 4, seed = 7, year = 2026)
  off <- do.call(diagonal_arrangement, args)
  on <- do.call(diagonal_arrangement, c(args, list(spread_checks = TRUE)))
  legacy <- off
  legacy$check_spacing <- NULL
  shown <- function(x, fun = print, ...) paste(capture.output(fun(x, ...)), collapse = "\n")
  cases <- list(
    on = list(text = shown(on, n = 2), has = c("Information on the design parameters", "Before.*After",
                                               "Candidate evaluations", "field book")),
    off = list(text = shown(off), has = c("Optimization: OFF", "Value"), lacks = "Candidate evaluations"),
    summary = list(text = shown(on, summary), has = c("2\\. Layout randomization", "3\\. Plot number layout",
                                                      "4\\. Structure", "5\\. Structure", "6\\. Check spacing",
                                                      "Per-check spacing", "Check-3")),
    saved_without_spacing = list(text = shown(legacy), has = character(), lacks = "Check spacing")
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    missing <- case$has[!vapply(case$has, grepl, logical(1), x = case$text)]
    present <- case$lacks[vapply(case$lacks, grepl, logical(1), x = case$text, fixed = TRUE)]
    expect(length(missing) + length(present) == 0L,
           sprintf("case %s: missing '%s', unexpected '%s'", nm, missing[1], present[1]))
  }
})

# ---- Replicate spacing and Field setup facts of the p-rep pages ----

test_that("the p-rep pages report the spacing of their replicated entries", {
  design <- partially_replicated(nrows = 15, ncols = 20, repGens = c(75, 150), repUnits = c(2, 1),
                                 l = 2, locationNames = c("North", "South"), seed = 7, rowcol_penalty = Inf)
  for (site in 1:2) {
    pairs <- design$pairsDistance[[site]]
    table <- app_replicate_spacing_table(pairs)
    facts <- app_replicate_spacing_facts(design, site)
    expect_equal(
      list(table = table$Value, engine_min = min(pairs$DIST),
           facts = stats::setNames(vapply(facts, `[[`, "", "value"), vapply(facts, `[[`, "", "label"))),
      list(table = c(sprintf("%.2f", min(pairs$DIST)), sprintf("%.2f", mean(pairs$DIST)), as.character(nrow(pairs)),
                     sprintf("%.1f%%", 100 * design$infoDesign$shared_pairs[[site]] / nrow(pairs))),
           engine_min = design$infoDesign$min_distance[[site]],
           facts = c(Optimization = "On", Distance = "Euclidean, in grid steps",
                     `Distinct rows and columns` = "Required", Seed = "7")),
      info = paste("site", site))
  }
  off <- partially_replicated(nrows = 15, ncols = 20, repGens = c(75, 150), repUnits = c(2, 1),
                              seed = 7, spread_reps = FALSE, dist_method = "manhattan")
  expect_identical(vapply(app_replicate_spacing_facts(off, 1L)[1:3], `[[`, "", "value"),
                   c("Off", "Manhattan, in grid steps", "Not required"))
})

test_that("Field setup of the p-rep page lists the run and the field", {
  view <- design_app_spec("pREPS")$setup$facts
  facts <- function(values, data, size) {
    items <- view(values, data, if (is.null(size)) list(NULL) else if (is.list(size)) size else list(size))
    stats::setNames(vapply(items, `[[`, "", "value"), vapply(items, `[[`, "", "label"))
  }
  values <- list(repGens = c(75, 150), repUnits = c(2, 1), plots = 300, l = 1)
  several <- list(repGens = c(20, 30, 100), repUnits = c(3, 2, 1), plots = 220, l = 3)
  upload <- data.frame(ENTRY = 1:6, NAME = letters[1:6], REPS = c(2, 2, 1, 1, 1, 1))
  x <- "×"
  cases <- list(
    fitting_field = list(got = facts(values, NULL, c(15, 20)),
      want = c(Entries = "225", Replicated = paste("75", x, "2 copies"), Unreplicated = paste("150", x, "1 copy"),
               Plots = "300", Field = paste("15", x, "20"), Fillers = "0")),
    field_with_fillers = list(got = facts(values, NULL, c(16, 19))[c("Field", "Fillers")],
                              want = c(Field = paste("16", x, "19"), Fillers = "4")),
    no_field_yet = list(got = names(facts(values, NULL, NULL)),
                        want = c("Entries", "Replicated", "Unreplicated", "Plots")),
    field_too_small = list(got = "Field" %in% names(facts(values, NULL, c(10, 10))), want = FALSE),
    several_groups = list(got = facts(several, NULL, c(11, 20)),
      want = c(Entries = "150", Replicated = paste("20", x, "3 copies"), Replicated = paste("30", x, "2 copies"),
               Unreplicated = paste("100", x, "1 copy"), `Plots per location` = "220",
               Field = paste("11", x, "20"), Fillers = "0", Locations = "3")),
    uploaded = list(got = facts(list(plots = 8, l = 1), upload, c(2, 4))[1:3],
      want = c(Entries = "6", Replicated = paste("2", x, "2 copies"), Unreplicated = paste("4", x, "1 copy")))
  )
  for (nm in names(cases)) expect_identical(cases[[nm]]$got, cases[[nm]]$want, info = nm)
})

test_that("Field setup of the multi-location p-rep page lists the allocation and each location's field", {
  view <- design_app_spec("multi_loc_preps")$setup$facts
  facts <- function(values, size) {
    items <- view(values, NULL, if (is.list(size)) size else list(size))
    stats::setNames(vapply(items, `[[`, "", "value"), vapply(items, `[[`, "", "label"))
  }
  x <- "×"
  values <- list(lines = 312, rep_checks = NULL, l = 6, copies_per_entry = "8",
                 optim_list = list(size_locations = rep(416, 6)))
  same <- rep(list(c(16, 26)), 6)
  mixed <- c(list(c(21, 20)), rep(list(c(26, 16)), 5))
  items <- view(values, NULL, mixed)
  by_label <- stats::setNames(items, vapply(items, `[[`, "", "label"))
  with_checks <- utils::modifyList(values, list(rep_checks = c(8, 8, 8)))
  cases <- list(
    one_field = list(got = facts(values, c(26, 16)),
      want = c(Entries = "312", `Copies per entry` = "8", Locations = "6", `Plots per location` = "416",
               Field = paste("26", x, "16"), Fillers = "0")),
    same_fields = list(got = facts(values, same)[["Field"]], want = paste("16", x, "26")),
    mixed_fields = list(got = c(by_label$Field$value, by_label$Fillers$value), want = c("By location", "0–4")),
    location_without_field = list(got = "Field" %in% names(facts(values, c(same[1:5], list(NULL)))), want = FALSE),
    with_checks = list(got = facts(with_checks, c(22, 20))[c("Checks", "Plots per location", "Fillers")],
                       want = c(Checks = "3", `Plots per location` = "440", Fillers = "0"))
  )
  for (nm in names(cases)) expect_identical(cases[[nm]]$got, cases[[nm]]$want, info = nm)
  # each location's field and fillers in the tooltip
  expect_match(paste(by_label$Field$tip, by_label$Fillers$tip),
               paste0("Location 1: 21 ", x, " 20, Location 2: 26 ", x, " 16.*Location 1: 4, Location 2: 0"))
})

# ---- Plot numbers of several locations ----

test_that("each location and replicate starts at its own plot number", {
  cases <- list(
    three_starts = list(t = 100, reps = 3, l = 3, plotNumber = c(101, 1001, 2001),
                        want = rbind(c(101, 201, 301), c(1001, 1101, 1201), c(2001, 2101, 2201))),
    more_than_a_hundred = list(t = 105, reps = 3, l = 1, plotNumber = 1, want = rbind(c(1, 106, 211))),
    two_reps = list(t = 20, reps = 2, l = 1, plotNumber = 1, want = rbind(c(1, 101))),
    start_1000 = list(t = 20, reps = 3, l = 1, plotNumber = 1000, want = rbind(c(1000, 1100, 1200)))
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    fb <- RCBD(t = case$t, reps = case$reps, l = case$l, plotNumber = case$plotNumber, seed = 1)$fieldBook
    first <- unname(tapply(fb$PLOT, list(fb$LOCATION, fb$REP), min))
    expect_equal(first, case$want, info = nm)
  }
})
