# partially_replicated() and multi_location_prep(): p-rep field books.

# Pairs of copies of the two-copy entries that share a ROW or a COLUMN, per
# location, counted from the field book alone
prep_shared_pairs <- function(fieldBook) {
  vapply(split(fieldBook, fieldBook$LOCATION), function(book) {
    book <- book[book$ENTRY > 0, ]
    counts <- table(book$ENTRY)
    pairs <- book[book$ENTRY %in% as.numeric(names(counts)[counts == 2L]), ]
    sum(vapply(split(pairs, pairs$ENTRY), function(p) {
      as.integer(p$ROW[1] == p$ROW[2]) + as.integer(p$COLUMN[1] == p$COLUMN[2])
    }, integer(1)))
  }, integer(1), USE.NAMES = FALSE)
}

# ---- Field book: copies and their REP numbers ----

test_that("each entry gets its copies, numbered REP 1 to the number of copies", {
  cases <- list(
    two_groups = list(nrows = 8, ncols = 5, repGens = c(10, 20), repUnits = c(2, 1)),
    three_groups = list(nrows = 11, ncols = 5, repGens = c(10, 20, 3), repUnits = c(2, 1, 5))
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    book <- do.call(partially_replicated, c(case, seed = 65784))$fieldBook
    want_copies <- rep(case$repUnits, case$repGens)
    copies <- as.vector(table(factor(book$ENTRY, levels = seq_along(want_copies))))
    reps_ok <- vapply(split(book$REP, book$ENTRY), function(r) identical(sort(as.integer(r)), seq_along(r)),
                      logical(1))
    expect(identical(copies, as.integer(want_copies)) && all(reps_ok),
           sprintf("case %s: entry %s has wrong copies or REP numbers", nm,
                   c(which(copies != want_copies), names(reps_ok)[!reps_ok])[1]))
  }
})

# ---- Fillers ----

test_that("prime-sized p-rep designs need allow_fillers, which adds one filler plot", {
  expect_error(partially_replicated(nrows = 6, ncols = 7, repGens = c(5, 31), repUnits = c(2, 1), seed = 123),
               class = "fieldhub_input_error")
  design <- partially_replicated(nrows = 6, ncols = 7, repGens = c(5, 31), repUnits = c(2, 1), seed = 123,
                                 allow_fillers = TRUE)
  book <- design$fieldBook
  filler <- book$TREATMENT == "Filler"
  copies <- table(book$ENTRY[!filler])
  expect_equal(
    list(info = design$infoDesign[c("experimental_plots", "field_capacity", "fillers")],
         filler_cells = c(sum(design$fillerField[[1]]), sum(design$layoutRandom[[1]] == 0),
                          sum(design$plotNumber[[1]] == 0), sum(filler)),
         filler_row = list(ENTRY = book$ENTRY[filler], PLOT = book$PLOT[filler], CHECKS = book$CHECKS[filler],
                           REP = book$REP[filler]),
         filler_in_pairs = any(design$pairsDistance[[1]]$geno == 0),
         copies = as.numeric(copies[as.character(1:36)])),
    list(info = list(experimental_plots = 41, field_capacity = 42, fillers = 1),
         filler_cells = c(1, 1, 1, 1),
         filler_row = list(ENTRY = 0, PLOT = 0, CHECKS = 0, REP = NA_integer_),
         filler_in_pairs = FALSE,
         copies = c(rep(2, 5), rep(1, 31))))
})

test_that("the filler is the last plot in planting order", {
  # Planting starts at ROW 1, COLUMN 1; the filler takes the last plot of the
  # top row, whose end depends on the planter and the parity of the rows.
  cases <- data.frame(planter = c("cartesian", "serpentine", "serpentine"), nrows = c(6, 6, 5),
                      column = c(7, 1, 7))
  for (i in seq_len(nrow(cases))) {
    nr <- cases$nrows[i]
    book <- partially_replicated(nrows = nr, ncols = 7, repGens = c(5, nr * 7 - 11), repUnits = c(2, 1),
                                 seed = 123, allow_fillers = TRUE, planter = cases$planter[i])$fieldBook
    filler <- book[book$TREATMENT == "Filler", ]
    expect_equal(c(filler$ROW, filler$COLUMN), c(nr, cases$column[i]),
                 info = paste(cases$planter[i], nr, "rows"))
  }
})

test_that("p-rep dimension options keep fillers opt-in and cap them by default", {
  expect_null(prep_dimension_options(41, allow_fillers = FALSE))
  options <- prep_dimension_options(41, allow_fillers = TRUE)
  capped <- prep_dimension_options(293, allow_fillers = TRUE)
  expanded <- prep_dimension_options(293, allow_fillers = TRUE, max_fillers = 12)
  ok <- c(offered = nrow(options) > 0,
          fewest_one = min(options$fillers) == 1,
          capacity_holds_plots = all(options$capacity - options$fillers == 41),
          labelled = all(grepl("filler", options$label)),
          default_cap = max(capped$fillers) <= 10,
          cap_raised = any(expanded$fillers > 10))
  expect(all(ok), sprintf("dimension options: %s fails", paste(names(ok)[!ok], collapse = ", ")))
})

# ---- Input errors ----

test_that("partially_replicated() rejects missing or fractional group counts by name", {
  # Regression: repGens = c(75, NA), as typed in the app, failed with a raw R error from 1:sum(NA).
  expect_error(partially_replicated(nrows = 10, ncols = 10, repGens = c(75, NA), repUnits = c(2, 1), seed = 1),
               "repGens", class = "fieldhub_input_error")
  expect_error(partially_replicated(nrows = 10, ncols = 10, repGens = c(20, 30), repUnits = c(2, 1.5), seed = 1),
               "repUnits", class = "fieldhub_input_error")
})

test_that("the row/column rule needs the optimizer and valid controls, checked before any draw", {
  cases <- list(
    prep_spread_off = quote(partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                                                 spread_reps = FALSE, rowcol_penalty = 1)),
    multi_spread_off = quote(multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, spread_reps = FALSE,
                                                 rowcol_penalty = Inf)),
    bad_scope = quote(partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                                           rowcol_scope = "rows"))
  )
  for (nm in names(cases)) {
    set.seed(9)
    before <- .Random.seed
    expect_error(eval(cases[[nm]]), class = "fieldhub_input_error", info = nm)
    expect_identical(.Random.seed, before, info = nm)
  }
})

# ---- The row/column rule ----

test_that("with the rule on no pair of copies shares a row or column, and it is reported only then", {
  on <- partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                             seed = 1, year = 2026, rowcol_penalty = Inf)
  off <- partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                              seed = 1, year = 2026)
  expect_equal(list(reported = on$infoDesign$shared_pairs, field_book = prep_shared_pairs(on$fieldBook),
                    off_reports = "shared_pairs" %in% names(off$infoDesign)),
               list(reported = 0L, field_book = 0L, off_reports = FALSE))
})

test_that("every location gets the rule, in single- and multi-location p-rep designs", {
  skip_on_cran()
  cases <- list(
    partially_replicated = quote(partially_replicated(nrows = 10, ncols = 20, repGens = c(40, 120),
                                                      repUnits = c(2, 1), l = 3, seed = 5, year = 2026,
                                                      rowcol_penalty = Inf)),
    multi_location_prep = quote(multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, seed = 2,
                                                    year = 2026, rowcol_penalty = Inf))
  )
  for (nm in names(cases)) {
    design <- eval(cases[[nm]])
    expect_equal(list(reported = design$infoDesign$shared_pairs, field_book = prep_shared_pairs(design$fieldBook),
                      recorded = design$metadata$parameters$rowcol_penalty),
                 list(reported = c(0L, 0L, 0L), field_book = c(0L, 0L, 0L), recorded = Inf), info = nm)
  }
})

test_that("a p-rep result saved before the row/column rule replays to the same field book", {
  x <- partially_replicated(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1), seed = 7, year = 2026)
  legacy <- x
  p <- legacy$metadata$parameters
  legacy$metadata$parameters <- p[setdiff(names(p), c("rowcol_penalty", "rowcol_scope"))]
  expect_identical(reproduce_design(legacy)$fieldBook, x$fieldBook)
})
