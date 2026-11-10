# The row/column rule through the p-rep designs (spec-m15 sections 5.2-5.4).
test_that("partially_replicated() reports shared pairs only when the rule is on", {
  on <- partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                             seed = 1, year = 2026, rowcol_penalty = Inf)
  expect_identical(names(on$infoDesign)[3:4], c("min_distance", "shared_pairs"))
  expect_identical(on$infoDesign$shared_pairs, 0L)
  off <- partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                              seed = 1, year = 2026)
  expect_false("shared_pairs" %in% names(off$infoDesign))
  expect_identical(utils::tail(names(off$metadata$parameters), 2L), c("rowcol_penalty", "rowcol_scope"))
})

test_that("each location gets the rule, and later locations follow the changed draws", {
  skip_on_cran()
  on <- partially_replicated(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1), l = 3,
                             seed = 5, year = 2026, rowcol_penalty = Inf)
  expect_identical(on$infoDesign$shared_pairs, c(0L, 0L, 0L))
  m <- multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, seed = 2, year = 2026,
                           rowcol_penalty = Inf)
  expect_identical(m$infoDesign$shared_pairs, c(0L, 0L, 0L))
  expect_identical(m$metadata$parameters$rowcol_penalty, Inf)
})

test_that("the rule needs the optimizer: spread_reps = FALSE is an error before any draw", {
  set.seed(9)
  before <- .Random.seed
  expect_error(partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                                    spread_reps = FALSE, rowcol_penalty = 1),
               class = "fieldhub_input_error")
  expect_error(multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, spread_reps = FALSE,
                                   rowcol_penalty = Inf),
               class = "fieldhub_input_error")
  expect_error(partially_replicated(nrows = 16, ncols = 25, repGens = c(80, 240), repUnits = c(2, 1),
                                    rowcol_scope = "rows"),
               class = "fieldhub_error")
  expect_identical(.Random.seed, before)
})

test_that("a p-rep result recorded before the rule replays to the same field book", {
  x <- partially_replicated(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1),
                            seed = 7, year = 2026)
  legacy <- x
  p <- legacy$metadata$parameters
  legacy$metadata$parameters <- p[setdiff(names(p), c("rowcol_penalty", "rowcol_scope"))]
  expect_identical(reproduce_design(legacy)$fieldBook, x$fieldBook)
})

test_that("optimized_arrangement() gains no rule arguments", {
  expect_false(any(c("rowcol_penalty", "rowcol_scope") %in% names(formals(optimized_arrangement))))
})
