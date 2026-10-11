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
  expect_error(diagonal_arrangement(kindExpt = "DBUDC", spread_checks = TRUE), "SUDC")
})
