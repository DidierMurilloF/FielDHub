# Factorial designs: full_factorial(), split_plot(), split_split_plot() and
# strip_plot(): factor inputs, crossing and field books.

# ---- Field books ------------------------------------------------------------

test_that("full_factorial() keeps plots in their location when plotNumber repeats", {
  # Regression: locations were stacked, sorted by PLOT and then labelled by
  # position, so with the same starting plot the locations interleaved.
  fb <- full_factorial(setfactors = c(2, 2), reps = 2, l = 2, type = 2,
                       plotNumber = c(101, 101), seed = 1)$fieldBook
  expect_equal(
    list(dups = anyDuplicated(fb[, c("LOCATION", "PLOT")]),
         per_location = as.vector(table(fb$LOCATION)), ids = fb$ID),
    list(dups = 0L, per_location = c(8L, 8L), ids = seq_len(16)))
})

test_that("full_factorial() keeps level labels that contain spaces", {
  # Regression: combinations were pasted with " " and split on " ", so
  # "Low N" became two values.
  data_factorial <- data.frame(factor = c("N", "N", "P", "P"),
                               level = c("Low N", "High N", "Low P", "High P"))
  fb <- full_factorial(reps = 2, l = 1, type = 2, seed = 1, data = data_factorial)$fieldBook
  expect_equal(
    list(N = sort(unique(fb$FACTOR_N)), P = sort(unique(fb$FACTOR_P)), comb = fb$TRT_COMB),
    list(N = c("High N", "Low N"), P = c("High P", "Low P"),
         comb = paste(fb$FACTOR_N, fb$FACTOR_P, sep = "*")))
})

test_that("split_split_plot() assigns every whole plot to each rep under CRD (type = 1)", {
  # Regression: the whole-plot column used rep(each = reps) instead of
  # rep(times = reps), so a rep could hold one whole plot twice.
  sspd <- split_split_plot(wp = 3, sp = 2, ssp = 2, reps = 2, type = 1, l = 1, seed = 1)
  expect_true(all(table(sspd$fieldBook$REP, sspd$fieldBook$WHOLE_PLOT) > 0))
})

# ---- Factorial level uploads ------------------------------------------------

test_that("factorial uploads allow the same level label in different factors", {
  entries <- data.frame(FACTOR = rep(c("N", "P"), each = 2L),
                        LEVEL = rep(c("Low", "High"), 2L))
  expect_true(check_input("factorial", entries))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(entries, path, row.names = FALSE)
  expect_identical(load_file("entries.csv", path, ",", check = TRUE, design = "factorial"),
                   list(dataUp = entries))
  out <- full_factorial(reps = 2, l = 2, plotNumber = c(101, 1001), seed = 38, data = entries)
  expect_equal(out$infoDesign$runs, 4L)
  expect_true(all(table(out$fieldBook$LOCATION, out$fieldBook$REP,
                        out$fieldBook$FACTOR_N, out$fieldBook$FACTOR_P) == 1L))
})

test_that("factorial uniqueness follows the complete rows used by the engine", {
  entries <- data.frame(FACTOR = c("N", "N", "P", "P", NA, NA),
                        LEVEL = c("Low", "High", "Low", "High", "X", "X"))
  expect_true(check_input("factorial", entries))
  expect_equal(full_factorial(reps = 2, seed = 38, data = entries)$infoDesign$runs, 4L)
})

test_that("factorial generation rejects duplicate levels within one factor", {
  entries <- data.frame(FACTOR = c("N", "N", "P", "P"),
                        LEVEL = c("Low", "Low", "Low", "High"))
  expect_false(check_input("factorial", entries))
  set.seed(101)
  before <- .Random.seed
  expect_error(full_factorial(reps = 2, seed = 38, data = entries),
               class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

# ---- Mixed count and label factor specifications ----------------------------

test_that("count and label forms can be combined consistently across factors", {
  cases <- list(
    split_plot = list(args = list(wp = c(3, 5), sp = 2),
                      factors = c("WHOLE_PLOT", "SUB_PLOT")),
    split_split_plot = list(args = list(wp = c("A", "B"), sp = c(3, 5), ssp = 2),
                            factors = c("WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT")),
    strip_plot = list(args = list(Hplots = c("A", "B"), Vplots = 2),
                      factors = c("HSTRIP", "VSTRIP"))
  )
  for (engine in names(cases)) {
    case <- cases[[engine]]
    out <- do.call(engine, c(case$args, list(reps = 2, seed = 19, plotNumber = 101)))
    # Two levels per factor, two reps: every combination once per rep.
    expect_equal(
      list(crossed = all(table(out$fieldBook[c("REP", case$factors)]) == 1L),
           plots = nrow(out$fieldBook)),
      list(crossed = TRUE, plots = as.integer(2^(length(case$factors) + 1L))),
      info = engine)
    expect_identical(do.call(engine, out$metadata$parameters), out, info = engine)
  }
})

# ---- Input errors -----------------------------------------------------------

test_that("split and strip factor controls are complete scalar counts or label vectors", {
  specifications <- list(split_plot = list(wp = 3, sp = 2),
    split_split_plot = list(wp = 3, sp = 2, ssp = 2), strip_plot = list(Hplots = 3, Vplots = 2))
  bad_values <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, TRUE,
    list(1, 2), matrix(2), c("A", NA), c("A", " "), c("A", "A"))
  stream_moved <- character()
  for (engine in names(specifications)) {
    for (parameter in names(specifications[[engine]])) for (bad in bad_values) {
      nm <- paste(engine, parameter, paste(deparse(bad), collapse = " "))
      args <- c(specifications[[engine]], list(reps = 2, plotNumber = 101, seed = 19))
      args[parameter] <- list(bad)
      set.seed(718)
      before <- .Random.seed
      expect_error(do.call(engine, args), class = "fieldhub_input_error", info = nm)
      if (!identical(.Random.seed, before)) stream_moved <- c(stream_moved, nm)
    }
  }
  expect(length(stream_moved) == 0,
         sprintf("the random stream moved on error for %s", stream_moved[1]))
})

test_that("full factorial level counts are complete finite positive whole numbers", {
  # c(2^20, 2^20): the product of counts is bounded before any expansion.
  for (bad in list(NULL, numeric(), c(NA, 2), c(NaN, 2), c(Inf, 2), c(-Inf, 2),
                   c(0, 2), c(-1, 2), c(2.5, 2), c(TRUE, FALSE), list(2, 3),
                   matrix(c(2, 3)), rep(1, 27), c(2^20, 2^20))) {
    expect_error(full_factorial(setfactors = bad, reps = 2, seed = 19),
                 class = "fieldhub_input_error", info = paste(deparse(bad), collapse = ""))
  }
})

test_that("factor tables need complete, usable, non-blank level columns", {
  bad_tables <- function(columns) list(
    empty = data.frame(), one_numeric_column = data.frame(A = 1:3),
    no_rows = as.data.frame(rep(list(character()), columns)),
    all_missing = as.data.frame(rep(list(c(NA, NA)), columns)),
    duplicated_levels = as.data.frame(rep(list(c("A", "A", "B")), columns)),
    blank_level = as.data.frame(rep(list(c("A", " ")), columns)))
  cases <- list()
  for (engine in c("split_plot", "split_split_plot", "strip_plot")) {
    tables <- bad_tables(if (engine == "split_split_plot") 3L else 2L)
    for (nm in names(tables)) {
      cases[[paste(engine, nm)]] <- list(engine = engine, data = tables[[nm]])
    }
  }
  factorial <- data.frame(FACTOR = rep(c("N", "P"), each = 2), LEVEL = rep(c("low", "high"), 2))
  blank_factor <- factorial; blank_factor$FACTOR[1] <- " "
  blank_level <- factorial; blank_level$LEVEL[1] <- " "
  factorial_tables <- list(one_column = data.frame(FACTOR = "N"), empty = data.frame(),
    no_rows = data.frame(FACTOR = character(), LEVEL = character()),
    no_complete_rows = data.frame(FACTOR = c("N", "P"), LEVEL = c(NA, NA)),
    blank_factor = blank_factor, blank_level = blank_level)
  for (nm in names(factorial_tables)) {
    cases[[paste("full_factorial", nm)]] <- list(engine = "full_factorial",
                                                 data = factorial_tables[[nm]])
  }
  for (nm in names(cases)) {
    args <- list(data = cases[[nm]]$data, reps = 2, seed = 19)
    if (cases[[nm]]$engine != "full_factorial") args$plotNumber <- 101
    expect_error(do.call(cases[[nm]]$engine, args), class = "fieldhub_input_error", info = nm)
  }
})
