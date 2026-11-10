test_that("augmented RCBD plot numbers can start at 101, 201, 301 by block", {
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 7,
                      plotNumber = 101, plotIncrement = 100)
  fb <- x$fieldBook
  for (block in seq_len(5)) {
    plots <- fb$PLOT[fb$BLOCK == block & fb$ENTRY != 0]
    expect_identical(plots, 100 * block + seq_along(plots))
  }
  expect_equal(anyDuplicated(fb$PLOT[fb$ENTRY != 0]), 0)
  expect_identical(do.call(RCBD_augmented, x$metadata$parameters), x)
})

test_that("block plot numbers agree with field maps across layouts and fillers", {
  cases <- list(
    list(lines = 122, checks = 4, b = 7, nrows = 14, ncols = 11),
    list(lines = 122, checks = 4, b = 6, nrows = 2, ncols = 75))
  for (args in cases) for (planter in c("serpentine", "cartesian")) {
    for (random in c(TRUE, FALSE)) for (stack in c("vertical", "horizontal")) {
      args_run <- c(args, list(planter = planter, random = random, repsStack = stack,
        repsExpt = 2, l = 2, plotNumber = c(101, 1001, 10001, 11001),
        seed = 7, year = 2026))
      old <- do.call(RCBD_augmented, args_run)
      x <- do.call(RCBD_augmented, c(args_run, list(plotIncrement = 100)))
      fb <- x$fieldBook
      by_cell <- function(book) {
        book <- book[order(book$LOCATION, book$EXPT, book$ROW, book$COLUMN),
                     !names(book) %in% c("ID", "PLOT")]
        rownames(book) <- NULL
        book
      }
      expect_identical(by_cell(fb), by_cell(old$fieldBook))
      expect_identical(x$layout_random_sites, old$layout_random_sites)
      expect_true(all(fb$PLOT[fb$ENTRY == 0] == 0))
      for (loc in seq_len(2)) for (expt in seq_len(2)) {
        part <- fb[fb$LOCATION == levels(fb$LOCATION)[loc] &
                     fb$EXPT == levels(fb$EXPT)[expt] & fb$ENTRY != 0, ]
        start <- args_run$plotNumber[(loc - 1) * 2 + expt]
        expect_equal(anyDuplicated(part$PLOT), 0)
        for (block in seq_len(args$b)) {
          plots <- part$PLOT[part$BLOCK == block]
          expect_equal(sort(plots), start + (block - 1) * 100 + seq_along(plots) - 1)
        }
      }
      for (loc in seq_len(2)) {
        part <- fb[fb$LOCATION == levels(fb$LOCATION)[loc], ]
        map <- as.matrix(x$layout_plots_sites[[loc]])
        cells <- cbind(nrow(map) + 1 - part$ROW, part$COLUMN)
        expect_equal(unname(map[cells]), part$PLOT)
      }
      expect_identical(x$plotNumber, as.matrix(x$layout_plots_sites[[1]]))
    }
  }
})

test_that("block plot increments reject invalid, overlapping and inexact ranges", {
  for (increment in list(0, -1, 1.5, NA_real_, Inf, '100', TRUE, c(100, 200))) {
    expect_error(RCBD_augmented(lines = 50, checks = 3, b = 5,
                               plotIncrement = increment, seed = 7),
                 "plotIncrement", class = "fieldhub_input_error")
  }
  expect_error(RCBD_augmented(lines = 50, checks = 3, b = 5,
                             plotIncrement = 12, seed = 7),
               "at least 13", class = "fieldhub_input_error")
  expect_error(RCBD_augmented(lines = 50, checks = 3, b = 5,
                             plotNumber = 2^53, plotIncrement = 100, seed = 7),
               "exact", class = "fieldhub_input_error")
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, plotIncrement = 13, seed = 7)
  expect_equal(anyDuplicated(x$fieldBook$PLOT), 0)
})

test_that("block plot numbering preserves the caller's random stream", {
  set.seed(82)
  before <- .Random.seed
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, plotIncrement = 100, seed = 7)
  expect_identical(.Random.seed, before)
  expect_identical(x, RCBD_augmented(lines = 50, checks = 3, b = 5,
                                   plotIncrement = 100, seed = 7))
})

test_that("the augmented RCBD app passes optional block numbering to the engine", {
  spec <- design_app_spec("RCBD_augmented")
  raw <- design_control_defaults(spec)
  raw$plot_by_block <- TRUE
  raw$plot_increment <- 100
  raw$plot_start <- '101'
  raw$seed <- '7'
  controls <- read_design_controls(spec, raw)
  values <- spec$values(controls, NULL)
  args <- design_args_RCBD_augmented(values)
  expect_identical(args$plotIncrement, 100)
  x <- do.call(RCBD_augmented, args)
  starts <- tapply(x$fieldBook$PLOT, x$fieldBook$BLOCK, min)
  expect_equal(as.numeric(starts), 101 + 100 * (seq_along(starts) - 1))
  raw$plot_by_block <- FALSE
  raw$plot_increment <- 'invalid while disabled'
  values <- spec$values(read_design_controls(spec, raw), NULL)
  expect_null(design_args_RCBD_augmented(values)$plotIncrement)
})


test_that("automatic block numbering never overlaps across experiment reps", {
  for (stack in c("vertical", "horizontal")) {
    x <- RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 3,
                        l = 2, plotNumber = c(101, 10001), plotIncrement = 100,
                        repsStack = stack, seed = 7)
    for (location in levels(x$fieldBook$LOCATION)) {
      fb <- x$fieldBook[x$fieldBook$LOCATION == location, ]
      expect_equal(anyDuplicated(fb$PLOT[fb$ENTRY != 0]), 0)
      offset <- if (location == levels(x$fieldBook$LOCATION)[1]) 101 else 10001
      for (rep in seq_len(3)) {
        plots <- fb$PLOT[fb$EXPT == levels(fb$EXPT)[rep] & fb$BLOCK == 1]
        expect_equal(min(plots), offset + (rep - 1) * 500)
      }
    }
    expect_identical(do.call(RCBD_augmented, x$metadata$parameters), x)
  }
  expect_error(RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 2,
                             plotNumber = c(101, 201), plotIncrement = 100, seed = 7),
               "overlap across reps", class = "fieldhub_input_error")
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 2,
                      plotNumber = c(101, 1001), plotIncrement = 100, seed = 7)
  expect_equal(anyDuplicated(x$fieldBook$PLOT), 0)
})


test_that("block numbering works with uploaded entry labels and larger blocks", {
  entries <- data.frame(ENTRY = 1:53, NAME = paste0("Entry-", 1:53))
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 2,
                      data = entries, plotNumber = 101, plotIncrement = 100, seed = 7)
  expect_equal(anyDuplicated(x$fieldBook$PLOT), 0)
  expect_setequal(x$fieldBook$TREATMENT, entries$NAME)
  expect_identical(do.call(RCBD_augmented, x$metadata$parameters), x)
  expect_error(RCBD_augmented(lines = 600, checks = 4, b = 5,
                             plotIncrement = 100, seed = 7),
               "at least 124", class = "fieldhub_input_error")
  x <- RCBD_augmented(lines = 600, checks = 4, b = 5, repsExpt = 2,
                      plotNumber = 1001, plotIncrement = 1000, seed = 7)
  expect_equal(anyDuplicated(x$fieldBook$PLOT), 0)
})
