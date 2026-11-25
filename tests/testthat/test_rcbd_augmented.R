# RCBD_augmented(): augmented randomized complete blocks, their field book,
# field map and block plot numbering.

# NULL when every block holds each check once and every line appears exactly
# once; otherwise a message naming the first problem.
arcbd_problem <- function(fb, lines, checks) {
  is_check <- fb$ENTRY %in% seq_len(checks)
  per_block <- table(factor(fb$BLOCK[is_check]),
                     factor(fb$ENTRY[is_check], levels = seq_len(checks)))
  if (any(per_block != 1)) {
    bad <- which(per_block != 1, arr.ind = TRUE)[1, ]
    return(sprintf("block %s holds check %d %d times", rownames(per_block)[bad[1]],
                   bad[2], per_block[bad[1], bad[2]]))
  }
  test_entries <- fb$ENTRY[fb$ENTRY > checks]
  if (anyDuplicated(test_entries)) {
    return(sprintf("line %s appears twice", test_entries[anyDuplicated(test_entries)]))
  }
  missing <- setdiff((checks + 1):(checks + lines), test_entries)
  if (length(missing)) return(sprintf("line %d is missing", missing[1]))
  NULL
}

# ---- Field book -------------------------------------------------------------

test_that("RCBD_augmented() without randomized entries keeps one set of checks per block", {
  # Regression: with random = FALSE and fillers, the first field row was
  # overwritten with fillers and a new set of all checks, ignoring blocks:
  # a block spanning several rows got a second set and lost lines.
  cases <- list(
    multi_row = list(b = 7, nrows = 14, ncols = 11, fillers = NULL),
    side_by_side = list(b = 6, nrows = 2, ncols = 75, fillers = 4)
  )
  for (nm in names(cases)) for (planter in c("serpentine", "cartesian")) {
    case <- cases[[nm]]
    fb <- RCBD_augmented(lines = 122, checks = 4, b = case$b, nrows = case$nrows,
                         ncols = case$ncols, random = FALSE, planter = planter,
                         seed = 7)$fieldBook
    problem <- arcbd_problem(fb, lines = 122, checks = 4)
    if (!is.null(case$fillers) && is.null(problem) && sum(fb$ENTRY == 0) != case$fillers) {
      problem <- sprintf("%d fillers, expected %d", sum(fb$ENTRY == 0), case$fillers)
    }
    expect(is.null(problem), sprintf("case %s/%s: %s", nm, planter, problem))
  }
})

test_that("RCBD_augmented() without randomized entries keeps the lines in field order", {
  arcbd <- RCBD_augmented(lines = 122, checks = 4, b = 7, nrows = 14, ncols = 11,
                          random = FALSE, seed = 7)
  problem <- arcbd_problem(arcbd$fieldBook, lines = 122, checks = 4)
  expect(is.null(problem), paste("design:", problem))
  layout <- as.matrix(arcbd$layoutRandom)
  bottom_row <- layout[nrow(layout), ]
  bottom_lines <- as.numeric(bottom_row[as.numeric(bottom_row) > 4])
  expect_equal(bottom_lines, sort(bottom_lines))
})

test_that("RCBD_augmented() returns numeric ENTRY and CHECKS columns", {
  # Regression: ENTRY was character for randomized designs without fillers,
  # and CHECKS was character ("NA" for fillers) whenever there were fillers.
  cases <- list(
    random_no_fillers = list(lines = 77, checks = 5, b = 7, nrows = 7, ncols = 16, random = TRUE),
    random_fillers = list(lines = 122, checks = 4, b = 5, nrows = 5, ncols = 29, random = TRUE),
    fixed_no_fillers = list(lines = 77, checks = 5, b = 7, nrows = 7, ncols = 16, random = FALSE),
    fixed_fillers = list(lines = 122, checks = 4, b = 5, nrows = 5, ncols = 29, random = FALSE)
  )
  for (nm in names(cases)) {
    fb <- do.call(RCBD_augmented, c(cases[[nm]], seed = 7))$fieldBook
    filler <- fb$TREATMENT == "Filler"
    expect_equal(
      list(ENTRY = typeof(fb$ENTRY), CHECKS = typeof(fb$CHECKS),
           check_codes = sort(unique(stats::na.omit(fb$CHECKS))),
           fillers_na = all(is.na(fb$CHECKS[filler])),
           others_na = any(is.na(fb$CHECKS[!filler]))),
      list(ENTRY = "double", CHECKS = "double", check_codes = c(0, 1),
           fillers_na = TRUE, others_na = FALSE),
      ignore_attr = TRUE, info = nm)
  }
})

test_that("the augmented RCBD field map draws filler plots as empty plots", {
  arcbd <- RCBD_augmented(lines = 122, checks = 4, b = 5, nrows = 5, ncols = 29,
                          random = TRUE, seed = 7)
  p <- plot_layout(arcbd, l = 1)
  book <- p$fieldBookXY
  filler <- is.na(book$CHECKS)
  expect_true(any(filler))
  drawn <- ggplot2::ggplot_build(p$out_layout)$data
  text <- drawn[[which(vapply(drawn, function(d) "label" %in% names(d), logical(1)))[1]]]
  tiles <- drawn[[1]]
  at <- function(d) match(paste(book$COLUMN, book$ROW), paste(d$x, d$y))
  # Fillers: white plots with grey numbers; checks and lines: dark numbers.
  expect_true(all(tiles$fill[at(tiles)][filler] == "#FFFFFF"))
  expect_true(all(text$colour[at(text)][filler] == "gray60"))
  expect_true(all(text$colour[at(text)][!filler] == "gray10"))
})

# ---- Block plot numbering (plotIncrement) -----------------------------------

test_that("augmented RCBD plot numbers can start at 101, 201, 301 by block", {
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 7,
                      plotNumber = 101, plotIncrement = 100)
  fb <- x$fieldBook[x$fieldBook$ENTRY != 0, ]
  want <- unlist(lapply(split(fb$PLOT, fb$BLOCK), function(p) seq_along(p)), use.names = FALSE) +
    100 * rep(sort(unique(fb$BLOCK)), times = as.vector(table(fb$BLOCK)))
  got <- unlist(split(fb$PLOT, fb$BLOCK), use.names = FALSE)
  expect_equal(got, want)
  expect_equal(anyDuplicated(fb$PLOT), 0)
  expect_identical(do.call(RCBD_augmented, x$metadata$parameters), x)
})

test_that("block plot numbers agree with field maps across layouts and fillers", {
  # plotIncrement renumbers plots only: placement, maps of entries and the
  # random stream must equal those of the same call without it.
  fields <- list(tall = list(lines = 122, checks = 4, b = 7, nrows = 14, ncols = 11),
                 wide = list(lines = 122, checks = 4, b = 6, nrows = 2, ncols = 75))
  grid <- expand.grid(field = names(fields), planter = c("serpentine", "cartesian"),
                      random = c(TRUE, FALSE), stack = c("vertical", "horizontal"),
                      stringsAsFactors = FALSE)
  by_cell <- function(book) {
    book <- book[order(book$LOCATION, book$EXPT, book$ROW, book$COLUMN),
                 !names(book) %in% c("ID", "PLOT")]
    rownames(book) <- NULL
    book
  }
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    nm <- paste(g, collapse = "/")
    args <- fields[[g$field]]
    args_run <- c(args, list(planter = g$planter, random = g$random, repsStack = g$stack,
                             repsExpt = 2, l = 2, plotNumber = c(101, 1001, 10001, 11001),
                             seed = 7, year = 2026))
    old <- do.call(RCBD_augmented, args_run)
    x <- do.call(RCBD_augmented, c(args_run, list(plotIncrement = 100)))
    fb <- x$fieldBook
    expect_identical(list(by_cell(fb), x$layout_random_sites),
                     list(by_cell(old$fieldBook), old$layout_random_sites), info = nm)

    # Each block of each (location, experiment rep) numbers its plots from
    # start + (block - 1) * 100 upwards; fillers keep plot 0.
    numbering_ok <- all(fb$PLOT[fb$ENTRY == 0] == 0)
    for (loc in seq_len(2)) for (expt in seq_len(2)) {
      part <- fb[fb$LOCATION == levels(fb$LOCATION)[loc] &
                   fb$EXPT == levels(fb$EXPT)[expt] & fb$ENTRY != 0, ]
      start <- args_run$plotNumber[(loc - 1) * 2 + expt]
      for (block in seq_len(args$b)) {
        plots <- part$PLOT[part$BLOCK == block]
        numbering_ok <- numbering_ok &&
          isTRUE(all.equal(sort(plots), start + (block - 1) * 100 + seq_along(plots) - 1))
      }
    }
    expect(numbering_ok, sprintf("case %s: block plot numbers are not %s", nm,
                                 "start + (block - 1) * 100 + 0, 1, ..."))

    # The plot map of each location shows the field book plot at every cell.
    on_map <- unlist(lapply(seq_len(2), function(loc) {
      part <- fb[fb$LOCATION == levels(fb$LOCATION)[loc], ]
      map <- as.matrix(x$layout_plots_sites[[loc]])
      unname(map[cbind(nrow(map) + 1 - part$ROW, part$COLUMN)]) == part$PLOT
    }))
    expect(all(on_map), sprintf("case %s: %d plots differ from the field map", nm,
                                sum(!on_map)))
    expect_identical(x$plotNumber, as.matrix(x$layout_plots_sites[[1]]), info = nm)
  }
})

test_that("automatic block numbering never overlaps across experiment reps", {
  for (stack in c("vertical", "horizontal")) {
    x <- RCBD_augmented(lines = 50, checks = 3, b = 5, repsExpt = 3,
                        l = 2, plotNumber = c(101, 10001), plotIncrement = 100,
                        repsStack = stack, seed = 7)
    fb <- x$fieldBook[x$fieldBook$ENTRY != 0, ]
    first <- fb[fb$BLOCK == 1, ]
    starts <- tapply(first$PLOT, list(first$LOCATION, first$EXPT), min)
    # Each experiment rep takes the next 5 blocks x 100 plots.
    want <- outer(c(101, 10001), (seq_len(3) - 1) * 500, `+`)
    expect_equal(list(dups = anyDuplicated(paste(fb$LOCATION, fb$PLOT)), starts = unname(starts)),
                 list(dups = 0L, starts = want), info = stack)
    expect_identical(do.call(RCBD_augmented, x$metadata$parameters), x)
  }
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
  x <- RCBD_augmented(lines = 600, checks = 4, b = 5, repsExpt = 2,
                      plotNumber = 1001, plotIncrement = 1000, seed = 7)
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

test_that("block plot increments reject invalid, overlapping and inexact ranges", {
  base <- list(lines = 50, checks = 3, b = 5, seed = 7)
  cases <- list(
    zero = list(plotIncrement = 0), negative = list(plotIncrement = -1),
    fractional = list(plotIncrement = 1.5), missing = list(plotIncrement = NA_real_),
    infinite = list(plotIncrement = Inf), character = list(plotIncrement = "100"),
    logical = list(plotIncrement = TRUE), two_values = list(plotIncrement = c(100, 200)),
    # 50 lines + 3 checks in 5 blocks need 13 plots per block.
    smaller_than_block = list(plotIncrement = 12),
    smaller_than_large_block = list(lines = 600, checks = 4, plotIncrement = 100),
    not_exact_in_double = list(plotNumber = 2^53, plotIncrement = 100),
    overlapping_reps = list(repsExpt = 2, plotNumber = c(101, 201), plotIncrement = 100)
  )
  for (nm in names(cases)) {
    expect_error(do.call(RCBD_augmented, utils::modifyList(base, cases[[nm]])),
                 class = "fieldhub_input_error", info = nm)
  }
  # The smallest increment that fits the block is accepted.
  x <- RCBD_augmented(lines = 50, checks = 3, b = 5, plotIncrement = 13, seed = 7)
  expect_equal(anyDuplicated(x$fieldBook$PLOT), 0)
})

# ---- App controls -----------------------------------------------------------

test_that("the augmented RCBD app passes optional block numbering to the engine", {
  spec <- design_app_spec("RCBD_augmented")
  raw <- design_control_defaults(spec)
  raw$plot_by_block <- TRUE
  raw$plot_increment <- 100
  raw$plot_start <- "101"
  raw$seed <- "7"
  args <- design_args_RCBD_augmented(spec$values(read_design_controls(spec, raw), NULL))
  x <- do.call(RCBD_augmented, args)
  starts <- tapply(x$fieldBook$PLOT, x$fieldBook$BLOCK, min)
  expect_equal(as.numeric(starts), 101 + 100 * (seq_along(starts) - 1))
  # A disabled control is ignored, even with an invalid value.
  raw$plot_by_block <- FALSE
  raw$plot_increment <- "invalid while disabled"
  values <- spec$values(read_design_controls(spec, raw), NULL)
  expect_null(design_args_RCBD_augmented(values)$plotIncrement)
})
