library(FielDHub)

# Maps each test entry of a DBUDC field book to the block it was assigned to
# in `blocks`, given entries numbered consecutively after the checks.
entry_block <- function(entry, checks, blocks) {
  findInterval(entry - checks - 1, cumsum(c(0, blocks))[-(length(blocks) + 1)])
}

test_that("DBUDC by column keeps entries in their own block with ten or more blocks", {
  # Regression test: get_random_stacked() matched the block labels
  # paste0("B", 1:b) in alphabetical order (B1, B10, B2, ...) against the
  # entry groups in numeric order, so with ten or more blocks the entries of
  # block 2 were placed in block 10's region, those of block 3 in block 2's,
  # and so on.
  blocks <- rep(40, 10)
  diag <- diagonal_arrangement(
    nrows = 20, ncols = 25, lines = 400, checks = 4, kindExpt = "DBUDC",
    splitBy = "column", blocks = blocks, plotNumber = 1, seed = 1
  )
  fb <- diag$fieldBook
  tests <- fb[fb$CHECKS == 0, ]
  expect_equal(nrow(tests), 400)
  expected_expt <- paste0("Block", entry_block(tests$ENTRY, 4, blocks))
  expect_equal(tests$EXPT, expected_expt)
})

test_that("DBUDC by column accepts unequal block sizes with ten or more blocks", {
  blocks <- c(30, 50, rep(40, 8))
  diag <- diagonal_arrangement(
    nrows = 20, ncols = 25, lines = 400, checks = 4, kindExpt = "DBUDC",
    splitBy = "column", blocks = blocks, plotNumber = 1, seed = 1
  )
  fb <- diag$fieldBook
  tests <- fb[fb$CHECKS == 0, ]
  expected_expt <- paste0("Block", entry_block(tests$ENTRY, 4, blocks))
  expect_equal(tests$EXPT, expected_expt)
})

test_that("DBUDC numbers plots in the order of exptName, whatever the names", {
  # Regression test: plot_number() took the size of each experiment from
  # table(), which sorts the names alphabetically, but numbered the
  # experiments in the order given. With unequal block sizes and names not in
  # alphabetical order, the sizes and names were paired wrongly and the call
  # failed with "missing value where TRUE/FALSE needed".
  expt_names <- c("Z", "Y", "X", "W", "V")
  diag <- diagonal_arrangement(
    nrows = 30, ncols = 26, lines = 720, checks = 5, kindExpt = "DBUDC",
    splitBy = "row", blocks = c(150, 155, 95, 200, 120), planter = "serpentine",
    plotNumber = 1, exptName = expt_names, seed = 1987
  )
  fb <- diag$fieldBook
  expect_false(anyNA(fb$PLOT))
  expect_equal(anyDuplicated(fb$PLOT), 0)
  first_plot <- tapply(fb$PLOT, fb$EXPT, min)[expt_names]
  last_plot <- tapply(fb$PLOT, fb$EXPT, max)[expt_names]
  expect_true(all(last_plot[-length(expt_names)] < first_plot[-1]))
})
