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

test_that("DBUDC randomizes a block that holds a single entry", {
  # Regression test: blocks were filled with sample(entries), and for a block
  # holding one numeric entry such as 5, sample(5) returns a permutation of
  # 1:5, so the block got the wrong entry and R warned about the length.
  for (split in c("row", "column")) {
    diag <- diagonal_arrangement(
      nrows = 20, ncols = 25, lines = 400, checks = 4, kindExpt = "DBUDC",
      splitBy = split, blocks = c(1, 399), plotNumber = 1, seed = 1
    )
    fb <- diag$fieldBook
    tests <- fb[fb$CHECKS == 0, ]
    expect_equal(tests$ENTRY[tests$EXPT == "Block1"], 5, info = split)
    expect_setequal(tests$ENTRY, 5:404)
  }
})

test_that("diagonal_arrangement() marks only the filler plots as fillers with a cartesian planter", {
  # Regression test: names_layout() was called without the planter, so with a
  # cartesian planter, an even number of rows and fillers, the experiment
  # names put the fillers at the wrong end of the first row: a check and an
  # entry got EXPT "Filler" and PLOT 0, and the real fillers got plot numbers.
  diag <- diagonal_arrangement(nrows = 10, ncols = 32, lines = 287, checks = 4,
                               planter = "cartesian", seed = 17)
  fb <- diag$fieldBook
  is_filler <- fb$TREATMENT == "Filler"
  expect_equal(sum(is_filler), 2)
  expect_true(all(fb$EXPT[is_filler] == "Filler"))
  expect_true(all(fb$PLOT[is_filler] == 0))
  expect_false(any(fb$EXPT[!is_filler] == "Filler"))
  expect_false(any(fb$PLOT[!is_filler] == 0))
})

# Percentages of checks offered for a field, read from the error listing them
available_checks_percent <- function(...) {
  msg <- tryCatch(diagonal_arrangement(..., checksPercent = 0.001),
                  error = function(e) conditionMessage(e))
  as.numeric(strsplit(sub(".*available for this field: ", "", sub("\\.$", "", msg)), ", ")[[1]])
}

test_that("diagonal_arrangement() uses the last percentage of checks by default", {
  args <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1)
  options <- do.call(available_checks_percent, args)
  expect_gt(length(options), 1)
  default <- do.call(diagonal_arrangement, args)
  last <- do.call(diagonal_arrangement, c(args, checksPercent = options[length(options)]))
  expect_identical(last, default)
})

test_that("diagonal_arrangement() places the percentage of checks requested", {
  args <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1)
  options <- do.call(available_checks_percent, args)
  first <- do.call(diagonal_arrangement, c(args, checksPercent = options[1]))
  default <- do.call(diagonal_arrangement, args)
  fb <- first$fieldBook
  checks_first <- sum(fb$CHECKS != 0)
  expect_false(checks_first == sum(default$fieldBook$CHECKS != 0))
  expect_setequal(fb$ENTRY[fb$CHECKS == 0 & fb$ENTRY > 0], 5:274)
  expect_error(do.call(diagonal_arrangement, c(args, checksPercent = 0.001)),
               "must be one of the percentages available")
})

test_that("DBUDC with sameEntries repeats the same entries in every block", {
  for (split in c("row", "column")) {
    args <- if (split == "row") {
      list(nrows = 30, ncols = 26, lines = 720, checks = 5, blocks = rep(180, 4))
    } else {
      list(nrows = 20, ncols = 25, lines = 400, checks = 4, blocks = rep(40, 10))
    }
    diag <- do.call(diagonal_arrangement, c(args, kindExpt = "DBUDC", splitBy = split,
                                            sameEntries = TRUE, plotNumber = 1, seed = 1))
    fb <- diag$fieldBook
    expect_equal(nrow(fb), args$nrows * args$ncols, info = split)
    tests <- fb[fb$CHECKS == 0 & fb$ENTRY > 0, ]
    per_block <- args$blocks[1]
    for (expt in unique(tests$EXPT)) {
      expect_setequal(tests$ENTRY[tests$EXPT == expt],
                      (args$checks + 1):(args$checks + per_block))
      expect_equal(anyDuplicated(tests$ENTRY[tests$EXPT == expt]), 0)
    }
  }
})

test_that("DBUDC with sameEntries accepts uploaded repeated entries", {
  blocks <- rep(40, 10)
  data <- data.frame(
    ENTRY = c(1:4, rep(5:44, times = 10)),
    NAME = c(paste0("CK", 1:4), rep(paste0("L", 5:44), times = 10))
  )
  diag <- diagonal_arrangement(nrows = 20, ncols = 25, checks = 4, kindExpt = "DBUDC",
                               splitBy = "column", blocks = blocks, sameEntries = TRUE,
                               plotNumber = 1, seed = 1, data = data)
  tests <- diag$fieldBook[diag$fieldBook$CHECKS == 0, ]
  for (expt in unique(tests$EXPT)) {
    expect_setequal(tests$ENTRY[tests$EXPT == expt], 5:44)
  }
  expect_equal(unique(tests$TREATMENT[tests$ENTRY == 5]), "L5")
})

test_that("sameEntries needs a DBUDC design with equal blocks", {
  expect_error(diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                    sameEntries = TRUE, seed = 1),
               "sameEntries")
  expect_error(diagonal_arrangement(nrows = 30, ncols = 26, lines = 720, checks = 5,
                                    kindExpt = "DBUDC", blocks = c(150, 155, 95, 200, 120),
                                    sameEntries = TRUE, seed = 1),
               "same size")
})

test_that("sparse_allocation() passes checksPercent to every location", {
  msg <- tryCatch(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                    seed = 1234, checksPercent = 0.001),
                  error = function(e) conditionMessage(e))
  expect_match(msg, "must be one of the percentages available")
  options <- as.numeric(strsplit(sub(".*available for this field: ", "", sub("\\.$", "", msg)), ", ")[[1]])
  default <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234)
  last <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234,
                            checksPercent = options[length(options)])
  expect_identical(last$fieldBook, default$fieldBook)
})
