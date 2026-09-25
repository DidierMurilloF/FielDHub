library(FielDHub)

# Each check appears exactly once in every block, and every line exactly once.
expect_valid_arcbd <- function(design, lines, checks) {
  fb <- design$fieldBook
  is_check <- fb$ENTRY %in% seq_len(checks)
  per_block <- table(factor(fb$BLOCK[is_check]),
                     factor(fb$ENTRY[is_check], levels = seq_len(checks)))
  expect_true(all(per_block == 1))
  test_entries <- fb$ENTRY[fb$ENTRY > checks]
  expect_setequal(test_entries, (checks + 1):(checks + lines))
  expect_equal(anyDuplicated(test_entries), 0)
}

test_that("RCBD_augmented() without randomized entries keeps one set of checks per block", {
  # Regression test: with random = FALSE and filler plots, the first row of
  # the field was overwritten with the fillers and a new set of all checks,
  # ignoring the blocks. A block spanning several rows kept its other checks
  # and got a second set (7 checks instead of 4, and lines lost), and blocks
  # sharing the first row lost most of theirs.
  for (planter in c("serpentine", "cartesian")) {
    multi_row <- RCBD_augmented(lines = 122, checks = 4, b = 7, nrows = 14, ncols = 11,
                                random = FALSE, planter = planter, seed = 7)
    expect_valid_arcbd(multi_row, lines = 122, checks = 4)
    side_by_side <- RCBD_augmented(lines = 122, checks = 4, b = 6, nrows = 2, ncols = 75,
                                   random = FALSE, planter = planter, seed = 7)
    expect_valid_arcbd(side_by_side, lines = 122, checks = 4)
    expect_equal(sum(side_by_side$fieldBook$ENTRY == 0), 4)
  }
})

test_that("RCBD_augmented() without randomized entries keeps the lines in field order", {
  arcbd <- RCBD_augmented(lines = 122, checks = 4, b = 7, nrows = 14, ncols = 11,
                          random = FALSE, seed = 7)
  expect_valid_arcbd(arcbd, lines = 122, checks = 4)
  layout <- as.matrix(arcbd$layoutRandom)
  bottom_row <- layout[nrow(layout), ]
  bottom_lines <- as.numeric(bottom_row[as.numeric(bottom_row) > 4])
  expect_equal(bottom_lines, sort(bottom_lines))
})
