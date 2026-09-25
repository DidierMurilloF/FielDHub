library(FielDHub)

# Each ENTRY must carry the label given at the same position in `t`.
expect_labels_kept <- function(fb, labels) {
  expect_setequal(unique(fb$TREATMENT), labels)
  expect_equal(as.character(fb$TREATMENT), labels[fb$ENTRY])
}

test_that("incomplete-block designs keep character treatment labels", {
  # Regression test: a character `t` was accepted but its labels were
  # replaced by "G-1", ..., "G-n" in the field book.
  labels <- paste0("V", 1:12)
  expect_labels_kept(
    incomplete_blocks(t = labels, k = 4, r = 2, seed = 1)$fieldBook, labels
  )
  expect_labels_kept(
    alpha_lattice(t = labels, k = 4, r = 2, seed = 1)$fieldBook, labels
  )
  expect_labels_kept(
    rectangular_lattice(t = labels, k = 3, r = 2, seed = 1)$fieldBook, labels
  )
  expect_labels_kept(
    row_column(t = labels, nrows = 3, r = 2, seed = 1)$fieldBook, labels
  )
  square_labels <- paste0("V", 1:16)
  expect_labels_kept(
    square_lattice(t = square_labels, k = 4, r = 2, seed = 1)$fieldBook,
    square_labels
  )
})

test_that("incomplete-block designs keep G- labels for numeric t", {
  fb <- incomplete_blocks(t = 12, k = 4, r = 2, seed = 1)$fieldBook
  expect_equal(fb$TREATMENT, paste0("G-", fb$ENTRY))
})

test_that("designs reject duplicated treatment labels", {
  labels <- c("A", "A", paste0("V", 1:10))
  expect_error(incomplete_blocks(t = labels, k = 4, r = 2, seed = 1),
               "unique treatment labels; duplicated: A")
  expect_error(row_column(t = labels, nrows = 3, r = 2, seed = 1),
               "unique treatment labels; duplicated: A")
  expect_error(RCBD(t = c("A", "A", "B"), reps = 2, seed = 1),
               "unique entry labels; duplicated: A")
  expect_error(CRD(t = c("A", "A", "B"), reps = 2, seed = 1),
               "unique treatment labels; duplicated: A")
})
