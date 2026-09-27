test_that("independent count checks detect missing, repeated, and unknown units", {
  book <- expand.grid(ENTRY = c("A", "B"), REP = 1:2, LOCATION = c("West", "East"))
  levels <- list(ENTRY = c("A", "B"), REP = 1:2, LOCATION = c("West", "East"))
  expect_true(has_invariant_counts(book, levels))
  expect_false(has_invariant_counts(book[-1, ], levels))
  expect_false(has_invariant_counts(rbind(book, book[1, ]), levels))
  expect_false(has_invariant_counts(subset(book, LOCATION == "West"), levels))
  bad <- book
  bad$ENTRY <- as.character(bad$ENTRY)
  bad$ENTRY[1] <- "unknown"
  expect_false(has_invariant_counts(bad, levels))
  bad$ENTRY[1] <- NA
  expect_false(has_invariant_counts(bad, levels))
  uneven <- data.frame(ENTRY = c("B", "A", "B"), LOCATION = "West")
  expect_true(has_invariant_counts(uneven, list(ENTRY = c("A", "B"), LOCATION = "West"), c(1, 2)))
  expect_false(has_invariant_counts(uneven, list(ENTRY = c("A", "B"), LOCATION = "West"), c(2, 1)))
})

test_that("coordinate checks respect the unit hierarchy and reject collisions", {
  book <- data.frame(LOCATION = c("West", "East"), ROW = 1L, COLUMN = 1L, PLOT = 101)
  expect_true(has_unique_units(book, c("LOCATION", "ROW", "COLUMN")))
  expect_false(has_unique_units(book, c("ROW", "COLUMN")))
  expect_false(has_unique_units(rbind(book, book[1, ]), c("LOCATION", "ROW", "COLUMN")))
  book$ROW[1] <- NA_integer_
  expect_false(has_unique_units(book, c("LOCATION", "ROW", "COLUMN")))
})

test_that("multiset checks preserve labels and multiplicities regardless of order", {
  book <- data.frame(ENTRY = c(1, 1, 2), NAME = c("A|B", "A|B", "C"))
  expect_true(has_same_units(book, book[3:1, ], c("ENTRY", "NAME")))
  expect_false(has_same_units(book, book[-1, ], c("ENTRY", "NAME")))
  bad <- book
  bad$NAME[1] <- "changed"
  expect_false(has_same_units(book, bad, c("ENTRY", "NAME")))
})

test_that("the independent block-efficiency oracle matches analytic designs", {
  complete <- expand.grid(ENTRY = 1:4, REP = 1:3)
  complete$IBLOCK <- 1L
  expect_equal(block_efficiency_oracle(complete), 1, tolerance = 1e-12)
  # All six pairs of four treatments form a BIBD: r=3, k=2, lambda=1.
  # C has three nonzero eigenvalues 2, hence relative A-efficiency 2/3.
  pairs <- utils::combn(1:4, 2)
  balanced <- data.frame(ENTRY = as.vector(pairs), REP = 1L, IBLOCK = rep(1:6, each = 2))
  expect_equal(block_efficiency_oracle(balanced), 2 / 3, tolerance = 1e-12)
  disconnected <- data.frame(ENTRY = rep(1:4, each = 2), REP = 1L, IBLOCK = rep(1:4, each = 2))
  expect_identical(block_efficiency_oracle(disconnected), 0)
})
