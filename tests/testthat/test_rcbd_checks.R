library(FielDHub)

test_that("integer checks take the first rows of data", {
  pool <- data.frame(TREATMENT = c("CK1", "CK2", paste0("G-", 1:4)))
  e <- rcbd_resolve_entries(checks = 2, rep_checks = 2, data = pool)

  expect_equal(e$TREATMENT, c("CK1", "CK2", paste0("G-", 1:4)))
  expect_equal(e$CHECKS, c(1L, 2L, 0L, 0L, 0L, 0L))
  expect_equal(e$ENTRY, 1:6)
  expect_equal(e$reps_per_block, c(2, 2, 1, 1, 1, 1))
})

test_that("character checks not in the pool are appended as new checks", {
  e <- rcbd_resolve_entries(t = 4, checks = c("CK1", "CK2"), rep_checks = c(3, 2))

  # t = 4 means 4 TEST entries; the checks are additional.
  expect_equal(e$TREATMENT, c("CK1", "CK2", paste0("T", 1:4)))
  expect_equal(e$CHECKS, c(1L, 2L, 0L, 0L, 0L, 0L))
  expect_equal(e$reps_per_block, c(3, 2, 1, 1, 1, 1))
  expect_equal(sum(e$reps_per_block), 9)  # block size k
})

test_that("character checks found in the pool are moved out of the test entries", {
  pool <- data.frame(TREATMENT = c("G-1", "CK1", "G-2", "G-3"))
  e <- rcbd_resolve_entries(checks = "CK1", rep_checks = 3, data = pool)

  expect_equal(e$TREATMENT, c("CK1", "G-1", "G-2", "G-3"))
  expect_equal(e$CHECKS, c(1L, 0L, 0L, 0L))
  expect_equal(e$reps_per_block, c(3, 1, 1, 1))
})

test_that("character t is usable as the entry pool", {
  e <- rcbd_resolve_entries(t = c("CK1", "A", "B", "C"), checks = "CK1", rep_checks = 2)
  expect_equal(e$TREATMENT, c("CK1", "A", "B", "C"))
  expect_equal(e$CHECKS, c(1L, 0L, 0L, 0L))
})

test_that("rep_checks recycles from a scalar", {
  e <- rcbd_resolve_entries(t = 5, checks = c("CK1", "CK2", "CK3"), rep_checks = 2)
  expect_equal(e$reps_per_block[1:3], c(2, 2, 2))
})

test_that("rep_checks defaults to 1 with a message", {
  expect_message(
    e <- rcbd_resolve_entries(t = 5, checks = "CK1"),
    "ordinary RCBD"
  )
  expect_equal(e$reps_per_block[1], 1)
})

test_that("entry resolution rejects bad input", {
  pool <- data.frame(TREATMENT = c("CK1", "CK2", "G-1"))

  expect_error(rcbd_resolve_entries(t = 5, checks = 2), "requires 'data'")
  expect_error(rcbd_resolve_entries(checks = 3, rep_checks = 2, data = pool),
               "at least one test entry")
  expect_error(rcbd_resolve_entries(t = 5, checks = c("CK1", "CK1"), rep_checks = 2),
               "unique")
  expect_error(rcbd_resolve_entries(t = 5, checks = "CK1", rep_checks = 0),
               "greater than or equal to 1")
  expect_error(rcbd_resolve_entries(t = 5, checks = "CK1", rep_checks = 2.5),
               "integers")
  expect_error(rcbd_resolve_entries(t = 5, checks = c("A", "B"), rep_checks = c(2, 2, 2)),
               "length 1 or 2")
  expect_error(rcbd_resolve_entries(t = 5, checks = list(1), rep_checks = 2),
               "positive integer or a character vector")
})

test_that("a check label colliding with a generated test label is rejected", {
  # t = 5 auto-generates T1..T5, so a check called "T2" would duplicate a label.
  expect_error(
    rcbd_resolve_entries(t = 5, checks = "T2", rep_checks = 2),
    "collide with the generated"
  )
  # No collision when the entries come from a pool instead.
  pool <- data.frame(TREATMENT = c("T2", "G-1", "G-2"))
  e <- rcbd_resolve_entries(checks = "T2", rep_checks = 2, data = pool)
  expect_equal(e$TREATMENT, c("T2", "G-1", "G-2"))
})

test_that("duplicate labels in the entry pool are rejected", {
  pool <- data.frame(TREATMENT = c("CK1", "G-1", "G-1"))
  expect_error(
    rcbd_resolve_entries(checks = 1, rep_checks = 2, data = pool),
    "unique entry labels"
  )
})

test_that("a check-heavy block warns", {
  expect_warning(
    rcbd_resolve_entries(t = 2, checks = "CK1", rep_checks = 4),
    "more than half"
  )
})

test_that("check labels not in the pool are rejected with clear error", {
  pool <- data.frame(TREATMENT = c("CK1", "CK2", "G-1"))
  expect_error(
    rcbd_resolve_entries(checks = "XYZ", rep_checks = 2, data = pool),
    "not found in the supplied entries"
  )
})

test_that("check labels not in pool suggest case-insensitive near-misses", {
  pool <- data.frame(TREATMENT = c("t2", "B", "C", "D"))
  expect_error(
    rcbd_resolve_entries(checks = "T2", rep_checks = 2, data = pool),
    "Did you mean"
  )
})

test_that("checks = NA is rejected", {
  expect_error(
    rcbd_resolve_entries(t = 5, checks = NA, rep_checks = 2),
    "positive integer or a character vector"
  )
})

test_that("rep_checks = Inf is rejected", {
  expect_error(
    rcbd_resolve_entries(t = 5, checks = "CK1", rep_checks = Inf),
    "finite"
  )
})

test_that("rep_checks = NA is rejected", {
  expect_error(
    rcbd_resolve_entries(t = 5, checks = "CK1", rep_checks = NA),
    "numeric"
  )
})

test_that("negative t is rejected", {
  expect_error(
    rcbd_resolve_entries(t = -3, checks = "CK1", rep_checks = 2),
    "non-negative"
  )
})
