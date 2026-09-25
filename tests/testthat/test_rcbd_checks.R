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
  # 5 check reps of 9 block plots trips the >50% density warning (Finding 9);
  # this test asserts only label resolution, so suppress it rather than
  # silently letting it leak into the test run.
  e <- suppressWarnings(
    rcbd_resolve_entries(t = 4, checks = c("CK1", "CK2"), rep_checks = c(3, 2))
  )

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
  # 6 check reps of 11 block plots trips the >50% density warning (Finding 9);
  # this test asserts only recycling, so suppress it rather than silently
  # letting it leak into the test run.
  e <- suppressWarnings(
    rcbd_resolve_entries(t = 5, checks = c("CK1", "CK2", "CK3"), rep_checks = 2)
  )
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

test_that("the high-density warning mentions the randomization consequence", {
  expect_warning(
    rcbd_resolve_entries(t = 2, checks = "CK1", rep_checks = 4),
    "nearly or fully determined"
  )
})

test_that("the high-density warning does not mention stratification when spread_checks = FALSE", {
  # Finding 6: the warning used to always describe the stratified-placement
  # constraint, even when spread_checks = FALSE means no stratification ever
  # happens. It should still warn (density is still high), but the wording
  # must not claim a constraint that is not in effect.
  expect_warning(
    rcbd_resolve_entries(t = 2, checks = "CK1", rep_checks = 4, spread_checks = FALSE),
    "more than half"
  )
  w <- tryCatch({
    rcbd_resolve_entries(t = 2, checks = "CK1", rep_checks = 4, spread_checks = FALSE)
    NULL
  }, warning = function(w) conditionMessage(w))
  expect_false(grepl("stratified", w, fixed = TRUE))
  expect_false(grepl("nearly or fully determined", w, fixed = TRUE))
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

test_that("rcbd_strata_bounds partitions positions contiguously", {
  expect_equal(rcbd_strata_bounds(10, 2), list(1:5, 6:10))
  expect_equal(rcbd_strata_bounds(10, 3), list(1:4, 5:7, 8:10))
  expect_equal(rcbd_strata_bounds(6, 1), list(1:6))
  # every position is used exactly once
  b <- rcbd_strata_bounds(17, 4)
  expect_equal(sort(unlist(b)), 1:17)
})

test_that("a randomized block has exact entry counts", {
  set.seed(11)
  e <- rcbd_resolve_entries(t = 6, checks = c("CK1", "CK2"), rep_checks = c(2, 2))
  blk <- rcbd_randomize_block(e, spread_checks = TRUE)

  expect_length(blk, 10)
  expect_equal(sum(blk == 1L), 2)  # CK1 twice
  expect_equal(sum(blk == 2L), 2)  # CK2 twice
  # every test entry exactly once
  expect_true(all(table(blk[blk > 2L]) == 1))
  expect_setequal(unique(blk), e$ENTRY)
})

test_that("spread_checks places copies of one check in distinct strata", {
  e <- rcbd_resolve_entries(t = 10, checks = "CK1", rep_checks = 3)
  strata <- rcbd_strata_bounds(13, 3)

  for (s in 1:50) {
    set.seed(s)
    blk <- rcbd_randomize_block(e, spread_checks = TRUE)
    pos <- which(blk == 1L)
    expect_length(pos, 3)
    hit <- vapply(pos, function(p) which(vapply(strata, function(z) p %in% z, logical(1))), integer(1))
    expect_equal(sort(hit), 1:3)  # one copy per stratum
  }
})

test_that("spread_checks = FALSE still yields exact counts", {
  set.seed(5)
  e <- rcbd_resolve_entries(t = 6, checks = c("CK1", "CK2"), rep_checks = c(2, 3))
  blk <- rcbd_randomize_block(e, spread_checks = FALSE)

  expect_length(blk, 11)
  expect_equal(sum(blk == 1L), 2)
  expect_equal(sum(blk == 2L), 3)
})

test_that("the retry loop recovers from a stratum collision", {
  # reps = c(6, 2) in a block of 9 is the smallest configuration where a single
  # placement attempt can exhaust a stratum: the r=6 check is placed first and
  # forces positions 7, 8, 9, after which the r=2 check finds its second
  # stratum [6:9] full roughly half the time. Measured single-attempt success
  # rate is ~0.49, so with 100 retries the fallback is effectively unreachable
  # (~1e-29) and every one of these must come back clean and correctly counted.
  e <- suppressWarnings(
    rcbd_resolve_entries(t = 1, checks = c("CK1", "CK2"), rep_checks = c(6, 2))
  )
  for (s in 1:40) {
    set.seed(s)
    blk <- expect_no_warning(rcbd_randomize_block(e, spread_checks = TRUE))
    expect_length(blk, 9)
    expect_equal(sum(blk == 1L), 6)
    expect_equal(sum(blk == 2L), 2)
    expect_equal(sum(blk == 3L), 1)
  }
})

test_that("exhausting the retries falls back with a warning", {
  # max_tries = 0 makes seq_len(0) empty, so the loop is skipped entirely and
  # the fallback path is reached deterministically. This is the only practical
  # way to exercise the warning, per the collision analysis above.
  e <- suppressWarnings(
    rcbd_resolve_entries(t = 1, checks = c("CK1", "CK2"), rep_checks = c(6, 2))
  )
  set.seed(3)
  expect_warning(
    blk <- rcbd_randomize_block(e, spread_checks = TRUE, max_tries = 0),
    "falling back"
  )
  expect_length(blk, 9)
  expect_equal(sum(blk == 1L), 6)
  expect_equal(sum(blk == 2L), 2)
})

test_that("randomization is reproducible under a seed", {
  e <- rcbd_resolve_entries(t = 8, checks = "CK1", rep_checks = 2)
  set.seed(42); a <- rcbd_randomize_block(e)
  set.seed(42); b <- rcbd_randomize_block(e)
  set.seed(43); c <- rcbd_randomize_block(e)
  expect_identical(a, b)
  expect_false(identical(a, c))
})

test_that("RCBD() with checks produces the wide field book", {
  d <- RCBD(t = 6, reps = 3, checks = c("CK1", "CK2"),
            rep_checks = c(2, 2), seed = 77)

  expect_named(d$fieldBook,
               c("ID", "LOCATION", "PLOT", "REP", "ENTRY", "CHECKS", "TREATMENT"))
  expect_equal(nrow(d$fieldBook), 10 * 3)          # n_units * reps
  expect_equal(d$infoDesign$plots_per_block, 10)
  expect_equal(d$infoDesign$checks, 2)
  expect_equal(d$infoDesign$check_names, c("CK1", "CK2"))
  expect_equal(d$infoDesign$rep_checks, c(2, 2))
  expect_true(d$infoDesign$spread_checks)
  expect_equal(d$infoDesign$number.of.treatments, 6)
  expect_identical(names(d$infoDesign)[length(d$infoDesign)], "id_design")
  expect_equal(d$infoDesign$id_design, 2)
})

test_that("RCBD() repeats each check the requested number of times per block", {
  d <- RCBD(t = 6, reps = 3, checks = c("CK1", "CK2"),
            rep_checks = c(2, 3), seed = 78)
  fb <- d$fieldBook

  for (r in 1:3) {
    blk <- fb[fb$REP == r, ]
    expect_equal(sum(blk$TREATMENT == "CK1"), 2)
    expect_equal(sum(blk$TREATMENT == "CK2"), 3)
    tests <- blk$TREATMENT[blk$CHECKS == 0]
    expect_equal(sort(tests), sort(paste0("T", 1:6)))
  }
})

test_that("ENTRY and CHECKS are consistent everywhere", {
  d <- RCBD(t = 5, reps = 2, l = 2, checks = "CK1", rep_checks = 2,
            plotNumber = c(1001, 2001),
            locationNames = c("FARGO", "MINOT"), seed = 79)
  fb <- d$fieldBook

  # one ENTRY id per label, everywhere
  map <- unique(fb[, c("ENTRY", "TREATMENT", "CHECKS")])
  expect_equal(nrow(map), 6)
  expect_equal(map$CHECKS[map$TREATMENT == "CK1"], 1L)
  expect_true(all(map$CHECKS[map$TREATMENT != "CK1"] == 0L))
  expect_equal(nrow(fb), 7 * 2 * 2)   # n_units * reps * locations
})

test_that("RCBD() accepts checks as a count over uploaded data", {
  pool <- data.frame(TREATMENT = c("CK1", "CK2", paste0("G-", 1:8)))
  d <- RCBD(reps = 2, checks = 2, rep_checks = 2, data = pool, seed = 80)

  expect_equal(d$infoDesign$plots_per_block, 12)
  expect_equal(d$infoDesign$check_names, c("CK1", "CK2"))
  expect_equal(d$infoDesign$number.of.treatments, 8)
})

test_that("plot numbers step by block size when checks are present", {
  d <- RCBD(t = 6, reps = 3, checks = "CK1", rep_checks = 2,
            plotNumber = 101, continuous = TRUE, seed = 81)
  # block size 8, continuous numbering over 3 blocks
  expect_equal(sort(d$fieldBook$PLOT), 101:124)
})

test_that("RCBD() keeps data in its 9th positional slot", {
  pool <- data.frame(TREATMENT = paste0("G-", 1:4))
  # 9 positional arguments, the 9th being `data` - the pre-1.5.0 call shape.
  d <- RCBD(NULL, 3, 1, 101, FALSE, "serpentine", 89076, "FARGO", pool)
  expect_equal(sort(unique(d$fieldBook$TREATMENT)), paste0("G-", 1:4))
  expect_equal(nrow(d$fieldBook), 12)
})

test_that("rcbd_resolve_entries() rejects an implausibly large block", {
  expect_error(
    rcbd_resolve_entries(t = 10, checks = c("A", "B"), rep_checks = c(1e9, 1e9)),
    "would build a block of 2,000,000,010 plots",
    fixed = TRUE
  )
  # The size check must win over the (lower-priority) check-density warning:
  # this input would also trip the density warning, but the error must fire
  # first and no warning should reach the caller.
  expect_no_warning(
    tryCatch(
      rcbd_resolve_entries(t = 10, checks = c("A", "B"), rep_checks = c(1e9, 1e9)),
      error = function(e) NULL
    )
  )
  # A block right at a plausible size is unaffected.
  ok <- rcbd_resolve_entries(t = 10, checks = c("A", "B"), rep_checks = c(2, 2))
  expect_s3_class(ok, "data.frame")
})
