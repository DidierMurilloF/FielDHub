# Fixed-replication 6 x 4 enumeration (research M1 specification, section 10.3):
# v = 6 pseudo treatments, each replicated twice, k = 3, s = 4. The fast suite
# checks the counts and a stratified connectivity sample; the exhaustive suite
# (FIELDHUB_ARC_EXHAUSTIVE=true) is the independent connectivity gate over all
# 18,360 representatives.

# Contraction objectives of every representative in `reps` for one mode
arc_enum_objectives <- function(reps, mode) {
  lapply(reps, arc_evaluate_objectives, v = 6L, mode = mode)
}

# Full-layout evaluator or oracle results from the expanded layouts
arc_enum_layouts <- function(reps, mode, evaluator) {
  lapply(reps, function(m) evaluator(arc_expand(m, 6L, mode)$codes))
}

arc_enum_connected <- function(results) {
  vapply(results, function(r) isTRUE(r$connected), logical(1))
}

# Counts and value classes of a list of objective results. Classes are the
# distinct (E_aug, V_tt) pairs among connected designs, rounded to 10 decimals.
arc_enum_summary <- function(results) {
  connected <- arc_enum_connected(results)
  values <- do.call(rbind, lapply(results[connected], function(r) {
    round(c(r$E_aug, r$V_tt), 10)
  }))
  classes <- unique(values)
  opposite <- 0L
  pairs <- 0L
  for (i in seq_len(nrow(classes) - 1L)) {
    for (j in (i + 1L):nrow(classes)) {
      pairs <- pairs + 1L
      # higher E_aug is better, lower V_tt is better
      if (sign(classes[i, 1] - classes[j, 1]) *
          sign(classes[j, 2] - classes[i, 2]) < 0) {
        opposite <- opposite + 1L
      }
    }
  }
  list(n = length(results), disconnected = sum(!connected),
       classes = nrow(classes), best = max(classes[, 1]),
       opposite = opposite, pairs = pairs)
}

# Representatives are generated once per file
enum_all <- arc_enumerate_6x4()
enum_restricted <- enum_all[vapply(enum_all, arc_is_row_binary, logical(1))]

test_that("the enumeration has the expected size and structure", {
  expect_length(enum_all, 18360L)
  expect_length(enum_restricted, 1920L)
  expect_true(all(vapply(enum_all, function(m) {
    identical(dim(m), c(3L, 4L)) && all(tabulate(m, 6L) == 2L)
  }, logical(1))))
})

test_that("column-binary enumeration reproduces the research counts", {
  summary <- arc_enum_summary(arc_enum_objectives(enum_all, "column_binary"))
  expect_equal(summary$n, 18360L)
  expect_equal(summary$disconnected, 5880L)
  expect_equal(summary$classes, 25L)
  expect_equal(summary$best, 0.450643777, tolerance = 1e-9)
  expect_equal(summary$opposite, 10L)
  expect_equal(summary$pairs, 300L)
})

test_that("restricted enumeration reproduces the research counts", {
  summary <- arc_enum_summary(arc_enum_objectives(enum_restricted, "restricted"))
  expect_equal(summary$n, 1920L)
  expect_equal(summary$disconnected, 240L)
  expect_equal(summary$classes, 6L)
  expect_equal(summary$best, 0.450643777, tolerance = 1e-9)
  expect_equal(summary$opposite, 0L)
  expect_equal(summary$pairs, 15L)
})

test_that("contraction and full-layout connectivity agree on a stratified sample", {
  # every restricted disconnected case, plus every 36th column-binary design
  restricted_connected <- arc_enum_connected(
    arc_enum_objectives(enum_restricted, "restricted"))
  disconnected <- enum_restricted[!restricted_connected]
  sampled <- enum_all[seq(36L, length(enum_all), by = 36L)]
  expect_length(disconnected, 240L)
  expect_length(sampled, 510L)

  check <- function(reps, mode) {
    from_contraction <- arc_enum_connected(arc_enum_objectives(reps, mode))
    from_layout <- arc_enum_connected(
      arc_enum_layouts(reps, mode, arc_evaluate_layout))
    expect_identical(from_contraction, from_layout)
  }
  check(disconnected, "restricted")
  check(sampled, "column_binary")
})

test_that("exhaustive: contraction, layout and oracle agree on every representative", {
  skip_if_not(identical(Sys.getenv("FIELDHUB_ARC_EXHAUSTIVE"), "true"),
              "exhaustive suite")

  run_set <- function(reps, mode, expected_disconnected) {
    objectives <- arc_enum_objectives(reps, mode)
    layouts <- arc_enum_layouts(reps, mode, arc_evaluate_layout)
    oracles <- arc_enum_layouts(reps, mode, arc_oracle)
    verdict <- arc_enum_connected(objectives)

    # identical connectivity verdicts and recomputed disconnected counts
    expect_identical(arc_enum_connected(layouts), verdict)
    expect_identical(arc_enum_connected(oracles), verdict)
    for (results in list(objectives, layouts, oracles)) {
      expect_equal(sum(!arc_enum_connected(results)), expected_disconnected)
    }

    # E_aug agreement on connected designs, and the best value from all three
    e_of <- function(results) {
      vapply(results[verdict], function(r) r$E_aug, numeric(1))
    }
    expect_equal(e_of(layouts), e_of(objectives), tolerance = 1e-10)
    expect_equal(e_of(oracles), e_of(objectives), tolerance = 1e-10)
    for (results in list(objectives, layouts, oracles)) {
      expect_equal(max(e_of(results)), 0.450643777, tolerance = 1e-9)
    }
  }
  run_set(enum_all, "column_binary", 5880L)
  run_set(enum_restricted, "restricted", 240L)
})
