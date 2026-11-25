# CRD(): completely randomized designs.

# ---- Field book -------------------------------------------------------------

test_that("CRD() builds field books for edge sizes and character treatments", {
  # single_unit: regression, sample(plotNumber:(plotNumber + N - 1)) became
  #   sample(101) when N == 1 and drew 101 plots.
  # character_treatments: regression, labels were recycled against REP so
  #   each label sat in one REP only; every label belongs in every REP once.
  cases <- list(
    single_unit = list(args = list(t = 1, reps = 1, seed = 1),
                       per_rep = matrix(1L, 1, 1)),
    character_treatments = list(args = list(t = c("A", "B", "C"), reps = 3, seed = 1),
                                per_rep = matrix(1L, 3, 3))
  )
  for (nm in names(cases)) {
    fb <- do.call(CRD, cases[[nm]]$args)$fieldBook
    counts <- unclass(table(fb$TREATMENT, fb$REP))
    expect_equal(counts, cases[[nm]]$per_rep, ignore_attr = TRUE, info = nm)
  }
})

# ---- Location argument ------------------------------------------------------

test_that("CRD accepts locationNames and the legacy locationName with a deprecation warning", {
  canonical <- CRD(t = 4, reps = 2, locationNames = "FARGO", seed = 19)
  expect_identical(unique(as.character(canonical$fieldBook$LOCATION)), "FARGO")
  expect_warning(legacy <- CRD(t = 4, reps = 2, locationName = "FARGO", seed = 19),
                 class = "fieldhub_deprecated_warning")
  expect_identical(legacy, canonical)
  expect_warning(positional <- CRD(4, 2, 101, "FARGO", 19),
                 class = "fieldhub_deprecated_warning")
  expect_identical(positional, canonical)
})

test_that("CRD keeps its default location and does not warn on canonical calls", {
  expect_silent(default <- CRD(t = 4, reps = 2, seed = 19))
  expect_silent(explicit <- CRD(t = 4, reps = 2, locationNames = NULL, seed = 19))
  expect_identical(default, explicit)
})

# ---- Input errors -----------------------------------------------------------

test_that("CRD rejects invalid counts, labels, tables and locations", {
  bad_counts <- list(NA_real_, NaN, Inf, -Inf, 0, -1, 1.5, numeric(), c(2, 3), "2", TRUE)
  cases <- list()
  for (i in seq_along(bad_counts)) {
    v <- bad_counts[[i]]
    tag <- paste(deparse(v), collapse = "")
    cases[[paste("reps", tag)]] <- list(t = 3, reps = v)
    cases[[paste("reps with labels", tag)]] <- list(t = letters[1:3], reps = v)
    cases[[paste("plotNumber", tag)]] <- list(t = 3, reps = 2, plotNumber = v)
  }
  for (v in list(NA_real_, Inf, numeric(), c(2, 3), TRUE, list(2), matrix(2))) {
    cases[[paste("t", paste(deparse(v), collapse = ""))]] <- list(t = v, reps = 2)
  }
  cases <- c(cases, list(
    `too many units` = list(t = .Machine$integer.max, reps = 2),
    `single character treatment` = list(t = "Wheat", reps = 3),
    `missing label` = list(t = c("A", NA), reps = 2),
    `empty label` = list(t = c("A", ""), reps = 2),
    `blank label` = list(t = c("A", "  "), reps = 2),
    `both location spellings` = list(t = 4, reps = 2, locationName = "FARGO",
                                     locationNames = "CASSLETON"),
    `both location spellings, legacy NULL` = list(t = 4, reps = 2, locationName = NULL,
                                                  locationNames = "FARGO")
  ))
  for (location in list(c("A", "B"), character(), NA_character_, "", "  ", list("A"))) {
    cases[[paste("location", paste(deparse(location), collapse = ""))]] <-
      list(t = 3, reps = 2, locationNames = location)
  }
  tables <- list(
    `fractional REP` = data.frame(TREATMENT = c("A", "B"), REP = c(1.5, 2.5)),
    `zero REP` = data.frame(TREATMENT = c("A", "B"), REP = c(0, 2)),
    `negative REP` = data.frame(TREATMENT = c("A", "B"), REP = c(-1, 2)),
    `infinite REP` = data.frame(TREATMENT = c("A", "B"), REP = c(Inf, 2)),
    `logical REP` = data.frame(TREATMENT = c("A", "B"), REP = c(TRUE, FALSE)),
    `duplicated labels` = data.frame(TREATMENT = c("A", "A"), REP = c(2, 3)),
    `empty table` = data.frame(TREATMENT = character(), REP = integer()),
    `missing REP` = data.frame(TREATMENT = c("A", "B"), REP = c(NA, NA)),
    `too many units in table` = data.frame(TREATMENT = c("A", "B"),
                                           REP = rep(.Machine$integer.max, 2))
  )
  for (nm in names(tables)) cases[[paste("data", nm)]] <- list(data = tables[[nm]])

  for (nm in names(cases)) {
    expect_error(do.call(CRD, c(cases[[nm]], seed = 1)), class = "fieldhub_input_error",
                 info = nm)
  }
})

test_that("CRD input errors leave the caller's random stream unchanged", {
  set.seed(102)
  before <- .Random.seed
  expect_error(CRD(t = letters[1:3], reps = 1.5, seed = 2), class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})
