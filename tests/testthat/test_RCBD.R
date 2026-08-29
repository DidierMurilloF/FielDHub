library(FielDHub)

test_that("RCBD() gives an informative error for a single character treatment", {
  # Regression test (C4): a single character treatment (e.g. t = "Wheat")
  # matched no branch in the t-handling (the third `else if` duplicated the
  # second), so nt was never set and RCBD() failed with "object 'nt' not found".
  # The third branch now tests length(t) == 1 and raises the intended message.
  expect_error(RCBD(t = "Wheat", reps = 3), "more than one treatment")
})

test_that("RCBD() without checks reproduces the FielDHub 1.5.0 field book", {
  # Backward-compatibility pin. The repeated-checks feature must not alter
  # the RNG path when `checks` is NULL, so this exact output must survive.
  d <- RCBD(t = 4, reps = 3, seed = 89076)

  expected <- data.frame(
    ID        = 1:12,
    LOCATION  = factor(rep("loc1", 12), levels = "loc1"),
    PLOT      = c(101, 102, 103, 104, 201, 202, 203, 204, 301, 302, 303, 304),
    REP       = rep(1:3, each = 4),
    TREATMENT = c("T3", "T4", "T2", "T1",
                  "T4", "T3", "T1", "T2",
                  "T1", "T2", "T4", "T3"),
    stringsAsFactors = FALSE
  )

  fb <- d$fieldBook
  rownames(fb) <- NULL
  expect_equal(fb, expected)
})

test_that("RCBD() without checks keeps its infoDesign shape", {
  d <- RCBD(t = 4, reps = 3, seed = 89076)

  expect_named(d$infoDesign, c(
    "blocks", "number.of.treatments", "treatments", "locations",
    "plotNumber", "locationNames", "seed", "id_design"
  ))
  # print.FielDHub drops the last element, so id_design must stay last.
  expect_identical(names(d$infoDesign)[length(d$infoDesign)], "id_design")
  expect_equal(d$infoDesign$id_design, 2)
  expect_equal(d$infoDesign$number.of.treatments, 4)
  expect_equal(d$infoDesign$treatments, c("T1", "T2", "T3", "T4"))
})
