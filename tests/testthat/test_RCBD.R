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

test_that("RCBD() without checks consumes exactly the RNG draws it used to", {
  # RCBD() calls set.seed(seed) internally, so the stream left behind depends
  # only on `seed` and on how many draws RCBD() consumes. Pinning the next
  # draw therefore detects any extra or reordered RNG consumption anywhere in
  # the function - including after the block loop, which the field-book pin
  # cannot see.
  invisible(RCBD(t = 4, reps = 3, seed = 89076))
  expect_equal(runif(1), 0.6712549932, tolerance = 1e-9)
})

test_that("RCBD() t=4/reps=3/seed=89076 fixture pins layoutRandom and full infoDesign", {
  # Extend the pin to catch mutations of layoutRandom and unasserted infoDesign fields.
  d <- RCBD(t = 4, reps = 3, seed = 89076)

  # Pin layoutRandom for single location
  expected_layoutRandom <- list(
    Loc_loc1 = matrix(c(
      "1", "T3 T4 T2 T1",
      "2", "T2 T1 T3 T4",
      "3", "T1 T2 T4 T3"
    ), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Block", "--Treatments--")))
  )
  expect_equal(d$layoutRandom, expected_layoutRandom)

  # Pin all infoDesign fields
  expect_equal(d$infoDesign$blocks, 3)
  expect_equal(d$infoDesign$number.of.treatments, 4)
  expect_equal(d$infoDesign$treatments, c("T1", "T2", "T3", "T4"))
  expect_equal(d$infoDesign$locations, 1)
  expect_equal(d$infoDesign$plotNumber, c(101, 201, 301))
  expect_equal(d$infoDesign$locationNames, "loc1")
  expect_equal(d$infoDesign$seed, 89076)
  expect_equal(d$infoDesign$id_design, 2)
})

test_that("RCBD() with multi-location, continuous, and cartesian planter pins field book and layoutRandom", {
  # Pin a different code path: l=2, data supplied, continuous=TRUE, planter="cartesian".
  d <- RCBD(reps = 2, l = 2, plotNumber = c(101, 1001), continuous = TRUE,
       planter = "cartesian", seed = 555, locationNames = c("A", "B"),
       data = data.frame(TREATMENT = paste0("G-", 1:4)))

  # Pin field book
  expected_fb <- data.frame(
    ID        = 1:16,
    LOCATION  = factor(c(rep("A", 8), rep("B", 8)), levels = c("A", "B")),
    PLOT      = c(101, 102, 103, 104, 105, 106, 107, 108,
                  1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008),
    REP       = rep(c(rep(1, 4), rep(2, 4)), 2),
    TREATMENT = c("G-2", "G-1", "G-4", "G-3", "G-1", "G-4", "G-3", "G-2",
                  "G-1", "G-2", "G-3", "G-4", "G-1", "G-2", "G-4", "G-3"),
    stringsAsFactors = FALSE
  )

  fb <- d$fieldBook
  rownames(fb) <- NULL
  expect_equal(fb, expected_fb)

  # Pin layoutRandom for multi-location
  expected_layoutRandom <- list(
    Loc_A = matrix(c(
      "1", "G-2 G-1 G-4 G-3",
      "2", "G-1 G-4 G-3 G-2"
    ), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Block", "--Treatments--"))),
    Loc_B = matrix(c(
      "1", "G-1 G-2 G-3 G-4",
      "2", "G-1 G-2 G-4 G-3"
    ), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Block", "--Treatments--")))
  )
  expect_equal(d$layoutRandom, expected_layoutRandom)
})
