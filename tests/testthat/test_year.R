library(FielDHub)

# Regression tests: the YEAR column was always taken from the system clock
# (export_design() even ignored the year it was given), so the same call
# produced different field books in different calendar years and could not
# be reproduced or pinned in tests.

year_designs <- function(year) {
  list(
    diagonal_arrangement = diagonal_arrangement(
      nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1, year = year
    ),
    RCBD_augmented = RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 1, year = year),
    optimized_arrangement = optimized_arrangement(
      nrows = 12, ncols = 10, lines = 100, amountChecks = 20, checks = 1:5,
      seed = 1, year = year
    ),
    partially_replicated = partially_replicated(
      nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2), seed = 1,
      year = year
    ),
    sparse_allocation = sparse_allocation(
      lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234, year = year
    ),
    multi_location_prep = multi_location_prep(
      lines = 80, l = 4, copies_per_entry = 5, checks = 2, rep_checks = c(4, 4),
      allow_fillers = TRUE, seed = 1, year = year
    )
  )
}

test_that("the YEAR column can be set with `year`", {
  for (design in names(designs <- year_designs(2020))) {
    expect_true(all(designs[[design]]$fieldBook$YEAR == "2020"), info = design)
  }
})

test_that("the YEAR column defaults to the current year", {
  designs <- year_designs(NULL)
  for (design in names(designs)) {
    expect_true(all(designs[[design]]$fieldBook$YEAR == format(Sys.Date(), "%Y")),
                info = design)
  }
})

test_that("`year` must be a single value", {
  expect_error(RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 1, year = c(2020, 2021)),
               "'year' must be a single value")
})
