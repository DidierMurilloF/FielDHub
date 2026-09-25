library(FielDHub)

# Regression tests: these inputs failed with errors that did not say what
# was wrong ("wrong sign in 'by' argument", "object 'nt' not found",
# "object 'WholePlots' not found", "argument of length 0",
# "non-character argument").

test_that("latin_square() explains that it needs more than one treatment", {
  expect_error(latin_square(t = 1, seed = 1), "more than one treatment")
})

test_that("CRD() accepts treatments given as a factor", {
  crd <- CRD(t = factor(c("A", "B", "C")), reps = 2, seed = 1)
  expect_equal(as.vector(table(crd$fieldBook$TREATMENT)), c(2L, 2L, 2L))
  expect_setequal(unique(crd$fieldBook$TREATMENT), c("A", "B", "C"))
})

test_that("split plots accept a count of whole plots with sub-plot labels", {
  spd <- split_plot(wp = 3, sp = c("a", "b"), reps = 2, seed = 1)
  expect_setequal(unique(spd$fieldBook$WHOLE_PLOT), 1:3)
  expect_setequal(unique(spd$fieldBook$SUB_PLOT), c("a", "b"))
  expect_equal(nrow(spd$fieldBook), 3 * 2 * 2)
  sspd <- split_split_plot(wp = 3, sp = c("a", "b"), ssp = 2, reps = 2, seed = 1)
  expect_setequal(unique(sspd$fieldBook$WHOLE_PLOT), 1:3)
  expect_setequal(unique(sspd$fieldBook$SUB_PLOT), c("a", "b"))
  expect_equal(nrow(sspd$fieldBook), 3 * 2 * 2 * 2)
})

test_that("split_families() asks for the number of locations", {
  df <- data.frame(ENTRY = 1:20, NAME = paste0("G", 1:20), FAMILY = rep(1:4, 5))
  expect_error(split_families(data = df), "number of locations")
})

test_that("sparse_allocation() explains when no field dimensions fit", {
  expect_error(
    sparse_allocation(lines = 60, l = 3, copies_per_entry = 2, checks = 3, seed = 1),
    "no field dimension options"
  )
})
