library(FielDHub)

test_that("split_split_plot() assigns every whole-plot to each rep under CRD (type = 1)", {
  # Regression test for a silent bug: in the CRD branch the whole-plot column was
  # built with rep(WholePlots, each = reps) instead of times = reps, so the
  # whole-plots were mis-assigned across replicates (a rep could contain one
  # whole-plot twice and miss another) rather than each rep containing every
  # whole-plot exactly once. The test is structural because the bug is silent
  # (no error is raised).
  sspd <- split_split_plot(wp = 3, sp = 2, ssp = 2, reps = 2, type = 1, l = 1, seed = 1)
  tab <- table(sspd$fieldBook$REP, sspd$fieldBook$WHOLE_PLOT)
  # Every rep must contain every whole-plot at least once.
  expect_true(all(tab > 0))
})
