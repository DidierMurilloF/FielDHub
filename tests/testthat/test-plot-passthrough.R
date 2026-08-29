# Regression tests for the desplot-argument passthrough introduced when
# add_gg_features() was replaced by plot_desplot() (requires desplot >= 1.11).
# Guarded so they skip on CRAN desplot 1.10, where the layout refactor is inert.

skip_if_layout_refactor_unavailable <- function() {
  skip_if_not_installed("desplot")
  if (utils::packageVersion("desplot") < "1.11") {
    skip("plot layout refactor requires desplot >= 1.11")
  }
}

test_that("plot() forwards extra arguments through to desplot (plot_desplot path)", {
  skip_if_layout_refactor_unavailable()
  d <- RCBD(t = 12, reps = 3, plotNumber = 101, seed = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  pl <- plot(d, xlab = "CUSTOM_X")
  expect_s3_class(pl$p, "ggplot")
  # extra_args must win over plot_desplot()'s own xlab = "COLUMNS" default
  expect_identical(pl$p$labels$x, "CUSTOM_X")
})

test_that("plot() forwards extra arguments on the do.call() path (prep)", {
  skip_if_layout_refactor_unavailable()
  p <- partially_replicated(
    nrows = 8, ncols = 5, repGens = c(10, 20), repUnits = c(2, 1), seed = 65784
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  pl <- plot(p, xlab = "CUSTOM_X")
  expect_s3_class(pl$p, "ggplot")
  expect_identical(pl$p$labels$x, "CUSTOM_X")
})
