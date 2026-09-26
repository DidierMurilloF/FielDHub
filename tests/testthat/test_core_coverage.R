library(FielDHub)

coverage_script <- system.file("ci/compare-core-coverage.R", package = "FielDHub")
coverage_env <- new.env(parent = baseenv())
sys.source(coverage_script, envir = coverage_env)

test_that("core coverage follows engines, helpers and renderers but excludes UI", {
  lines <- data.frame(
    filename = c("R/fct_alpha.R", "R/utils_beta.R", "R/render_desplot.R",
                 "C:\\package\\R\\render_other.R", "R/mod_gamma.R",
                 "R/app_ui.R", "tests/R/render_fixture.Rmd"),
    value = c(2, 0, 3, 0, 0, 0, 0)
  )

  out <- coverage_env$core_coverage_summary(lines)

  expect_identical(out$covered, 2L)
  expect_identical(out$total, 4L)
  expect_identical(out$percent, 50)
})

test_that("core coverage reports an empty core explicitly", {
  lines <- data.frame(filename = "R/mod_gamma.R", value = 1)
  expect_error(coverage_env$core_coverage_summary(lines),
               "No core coverage records")
})
