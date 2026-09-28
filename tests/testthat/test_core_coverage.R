library(FielDHub)

coverage_script <- system.file("ci/compare-core-coverage.R", package = "FielDHub")
coverage_env <- new.env(parent = baseenv())
sys.source(coverage_script, envir = coverage_env)

test_that("core coverage counts every package source file but the Shiny layer", {
  lines <- data.frame(
    filename = c("R/fct_alpha.R", "R/engine_beta.R", "R/layout_gamma.R",
                 "C:\\package\\R\\render_other.R", "R/validate_delta.R",
                 "R/result_epsilon.R", "R/sim_zeta.R", "R/io_eta.R",
                 "R/globals.R", "R/utils_legacy.R",
                 "R/mod_gamma.R", "R/app_ui.R", "C:\\package\\R\\app_server.R",
                 "R/run_app.R", "tests/R/render_fixture.Rmd"),
    value = c(2, 0, 3, 0, 1, 1, 0, 1, 1, 0,
              5, 5, 5, 5, 5)
  )

  out <- coverage_env$core_coverage_summary(lines)

  expect_identical(out$covered, 6L)
  expect_identical(out$total, 10L)
  expect_identical(out$percent, 60)
})

test_that("core coverage keeps files whose names only contain app or mod", {
  lines <- data.frame(
    filename = c("R/layout_app_view.R", "R/engine_model.R", "R/run_app_helpers.R"),
    value = c(1, 0, 1)
  )
  out <- coverage_env$core_coverage_summary(lines)
  expect_identical(out$total, 3L)
  expect_identical(out$covered, 2L)
})

test_that("core coverage reports an empty core explicitly", {
  lines <- data.frame(filename = c("R/mod_gamma.R", "R/app_ui.R", "R/run_app.R"),
                      value = 1)
  expect_error(coverage_env$core_coverage_summary(lines),
               "No core coverage records")
})
