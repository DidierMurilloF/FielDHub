library(FielDHub)

test_that("spinner styling is an explicit overridable configuration", {
  expect_identical(FielDHub:::fieldhub_spinner_options(),
                   list(color = "#2c7da3", color.background = "#ffffff", size = 2))
  expect_identical(FielDHub:::fieldhub_spinner_options(type = 5, size = 1),
                   list(color = "#2c7da3", color.background = "#ffffff", size = 1, type = 5))
})

test_that("building the UI is not configured through process-global options", {
  # Inspect the function body only; this is not a Shiny session test.
  uses_options <- function(code) {
    if (missing(code) || !is.call(code)) return(FALSE)
    if (identical(code[[1]], as.name("options"))) return(TRUE)
    any(vapply(as.list(code), uses_options, logical(1)))
  }
  expect_false(uses_options(body(FielDHub:::app_ui)))
})
