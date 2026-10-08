test_that("busy feedback never overwrites control enablement or leaks document-wide", {
  text <- paste(readLines(system.file("app/www/shinybusy.js", package = "FielDHub")), collapse = "\n")
  expect_false(grepl("\\.disabled[[:space:]]*=", text))
  expect_false(grepl("console.log", text, fixed = TRUE))
  expect_match(text, 'getElementById("fieldhub-app")', fixed = TRUE)
  expect_match(text, '"aria-busy"', fixed = TRUE)
  expect_match(text, '"shiny:disconnected"', fixed = TRUE)
  expect_match(text, 'status.classList.toggle("sr-only", busy)', fixed = TRUE)
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  expect_match(css, '#fieldhub-status:not(:empty):not(.sr-only)', fixed = TRUE)
})

test_that("the navbar logo is scoped, accessible, and inserted once", {
  text <- paste(readLines(system.file("app/www/corner.js", package = "FielDHub")), collapse = "\n")
  expect_match(text, "#fieldhub-app .navbar .container-fluid", fixed = TRUE)
  expect_match(text, "fieldhub-navbar-logo", fixed = TRUE)
  expect_match(text, 'alt="North Dakota State University"', fixed = TRUE)
})
