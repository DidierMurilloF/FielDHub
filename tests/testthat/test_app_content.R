test_that("help topics follow the shared app registry and public API names", {
  registry <- fieldhub_app_registry()
  topics <- fieldhub_help_topics()
  expect_identical(topics$label, vapply(registry, `[[`, character(1), "label"))
  expect_identical(topics$group, vapply(registry, `[[`, character(1), "group"))
  engines <- vapply(registry, `[[`, character(1), "engine")
  expect_identical(topics$engine, engines)
  expect_identical(topics$help, paste0('help("', engines, '", package = "FielDHub")'))
  expect_identical(topics$url, paste0("https://didiermurillof.github.io/FielDHub/reference/", engines, ".html"))
})

test_that("the About page uses maintained package metadata", {
  info <- fieldhub_about_info()
  description <- utils::packageDescription("FielDHub")
  expect_identical(info$version, description$Version)
  expect_identical(info$authors, description$Author)
  expect_identical(info$license, description$License)
  expect_identical(info$issues, description$BugReports)
})

test_that("welcome and team fragments do not load remote styles or whole HTML documents", {
  for (name in c("home.html", "aboutUs.html")) {
    content <- paste(readLines(system.file("app/www", name, package = "FielDHub"), warn = FALSE), collapse = "\n")
    expect_false(grepl("<(html|head|body)([ >])|<!DOCTYPE", content, ignore.case = TRUE))
    expect_false(grepl("<link[^>]+https?://", content, ignore.case = TRUE))
  }
})

test_that("application styles are scoped to the FielDHub root", {
  for (name in c("style.css", "mobile.css")) {
    css <- paste(readLines(system.file("app/www", name, package = "FielDHub"), warn = FALSE), collapse = "\n")
    css <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
    selectors <- regmatches(css, gregexpr("[^{}]+(?=\\{)", css, perl = TRUE))[[1]]
    selectors <- trimws(unlist(strsplit(selectors, ",", fixed = TRUE)))
    selectors <- selectors[!startsWith(selectors, "@media")]
    expect_true(all(startsWith(selectors, "#fieldhub-app") | startsWith(selectors, ".fieldhub-")), info = name)
  }
})

test_that("the app builds Help and About from shared content functions", {
  code <- paste(deparse(body(app_ui)), collapse = " ")
  expect_match(code, "app_help_ui", fixed = TRUE)
  expect_match(code, "app_about_ui", fixed = TRUE)
  expect_match(code, 'id = "fieldhub-app"', fixed = TRUE)
})

test_that("design-page rows stay within the edge-to-edge app container", {
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  expect_match(css, "#fieldhub-app .tab-pane > .row {\n  margin-left: 0;\n  margin-right: 0;\n}", fixed = TRUE)
})
