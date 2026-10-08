test_that("the standard app installs with FielDHub while workers and tooling are optional", {
  app <- c("shiny", "htmltools", "DT", "bslib", "promises", "shinyjs")
  packages <- function(field) {
    text <- utils::packageDescription("FielDHub", fields = field)
    trimws(sub("\\s*\\(.*$", "", strsplit(text, ",")[[1L]]))
  }
  expect_identical(app_dependencies(), app)
  expect_setequal(packages("Imports"),
    c("dplyr", "blocksdesign", "ggplot2", "rlang", "viridisLite", "desplot", app))
  expect_setequal(packages("Suggests"),
    c("mirai", "codetools", "testthat", "spelling", "knitr", "rmarkdown"))
  # Imports installs packages; qualified calls do not need whole-namespace imports.
  expect_length(intersect(names(getNamespaceImports("FielDHub")), app), 0L)
})

test_that("the app dependency guard reports only unavailable packages", {
  checked <- character()
  available <- function(package) {checked <<- c(checked, package); TRUE}
  expect_null(app_check_dependencies(available))
  expect_identical(checked, app_dependencies())
  error <- tryCatch(app_check_dependencies(function(package) !package %in% c("DT", "shiny")),
                    fieldhub_dependency_error = identity)
  expect_s3_class(error, "fieldhub_dependency_error")
  expect_identical(error$packages, c("shiny", "DT"))
  expect_match(conditionMessage(error), 'install.packages(c("shiny", "DT"))', fixed = TRUE)
})

test_that("app startup checks dependencies before constructing the application", {
  calls <- as.list(body(run_app))[-1L]
  expect_identical(fieldhub_uninstrument(calls[[1L]]), quote(app_check_dependencies()))
})

test_that("app functions do not rely on a whole Shiny namespace import", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("codetools")
  namespace <- asNamespace("FielDHub")
  exports <- getNamespaceExports("shiny")
  for (name in c("app_ui", "app_add_external_resources", "run_app",
                  ls(namespace, pattern = "^mod_.*_(ui|server)$"))) {
    globals <- codetools::findGlobals(get(name, namespace), merge = FALSE)
    expect_length(intersect(c(globals$functions, globals$variables), exports), 0L)
  }
})
