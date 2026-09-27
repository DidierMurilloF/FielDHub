test_that("app dependencies are optional and not imported at namespace load", {
  app <- c("golem", "shiny", "htmltools", "DT", "bslib", "shinycssloaders",
           "plotly", "shinyalert", "shinyjs")
  packages <- function(field) {
    text <- utils::packageDescription("FielDHub", fields = field)
    trimws(sub("\\s*\\(.*$", "", strsplit(text, ",")[[1L]]))
  }
  expect_length(intersect(packages("Imports"), app), 0L)
  expect_true(all(app %in% packages("Suggests")))
  expect_length(intersect(names(getNamespaceImports("FielDHub")), app), 0L)
})

test_that("the app dependency guard reports only unavailable packages", {
  checked <- character()
  available <- function(package) {checked <<- c(checked, package); TRUE}
  expect_null(check_app_dependencies(available))
  expect_identical(checked, app_dependencies())
  error <- tryCatch(check_app_dependencies(function(package) !package %in% c("DT", "shiny")),
                    fieldhub_dependency_error = identity)
  expect_s3_class(error, "fieldhub_dependency_error")
  expect_identical(error$packages, c("shiny", "DT"))
  expect_match(conditionMessage(error), 'install.packages(c("shiny", "DT"))', fixed = TRUE)
})

test_that("app startup checks dependencies before constructing the application", {
  calls <- as.list(body(run_app))[-1L]
  expect_identical(calls[[1L]], quote(check_app_dependencies()))
})

test_that("app functions do not rely on a whole Shiny namespace import", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("codetools")
  namespace <- asNamespace("FielDHub")
  exports <- getNamespaceExports("shiny")
  for (name in c("app_ui", "golem_add_external_resources", "run_app",
                  ls(namespace, pattern = "^mod_.*_(ui|server)$"))) {
    globals <- codetools::findGlobals(get(name, namespace), merge = FALSE)
    expect_length(intersect(c(globals$functions, globals$variables), exports), 0L)
  }
})
