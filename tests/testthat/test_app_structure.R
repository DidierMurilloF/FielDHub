test_that("app ui", {
  for (package in app_dependencies()) skip_if_not_installed(package)
  ui <- app_ui()
  expect_s3_class(ui, "shiny.tag.list")
  fmls <- formals(app_ui)
  for (i in c("request")){
    expect_true(i %in% names(fmls))
  }
})

test_that("native Shiny registers bundled resources without a framework wrapper", {
  ui <- app_add_external_resources()
  expect_identical(unname(shiny::resourcePaths()[["www"]]),
    normalizePath(app_sys("app/www"), winslash = "/"))
  expect_true(file.exists(app_sys("app/www/favicon.ico")))
  expect_match(htmltools::renderTags(ui)$head, 'href="www/favicon.ico"', fixed = TRUE)
  dependencies <- htmltools::findDependencies(ui)
  dependency <- Filter(function(x) identical(x$name, "fieldhub-resources"), dependencies)[[1L]]
  expect_true(all(c("output-feedback.js", "task-feedback.js", "layout-images.js") %in% dependency$script))
  app <- run_app(launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
  expect_null(app$appOptions$golem_options)
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")){
    expect_true(i %in% names(fmls))
  }
})
