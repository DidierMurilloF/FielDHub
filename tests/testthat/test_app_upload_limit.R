test_that("app startup registers restoration of the existing upload limit", {
  previous <- options(shiny.maxRequestSize = 7 * 1024^2)
  on.exit(options(previous), add = TRUE)
  callbacks <- list()
  register <- function(callback) callbacks[[length(callbacks) + 1L]] <<- callback

  app_upload_limit(register_stop = register)
  expect_identical(getOption("shiny.maxRequestSize"), 100 * 1024^2)
  expect_length(callbacks, 1L)
  callbacks[[1]]()
  expect_identical(getOption("shiny.maxRequestSize"), 7 * 1024^2)

  # A repeated stop callback must not undo a subsequent host setting.
  options(shiny.maxRequestSize = 9 * 1024^2)
  callbacks[[1]]()
  expect_identical(getOption("shiny.maxRequestSize"), 9 * 1024^2)
})

test_that("app shutdown restores an absent upload-limit option", {
  previous <- options(shiny.maxRequestSize = NULL)
  on.exit(options(previous), add = TRUE)
  restore <- NULL
  app_upload_limit(register_stop = function(callback) restore <<- callback)
  expect_identical(getOption("shiny.maxRequestSize"), 100 * 1024^2)
  restore()
  expect_false("shiny.maxRequestSize" %in% names(options()))
})

test_that("failed stop registration does not leave the upload limit changed", {
  previous <- options(shiny.maxRequestSize = 11 * 1024^2)
  on.exit(options(previous), add = TRUE)
  expect_error(app_upload_limit(register_stop = function(callback) {
    stop("registration failed")
  }), "registration failed")
  expect_identical(getOption("shiny.maxRequestSize"), 11 * 1024^2)
})

test_that("each app restart captures the current host upload limit", {
  previous <- options(shiny.maxRequestSize = 13 * 1024^2)
  on.exit(options(previous), add = TRUE)
  for (limit in c(13, 17) * 1024^2) {
    options(shiny.maxRequestSize = limit)
    restore <- NULL
    app_upload_limit(register_stop = function(callback) restore <<- callback)
    restore()
    expect_identical(getOption("shiny.maxRequestSize"), limit)
  }
})

test_that("run_app defers the upload limit to the application startup hook", {
  # Inspect wiring only; do not create a Shiny session or run a server.
  code <- paste(deparse(body(run_app)), collapse = "\n")
  expect_match(code, "onStart = function()", fixed = TRUE)
  expect_match(code, "app_upload_limit()", fixed = TRUE)
  expect_match(code, "runtime$start()", fixed = TRUE)
  expect_false("options" %in% all.names(body(run_app), functions = TRUE))
})
