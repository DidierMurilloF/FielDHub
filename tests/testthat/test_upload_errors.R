test_that("upload errors share wording while keeping design-specific columns", {
  expect_identical(upload_error_message(list(bad_format = TRUE), "columns"),
                   "Invalid file; Please upload a .csv file.")
  expect_identical(upload_error_message(list(duplicated_vals = TRUE), "columns"),
                   "Check input file for duplicate values.")
  expect_identical(upload_error_message(list(missing_cols = TRUE), "Use ENTRY and NAME"),
                   "Use ENTRY and NAME")
  expect_null(upload_error_message(list(dataUp = data.frame()), "columns"))
  expect_null(upload_error_message(list(), "columns"))
  expect_null(upload_error_message(list(unknown = TRUE), "columns"))
})

test_that("the upload alert adapter reports a single error and returns NULL", {
  alerts <- list()
  notify <- function(...) alerts[[length(alerts) + 1L]] <<- list(...)
  result <- app_upload_error(list(missing_cols = TRUE), "Use ENTRY and NAME", notify)
  expect_null(result)
  expect_identical(alerts, list(list("Use ENTRY and NAME")))
  app_upload_error(list(dataUp = data.frame()), "columns", notify)
  expect_length(alerts, 1L)
})

test_that("all registered modules share the upload error adapter", {
  # Inspect function bodies without starting a Shiny server or session.
  namespace <- asNamespace("FielDHub")
  for (entry in fieldhub_app_registry()) {
    code <- body(get(entry$server, envir = namespace))
    expect_true("app_upload_error" %in% all.names(code, functions = TRUE),
                info = entry$server)
  }
})
