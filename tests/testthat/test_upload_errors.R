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

test_that("all registered modules share the upload adapter (R/app_upload.R)", {
  # Inspect function bodies without starting a Shiny server or session.
  # app_upload_error() (the earlier per-module adapter) is superseded by
  # read_design_upload()/app_read_upload(): a failed upload now raises a
  # classed fieldhub_input_error that app_attempt() reports the same way
  # any other event error is reported, instead of a flag the caller
  # branches on.
  namespace <- asNamespace("FielDHub")
  for (entry in fieldhub_app_registry()) {
    code <- body(get(entry$server, envir = namespace))
    expect_true("app_read_upload" %in% all.names(code, functions = TRUE),
                info = entry$server)
    expect_true("app_upload_dialog_observer" %in% all.names(code, functions = TRUE),
                info = entry$server)
  }
})
