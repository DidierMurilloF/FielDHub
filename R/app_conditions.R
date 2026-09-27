#' Show the errors of FielDHub functions as Shiny validation messages
#'
#' @description The design functions signal their errors as conditions of
#' class \code{fieldhub_error}. In the app, this shows them where the output
#' would be, as validation messages, so the user sees what to change.
#'
#' @param expr A call to a FielDHub function.
#'
#' @noRd
validate_design <- function(expr) {
  tryCatch(
    expr,
    fieldhub_error = function(e) shiny::validate(conditionMessage(e))
  )
}

#' Show a shared upload error while preserving each design's column guidance
#' @noRd
app_upload_error <- function(result, missing_columns, notify = shinyalert::shinyalert) {
  message <- upload_error_message(result, missing_columns)
  if (!is.null(message)) notify("Error!!", message, type = "error")
  invisible(NULL)
}
