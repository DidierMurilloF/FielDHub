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
