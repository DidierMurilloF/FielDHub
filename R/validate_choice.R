#' Match a design option using the common structured input-error contract
#'
#' Retain match.arg() defaults and unambiguous abbreviations. The original
#' error remains available as the parent condition for diagnostic use.
#' @noRd
match_design_choice <- function(value, choices, argument) {
  caller <- sys.call(-1)
  tryCatch(match.arg(value, choices), error = function(e) {
    fieldhub_abort(conditionMessage(e),
                   data = list(argument = argument, choices = choices, value = value, parent = e),
                   call = caller)
  })
}
