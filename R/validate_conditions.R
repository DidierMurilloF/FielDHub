#' Signal an error from a FielDHub function
#'
#' @description The design functions signal their errors as conditions of
#' class \code{fieldhub_error}, with a more specific class first, so that
#' scripts can catch them with \code{tryCatch()} and the app can show them to
#' its users. \code{fieldhub_input_error} is for invalid arguments and
#' \code{fieldhub_dimension_error} for field dimensions that do not fit the
#' entries.
#'
#' @param ... Parts of the error message, concatenated without a separator.
#' @param class More specific classes of the condition.
#' @param data Named list of fields added to the condition, such as the valid
#'   options.
#' @param call The call reported with the error, by default the call to the
#'   function that signals it.
#' @param call. Compatibility with `stop()`: when `FALSE`, omit the call.
#'
#' @noRd
fieldhub_abort <- function(..., class = "fieldhub_input_error", data = list(),
                           call = sys.call(-1), call. = NULL) {
  if (identical(call., FALSE)) call <- NULL
  message <- paste0(..., collapse = "")
  condition <- structure(
    c(list(message = message, call = call), data),
    class = unique(c(class, "fieldhub_error", "error", "condition"))
  )
  stop(condition)
}

#' Signal a warning from FielDHub
#'
#' @inheritParams fieldhub_abort
#' @noRd
fieldhub_warn <- function(..., class = "fieldhub_warning", data = list(),
                          call = sys.call(-1)) {
  condition <- structure(
    c(list(message = paste0(..., collapse = ""), call = call), data),
    class = unique(c(class, "fieldhub_warning", "warning", "condition"))
  )
  warning(condition)
}

#' Signal that the field dimensions do not fit the entries
#'
#' @param message Why the dimensions do not fit.
#' @param options Data frame with the valid options, or NULL. It is kept in the
#'   \code{options} field of the condition.
#' @param labels Character vector describing each valid option, listed in the
#'   message.
#' @param no_options Explanation added to the message when there is no valid
#'   option.
#' @param call The call reported with the error.
#'
#' @noRd
stop_dimensions <- function(message, options = NULL, labels = NULL, no_options = NULL,
                            call = sys.call(-1)) {
  if (length(labels) > 0) {
    message <- paste0(message, "\nValid options:\n", paste0("  ", labels, collapse = "\n"))
  } else if (!is.null(no_options)) {
    message <- paste0(message, "\n", no_options)
  }
  fieldhub_abort(message, class = "fieldhub_dimension_error",
                 data = list(options = options), call = call)
}

#' The message the app shows a user for a problem
#'
#' @description FielDHub conditions (\code{fieldhub_error},
#' \code{fieldhub_warning}) are written for the user, so they are shown
#' verbatim. Any other error or warning comes from R or a dependency: it is
#' shown after "Unexpected problem: " (or "Unexpected warning: ") so the user
#' can tell it apart from a problem with their input. A character value, or
#' a Shiny validation condition, is a message already written for the user.
#'
#' @param problem A condition, or a character message.
#' @return A single string.
#' @noRd
problem_message <- function(problem) {
  if (is.character(problem)) return(paste(problem, collapse = "\n"))
  if (!inherits(problem, "condition")) {
    fieldhub_abort("A problem must be a condition or a character message.")
  }
  message <- conditionMessage(problem)
  # A Shiny validation condition ("validation") already carries a message
  # written for the user, typically by validate_design().
  if (inherits(problem, c("fieldhub_error", "fieldhub_warning", "validation"))) return(message)
  prefix <- if (inherits(problem, "warning")) "Unexpected warning: " else "Unexpected problem: "
  paste0(prefix, message)
}

#' Whether a problem stops the work (error) or only informs (warning)
#'
#' @param problem A condition, or a character message (an error).
#' @return \code{"warning"} for a warning condition, \code{"error"} otherwise.
#' @noRd
problem_severity <- function(problem) {
  if (inherits(problem, "warning")) "warning" else "error"
}

#' Evaluate work, collecting the FielDHub warnings it signals
#'
#' @description FielDHub warnings are muffled and collected; any other
#' warning keeps R's own handling. When the work fails, \code{on_error} is
#' called with the error and the warnings collected so far, before the
#' error unwinds (a calling handler): it may signal another condition in its
#' place, or return to let the error continue.
#'
#' @param expr The work to evaluate.
#' @param on_error \code{NULL}, or a function of the error and the list of
#'   warnings collected before it.
#' @return A list with the \code{value} of \code{expr} and its
#'   \code{warnings} (a list of \code{fieldhub_warning} conditions).
#' @noRd
capture_fieldhub_warnings <- function(expr, on_error = NULL) {
  warnings <- list()
  value <- withCallingHandlers(
    expr,
    fieldhub_warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      if (!is.null(on_error)) on_error(e, warnings)
    }
  )
  list(value = value, warnings = warnings)
}
