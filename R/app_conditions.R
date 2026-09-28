# One way to report problems in the app.
#
# Every problem the app shows goes through problem_message()
# (R/validate_conditions.R), so a condition reads the same wherever it
# appears:
#
# - validate_design() wraps work whose result an output draws: the problem is
#   shown where the output would be, as a Shiny validation message. This is
#   safe inside observers too (a validation error only stops that observer).
# - app_report_problem() is for events with no output slot (a rejected
#   upload, a Run! that fails before any output exists): a dialog for an
#   error, a non-blocking notice for a warning. app_attempt() evaluates such
#   work and reports what it signals.
# - app_present_problem() is the only function that draws a dialog or a
#   notice.
#
# FielDHub warnings (fieldhub_default_warning, fieldhub_design_warning, ...)
# raised while the app builds a design are collected by
# app_capture_conditions() and shown to the user as notices instead of being
# logged to the R console. Any other warning is not FielDHub's to explain: it
# keeps R's own handling (Shiny logs it to the console), so it is never lost
# silently.

#' Show any error of app work where its output would be
#'
#' @description FielDHub conditions are shown verbatim; any other error as
#' "Unexpected problem: <message>" (see \code{problem_message()}), as a Shiny
#' validation message. \code{shiny::req()} and \code{shiny::validate()}
#' conditions (\code{shiny.silent.error}) pass through untouched. FielDHub
#' warnings are shown as notices and the value is returned.
#'
#' @param expr The work to evaluate, typically a call to a FielDHub function.
#' @param notice Function called with each FielDHub warning.
#'
#' @noRd
validate_design <- function(expr, notice = app_report_problem) {
  # A calling handler: a shiny.silent.error is left to propagate as it is,
  # any other error is replaced by the validation message.
  captured <- withCallingHandlers(
    app_capture_conditions(expr),
    error = function(e) {
      if (!inherits(e, "shiny.silent.error")) shiny::validate(problem_message(e))
    }
  )
  for (condition in captured$warnings) notice(condition)
  captured$value
}

#' Evaluate app work, collecting the FielDHub warnings it signals
#'
#' @param expr The work to evaluate.
#' @return A list with the \code{value} of \code{expr} and the
#'   \code{warnings} (a list of \code{fieldhub_warning} conditions) it
#'   signalled. Other warnings are not collected: they keep R's handling.
#' @noRd
app_capture_conditions <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(
    expr,
    fieldhub_warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

#' Report a problem of an event that has no output to show it in
#'
#' @param problem A condition, or a message written for the user.
#' @param severity \code{"error"} (a dialog), \code{"warning"} (a notice) or
#'   \code{"info"} (a dialog).
#' @param title Optional dialog title.
#' @param notify The presenter, called with the message, severity and title.
#' @return The message shown, invisibly.
#' @noRd
app_report_problem <- function(problem, severity = problem_severity(problem), title = NULL,
                               notify = app_present_problem) {
  message <- problem_message(problem)
  notify(message, severity = severity, title = title)
  invisible(message)
}

#' Evaluate work that has no output slot, reporting what it signals
#'
#' @param expr The work to evaluate.
#' @param report Function called with each error or FielDHub warning.
#' @return The value of \code{expr}, or \code{NULL} when it failed.
#'   \code{shiny.silent.error} conditions pass through.
#' @noRd
app_attempt <- function(expr, report = app_report_problem) {
  captured <- tryCatch(
    app_capture_conditions(expr),
    error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      report(e)
      NULL
    }
  )
  if (is.null(captured)) return(NULL)
  for (condition in captured$warnings) report(condition)
  captured$value
}

#' Draw a problem for the user: the only dialog/notice in the app
#'
#' @param message The message, from \code{problem_message()}.
#' @param severity \code{"error"} or \code{"info"} open a dialog;
#'   \code{"warning"} shows a non-blocking notice.
#' @param title Optional dialog title.
#' @param session The Shiny session. Outside one (scripts, tests) the problem
#'   is signalled as an R warning or message instead, so it is not lost.
#' @noRd
app_present_problem <- function(message, severity = c("error", "warning", "info"),
                                title = NULL, session = shiny::getDefaultReactiveDomain()) {
  severity <- match.arg(severity)
  label <- if (!is.null(title)) title else switch(severity,
    error = "Error", warning = "Warning", info = "Information")
  if (is.null(session)) {
    text <- paste0(label, ": ", message)
    if (identical(severity, "warning")) warning(text, call. = FALSE) else message(text)
    return(invisible(NULL))
  }
  if (identical(severity, "warning")) {
    shiny::showNotification(
      shiny::tagList(shiny::strong(label), shiny::br(), message),
      type = "warning", duration = 10, session = session
    )
  } else {
    shinyalert::shinyalert(label, message, type = severity, session = session)
  }
  invisible(NULL)
}

#' Show a shared upload error while preserving each design's column guidance
#' @noRd
app_upload_error <- function(result, missing_columns, notify = app_report_problem) {
  message <- upload_error_message(result, missing_columns)
  if (!is.null(message)) notify(message)
  invisible(NULL)
}
#' Explain an empty plot panel where the plot would be
#'
#' @inheritParams plot_state_message
#' @return \code{NULL} invisibly when the plot can be drawn; otherwise a
#'   Shiny validation message with the explanation.
#' @noRd
app_plot_state <- function(design, settings, view) {
  message <- plot_state_message(design, settings, view)
  shiny::validate(shiny::need(is.null(message), message))
  invisible(NULL)
}

#' The current value of a design reactive, or NULL when it has not run
#'
#' @description A design that has not been run yet stops with an empty
#' \code{shiny::req()}; that becomes \code{NULL}, for
#' \code{plot_state_message()}. A design that failed keeps its message.
#' @param design A function (reactive) returning the design.
#' @noRd
app_design_state <- function(design) {
  tryCatch(design(), shiny.silent.error = function(e) {
    if (nzchar(conditionMessage(e))) stop(e)
    NULL
  })
}
