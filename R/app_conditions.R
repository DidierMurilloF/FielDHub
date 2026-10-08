# One way to report problems in the app.
#
# Every problem the app shows goes through problem_message()
# (R/validate_conditions.R), so a condition reads the same wherever it
# appears:
#
# - validate_design() wraps work whose result an output draws: the problem is
#   shown where the output would be, as a Shiny validation message. This is
#   safe inside observers too (a validation error only stops that observer);
#   an observer passes report = TRUE so an unexpected error, which it has no
#   output to show in, is also reported in a dialog.
# - app_report_problem() is for events with no output slot (a rejected
#   upload, a Run! that fails before any output exists): a dialog for an
#   error, a non-blocking notice for a warning. app_attempt() evaluates such
#   work and reports what it signals.
# - app_present_problem() is the only function that draws a dialog or a
#   notice.
# - An error that is not a FielDHub condition is a bug: it is always logged
#   to the R console (app_log_problem()), and shown to the user as
#   "Unexpected problem: <message>".
#
# FielDHub warnings (fieldhub_default_warning, fieldhub_design_warning, ...)
# raised while the app builds a design are collected by
# capture_fieldhub_warnings() and shown to the user as notices instead of being
# logged to the R console. Any other warning is not FielDHub's to explain: it
# keeps R's own handling (Shiny logs it to the console), so it is never lost
# silently.

#' Show any error of app work where its output would be
#'
#' @description FielDHub conditions are shown verbatim; any other error as
#' "Unexpected problem: <message>" (see \code{problem_message()}), as a Shiny
#' validation message. \code{shiny::req()} and \code{shiny::validate()}
#' conditions (\code{shiny.silent.error}) pass through untouched. FielDHub
#' warnings are shown as notices, also those raised before a failure, and
#' the value is returned.
#'
#' An error that is not a FielDHub condition is a bug, not a problem with
#' the user's input: it is always logged to the R console
#' (\code{app_log_problem()}). Inside an observer, which has no output to
#' show a validation message in (Shiny silently stops it), pass
#' \code{report = TRUE} so the user is also told in a dialog.
#'
#' @param expr The work to evaluate, typically a call to a FielDHub function.
#' @param report Whether to report an unexpected error through
#'   \code{notice} too (for observers).
#' @param notice Function called with each FielDHub warning, and with an
#'   unexpected error when \code{report} is \code{TRUE}.
#'
#' @noRd
validate_design <- function(expr, report = FALSE, notice = app_report_problem) {
  # A calling handler: a shiny.silent.error is left to propagate as it is,
  # any other error is replaced by the validation message.
  captured <- capture_fieldhub_warnings(expr, on_error = function(e, warnings) {
    if (inherits(e, "shiny.silent.error")) return(invisible(NULL))
    for (condition in warnings) notice(condition)
    if (!inherits(e, "fieldhub_error")) {
      app_log_problem(e)
      if (isTRUE(report)) notice(e)
    }
    shiny::validate(problem_message(e))
  })
  for (condition in captured$warnings) notice(condition)
  captured$value
}

#' Evaluate app work, collecting the FielDHub warnings it signals
#'
#' @description The name the app layer uses for
#' \code{capture_fieldhub_warnings()} (R/validate_conditions.R).
#' @inheritParams capture_fieldhub_warnings
#' @noRd
app_capture_conditions <- function(expr) capture_fieldhub_warnings(expr)

#' Log an unexpected (non-FielDHub) error to the R console
#'
#' @param condition The error.
#' @noRd
app_log_problem <- function(condition) {
  call <- conditionCall(condition)
  # stop() called directly in the work reports the capturing handler as its
  # call, which says nothing about where the problem is
  if (is.call(call) && identical(call[[1]], quote(withCallingHandlers))) call <- NULL
  where <- if (is.null(call)) "" else paste0(" in ", paste(deparse(call, nlines = 1L), collapse = ""))
  message("FielDHub: unexpected problem", where, ": ", conditionMessage(condition))
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
#' @description An error is reported through \code{report} (an unexpected
#' one is also logged, see \code{app_log_problem()}), together with the
#' FielDHub warnings signalled before it; after success, each FielDHub
#' warning is reported. \code{shiny.silent.error} conditions pass through.
#'
#' @param expr The work to evaluate.
#' @param report Function called with each error or FielDHub warning.
#' @param fail What a failure returns: \code{"null"} returns \code{NULL};
#'   \code{"validate"} keeps the failure as a Shiny validation message, so a
#'   design reactive that failed explains why in every output that reads it
#'   (see \code{app_design_state()}).
#' @return The value of \code{expr}, or \code{NULL} when it failed.
#' @noRd
app_attempt <- function(expr, report = app_report_problem, fail = c("null", "validate")) {
  fail <- match.arg(fail)
  failure <- NULL
  captured <- tryCatch(
    capture_fieldhub_warnings(expr, on_error = function(e, warnings) {
      if (!inherits(e, "shiny.silent.error")) for (condition in warnings) report(condition)
    }),
    error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      if (!inherits(e, "fieldhub_error")) app_log_problem(e)
      report(e)
      failure <<- e
      NULL
    }
  )
  if (!is.null(failure)) {
    if (identical(fail, "validate")) shiny::validate(problem_message(failure))
    return(NULL)
  }
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
  if (identical(severity, "error") && startsWith(message, "Unexpected problem: ")) {
    if (is.null(session$userData$fieldhub_problem_notice_gate)) {
      session$userData$fieldhub_problem_notice_gate <- problem_notice_gate()
    }
    if (!session$userData$fieldhub_problem_notice_gate(message)) return(invisible(NULL))
  }
  if (identical(severity, "warning")) {
    shiny::showNotification(
      shiny::tagList(shiny::strong(label), shiny::br(), message),
      type = "warning", duration = 10, session = session
    )
  } else {
    shiny::showModal(shiny::modalDialog(
      title = label,
      shiny::p(message),
      footer = shiny::modalButton("OK"),
      easyClose = TRUE
    ), session = session)
  }
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

#' The current state of a design reactive, for plot_state_message()
#'
#' @description A design that has not been run yet stops with an empty
#' \code{shiny::req()}; that becomes \code{NULL}. A design that failed
#' (a validation message, from \code{validate_design()} or
#' \code{app_attempt(fail = "validate")}) is returned as that condition,
#' so the panel shows why.
#' @param design A function (reactive) returning the design.
#' @return The design, \code{NULL}, or the condition it failed with.
#' @noRd
app_design_state <- function(design) {
  tryCatch(design(), shiny.silent.error = function(e) {
    if (nzchar(conditionMessage(e))) e else NULL
  })
}
