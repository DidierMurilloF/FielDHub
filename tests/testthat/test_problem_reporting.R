library(FielDHub)

# One way to report problems (Milestone 3): the app turns every condition
# into the same user-facing wording, whether it shows it where an output
# would be (validate_design()) or in a dialog/notice for an event with no
# output (app_report_problem()). All tests call plain functions; the Shiny
# presentation is injected, never started.

input_error <- function(message = "Starting Plot Number cannot be blank.") {
  tryCatch(fieldhub_abort(message), error = function(e) e)
}

test_that("problem_message() shows FielDHub conditions verbatim and flags other errors", {
  expect_identical(problem_message(input_error()), "Starting Plot Number cannot be blank.")
  expect_identical(
    problem_message(simpleError("missing value where TRUE/FALSE needed")),
    "Unexpected problem: missing value where TRUE/FALSE needed"
  )
  warning <- tryCatch(fieldhub_warn("Using plotNumber 1001.", class = "fieldhub_default_warning"),
                      warning = function(w) w)
  expect_identical(problem_message(warning), "Using plotNumber 1001.")
  expect_identical(problem_message(simpleWarning("NAs introduced by coercion")),
                   "Unexpected warning: NAs introduced by coercion")
  expect_identical(problem_message("Blocks should have the same size."),
                   "Blocks should have the same size.")
  expect_error(problem_message(42), class = "fieldhub_input_error")
})

test_that("problem_severity() tells warnings from errors", {
  expect_identical(problem_severity(input_error()), "error")
  expect_identical(problem_severity(simpleError("x")), "error")
  expect_identical(problem_severity(simpleWarning("x")), "warning")
  expect_identical(problem_severity("A message"), "error")
})

test_that("validate_design() turns any error into a validation message and lets req() pass", {
  skip_if_not_installed("shiny")
  unexpected <- tryCatch(validate_design(stop(simpleError("missing value where TRUE/FALSE needed"))),
                         error = function(e) e)
  expect_s3_class(unexpected, "shiny.silent.error")
  expect_s3_class(unexpected, "validation")
  expect_identical(conditionMessage(unexpected),
                   "Unexpected problem: missing value where TRUE/FALSE needed")

  classed <- tryCatch(validate_design(fieldhub_abort("Starting Plot Number cannot be blank.")),
                      error = function(e) e)
  expect_s3_class(classed, "shiny.silent.error")
  expect_identical(conditionMessage(classed), "Starting Plot Number cannot be blank.")

  silent <- tryCatch(validate_design(shiny::req(FALSE)), error = function(e) e)
  expect_s3_class(silent, "shiny.silent.error")
  expect_identical(conditionMessage(silent), "")

  expect_identical(validate_design(1 + 1), 2)
})

test_that("validate_design() and app_report_problem() word the same condition the same way", {
  skip_if_not_installed("shiny")
  for (condition in list(input_error(), simpleError("subscript out of bounds"))) {
    shown <- tryCatch(validate_design(stop(condition)), error = conditionMessage)
    reported <- NULL
    app_report_problem(condition, notify = function(message, ...) reported <<- message)
    expect_identical(reported, shown)
  }
})

test_that("app_report_problem() passes the message, severity and title to its presenter", {
  calls <- list()
  notify <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  out <- app_report_problem(input_error("Too few entries."), notify = notify)
  expect_identical(out, "Too few entries.")
  expect_identical(calls[[1]], list("Too few entries.", severity = "error", title = NULL))

  app_report_problem("By unchecking this option only the checks are randomized.",
                     severity = "warning", notify = notify)
  expect_identical(calls[[2]]$severity, "warning")

  app_report_problem("No field was found.", title = "No field dimensions available",
                     notify = notify)
  expect_identical(calls[[3]]$title, "No field dimensions available")
})

test_that("app_capture_conditions() returns the value and the FielDHub warnings", {
  out <- app_capture_conditions({
    fieldhub_warn("plotNumber has the wrong length; using 1001.",
                  class = "fieldhub_default_warning")
    fieldhub_warn("Only one location.", class = "fieldhub_design_warning")
    "design"
  })
  expect_identical(out$value, "design")
  expect_length(out$warnings, 2L)
  expect_s3_class(out$warnings[[1]], "fieldhub_default_warning")
  expect_s3_class(out$warnings[[2]], "fieldhub_design_warning")

  # A real engine warning is captured instead of reaching the console
  expect_warning(
    out <- app_capture_conditions(RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1)),
    NA
  )
  expect_s3_class(out$value, "FielDHub")
  expect_s3_class(out$warnings[[1]], "fieldhub_default_warning")

  # Other warnings are not FielDHub's to explain: they keep R's own handling
  expect_warning(out <- app_capture_conditions({warning("plain"); 1}), "plain")
  expect_identical(out$value, 1)
  expect_length(out$warnings, 0L)
})

test_that("validate_design() shows FielDHub warnings as notices and returns the value", {
  skip_if_not_installed("shiny")
  notices <- list()
  value <- validate_design({
    fieldhub_warn("Using 1001.", class = "fieldhub_default_warning")
    3
  }, notice = function(w) notices[[length(notices) + 1L]] <<- w)
  expect_identical(value, 3)
  expect_length(notices, 1L)
  expect_identical(problem_message(notices[[1]]), "Using 1001.")
})

test_that("app_attempt() reports errors and warnings of work with no output", {
  reported <- list()
  report <- function(problem) reported[[length(reported) + 1L]] <<- problem_message(problem)

  expect_null(app_attempt(stop("boom"), report = report))
  expect_identical(reported[[1]], "Unexpected problem: boom")

  value <- app_attempt({
    fieldhub_warn("Few plots.", class = "fieldhub_design_warning")
    "ok"
  }, report = report)
  expect_identical(value, "ok")
  expect_identical(reported[[2]], "Few plots.")

  skip_if_not_installed("shiny")
  silent <- tryCatch(app_attempt(shiny::req(FALSE), report = report), error = function(e) e)
  expect_s3_class(silent, "shiny.silent.error")
  expect_length(reported, 2L)
})
