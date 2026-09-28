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
  # An unexpected error is also logged to the R console, never lost
  expect_message(
    unexpected <- tryCatch(validate_design(stop(simpleError("missing value where TRUE/FALSE needed"))),
                           error = function(e) e),
    "FielDHub: unexpected problem: missing value where TRUE/FALSE needed", fixed = TRUE
  )
  expect_s3_class(unexpected, "shiny.silent.error")
  expect_s3_class(unexpected, "validation")
  expect_identical(conditionMessage(unexpected),
                   "Unexpected problem: missing value where TRUE/FALSE needed")

  # A FielDHub condition is the user's input problem: not logged
  expect_silent(
    classed <- tryCatch(validate_design(fieldhub_abort("Starting Plot Number cannot be blank.")),
                        error = function(e) e)
  )
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
    shown <- suppressMessages(tryCatch(validate_design(stop(condition)), error = conditionMessage))
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

test_that("capture_fieldhub_warnings() hands the warnings before a failure to on_error", {
  seen <- NULL
  err <- tryCatch(
    capture_fieldhub_warnings({
      fieldhub_warn("Using 1001.", class = "fieldhub_default_warning")
      stop("boom")
    }, on_error = function(e, warnings) seen <<- list(e, warnings)),
    error = function(e) e
  )
  # on_error is a calling handler: returning lets the error continue
  expect_identical(conditionMessage(err), "boom")
  expect_identical(conditionMessage(seen[[1]]), "boom")
  expect_length(seen[[2]], 1L)
  expect_s3_class(seen[[2]][[1]], "fieldhub_default_warning")
  expect_identical(capture_fieldhub_warnings(2)$value, 2)
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

  expect_message(expect_null(app_attempt(stop("boom"), report = report)),
                 "FielDHub: unexpected problem", fixed = TRUE)
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

test_that("app_attempt() reports the warnings raised before a failure with the error", {
  reported <- list()
  report <- function(problem) reported[[length(reported) + 1L]] <<- problem_message(problem)
  expect_null(app_attempt({
    fieldhub_warn("Using 1001.", class = "fieldhub_default_warning")
    fieldhub_abort("Too few entries.")
  }, report = report))
  expect_identical(reported, list("Using 1001.", "Too few entries."))
})

test_that("app_attempt(fail = \"validate\") keeps the failure as the design's state", {
  skip_if_not_installed("shiny")
  reported <- list()
  report <- function(problem) reported[[length(reported) + 1L]] <<- problem_message(problem)
  failed <- tryCatch(app_attempt(fieldhub_abort("Too few entries."), report = report,
                                 fail = "validate"),
                     error = function(e) e)
  expect_s3_class(failed, "shiny.silent.error")
  expect_identical(conditionMessage(failed), "Too few entries.")
  expect_identical(reported, list("Too few entries."))
  # A design reactive that failed this way is explained in its panels
  state <- app_design_state(function() app_attempt(fieldhub_abort("Too few entries."),
                                                   report = report, fail = "validate"))
  expect_identical(plot_state_message(state, NULL, "layout"), "Too few entries.")
  expect_identical(plot_state_message(state, NULL, "heatmap"), "Too few entries.")
})

test_that("validate_design(report = TRUE) also reports unexpected errors, for observers", {
  skip_if_not_installed("shiny")
  noticed <- list()
  notice <- function(problem) noticed[[length(noticed) + 1L]] <<- problem_message(problem)
  expect_message(
    err <- tryCatch(validate_design(stop("subscript out of bounds"), report = TRUE,
                                    notice = notice),
                    error = function(e) e),
    "unexpected problem", fixed = TRUE
  )
  expect_s3_class(err, "shiny.silent.error")
  expect_identical(noticed, list("Unexpected problem: subscript out of bounds"))

  # FielDHub input problems in an observer keep the selector quiet (Task 13)
  noticed <- list()
  tryCatch(validate_design(fieldhub_abort("Input # of Checks cannot be blank."),
                           report = TRUE, notice = notice),
           error = function(e) e)
  expect_length(noticed, 0L)

  # Without report, an unexpected error is logged but not reported
  noticed <- list()
  suppressMessages(tryCatch(validate_design(stop("boom"), notice = notice), error = identity))
  expect_length(noticed, 0L)
})

test_that("validate_design() shows the warnings raised before a failure", {
  skip_if_not_installed("shiny")
  noticed <- list()
  notice <- function(problem) noticed[[length(noticed) + 1L]] <<- problem_message(problem)
  err <- tryCatch(validate_design({
    fieldhub_warn("Using 1001.", class = "fieldhub_default_warning")
    fieldhub_abort("Too few entries.")
  }, notice = notice), error = function(e) e)
  expect_identical(conditionMessage(err), "Too few entries.")
  expect_identical(noticed, list("Using 1001."))
})

test_that("only the one presentation helper refers to shinyalert", {
  # Ruling R2: inspect the namespace, not R/ sources. Formals count too, so a
  # default argument such as `notify = shinyalert::shinyalert` is caught.
  refers_to_shinyalert <- function(f) {
    "shinyalert" %in% c(all.names(body(f)), unlist(lapply(formals(f), all.names)))
  }
  functions <- c(core_functions(), app_functions())
  expect_setequal(names(Filter(refers_to_shinyalert, functions)), "app_present_problem")
})

test_that("no app function writes a literal shiny::validate() message", {
  # Messages the app shows are written as FielDHub conditions and reach the
  # user through validate_design()/app_report_problem(); only
  # validate_design(), app_attempt(fail = "validate") and app_plot_state()
  # call shiny::validate() itself.
  calls_validate <- function(f) {
    grepl("shiny::validate(", paste(deparse(body(f)), collapse = "\n"), fixed = TRUE)
  }
  expect_setequal(names(Filter(calls_validate, c(core_functions(), app_functions()))),
                  c("validate_design", "app_plot_state", "app_attempt"))
})

test_that("no module catches conditions itself", {
  # Catch-all tryCatch(error = <alert>) blocks and ad-hoc warning collectors
  # used to word the same condition differently in each module. Modules now
  # use validate_design(), app_attempt() or app_report_problem() instead;
  # the only handlers left are those helpers in R/app_conditions.R.
  catches <- function(f) {
    fieldhub_calls_named(body(f), c("tryCatch", "withCallingHandlers", "try",
                                    "showNotification", "conditionMessage"))
  }
  functions <- app_functions()
  expect_setequal(names(Filter(catches, functions)),
                  c("app_attempt", "app_present_problem", "app_design_state",
                    "app_log_problem"))
})

test_that("the p-rep modules share the no-dimensions explanation", {
  expect_identical(prep_no_dimensions_problem(FALSE)$severity, "info")
  expect_identical(prep_no_dimensions_problem(FALSE)$title, "Filler plots required")
  expect_match(prep_no_dimensions_problem(FALSE, 10)$message, "no more than 10 filler plots",
               fixed = TRUE)
  expect_identical(prep_no_dimensions_problem(TRUE)$severity, "error")
  expect_match(prep_no_dimensions_problem(TRUE, 7)$message, "7 or fewer filler plots",
               fixed = TRUE)
})

test_that("plot_state_message() explains each empty plot state", {
  design <- structure(list(fieldBook = data.frame(PLOT = 1)), class = "FielDHub")
  expect_identical(plot_state_message(NULL, NULL, "layout"),
                   "Run the design to see the field layout.")
  expect_identical(plot_state_message(NULL, NULL, "heatmap"),
                   "Run the design to see the heatmap.")
  expect_null(plot_state_message(design, NULL, "layout"))
  expect_identical(plot_state_message(design, NULL, "heatmap"),
                   "Simulate data to see the heatmap.")
  expect_null(plot_state_message(design, list(response_name = "YIELD"), "heatmap"))
  expect_identical(plot_state_message(input_error("Too few entries."), NULL, "layout"),
                   "Too few entries.")
  expect_identical(plot_state_message(simpleError("boom"), NULL, "heatmap"),
                   "Unexpected problem: boom")
  expect_error(plot_state_message(design, NULL, "table"), class = "fieldhub_input_error")
})

test_that("app_plot_state() shows the explanation as a validation message", {
  skip_if_not_installed("shiny")
  err <- tryCatch(app_plot_state(NULL, NULL, "layout"), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
  expect_identical(conditionMessage(err), "Run the design to see the field layout.")
  design <- structure(list(fieldBook = data.frame(PLOT = 1)), class = "FielDHub")
  expect_null(app_plot_state(design, NULL, "layout"))
})

test_that("app_design_state() reads a design that has not run as NULL, a failed one as its condition", {
  skip_if_not_installed("shiny")
  expect_null(app_design_state(function() shiny::req(FALSE)))
  expect_identical(app_design_state(function() "design"), "design")
  failed <- app_design_state(function() shiny::validate("Too few entries."))
  expect_s3_class(failed, "validation")
  # A validation message is already written for the user: shown verbatim
  expect_identical(plot_state_message(failed, NULL, "layout"), "Too few entries.")
})

test_that("layout and heatmap outputs explain their empty states", {
  # Shared workflows and each spatial module's main layout output
  functions <- app_functions()
  for (name in c("app_classic_workflow", "app_spatial_workflow")) {
    expect_true(fieldhub_calls_named(body(functions[[name]]), "app_plot_state"), info = name)
  }
  for (module in c("Diagonal", "diagonal_multiple", "sparse_allocation", "Optim", "pREPS",
                   "multi_loc_preps", "RCBD_augmented")) {
    expect_true(fieldhub_calls_named(spatial_server_body(module), "app_plot_state"), info = module)
  }
  # The classic heatmap no longer opens a dialog from inside a reactive
  expect_false(fieldhub_calls_named(body(functions$app_classic_workflow), "modalDialog"))
})

test_that("observers ask validate_design() to report unexpected errors, other callers do not", {
  # An observer has no output to show a validation message in, and Shiny
  # stops it silently; report = TRUE also tells the user in a dialog.
  reports <- function(call) {
    isTRUE(eval(as.list(call)[["report"]]))
  }
  offenders <- character()
  walk <- function(e, in_observer, where) {
    if (!is.call(e)) return(invisible())
    head <- fieldhub_call_head_name(e[[1]])
    if (identical(head, "validate_design") && reports(e) != in_observer) {
      offenders <<- c(offenders, paste0(where, ": ", paste(deparse(e, nlines = 1L), collapse = "")))
    }
    inside <- in_observer || isTRUE(head %in% c("observe", "observeEvent"))
    for (i in seq_along(e)) if (is.call(e[[i]])) walk(e[[i]], inside, where)
  }
  functions <- app_functions()
  for (name in names(functions)) walk(body(functions[[name]]), FALSE, name)
  expect_identical(offenders, character(0))
})
