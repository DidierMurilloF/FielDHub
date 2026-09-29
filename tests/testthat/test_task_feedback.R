test_that("task buttons point to an accessible loading panel", {
  skip_if_not_installed("shiny")
  button <- as.character(app_task_button("prep-run", "Run!"))
  expect_match(button, 'data-fieldhub-task="prep-run"', fixed = TRUE)
  expect_match(button, 'aria-controls="prep-run_feedback"', fixed = TRUE)
  panel <- as.character(app_task_feedback("prep-run"))
  for (text in c('id="prep-run_feedback"', 'id="prep-run_status"',
                 'role="status"', 'aria-live="polite"', 'aria-atomic="true"',
                 'fieldhub-task-spinner', 'hidden', 'Please wait')) {
    expect_match(panel, text, fixed = TRUE)
  }
  expect_match(as.character(app_task_button("quick-run", "Run!")),
                'data-fieldhub-task="quick-run"', fixed = TRUE)
})

test_that("task feedback messages are namespaced and explicitly clear terminal states", {
  messages <- list()
  session <- list(ns = function(id) paste0("prep-", id),
    sendCustomMessage = function(type, message) messages[[length(messages) + 1L]] <<- list(type, message))
  app_report_task_feedback(session, "run", TRUE, "Optimizing allocation...")
  app_report_task_feedback(session, "run", FALSE)
  expect_identical(messages, list(
    list("fieldhub-task-feedback", list(id = "prep-run", busy = TRUE, message = "Optimizing allocation...")),
    list("fieldhub-task-feedback", list(id = "prep-run", busy = FALSE, message = ""))))
})

test_that("every shared page puts task feedback above all result tabs", {
  skip_if_not_installed("shiny")
  for (package in app_dependencies()) skip_if_not_installed(package)
  for (spec in fieldhub_design_specs()) {
    ui <- mod_design_ui("page", spec)
    html <- as.character(ui)
    main <- regexpr('class="col-sm-8"', html, fixed = TRUE)[1L]
    feedback <- regexpr('id="page-run_feedback"', html, fixed = TRUE)[1L]
    tabs <- regexpr('class="tabbable"', html, fixed = TRUE)[1L]
    expect_true(main > 0, info = spec$module)
    expect_true(feedback > main, info = spec$module)
    expect_true(tabs > feedback, info = spec$module)
    expect_match(html, 'data-fieldhub-task="page-run"', fixed = TRUE)
    expect_equal(length(regmatches(html, gregexpr('id="page-run_status"', html, fixed = TRUE))[[1L]]), 1L)
    if (identical(spec$kind, "spatial")) {
      randomize <- regexpr('id="page-randomize_feedback"', html, fixed = TRUE)[1L]
      expect_true(randomize > main, info = spec$module)
      expect_true(tabs > randomize, info = spec$module)
      expect_match(html, 'data-fieldhub-task="page-randomize"', fixed = TRUE)
      for (panel in spec$panels) {
        output <- htmltools::tagQuery(ui)$find(paste0("#page-", panel$id))
        expect_equal(output$parents(".shiny-spinner-output-container")$length(), 1L)
      }
    }
  }
  expect_true("app_report_task_feedback" %in% all.names(body(app_design_task)))
})
