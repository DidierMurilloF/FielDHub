test_that("task buttons and tab-local loading regions are accessible", {
  skip_if_not_installed("shiny")
  button <- as.character(app_task_button("prep-run", "Run!", results_id = "prep-results"))
  expect_match(button, 'data-fieldhub-task="prep-run"', fixed = TRUE)
  expect_match(button, 'aria-controls="prep-results"', fixed = TRUE)
  panel <- as.character(app_task_feedback(c("prep-run", "prep-randomize"), shiny::div(id = "plot")))
  for (text in c('class="fieldhub-task-region"', 'data-fieldhub-tasks="prep-run prep-randomize"',
                 'role="status"', 'aria-live="polite"', 'aria-atomic="true"',
                 'fieldhub-task-spinner', 'hidden', 'id="plot"', 'fieldhub-task-content')) {
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

test_that("every result tab contains its own loading state instead of an external banner", {
  skip_if_not_installed("shiny")
  for (package in app_dependencies()) skip_if_not_installed(package)
  for (spec in fieldhub_design_specs()) {
    ui <- mod_design_ui("page", spec)
    query <- htmltools::tagQuery(ui)
    panes <- query$find(".tab-pane")$selectedTags()
    expect_gt(length(panes), 0L)
    expect_equal(query$find(".fieldhub-task-feedback")$length(), length(panes))
    expect_equal(query$find("#page-results .fieldhub-task-region")$length(), length(panes))
    for (pane in panes) {
      tasks <- "page-run"
      allocation_setup <- identical(pane$attribs[["data-value"]], "setup") &&
        identical(spec$setup$stage, "run")
      if (identical(spec$kind, "spatial") && !allocation_setup) tasks <- c(tasks, "page-randomize")
      region <- htmltools::tagQuery(pane)$find(".fieldhub-task-region")$selectedTags()
      expect_length(region, 1L)
      expect_identical(region[[1L]]$attribs[["data-fieldhub-tasks"]], paste(tasks, collapse = " "))
      content <- htmltools::tagQuery(pane)$find(".fieldhub-task-content")
      expect_equal(content$length(), 1L)
      expect_equal(content$find(".fieldhub-task-feedback")$length(), 0L)
    }
    expect_match(as.character(ui), 'data-fieldhub-task="page-run"', fixed = TRUE)
    if (identical(spec$kind, "spatial")) {
      expect_match(as.character(ui), 'data-fieldhub-task="page-randomize"', fixed = TRUE)
      for (panel in spec$panels) {
        output <- htmltools::tagQuery(ui)$find(paste0("#page-", panel$id))
        expect_equal(output$parents(".shiny-spinner-output-container")$length(), 1L)
      }
    }
  }
  expect_true("app_report_task_feedback" %in% all.names(body(app_design_task)))
  expect_match(paste(deparse(body(app_spatial_page)), collapse = " "),
    'shiny::outputOptions(output, "status", suspendWhenHidden = FALSE)', fixed = TRUE)
})

test_that("allocation tables use six-row pages without truncating their data", {
  skip_if_not_installed("DT")
  allocation <- do_optim(design = "prep", lines = 12, l = 3, copies_per_entry = 4, seed = 16)
  data <- allocation_view(allocation, paste0("Entry", seq_len(12)), average = TRUE)
  for (module in c("multi_loc_preps", "sparse_allocation")) {
    setup <- design_app_spec(module)$setup
    table <- app_spatial_table(data, caption = setup$caption, design = allocation,
                               export = setup$export, rows = setup$rows)
    expect_identical(setup$rows, 6L)
    expect_identical(table$x$options$pageLength, 6L)
    expect_true(table$x$options$paging)
    expect_true(table$x$options$scrollCollapse)
    expect_identical(table$x$options$lengthMenu, c(6L, 10L, 25L, 50L, 100L))
    expect_identical(table$x$options$dom, "Blfrtip")
    expect_length(table$x$options$buttons, 3L)
    expect_identical(nrow(table$x$data), nrow(data))
    expect_identical(table$x$data[[1L]], rownames(data))
  }
  expect_identical(app_spatial_table(data)$x$options$pageLength, nrow(data))
})
