library(FielDHub)

test_that("tasks and outputs share one accessible loading appearance", {
  indicator <- app_loading_indicator("Loading results...")
  query <- htmltools::tagQuery(shiny::div(indicator))
  expect_equal(query$find(".fieldhub-task-spinner")$length(), 1L)
  expect_equal(query$find(".fieldhub-task-message")$length(), 1L)
  expect_match(as.character(indicator), "Loading results...", fixed = TRUE)
  output <- shiny::imageOutput("page-layout", width = NULL, height = NULL)
  wrapper <- app_output_feedback(output)
  expect_identical(wrapper$attribs[["aria-busy"]], "false")
  expect_identical(wrapper$children[[1L]]$children[[1L]], output)
  feedback <- htmltools::tagQuery(wrapper)$find(".fieldhub-output-feedback")$selectedTags()[[1L]]
  expect_identical(feedback$attribs$hidden, "hidden")
  expect_identical(feedback$attribs$role, "status")
  expect_identical(feedback$attribs[["aria-live"]], "polite")
  expect_identical(as.character(shiny::tagList(feedback$children)), as.character(indicator))
  expect_true("app_loading_indicator" %in% all.names(body(app_task_feedback)))
  expect_false("withProgress" %in% all.names(body(app_design_task)))
  expect_false("withProgress" %in% all.names(body(app_spatial_page)))
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  expect_match(css, "#fieldhub-app .fieldhub-task-feedback,\n#fieldhub-app .fieldhub-output-feedback {", fixed = TRUE)
  expect_match(css, "prefers-reduced-motion: reduce", fixed = TRUE)
  expect_false(grepl("shiny-spinner", css, fixed = TRUE))
})

test_that("building the UI is not configured through process-global options", {
  # Inspect the function body only; this is not a Shiny session test.
  uses_options <- function(code) {
    if (missing(code) || !is.call(code)) return(FALSE)
    if (identical(code[[1]], as.name("options"))) return(TRUE)
    any(vapply(as.list(code), uses_options, logical(1)))
  }
  expect_false(uses_options(body(FielDHub:::app_ui)))
})
