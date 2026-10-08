library(FielDHub)

test_that("strict numeric reads return values or classed input conditions", {
  expect_identical(FielDHub:::parse_whole_numbers("101, 201", "Plot start"), c(101, 201))
  expect_error(FielDHub:::parse_whole_numbers("101,", "Plot start"),
               "empty value", class = "fieldhub_input_error")
  expect_error(FielDHub:::parse_whole_numbers("abc", "Plot start"),
               "Plot start", class = "fieldhub_input_error")
  expect_error(FielDHub:::parse_whole_numbers(NA_character_, "Plot start"),
               "blank", class = "fieldhub_input_error")
})

test_that("all app modules use the shared strict plot-number reader", {
  # Static architecture check only: do not invoke Shiny module servers.
  namespace <- asNamespace("FielDHub")
  for (entry in FielDHub:::fieldhub_app_registry()) {
    code <- paste(deparse(body(get(entry$server, namespace))), collapse = "\n")
    expect_false(grepl("strsplit\\(input\\$(sparse_)?plot_start", code), info = entry$server)
    if (!is.null(entry$spec)) {
      # a generic page reads its starting plots through its plot_start
      # control, whose parser is the shared strict reader
      plot_start <- Filter(function(control) identical(control$id, "plot_start"),
                           FielDHub:::design_app_spec(entry$spec)$controls)
      expect_length(plot_start, 1L)
      expect_identical(plot_start[[1L]]$parse("101, 201", list()), c(101, 201))
      expect_error(plot_start[[1L]]$parse("101,x", list()), "Starting Plot Number",
                   class = "fieldhub_input_error")
      next
    }
    expect_true(grepl("parse_whole_numbers\\(input\\$(sparse_)?plot_start", code),
                info = entry$server)
  }
  expect_true("parse_whole_numbers" %in% all.names(body(FielDHub:::parse_control_whole_numbers)))
})
