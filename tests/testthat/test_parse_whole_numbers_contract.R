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
    expect_true(grepl("parse_whole_numbers\\(input\\$(sparse_)?plot_start", code),
                info = entry$server)
    expect_false(grepl("strsplit\\(input\\$(sparse_)?plot_start", code), info = entry$server)
  }
})
