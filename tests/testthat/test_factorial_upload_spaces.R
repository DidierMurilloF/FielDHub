# Issue #43: the factorial page failed when an uploaded FACTOR or LEVEL entry
# contained spaces (e.g. "No nitrogen"). These tests follow the app's own path
# (upload reader, page spec, builder, engine, layout, simulation, export) with
# plain functions.

factorial_upload <- data.frame(
  FACTOR = c("Nitrogen rate", "Nitrogen rate", "Irrigation type", "Irrigation type"),
  LEVEL = c("No nitrogen", "High nitrogen", "Dry land", "Full water")
)

factorial_page_design <- function(data, type) {
  spec <- design_app_spec("FD")
  upload <- app_upload_spec(spec$upload)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(data, path, row.names = FALSE)
  file <- read_design_upload(path, ",", design = upload$validation_design,
                             missing_columns = upload$missing_columns, name = "entries.csv")
  shaped <- design_upload_data(spec$upload_shape(file$data))
  raw <- stats::setNames(lapply(spec$controls, `[[`, "value"),
                         vapply(spec$controls, `[[`, character(1), "id"))
  raw$type <- as.character(type)
  raw$l <- 2L
  raw$location_names <- "FARGO,CASSELTON"
  raw$plot_start <- "101,1001"
  raw$seed <- 7L
  parsed <- read_design_controls(spec, raw, uploaded = TRUE)
  shaped <- spec$data(shaped, parsed)
  values <- spec$values(parsed, shaped)
  do.call(spec$engine, spec$args(values, shaped))
}

test_that("factor and level names with spaces build the factorial page's design", {
  for (type in c(1, 2)) {
    x <- factorial_page_design(factorial_upload, type)
    direct <- full_factorial(reps = 3, l = 2, type = type, plotNumber = c(101, 1001),
                             locationNames = c("FARGO", "CASSELTON"), seed = 7,
                             data = factorial_upload)
    expect_identical(x$fieldBook, direct$fieldBook, info = paste("type", type))
    expect_setequal(unique(x$fieldBook$TRT_COMB),
                    c("No nitrogen*Dry land", "No nitrogen*Full water",
                      "High nitrogen*Dry land", "High nitrogen*Full water"))
    expect_true(all(c("FACTOR_Nitrogen rate", "FACTOR_Irrigation type") %in% names(x$fieldBook)))
  }
})

test_that("the factorial page's results work with names containing spaces", {
  spec <- classic_workflow_spec("FD")
  x <- factorial_page_design(factorial_upload, 2)
  view <- checked_layout_view(x, layout = 1, planter = "serpentine", location = 2,
                              stacked = "vertical")
  book <- classic_workflow_book(view[[spec$book_component]],
                                list(min_value = 1, max_value = 10, response_name = "YIELD"),
                                7, spec$order_by_id)
  expect_true("YIELD" %in% names(book$df))
  expect_silent(field_book_table_data(book$df, factor_columns = spec$table_columns))
  for (plot_type in 1:3) {
    exported <- classic_workflow_layout(book$df, 1L, plot_type, spec)
    expect_true(is.data.frame(exported$file) && nrow(exported$file) > 0L, info = paste("plot type", plot_type))
  }
  expect_identical(reproduce_design(x)$fieldBook, x$fieldBook)
})
