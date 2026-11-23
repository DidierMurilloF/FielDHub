# Helper-level checks of the augmented row-column app outputs: the exports,
# the reproduction code and the replay wording. The shared spatial workflow
# supplies the buttons, the busy state and the worker lifecycle.

arc_app_design <- function(...) {
  values <- list(nrows = 10, ncols = 8, lines = 48, checks = 4, check_direction = "columns",
                 budget = 150, l = 1, plot_start = 1, seed = 11, location_names = "FARGO")
  values <- utils::modifyList(values, list(...))
  do.call(augmented_row_column, design_args_Augmented_RowCol(values))
}

test_that("the field book and layout exports keep CHECK_DIRECTION and physical coordinates", {
  x <- arc_app_design()
  spec <- spatial_workflow_spec("Augmented_RowCol")
  expect_true("CHECK_DIRECTION" %in% spec$table_columns)
  book <- spatial_workflow_book(x$fieldBook, NULL, 10, 8)$df
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  csv_download_handlers(function() csv_export_filename(x), function() as.data.frame(book))$content(file)
  lines <- readLines(file)
  expect_false(any(grepl("^PK", lines)))
  back <- utils::read.csv(file, check.names = FALSE)
  expect_true(all(spec$table_columns %in% names(back)))
  expect_identical(back$CHECK_DIRECTION, as.character(x$fieldBook$CHECK_DIRECTION))
  expect_identical(as.integer(back$ROW), as.integer(x$fieldBook$ROW))
  expect_identical(as.integer(back$COLUMN), as.integer(x$fieldBook$COLUMN))
  expect_match(csv_export_filename(x), "^FielDHub_augmented_row_column_fieldbook_")

  plot <- checked_layout_view(x, location = 1)$out_layout
  grid <- app_plot_grid_data(plot, "ENTRY")
  expect_identical(dim(grid), c(10L, 9L))
  expect_identical(grid$ROW, 10:1)
  for (format in c("png", "pdf")) {
    out <- tempfile(fileext = paste0(".", format))
    app_spatial_plot_file(plot, out, format)
    expect_gt(file.size(out), 0)
    unlink(out)
  }
})

test_that("reproduction code from an app-built result replays the deterministic content", {
  directory <- tempfile("fieldhub-arc-code-")
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({setwd(previous); unlink(directory, recursive = TRUE)}, add = TRUE)
  x <- arc_app_design()
  handlers <- design_export_handlers(function() x)
  handlers$content(handlers$filename())
  code <- handlers$code()
  expect_match(code, "augmented_row_column(", fixed = TRUE)
  expect_false(grepl("Standalone call unavailable", code, fixed = TRUE))
  environment <- new.env(parent = globalenv())
  eval(parse(text = code), envir = environment)
  expect_identical(arc_deterministic_view(environment$design), arc_deterministic_view(x))
})

test_that("the replay wording is in the reproduction code, from one source", {
  x <- arc_app_design()
  code <- design_export_handlers(function() x)$code()
  notes <- design_replay_notes()$augmented_row_column
  for (note in notes) expect_match(code, note, fixed = TRUE)
  expect_match(paste(notes, collapse = " "), "did not hit `time_limit`", fixed = TRUE)
  expect_match(paste(notes, collapse = " "), "timing-dependent", fixed = TRUE)
  expect_match(paste(notes, collapse = " "), "200 rows", fixed = TRUE)
  # other designs get no extra lines
  expect_identical(design_replay_comments(RCBD(t = 5, reps = 3, seed = 38)), character())
  expect_false(grepl("# Replay:", design_export_handlers(function() RCBD(t = 5, reps = 3, seed = 38))$code(),
                     fixed = TRUE))
})

test_that("the page names the search while it runs", {
  spec <- design_app_spec("Augmented_RowCol")
  expect_true(spec$long_running)
  expect_identical(spec$busy_message, "Searching for a balanced design...")
  expect_identical(design_engine_name(spec$engine), "augmented_row_column")
})

test_that("the page's summary states each fact of the design once", {
  design <- augmented_row_column(nrows = 8, ncols = 10, checks = c("CA", "CB", "CC", "CD"),
                                 check_direction = "rows", budget = 200, l = 2,
                                 locationNames = c("North", "South"), seed = 3)
  items <- app_summary_items(design$infoDesign, design$fieldBook)
  labels <- vapply(items, `[[`, "", "label")
  expect_identical(anyDuplicated(labels), 0L)
  # the rows, the columns and the number of checks, not also as v, s and k
  expect_false(any(c("V", "S", "K") %in% labels))
  value <- function(label) items[[which(labels == label)]]$value
  expect_identical(c(value("Rows"), value("Columns"), value("Checks")), c("8", "10", "4"))
  expect_identical(items[[which(labels == "Checks")]]$names, c("CA", "CB", "CC", "CD"))
  expect_identical(items[[which(labels == "Locations")]]$names, c("North", "South"))
  # the check direction is a parameter: the field book preview does not repeat it on every plot
  page <- as.character(app_design_summary(design))
  expect_match(page, "Check direction", fixed = TRUE)
  expect_false(grepl("CHECK_DIRECTION", page, fixed = TRUE))
  expect_true("CHECK_DIRECTION" %in% names(design$fieldBook))
})
