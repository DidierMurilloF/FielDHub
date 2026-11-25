# What the app lets a user download or copy: the CSV field books and
# layouts, the plot images, the reproduction code, and the tables of the
# result tabs.

library(FielDHub)

# The first bytes of `file`: a zip archive starts with "PK".
file_magic <- function(file) rawToChar(readBin(file, "raw", n = 2L))

arc_app_design <- function(...) {
  values <- list(nrows = 10, ncols = 8, lines = 48, checks = 4, check_direction = "columns",
                 budget = 150, l = 1, plot_start = 1, seed = 11, location_names = "FARGO")
  values <- utils::modifyList(values, list(...))
  do.call(augmented_row_column, design_args_Augmented_RowCol(values))
}

# ---- CSV field books ---------------------------------------------------------

test_that("CSV downloads contain only the requested table for every design", {
  directory <- tempfile("fieldhub-csv-test-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  file <- file.path(directory, "download.csv")
  expected <- tempfile(fileext = ".csv")
  on.exit(unlink(expected), add = TRUE)
  designs <- names(catalogue)[!vapply(catalogue, function(entry) {
    entry$fun %in% c("do_optim", "split_families", "swap_pairs")
  }, logical(1))]
  for (name in designs) {
    design <- catalogue_design(name)
    book <- design$fieldBook
    before <- serialize(design, NULL)
    handlers <- csv_download_handlers(function() csv_export_filename(design), function() book)
    handlers$content(file)
    utils::write.csv(book, expected, row.names = FALSE, fileEncoding = "UTF-8")
    ok <- c(filename = grepl("^FielDHub_[a-z_]+_fieldbook_[0-9-]+[.]csv$", handlers$filename()),
            content = identical(unname(tools::md5sum(file)), unname(tools::md5sum(expected))),
            one_file = identical(list.files(directory, all.files = TRUE, no.. = TRUE), "download.csv"),
            design_unchanged = identical(serialize(design, NULL), before))
    expect(all(ok), sprintf("design %s: %s fails", name, names(ok)[!ok][1L]))
  }
})

test_that("plain field books retain all locations and simulated traits", {
  design <- RCBD(t = 4, reps = 2, l = 2, plotNumber = c(101, 1001),
                 locationNames = c("West", "East"), seed = 27)
  book <- field_layout(design)
  book <- simulate_classic_field_book(book, 10, 20, "YIELD", 27)$field_book
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  csv_download_handlers(function() "fieldbook.csv", function() book)$content(file)
  actual <- utils::read.csv(file, check.names = FALSE)
  expect_identical(names(actual), names(book))
  expect_setequal(actual$LOCATION, c("WEST", "EAST"))
  # write.csv keeps 15 significant digits
  expect_equal(actual$YIELD, book$YIELD, tolerance = 1e-14)
})

test_that("CSV downloads write the data current at download time and keep quoted labels", {
  book <- data.frame(ENTRY = 1:3, TREATMENT = c('No nitrogen', 'A, "B"', 'Caf\u00e9'),
                     check.names = FALSE)
  handlers <- csv_download_handlers(function() "layout.csv", function() book)
  book$PLOT <- 101:103
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  handlers$content(file)
  expect_identical(utils::read.csv(file, fileEncoding = "UTF-8"), book)
})

test_that("CSV filenames identify the design, view and selected location", {
  design <- catalogue_design("partially_replicated_fillers")
  expect_identical(
    c(csv_export_filename(design, date = "2026-09-30"),
      csv_export_filename(design, "field_layout", 2, "2026-09-30"),
      csv_export_filename(design, "plot_numbers", 1, "2026-09-30")),
    c("FielDHub_partially_replicated_fieldbook_2026-09-30.csv",
      "FielDHub_partially_replicated_field_layout_location_2_2026-09-30.csv",
      "FielDHub_partially_replicated_plot_numbers_location_1_2026-09-30.csv"))
  unsafe <- list(NULL, "", NA_character_, c("a.csv", "b.csv"), "../a.csv", "a/b.csv", "a\\b.csv",
                 "a\nb.csv", "x.zip")
  accepted <- vapply(unsafe, function(name) {
    !inherits(tryCatch(csv_download_handlers(function() name, function() NULL)$filename(),
                       error = identity), "fieldhub_input_error")
  }, logical(1))
  expect(!any(accepted), sprintf("filename %s is accepted", deparse(unsafe[accepted][1L])))
})

test_that("unavailable or malformed CSV data fails without writing files", {
  file <- tempfile(fileext = ".csv")
  for (book in list(NULL, list(), data.frame(), data.frame(x = I(list(1, 2))))) {
    handlers <- csv_download_handlers(function() "fieldbook.csv", function() book)
    expect_error(handlers$content(file), class = "fieldhub_input_error")
  }
  expect_false(file.exists(file))
})

test_that("the field book and plot downloads of a page are plain CSV files", {
  # A classic and a spatial page, as a user downloads from them
  classic <- classic_workflow_spec("RCBD")
  spatial <- spatial_workflow_spec("Diagonal")
  classic_design <- RCBD(6, 3, nrows = 3, seed = 12)
  spatial_design <- diagonal_arrangement(15, 20, 270, 4, seed = 12)
  view <- checked_layout_view(classic_design, layout = 1, location = 1)
  is_plain_csv <- function(file, book) {
    !identical(file_magic(file), "PK") &&
      identical(names(utils::read.csv(file, check.names = FALSE)), names(book)) &&
      identical(nrow(utils::read.csv(file)), nrow(book))
  }
  shiny::testServer(function(input, output, session) {
    state <- app_classic_workflow(input, output, session,
      design = function() classic_design, layout = function() view,
      seed = function() 12, selected = function() 1L, spec = classic,
      simulation_ready = function() TRUE)
  }, {
    do.call(session$setInputs, stats::setNames(list("1"), classic$ids[["plot_type"]]))
    book <- as.data.frame(state$book()$df)
    expect_true(is_plain_csv(output[[classic$ids[["field_book_download"]]]], book))
    expect_false(identical(file_magic(output[[classic$ids[["layout_download"]]]]), "PK"))
  })
  shiny::testServer(function(input, output, session) {
    state <- app_spatial_workflow(input, output, session,
      design = function() spatial_design, seed = function() 12,
      selected = function() 1L, visible = function() TRUE,
      simulation_ready = function() TRUE, book_ready = function() TRUE, spec = spatial)
  }, {
    session$flushReact()
    book <- as.data.frame(state$book()$df)
    expect_true(is_plain_csv(output[[spatial$ids[["download"]]]], book))
  })
})

test_that("the augmented row-column field book keeps CHECK_DIRECTION and physical coordinates", {
  x <- arc_app_design()
  book <- app_trait_simulation(x, x$fieldBook)$df
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  csv_download_handlers(function() csv_export_filename(x), function() as.data.frame(book))$content(file)
  expect_false(identical(file_magic(file), "PK"))
  back <- utils::read.csv(file, check.names = FALSE)
  expect_identical(back[c("CHECK_DIRECTION", "ROW", "COLUMN")],
                   data.frame(CHECK_DIRECTION = as.character(x$fieldBook$CHECK_DIRECTION),
                              ROW = as.integer(x$fieldBook$ROW), COLUMN = as.integer(x$fieldBook$COLUMN)))
  expect_match(csv_export_filename(x), "^FielDHub_augmented_row_column_fieldbook_")
  plot <- checked_layout_view(x, location = 1)$out_layout
  sizes <- vapply(c("png", "pdf"), function(format) {
    out <- tempfile(fileext = paste0(".", format))
    on.exit(unlink(out))
    app_spatial_plot_file(plot, out, format)
    file.size(out)
  }, numeric(1))
  expect_true(all(sizes > 0), info = paste(names(sizes), sizes, collapse = ", "))
})

# ---- download buttons --------------------------------------------------------

test_that("design pages label their downloads as CSV and append no reproduction footer", {
  for (package in app_dependencies()) skip_if_not_installed(package)
  specs <- fieldhub_design_specs()
  pages <- vapply(specs, function(spec) as.character(mod_design_ui("page", spec)), "")
  excel <- grepl(">\\s*Excel\\s*<|\\bExcel\\b", pages)
  expect(!any(excel), sprintf("the %s page offers an Excel download", names(specs)[excel][1L]))
  menus <- vapply(app_design_menus(), function(menu) as.character(shiny::navbarPage("Designs", menu)), "")
  expect_false(any(grepl("Save design and reproduction code", menus, fixed = TRUE)))
})

# ---- reproduction code -------------------------------------------------------

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

# ---- result tables -----------------------------------------------------------

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
  expect_identical(list(items[[which(labels == "Checks")]]$names, items[[which(labels == "Locations")]]$names),
                   list(c("CA", "CB", "CC", "CD"), c("North", "South")))
  # the check direction is a parameter: the field book preview does not repeat it on every plot
  page <- as.character(app_design_summary(design))
  expect_match(page, "Check direction", fixed = TRUE)
  expect_false(grepl("CHECK_DIRECTION", page, fixed = TRUE))
})

test_that("field-book tables make the named columns filterable and keep the other data", {
  book <- data.frame(ID = 1:3, LOCATION = c("Z", "A", "Z"), PLOT = 101:103,
                     FACTOR_A = c("low", "high", "low"), TRT_COMB = c("1", "2", "1"),
                     YIELD = c(1.2, 3.4, 2.1), FACTOR_C = c(4.1, 5.2, 6.3))
  columns <- c("LOCATION", "PLOT", "FACTOR_A", "TRT_COMB")
  expected <- book
  expected[columns] <- lapply(expected[columns], as.factor)
  expect_identical(field_book_table_data(book, columns), expected)
  expect_identical(field_book_table_data(book[rev(names(book))], columns), expected[rev(names(expected))])
  # the augmented pages collapse their table's scroll area and still show every plot on one page
  collapsed <- app_field_book_table(book, columns, collapse = TRUE)
  expect_identical(list(collapsed$x$options$pageLength, nrow(collapsed$x$data)), list(3L, 3L))
})

test_that("invalid field-book tables and display controls have classed errors", {
  book <- data.frame(ID = 1:3, TREATMENT = letters[1:3])
  calls <- c(
    lapply(list(NULL, list(), book[FALSE, ], setNames(book, c("ID", "ID"))),
           function(bad) function() field_book_table_data(bad, "ID")),
    lapply(list("missing", NA_character_, 1, c("ID", "ID")),
           function(columns) function() field_book_table_data(book, columns)),
    list(function() field_book_table_options(0L),
         function() field_book_table_options(6L, height = Inf),
         function() field_book_table_options(6L, collapse = NA)))
  classed <- vapply(calls, function(f) inherits(tryCatch(f(), error = identity), "fieldhub_input_error"),
                    logical(1))
  expect(all(classed), sprintf("case %d is not a fieldhub_input_error", which(!classed)[1L]))
})

test_that("allocation tables use six-row pages without truncating their data", {
  skip_if_not_installed("DT")
  allocation <- do_optim(design = "prep", lines = 12, l = 3, copies_per_entry = 4, seed = 16)
  data <- allocation_view(allocation, paste0("Entry", seq_len(12)), average = TRUE)
  for (module in c("multi_loc_preps", "sparse_allocation")) {
    setup <- design_app_spec(module)$setup
    table <- app_spatial_table(data, caption = setup$caption, design = allocation,
                               export = setup$export, rows = setup$rows)
    expect_identical(list(table$x$options$pageLength, isTRUE(table$x$options$paging),
                          nrow(table$x$data), table$x$data[[1L]]),
                     list(6L, TRUE, nrow(data), rownames(data)), info = module)
  }
  expect_identical(app_spatial_table(data)$x$options$pageLength, nrow(data))
})
