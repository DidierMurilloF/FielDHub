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
    expect_match(handlers$filename(), "^FielDHub_[a-z_]+_fieldbook_[0-9-]+[.]csv$", info = name)
    handlers$content(file)
    utils::write.csv(book, expected, row.names = FALSE, fileEncoding = "UTF-8")
    expect_identical(unname(tools::md5sum(file)), unname(tools::md5sum(expected)), info = name)
    expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "download.csv", info = name)
    expect_identical(serialize(design, NULL), before, info = name)
  }
})

test_that("plain field books retain all locations and simulated traits", {
  design <- RCBD(t = 4, reps = 2, l = 2, plotNumber = c(101, 1001),
                  locationNames = c("West", "East"), seed = 27)
  book <- field_layout(design)
  book <- simulate_classic_field_book(book, 10, 20, "YIELD", 27)$field_book
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  handlers <- csv_download_handlers(function() "fieldbook.csv", function() book)
  handlers$content(file)
  actual <- utils::read.csv(file, check.names = FALSE)
  expect_identical(names(actual), names(book))
  expect_identical(nrow(actual), nrow(book))
  expect_setequal(actual$LOCATION, c("WEST", "EAST"))
  expect_equal(actual$YIELD, book$YIELD)
})

test_that("CSV callbacks read the current data lazily and preserve quoted labels", {
  count <- 0L
  book <- data.frame(ENTRY = 1:3, TREATMENT = c('No nitrogen', 'A, "B"', 'Caf\u00e9'),
                     check.names = FALSE)
  handlers <- csv_download_handlers(function() "layout.csv", function() {count <<- count + 1L; book})
  expect_identical(handlers$filename(), "layout.csv")
  expect_identical(count, 0L)
  book$PLOT <- 101:103
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  handlers$content(file)
  expect_identical(count, 1L)
  expect_identical(utils::read.csv(file, fileEncoding = "UTF-8"), book)
})

test_that("CSV filenames identify the design, view and selected location", {
  design <- catalogue_design("partially_replicated_fillers")
  expect_identical(csv_export_filename(design, date = "2026-09-30"),
                   "FielDHub_partially_replicated_fieldbook_2026-09-30.csv")
  expect_identical(csv_export_filename(design, "field_layout", 2, "2026-09-30"),
                   "FielDHub_partially_replicated_field_layout_location_2_2026-09-30.csv")
  expect_identical(csv_export_filename(design, "plot_numbers", 1, "2026-09-30"),
                   "FielDHub_partially_replicated_plot_numbers_location_1_2026-09-30.csv")
  for (name in list(NULL, "", NA_character_, c("a.csv", "b.csv"), "../a.csv", "a/b.csv", "a\\b.csv", "a\nb.csv", "x.zip")) {
    handlers <- csv_download_handlers(function() name, function() NULL)
    expect_error(handlers$filename(), class = "fieldhub_input_error")
  }
})

test_that("unavailable or malformed CSV data fails without writing files", {
  file <- tempfile(fileext = ".csv")
  for (book in list(NULL, list(), data.frame(), data.frame(x = I(list(1, 2))))) {
    handlers <- csv_download_handlers(function() "fieldbook.csv", function() book)
    expect_error(handlers$content(file), class = "fieldhub_input_error")
    expect_false(file.exists(file))
  }
  expect_false("zip" %in% app_dependencies())
  expect_false(grepl("ZIP", as.character(app_reproduction_ui(shiny::NS("page"))), fixed = TRUE))
})
