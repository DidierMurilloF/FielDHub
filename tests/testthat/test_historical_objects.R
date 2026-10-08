historical_bundle <- readRDS(test_path("fixtures", "v1.5.0-designs.rds"))

test_that("historical fixtures are real results from the 1.5.0 release source", {
  expect_identical(historical_bundle$source_version, "1.5.0")
  expect_identical(historical_bundle$source_commit, "13585d0a6ce39a76125dcdbf57315ff1c992eb11")
  expect_length(historical_bundle$designs, 19L)
  for (x in historical_bundle$designs) {
    expect_identical(class(x), "FielDHub")
    expect_null(x$metadata)
  }
})

for (name in names(historical_bundle$designs)) local({
  engine <- name
  saved <- historical_bundle$designs[[engine]]
  test_that(paste("saved 1.5.0", engine, "prints without changing its data"), {
    original <- serialize(saved, NULL, version = 2)
    text <- utils::capture.output(printed <- withVisible(print(saved)))
    expect_true(any(nzchar(text)))
    expect_false(printed$visible)
    expect_identical(printed$value, saved)
    text <- utils::capture.output(print(summary(saved)))
    expect_true(any(nzchar(text)))
    expect_identical(serialize(saved, NULL, version = 2), original)
  })
  if (engine != "split_families") {
    test_that(paste("saved 1.5.0", engine, "still produces a field layout and plot"), {
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
      original <- serialize(saved, NULL, version = 2)
      book <- field_layout(saved)
      expect_s3_class(book, "data.frame")
      expect_gt(nrow(book), 0)
      expect_true(all(c("ID", "PLOT", "ROW", "COLUMN") %in% names(book)))
      plotted <- suppressWarnings(suppressMessages(plot(saved)))
      expect_identical(plotted$field_book, book)
      expect_identical(serialize(saved, NULL, version = 2), original)
    })
  }
})
