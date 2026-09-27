test_that("export grids place values by named coordinates without reordering labels", {
  book <- data.frame(ROW = c(2L, 1L, 2L, 1L), COLUMN = c(2L, 1L, 1L, 2L),
                     TREATMENT = c("D", "A", "C", "B"))
  expected <- matrix(c("A", "C", "B", "D"), 2, 2)
  expect_identical(field_book_export_grid(book, "TREATMENT"), expected)
  book <- book[4:1, c("TREATMENT", "COLUMN", "ROW")]
  book$ROW <- factor(book$ROW, levels = 2:1)
  book$COLUMN <- as.character(book$COLUMN)
  expect_identical(field_book_export_grid(book, "TREATMENT"), expected)
  book$ENTRY <- c(0L, 3L, 1L, 4L)
  expect_identical(field_book_export_grid(book, "ENTRY"), matrix(c(1L, 3L, 0L, 4L), 2, 2))
})

test_that("export grids explain invalid coordinate maps", {
  book <- data.frame(ROW = c(1, 1, 2, 2), COLUMN = c(1, 2, 1, 2), ENTRY = 1:4)
  cases <- list(book[-1, ], book[c(1, 1, 3, 4), ], book[FALSE, ],
                transform(book, ROW = c(1, NA, 2, 2)),
                transform(book, ROW = c(1, 1, 3, 3)),
                transform(book, COLUMN = c(0, 1, 0, 1)),
                transform(book, COLUMN = c(1.5, 2, 1.5, 2)),
                book[c("ENTRY", "COLUMN")], list(ROW = 1, COLUMN = 1, ENTRY = 1))
  for (bad in cases) expect_error(field_book_export_grid(bad, "ENTRY"), class = "fieldhub_input_error")
  for (type in list(NULL, NA_character_, "MISSING", c("ROW", "COLUMN"))) {
    expect_error(field_book_export_grid(book, type), class = "fieldhub_input_error")
  }
  book$ENTRY <- I(matrix(1:4, ncol = 1))
  expect_error(field_book_export_grid(book, "ENTRY"), class = "fieldhub_input_error")
})

test_that("layout exports reject invalid locations with classed errors", {
  book <- data.frame(LOCATION = "A", ROW = c(1, 1, 2, 2), COLUMN = c(1, 2, 1, 2), ENTRY = 1:4)
  for (selected in list(NULL, 0, 2, NA_real_, 1.5, c(1, 1), "A")) {
    expect_error(export_layout(book, selected), class = "fieldhub_input_error")
  }
  expect_error(export_layout(book[names(book) != "LOCATION"], 1), class = "fieldhub_input_error")
  expect_error(export_layout(transform(book, LOCATION = NA_character_), 1), class = "fieldhub_input_error")
})
