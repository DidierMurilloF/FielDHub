heatmap_test_book <- function() {
  data.frame(LOCATION = rep(c("B", "A"), each = 4),
             ROW = rep(c(1L, 1L, 2L, 2L), 2), COLUMN = rep(c(1L, 2L, 1L, 2L), 2),
             TREATMENT = paste0("T", 1:8), ENTRY = 1:8,
             CHECKS = rep(c(1L, 0L, 0L, 0L), 2), YIELD = (1:8) / 3)
}

test_that("heatmap data selects locations and values by name", {
  book <- heatmap_test_book()
  actual <- field_book_heatmap_data(book, "YIELD", selected = 2, include_checks = TRUE)
  expect_identical(as.character(actual$LOCATION), rep("A", 4))
  expect_identical(actual$YIELD, book$YIELD[5:8])
  expect_identical(actual$text[1], "Site: A\nRow: 1\nCol: 1\nTreatment: T5\nCheck: yes\nYIELD : 1.67")
  expect_s3_class(actual$ROW, "factor")
  expect_s3_class(actual$COLUMN, "factor")
  altered <- book[rev(names(book))]
  altered$NOTES <- "keep"
  reordered <- field_book_heatmap_data(altered, "YIELD", selected = 2, include_checks = TRUE)
  expect_identical(reordered[names(actual)], actual)
  expect_identical(reordered$NOTES, rep("keep", 4))
})

test_that("heatmap tooltip policies retain existing entry and site wording", {
  book <- heatmap_test_book()
  out <- field_book_heatmap_data(book, "YIELD", label_column = "ENTRY", label_title = "Entry",
                                 include_site = FALSE)
  expect_identical(out$text[1], "Row: 1\nCol: 1\nEntry: 1\nYIELD : 0.33")
  book$YIELD[2] <- NA_real_
  expect_true(is.na(field_book_heatmap_data(book, "YIELD")$YIELD[2]))
})

test_that("invalid heatmap selections and schemas give classed errors", {
  book <- heatmap_test_book()
  for (selected in list(NULL, NA_real_, 0, 3, 1.5, "A", c(1, 2))) {
    expect_error(field_book_heatmap_data(book, "YIELD", selected), class = "fieldhub_input_error")
  }
  for (name in list(NULL, NA_character_, "", "missing", c("YIELD", "ENTRY"))) {
    expect_error(field_book_heatmap_data(book, name), class = "fieldhub_input_error")
  }
  for (bad in list(book[FALSE, ], book[names(book) != "ROW"],
                   transform(book, LOCATION = NA_character_),
                   transform(book, ROW = NA_real_),
                   transform(book, YIELD = "bad"),
                   transform(book, YIELD = Inf))) {
    expect_error(field_book_heatmap_data(bad, "YIELD"), class = "fieldhub_input_error")
  }
})

test_that("all classic modules use the shared heatmap builder", {
  modules <- names(fieldhub_classic_workflows())
  for (module in modules) {
    code <- design_server_body(module)
    expect_identical(sum(all.names(code) == "app_classic_workflow"), 1L)
  }
  expect_identical(sum(all.names(body(app_classic_workflow)) == "app_field_heatmap"), 1L)
})
