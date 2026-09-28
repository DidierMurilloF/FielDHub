test_that("field-book table preparation uses explicit column names and preserves other data", {
  book <- data.frame(ID = 1:3, LOCATION = c("Z", "A", "Z"), PLOT = 101:103,
                     FACTOR_A = c("low", "high", "low"), TRT_COMB = c("1", "2", "1"),
                     YIELD = c(1.2, 3.4, 2.1), FACTOR_C = c(4.1, 5.2, 6.3))
  columns <- c("LOCATION", "PLOT", "FACTOR_A", "TRT_COMB")
  out <- field_book_table_data(book, columns)
  expected <- book
  expected[columns] <- lapply(expected[columns], as.factor)
  expect_identical(out, expected)
  expect_identical(field_book_table_data(book[rev(names(book))], columns), expected[rev(names(expected))])
  expect_identical(out$YIELD, book$YIELD)
  expect_identical(out$FACTOR_C, book$FACTOR_C)
  expect_identical(book$PLOT, 101:103)
})

test_that("table options preserve legacy heights and optional scroll collapsing", {
  expected <- list(pageLength = 6L, autoWidth = FALSE, scrollX = TRUE,
                    scrollY = "500px", columnDefs = list(list(className = "dt-center", targets = "_all")))
  expect_identical(field_book_table_options(6L), expected)
  collapsed <- field_book_table_options(6L, height = 600, collapse = TRUE)
  expect_identical(names(collapsed), c("pageLength", "autoWidth", "scrollX", "scrollCollapse", "scrollY", "columnDefs"))
  expect_true(collapsed$scrollCollapse)
  expect_identical(collapsed$scrollY, "600px")
})

test_that("invalid field-book tables and display controls have classed errors", {
  book <- data.frame(ID = 1:3, TREATMENT = letters[1:3])
  for (bad in list(NULL, list(), book[FALSE, ], setNames(book, c("ID", "ID")))) {
    expect_error(field_book_table_data(bad, "ID"), class = "fieldhub_input_error")
  }
  for (columns in list("missing", NA_character_, 1, c("ID", "ID"))) {
    expect_error(field_book_table_data(book, columns), class = "fieldhub_input_error")
  }
  expect_error(field_book_table_options(0L), class = "fieldhub_input_error")
  expect_error(field_book_table_options(6L, height = Inf), class = "fieldhub_input_error")
  expect_error(field_book_table_options(6L, collapse = NA), class = "fieldhub_input_error")
})

test_that("each app field-book view delegates to the shared table component", {
  registry <- fieldhub_app_registry()
  for (entry in registry) {
    # a spatial page of the generic module runs its workflow in
    # app_spatial_page() (spatial_server_body())
    code <- if (identical(entry$workflow_family, "spatial")) {
      spatial_server_body(entry$workflow)
    } else {
      body(get(entry$server, asNamespace("FielDHub")))
    }
    workflow <- paste0("app_", entry$workflow_family, "_workflow")
    expect_identical(sum(all.names(code) == workflow), 1L)
    code <- body(get(workflow, asNamespace("FielDHub")))
    expect_identical(sum(all.names(code) == "app_field_book_table"), 1L)
  }
})
