test_that("layout downloads select the same location as plots and heatmaps", {
  design <- full_factorial(setfactors = c(2, 2), reps = 2, l = 2, plotNumber = c(1, 101),
                  locationNames = c("West", "East"), seed = 17)
  book <- field_layout(design)
  book$YIELD <- seq_len(nrow(book))
  for (site in 1:2) {
    plotted <- plot_layout(design, l = site)$fieldBookXY
    heatmap <- field_book_heatmap_data(book, "YIELD", selected = site, label_column = "TRT_COMB")
    exported <- export_layout(book, selected = site, plotOn = TRUE)$file
    expect_identical(as.character(exported[2L, 1L]), unique(as.character(plotted$LOCATION)))
    expect_identical(as.character(exported[2L, 1L]), unique(as.character(heatmap$LOCATION)))
    expect_setequal(as.numeric(unlist(exported[-c(1L, 2L), -1L])), as.numeric(as.character(plotted$PLOT)))
  }
  book$LOCATION <- factor(book$LOCATION, levels = c("East", "West"))
  expect_identical(as.character(export_layout(book, 1)$file[2L, 1L]), "West")
})

test_that("location grids use named coordinates rather than positional slices", {
  book <- data.frame(LOCATION = rep(c("West", "East"), each = 4),
                     ROW = rep(c(1L, 1L, 2L, 2L), 2), COLUMN = rep(c(1L, 2L, 1L, 2L), 2),
                     EXPT = letters[1:8])
  expected <- list(matrix(c("c", "a", "d", "b"), 2), matrix(c("g", "e", "h", "f"), 2))
  expect_identical(field_book_location_grids(book, "EXPT", reverse_rows = TRUE), expected)
  altered <- book[c(3, 1, 4, 2, 7, 5, 8, 6), rev(names(book))]
  expect_identical(field_book_location_grids(altered, "EXPT", reverse_rows = TRUE), expected)
  expect_identical(field_book_location_grids(book, "EXPT")[[1]], expected[[1]][2:1, , drop = FALSE])
  different <- rbind(book, data.frame(LOCATION = "North", ROW = 1L, COLUMN = 1L, EXPT = "i"))
  expect_identical(field_book_location_grids(different, "EXPT", reverse_rows = TRUE)[[3]], matrix("i"))
  expect_error(field_book_location_grids(book[-1L, ], "EXPT"), class = "fieldhub_input_error")
  expect_error(field_book_location_grids(book, "EXPT", reverse_rows = NA), class = "fieldhub_input_error")
})

test_that("shared location order follows appearance and rejects malformed identifiers", {
  for (values in list(c("Z", "A", "Z"), factor(c("Z", "A", "Z")), c(10, 2, 1))) {
    book <- data.frame(LOCATION = values)
    expect_identical(field_book_locations(book), unique(as.character(values)))
  }
  for (book in list(NULL, data.frame(LOCATION = character()), data.frame(OTHER = 1),
                    data.frame(LOCATION = NA_character_))) {
    expect_error(field_book_locations(book), class = "fieldhub_input_error")
  }
  # The multiple diagonal page's experiment grid reads the field book's
  # locations by name, through experiment_grid_view()
  code <- body(experiment_grid_view)
  expect_identical(sum(all.names(code) == "field_book_location_grids"), 1L)
  expect_identical(sum(all.names(code) == "field_book_locations"), 1L)
  panels <- design_app_spec("diagonal_multiple")$panels
  experiment <- Filter(function(panel) identical(panel$id, "expt_layout"), panels)[[1]]
  expect_identical(experiment$type, "image")
  expect_true("experiment_grid_view" %in% all.names(body(experiment$grid)))
})
