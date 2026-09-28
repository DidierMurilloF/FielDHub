test_that("spatial heatmaps select simulation locations and columns by name", {
  data <- data.frame(ID = 1:4, ENTRY = c(1, 2, 3, 0),
                     ROW = factor(c(1, 1, 2, 2)), COLUMN = factor(c(1, 2, 1, 2)),
                     YIELD = c(1.234, 4.567, 2.111, NA), text = letters[1:4])
  second <- transform(data, YIELD = YIELD + 10)
  expect_identical(spatial_heatmap_data(list(data, second), "YIELD", 2), second)
  changed <- second[rev(names(second))]
  changed$NOTES <- "preserve"
  out <- spatial_heatmap_data(list(data, changed), "YIELD", 2)
  expect_identical(out[names(second)], second)
  expect_identical(out$NOTES, rep("preserve", 4))
  expect_identical(out$YIELD, second$YIELD)
  expect_identical(out$text, second$text)
})

test_that("spatial heatmaps reject invalid selections and schema before plotting", {
  data <- data.frame(ROW = 1:2, COLUMN = 1L, YIELD = c(1, NA), text = c("a", "b"))
  for (selected in list(NULL, 0, 2, 1.5, NA_real_, c(1, 1), "1")) {
    expect_error(spatial_heatmap_data(list(data), "YIELD", selected),
                 class = "fieldhub_input_error")
  }
  for (response in list(NULL, "", "missing", NA_character_, c("YIELD", "text"))) {
    expect_error(spatial_heatmap_data(list(data), response), class = "fieldhub_input_error")
  }
  for (bad in list(NULL, list(), data, list(NULL), list(data[FALSE, ]),
                   list(data[c("ROW", "YIELD", "text")]),
                   list(transform(data, ROW = NA_real_)),
                   list(transform(data, YIELD = Inf)),
                   list(transform(data, YIELD = "invalid")),
                   list(transform(data, text = 1:2)))) {
    expect_error(spatial_heatmap_data(bad, "YIELD"), class = "fieldhub_input_error")
  }
})

test_that("all spatial modules use the shared heatmap builder", {
  modules <- c("Diagonal", "diagonal_multiple", "sparse_allocation", "Optim",
                "RCBD_augmented", "pREPS", "multi_loc_preps")
  for (module in modules) {
    expect_identical(sum(all.names(spatial_server_body(module)) == "app_spatial_workflow"), 1L)
  }
  expect_identical(sum(all.names(body(app_spatial_workflow)) == "app_spatial_heatmap"), 1L)
})
