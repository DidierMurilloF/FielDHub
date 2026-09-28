test_that("spatial workflow books preserve simulation records separately from display IDs", {
  design <- partially_replicated(nrows = 4, ncols = 4, repGens = c(8, 4), repUnits = c(1, 2),
                                 seed = 18, year = 2026, spread_reps = FALSE)
  book <- design$fieldBook
  book$ID <- seq_len(nrow(book)) + 100L
  settings <- list(min_value = 0, max_value = 20, response_name = "YIELD",
                    correlation_x = .4, correlation_y = .5)
  set.seed(816)
  before <- .Random.seed
  expect_identical(spatial_workflow_book(book, NULL, seed = stop("unused"),
                                         nrows = stop("unused"), ncols = stop("unused")),
                   list(df = book, simulation = NULL))
  expected <- simulate_spatial_field_book(book, 4, 4, .4, .5, 0, 20, "YIELD", seed = 14)
  for (renumber in c(FALSE, TRUE)) {
    actual <- spatial_workflow_book(book, settings, 4, 4, 14, renumber_display = renumber)
    display <- expected$field_book
    if (renumber) display$ID <- seq_len(nrow(display))
    expect_identical(actual, list(df = display, simulation = expected))
  }
  plain_display <- book
  plain_display$ID <- seq_len(nrow(book))
  expect_identical(spatial_workflow_book(book, renumber_display = TRUE),
                   list(df = plain_display, simulation = NULL))
  expect_identical(.Random.seed, before)
  for (bad in list(NULL, list(), book[FALSE, ])) {
    expect_error(spatial_workflow_book(bad), class = "fieldhub_input_error")
  }
  for (bad in list(1, list(), settings[1:3])) {
    expect_error(spatial_workflow_book(book, bad, 4, 4, 14), class = "fieldhub_input_error")
  }
  expect_error(spatial_workflow_book(book, renumber_display = NA), class = "fieldhub_input_error")
})

test_that("spatial workflows keep per-module view and correlation identifiers", {
  registry <- fieldhub_spatial_workflows()
  expect_identical(names(registry), c("Diagonal", "diagonal_multiple", "Optim", "RCBD_augmented",
                                      "pREPS", "multi_loc_preps", "sparse_allocation"))
  for (module in names(registry)) {
    spec <- spatial_workflow_spec(module)
    expect_identical(anyDuplicated(c(spec$ids, spec$simulation_ids, spec$correlation_ids,
                                      spec$heatmap_checkbox)), 0L)
    code <- spatial_server_body(module)
    expect_identical(sum(all.names(code) == "app_spatial_workflow"), 1L)
    expect_false(any(c("app_csv_archive", "simulate_spatial_field_book", "app_spatial_heatmap") %in% all.names(code)))
  }
  expect_true(spatial_workflow_spec("multi_loc_preps")$renumber_display)
  expect_true(spatial_workflow_spec("RCBD_augmented")$table_collapse)
  expect_true(spatial_workflow_spec("Optim")$heatmap_title)
  for (bad in list(NULL, NA_character_, "unknown", 1, c("Diagonal", "pREPS"))) {
    expect_error(spatial_workflow_spec(bad), class = "fieldhub_input_error")
  }
})

test_that("shared spatial dialogs preserve controls and use trait terminology", {
  for (module in names(fieldhub_spatial_workflows())) {
    spec <- spatial_workflow_spec(module)
    for (failed in c(FALSE, TRUE)) {
      html <- as.character(app_spatial_simulation_modal(shiny::NS("example"), spec, failed))
      for (id in c(spec$simulation_ids, spec$correlation_ids, spec$heatmap_checkbox)) {
        expect_match(html, paste0('id="example-', id, '"'), fixed = TRUE)
      }
      expect_match(html, "Trait name:", fixed = TRUE)
      expect_false(grepl("Input Trial Name:", html, fixed = TRUE))
      expect_identical(grepl("Invalid input of data max and min", html, fixed = TRUE), failed)
    }
  }
})
