# Business logic and static bindings only; no reactive or browser sessions.
test_that("classic workflow books preserve the unsimulated view and replay simulations", {
  book <- data.frame(ID = c(2L, 1L), LOCATION = c("B", "B"), PLOT = c(102, 101),
                     ROW = 1:2, COLUMN = 1L, TREATMENT = c("A", "B"))
  set.seed(811)
  before <- .Random.seed
  plain <- classic_workflow_book(book, NULL, seed = stop("unused seed"))
  expect_identical(plain, list(df = book, simulation = NULL))
  settings <- list(min_value = 1, max_value = 10, response_name = "YIELD")
  for (ordered in c(TRUE, FALSE)) {
    expected <- simulate_classic_field_book(book, 1, 10, "YIELD", 14, order_by_id = ordered)
    actual <- classic_workflow_book(book, settings, 14, order_by_id = ordered)
    expect_identical(actual, list(df = expected$field_book, simulation = expected))
  }
  expect_identical(.Random.seed, before)
  for (bad in list(NULL, list(), book[FALSE, ])) {
    expect_error(classic_workflow_book(bad), class = "fieldhub_input_error")
  }
  for (bad in list(1, list(), list(min_value = 1), list(min_value = NA, max_value = 10, response_name = "YIELD"))) {
    expect_error(classic_workflow_book(book, bad, 14), class = "fieldhub_input_error")
  }
})

test_that("the classic workflow registry preserves module-specific view contracts", {
  expected <- c(CRD = "CRD_fieldbook", RCBD = "RCBD_fieldbook", LSD = "LSD_fieldbook",
                 Latin_Rectangle = "field_book",
                 FD = "FD.Output", SPD = "SPD.output", SSPD = "SSPD.output",
                 STRIPD = "STRIP.output", IBD = "IBD.output", RowCol = "rowcolD",
                 Alpha_Lattice = "ALPHA_fieldbook", Square_Lattice = "square_fieldbook",
                 Rectangular_Lattice = "rectangular_fieldbook")
  registry <- fieldhub_classic_workflows()
  expect_identical(names(registry), names(expected))
  for (module in names(expected)) {
    spec <- classic_workflow_spec(module)
    expect_identical(spec$ids[["table"]], expected[[module]])
    expect_identical(anyDuplicated(c(spec$ids, spec$simulation_ids)), 0L)
    code <- design_server_body(module)
    expect_identical(sum(all.names(code) == "app_classic_workflow"), 1L)
    expect_false(any(c("app_csv_download", "app_field_heatmap", "simulate_classic_field_book") %in% all.names(code)))
  }
  expect_identical(classic_workflow_spec("RowCol")$table_height, 490)
  expect_true(classic_workflow_spec("FD")$table_extensions)
  expect_identical(classic_workflow_spec("RCBD")$export_label, "TREATMENT")
  expect_identical(classic_workflow_spec("CRD")$book_component, "fieldBookXY")
  for (bad in list(NULL, NA_character_, "unknown", 1, c("CRD", "RCBD"))) {
    expect_error(classic_workflow_spec(bad), class = "fieldhub_input_error")
  }
})

test_that("workflow layout export retains label and plot-number choices", {
  book <- data.frame(LOCATION = rep("A", 4), ROW = c(1, 1, 2, 2), COLUMN = c(1, 2, 1, 2),
                     PLOT = 101:104, ENTRY = 1:4, TREATMENT = letters[1:4])
  spec <- classic_workflow_spec("RCBD")
  expect_identical(classic_workflow_layout(book, 1, "1", spec),
                   export_layout(book, 1, type_pref = "TREATMENT"))
  expect_identical(classic_workflow_layout(book, 1, 2, spec), export_layout(book, 1, TRUE))
  expect_identical(classic_workflow_layout(book, 1, "3", spec),
                   export_layout(book, 1, type_pref = "TREATMENT"))
  expect_identical(classic_workflow_layout(book, 1, 1, classic_workflow_spec("IBD")), export_layout(book, 1))
  for (bad in list(NULL, NA, "bad", c(1, 2))) {
    expect_error(classic_workflow_layout(book, 1, bad, spec), class = "fieldhub_input_error")
  }
})
