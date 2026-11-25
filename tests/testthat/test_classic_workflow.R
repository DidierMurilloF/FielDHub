# Business logic and static bindings only.
test_that("classic workflow books preserve unsimulated data and simulation row alignment", {
  x <- CRD(t = 4, reps = 2, seed = 12)
  book <- x$fieldBook[rev(seq_len(nrow(x$fieldBook))), ]
  set.seed(811)
  before <- .Random.seed
  expect_identical(app_trait_simulation(x, book, seed = stop("unused seed")),
                   list(df = book, simulation = NULL))
  settings <- list(mean = 6, residual_sd = 0.4, trait = "YIELD")
  reference <- do.call(stats::simulate, c(list(object = x, seed = 14), settings))
  for (ordered in c(TRUE, FALSE)) {
    actual <- app_trait_simulation(x, book, settings, 14, order_by_id = ordered)
    expected <- actual$simulation$field_book
    if (ordered) expected <- expected[order(expected$ID), ]
    expect_identical(actual$df$YIELD, expected$YIELD)
    expect_identical(actual$df$YIELD,
                     reference$field_book$YIELD[match(actual$df$ID, reference$field_book$ID)])
    expect_identical(reproduce_simulation(actual$simulation), actual$simulation)
  }
  expect_identical(.Random.seed, before)
  for (bad in list(NULL, list(), book[FALSE, ])) {
    expect_error(app_trait_simulation(x, bad), class = "fieldhub_input_error")
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
