# The p-rep modules' "Keep copies in distinct rows and columns" box (spec-m15 section 6).
test_that("the pREPS box maps to the strict rule and matches the API call", {
  values <- list(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1),
                 planter = "serpentine", l = 1, plot_start = 101, seed = 31,
                 expt_name = "Expt1", location_names = "A", allow_fillers = FALSE,
                 rowcol_distinct = TRUE)
  built <- design_args_pREPS(shiny_shaped(values))
  expect_identical(built$rowcol_penalty, Inf)
  via_app <- do.call(partially_replicated, built)
  direct <- partially_replicated(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1),
                                 plotNumber = 101, seed = 31, exptName = "Expt1",
                                 locationNames = "A", rowcol_penalty = Inf)
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$infoDesign$shared_pairs, 0L)
  unchecked <- design_args_pREPS(shiny_shaped(utils::modifyList(values, list(rowcol_distinct = FALSE))))
  expect_identical(unchecked$rowcol_penalty, 0)
  missing_box <- design_args_pREPS(shiny_shaped(values[setdiff(names(values), "rowcol_distinct")]))
  expect_identical(missing_box$rowcol_penalty, 0)
})

test_that("the multi-location p-rep box maps to the strict rule", {
  expect_identical(design_args_multi_loc_preps(list(l = 3, rowcol_distinct = TRUE))$rowcol_penalty, Inf)
  expect_identical(design_args_multi_loc_preps(list(l = 3, rowcol_distinct = FALSE))$rowcol_penalty, 0)
  expect_identical(design_args_multi_loc_preps(list(l = 3))$rowcol_penalty, 0)
})

test_that("both p-rep modules forward the box through their values", {
  specs <- fieldhub_design_specs()
  prep_values <- specs$pREPS$values(list(rowcol_distinct = TRUE, repGens = c(40, 120), repUnits = c(2, 1)), NULL)
  expect_identical(prep_values$rowcol_distinct, TRUE)
  multi_values <- specs$multi_loc_preps$values(list(rowcol_distinct = TRUE, lines = 150, l = 3), NULL)
  expect_identical(multi_values$rowcol_distinct, TRUE)
})

test_that("both p-rep modules offer the box once, unchecked by default", {
  specs <- fieldhub_design_specs()
  for (module in c("pREPS", "multi_loc_preps")) {
    ids <- vapply(specs[[module]]$controls, `[[`, "", "id")
    expect_identical(sum(ids == "rowcol_distinct"), 1L, label = module)
    control <- specs[[module]]$controls[[which(ids == "rowcol_distinct")]]
    expect_false(isTRUE(control$value), label = module)
  }
})
