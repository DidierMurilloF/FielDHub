test_that("printed parameters omit the design identifier by name, not position", {
  x <- list(infoDesign = list(id_design = 2, blocks = 3, seed = 38, custom = "keep"))
  expect_identical(utils::capture.output(str_parameters(x)),
                   utils::capture.output(str(x$infoDesign[c("blocks", "seed", "custom")])))
})

test_that("printing an identifier-only parameter list shows an empty list", {
  x <- list(infoDesign = list(id_design = 2))
  expect_identical(utils::capture.output(str_parameters(x)),
                   utils::capture.output(str(structure(list(), names = character()))))
})
