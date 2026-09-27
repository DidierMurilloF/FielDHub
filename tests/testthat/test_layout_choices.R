test_that("layout choices are plain indices and do not draw plots", {
  design <- RCBD(t = 6, reps = 4, l = 2, plotNumber = c(101, 201), seed = 21)
  for (planter in c("serpentine", "cartesian")) {
    for (stacked in c("vertical", "horizontal", "grid_panel")) {
      options <- layout_options(design, planter = planter, stacked = stacked)
      for (location in seq_along(options)) {
        expect_identical(layout_choices(design, planter, stacked, location),
                         seq_along(options[[location]]))
      }
    }
  }
  expect_false(any(c("plot_layout", "draw_layout") %in% all.names(body(layout_choices))))
})

test_that("unavailable stacking has no phantom layout choices", {
  design <- RCBD(t = 6, reps = 3, seed = 22)
  expect_identical(layout_choices(design, stacked = "grid_panel"), integer())
  expect_identical(layout_choices(CRD(t = 3, reps = 2, seed = 23)),
                   seq_along(layout_options(CRD(t = 3, reps = 2, seed = 23))[[1L]]))
})

test_that("layout choice requests reject invalid controls with classed errors", {
  design <- RCBD(t = 6, reps = 2, seed = 24)
  for (location in list(NULL, numeric(), NA_real_, NaN, Inf, 0, -1, 1.5, "1", c(1, 2), 2)) {
    expect_error(layout_choices(design, location = location), class = "fieldhub_input_error")
  }
  expect_error(layout_choices(NULL), class = "fieldhub_input_error")
  expect_error(layout_choices(design, planter = NA_character_), class = "fieldhub_input_error")
  expect_error(layout_choices(design, stacked = "diagonal"), class = "fieldhub_input_error")
})

test_that("layout choices support legacy design classes without changing global state", {
  design <- RCBD(t = 6, reps = 2, seed = 25)
  legacy <- design
  class(legacy) <- "FielDHub"
  legacy$metadata <- NULL
  set.seed(31)
  before <- .Random.seed
  settings <- options()
  expect_identical(layout_choices(legacy), layout_choices(design))
  expect_true(identical(.Random.seed, before))
  expect_identical(options(), settings)
})

test_that("classic layout selectors use shared core choices without rendering", {
  modules <- c("CRD", "RCBD", "LSD", "FD", "SPD", "SSPD", "STRIPD", "IBD",
               "RowCol", "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")
  for (module in modules) {
    code <- body(get(paste0("mod_", module, "_server"), asNamespace("FielDHub")))
    expect_false("newBooks" %in% all.names(code), info = module)
    expect_identical(sum(all.names(code) == "layout_choices"), 1L)
  }
})
