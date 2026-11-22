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
  modules <- names(fieldhub_classic_workflows())
  for (module in modules) {
    code <- design_server_body(module)
    expect_false("newBooks" %in% all.names(code), info = module)
    expect_identical(sum(all.names(code) == "layout_choices"), 0L, info = module)
  }
  panel <- body(app_classic_layout_panel)
  # the choices of the core, named by the shape of each option
  expect_identical(sum(all.names(panel) == "layout_choice_labels"), 1L)
  expect_identical(sum(all.names(body(layout_choice_labels)) == "layout_choices"), 1L)
  expect_false("newBooks" %in% all.names(panel))
})

test_that("the layout options are named by the shape of their field, with the values of the core", {
  crd <- CRD(t = 15, reps = 4, seed = 5)
  expect_identical(layout_choice_labels(crd), c("4 \u00d7 15" = 1L, "15 \u00d7 4" = 2L))
  expect_identical(unname(layout_choice_labels(crd)), layout_choices(crd))
  # two options of one shape are numbered
  lattice <- square_lattice(t = 49, k = 7, reps = 3, seed = 1)
  expect_identical(names(layout_choice_labels(lattice)), c("1: 21 \u00d7 7", "2: 21 \u00d7 7"))
  expect_identical(names(layout_choice_labels(lattice, stacked = "horizontal")),
                   c("1: 7 \u00d7 21", "2: 7 \u00d7 21"))
  expect_identical(layout_choice_labels(lattice, stacked = "grid_panel"), integer())
  for (design in list(crd, lattice)) {
    expect_identical(unname(layout_choice_labels(design)), layout_choices(design))
  }
  expect_identical(checked_layout_view(crd, layout = 2, planter = "serpentine", location = 1,
                                       stacked = "vertical")$out_layout$labels$title,
                   "Completely Randomized Design 15 \u00d7 4")
})

test_that("the CRD page has a summary of its own parameters and plain messages", {
  spec <- design_app_spec("CRD")
  expect_true(spec$summary)
  expect_true(spec$summary_field)
  design <- CRD(t = 15, reps = 4, seed = 5, locationNames = "FARGO")
  book <- checked_layout_view(design, layout = 1, planter = "serpentine", location = 1,
                              stacked = "vertical")$fieldBookXY
  items <- app_summary_items(design$infoDesign, book, field = TRUE)
  expect_identical(vapply(items, `[[`, "", "label"),
                   c("Treatments", "Replicates", "Plots", "Field", "Location"))
  expect_identical(vapply(items, `[[`, "", "value"), c("15", "4", "60", "4 \u00d7 15", "FARGO"))
  page <- as.character(app_design_summary(design, tabset = "tabs", book = book, field = TRUE))
  expect_match(page, "First 6 of 60 plots", fixed = TRUE)
  expect_match(page, "See all", fixed = TRUE)
  expect_false(grepl("Efficiency", page, fixed = TRUE))
  expect_false(grepl("style=", page, fixed = TRUE))
  # without the layout in view, the parameters of the design alone
  expect_identical(vapply(app_summary_items(design$infoDesign, design$fieldBook), `[[`, "", "label"),
                   c("Treatments", "Replicates", "Location"))
  raw <- list(t = 15, reps = 0, planter = "serpentine", plot_start = 101, location_names = "FARGO", seed = NA)
  expect_error(read_design_controls(spec, raw, uploaded = FALSE), "Replicates must be 1 or more.", fixed = TRUE)
  raw$reps <- 4; raw$t <- 0
  expect_error(read_design_controls(spec, raw, uploaded = FALSE), "Treatments must be 1 or more.", fixed = TRUE)
  expect_false(any(grepl("for \"", deparse(body(app_field_heatmap)), fixed = TRUE)))
})

test_that("a design that failed is not also reported in a dialog", {
  problem <- tryCatch(fieldhub_abort("Too few entries."), error = identity)
  # outside a session a dialog is a message: there is none
  expect_silent(job <- app_accept_design_job(list(ok = FALSE, condition = problem, warnings = list()), identity))
  expect_false(job$ok)
  expect_false("app_report_problem" %in% all.names(body(app_classic_layout)))
})
