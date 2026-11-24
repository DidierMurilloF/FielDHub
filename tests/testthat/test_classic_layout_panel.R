# Plain choice calculations, HTML construction, and static bindings only.
test_that("a 'Reps layout' is offered only where it has a layout option", {
  base <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal")
  panel <- c(base, "Grid Panel" = "grid_panel")
  # Replicates lie in rows and columns of replicates when their number is a
  # product of two numbers above one
  for (reps in 2:9) {
    expected <- if (reps %in% c(4, 6, 8, 9)) panel else base
    expect_identical(layout_stacking_choices(RCBD(t = 4, reps = reps, seed = 1)), expected,
                     info = reps)
    expect_identical(layout_stacking_choices(split_plot(wp = 2, sp = 2, reps = reps, seed = 1)),
                     expected, info = reps)
    expect_identical(layout_stacking_choices(latin_square(t = 3, reps = reps, seed = 1)),
                     expected, info = reps)
  }
  # The lattice pages keep the Grid Panel they had
  for (reps in 2:6) {
    lattice <- alpha_lattice(t = 12, k = 3, reps = reps, seed = 1)
    expect_identical(layout_stacking_choices(lattice), if (reps %in% c(4, 6)) panel else base,
                     info = reps)
    for (stacked in layout_stacking_choices(lattice)) {
      expect_gt(length(unlist(layout_choice_labels(lattice, stacked = stacked,
                                                   include_grids = TRUE))), 0L)
    }
  }
  # No replicates to arrange: the two stackings of the numbered layouts
  without <- list(full_factorial(setfactors = c(2, 3), reps = 4, type = 1, seed = 1),
                  split_plot(wp = 3, sp = 2, reps = 4, type = 1, seed = 1),
                  row_column(t = 12, nrows = 3, reps = 4, seed = 1, iterations = 10))
  for (design in without) expect_identical(layout_stacking_choices(design), base)
  # A design built with dimensions has its one grid under every numbered stacking
  expect_identical(layout_stacking_choices(RCBD(t = 4, reps = 4, seed = 1, nrows = 4)), base)
})

test_that("classic layout presentation is described by the workflow registry", {
  modules <- names(fieldhub_classic_workflows())
  for (module in modules) {
    spec <- classic_workflow_spec(module)
    expect_true(is.list(spec$layout), info = module)
    ids <- spec$layout$ids
    expected <- c("layout", if (!module %in% c("CRD", "Latin_Rectangle")) "stacked",
                  if (!module %in% c("CRD", "LSD")) "location")
    expect_identical(names(ids), expected, info = module)
    expect_identical(anyDuplicated(c(spec$ids, spec$simulation_ids, ids, spec$layout$output)), 0L)
    expect_true(all(spec$layout$widths %in% 1:12))
    code <- design_server_body(module)
    expect_identical(sum(all.names(code) == "app_classic_layout"), 1L, info = module)
    expect_false(any(c("app_layout_selection", "layout_choices", "sqrt") %in% all.names(code)), info = module)
  }
  binding <- body(app_classic_layout)
  expect_identical(sum(all.names(binding) == "app_layout_selection"), 1L)
  expect_identical(sum(all.names(binding) == "app_classic_layout_panel"), 1L)
})

test_that("shared layout panels retain control labels, namespacing, and options", {
  skip_if_not_installed("shiny")
  set.seed(18)
  before <- .Random.seed
  x <- RCBD(t = 6, reps = 4, l = 2, plotNumber = c(101, 1001), seed = 7)
  for (module in names(fieldhub_classic_workflows())) {
    spec <- classic_workflow_spec(module)
    html <- as.character(app_classic_layout_panel(shiny::NS("panel"), spec, x,
                                                  locations = c(2, 4), planter = "cartesian"))
    for (id in c(spec$ids[["plot_type"]], spec$layout$ids)) {
      expect_true(grepl(paste0('id="panel-', id, '"'), html, fixed = TRUE), info = module)
    }
    for (label in c("Type of plot", "Entries", "Plots", "Heatmap", "Layout option")) {
      expect_true(grepl(label, html, fixed = TRUE), info = module)
    }
    # Four blocks lie in a panel of two by two on every page with a "Reps layout"
    expect_identical(grepl("Grid Panel", html, fixed = TRUE), "stacked" %in% names(spec$layout$ids))
    if ("location" %in% names(spec$layout$ids)) {
      expect_true(grepl('<option value="4">4</option>', html, fixed = TRUE))
    }
  }
  html <- as.character(app_classic_layout_panel(shiny::NS("panel"), classic_workflow_spec("RCBD"), x))
  expect_true(grepl('<option value="2">2</option>', html, fixed = TRUE))
  expect_error(app_classic_layout_panel(identity, classic_workflow_spec("RCBD"), NULL),
                class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

test_that("classic layout menus keep the numbered layouts and group the field grids", {
  design <- split_plot(wp = 3, sp = 2, reps = 4, seed = 1)
  numbered <- layout_choice_labels(design)
  choices <- layout_choice_labels(design, include_grids = TRUE)
  expect_identical(choices[seq_along(numbered)], as.list(numbered))
  expect_identical(names(choices)[-seq_along(numbered)], "Field grid")
  expect_identical(choices[["Field grid"]],
                   list("Grid 12 \u00d7 2" = "grid:12:2", "Grid 24 \u00d7 1" = "grid:24:1"))

  spec <- design_app_spec("SPD")$workflow
  html <- as.character(app_classic_layout_panel(identity, spec, design))
  for (i in seq_along(numbered)) {
    expect_true(grepl(paste0('<option value="', numbered[[i]], '"',
                             if (i == 1L) " selected", ">", names(numbered)[i], "</option>"),
                      html, fixed = TRUE))
  }
  expect_true(grepl('<optgroup label="Field grid">', html, fixed = TRUE))
  expect_true(grepl('<option value="grid:12:2">Grid 12 \u00d7 2</option>', html, fixed = TRUE))
  expect_true(grepl('<option value="grid_panel">Grid Panel</option>', html, fixed = TRUE))

  # Three Latin squares stack as 9 x 3 and 3 x 9, the numbered layouts: no
  # field grid is offered, and the menu is the numbered one
  squares <- latin_square(t = 3, reps = 3, seed = 1)
  for (stacked in c("vertical", "horizontal")) {
    expect_identical(layout_choice_labels(squares, stacked = stacked, include_grids = TRUE),
                     layout_choice_labels(squares, stacked = stacked))
  }
  expect_false(grepl("Grid Panel", as.character(app_classic_layout_panel(
    identity, design_app_spec("LSD")$workflow, squares)), fixed = TRUE))

  stored <- RCBD(t = 6, reps = 6, nrows = 4, ncols = 9, seed = 1)
  expect_identical(layout_choice_labels(stored, include_grids = TRUE), layout_choice_labels(stored))
})

test_that("the Grid Panel menu lists the fields with rows and columns of replicates", {
  # A page without numbered grid panels: the grids alone, the most square first
  rcbd <- RCBD(t = 6, reps = 4, seed = 1)
  expect_identical(layout_choice_labels(rcbd, stacked = "grid_panel", include_grids = TRUE),
                   list("Field grid" = list("Grid 4 \u00d7 6" = "grid:4:6", "Grid 6 \u00d7 4" = "grid:6:4",
                                            "Grid 2 \u00d7 12" = "grid:2:12",
                                            "Grid 12 \u00d7 2" = "grid:12:2")))
  expect_identical(layout_choice_labels(rcbd, stacked = "grid_panel"), integer())
  expect_identical(layout_choice_labels(RCBD(t = 6, reps = 6, seed = 1), stacked = "grid_panel",
                                        include_grids = TRUE)[["Field grid"]][["Grid 4 \u00d7 9"]],
                   "grid:4:9")
  # A page with numbered grid panels adds the grids that are not numbered
  lattice <- alpha_lattice(t = 12, k = 3, reps = 4, seed = 36)
  numbered <- layout_choice_labels(lattice, stacked = "grid_panel")
  choices <- layout_choice_labels(lattice, stacked = "grid_panel", include_grids = TRUE)
  expect_gt(length(numbered), 0L)
  expect_identical(choices[seq_along(numbered)], as.list(numbered))
  grids <- names(choices[["Field grid"]])
  expect_true(all(c("Grid 2 \u00d7 24", "Grid 4 \u00d7 12", "Grid 12 \u00d7 4") %in% grids))
  expect_length(intersect(sub("^Grid ", "", grids), names(numbered)), 0L)
  # The stacks list their own fields: the one-row field is a horizontal stack
  expect_identical(layout_choice_labels(rcbd, stacked = "horizontal", include_grids = TRUE)[["Field grid"]],
                   list("Grid 1 \u00d7 24" = "grid:1:24"))
  expect_identical(layout_choice_labels(rcbd, include_grids = TRUE), layout_choice_labels(rcbd))
})

test_that("every option of a 'Reps layout' menu draws its replicates as the menu says", {
  # Regression test: the "Field grid" group was the same under every
  # stacking, so the vertical menu of an RCBD offered "Grid 1 x 24" (the
  # replicates side by side) and the horizontal menu of a split plot
  # offered "Grid 18 x 1" (the replicates one above another).
  designs <- list(
    RCBD(t = 6, reps = 4, seed = 1),
    RCBD(t = 6, reps = 6, seed = 1),
    full_factorial(setfactors = c(2, 3), reps = 4, type = 2, seed = 1),
    latin_square(t = 4, reps = 4, seed = 7),
    split_plot(wp = 3, sp = 2, reps = 4, seed = 1),
    split_plot(wp = 3, sp = 2, reps = 3, seed = 11),
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 4, seed = 1),
    strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 1),
    incomplete_blocks(t = 12, k = 4, reps = 4, seed = 1),
    incomplete_blocks(t = 12, k = 4, reps = 3, seed = 17),
    alpha_lattice(t = 12, k = 3, reps = 4, seed = 36),
    square_lattice(t = 16, k = 4, reps = 4, seed = 1),
    rectangular_lattice(t = 12, k = 3, reps = 4, seed = 1)
  )
  shape <- function(labels) sub(" \u00b7 .*| \\([0-9]+\\)$", "", sub("^Grid ", "", labels))
  for (design in designs) {
    unit <- if (inherits(design, "fieldhub_latin_square")) "SQUARE" else "REP"
    for (planter in c("serpentine", "cartesian")) {
      for (stacked in layout_stacking_choices(design, planter = planter)) {
        choices <- layout_choice_labels(design, planter = planter, stacked = stacked,
                                        include_grids = TRUE)
        info <- paste(class(design)[1L], planter, stacked)
        expect_gt(length(choices), 0L)
        group <- if (is.list(choices)) choices[["Field grid"]]
        # A field grid never repeats a numbered layout of its stacking
        expect_length(intersect(shape(names(group)), shape(setdiff(names(choices), "Field grid"))),
                      0L)
        choices <- unlist(choices)
        for (i in seq_along(choices)) {
          book <- app_selected_layout_view(design, choices[[i]], planter, 1,
                                           stacked)$allSitesFieldbook
          expect_identical(replicates_lie(book, unit), stacked,
                           info = paste(info, names(choices)[i]))
        }
      }
    }
  }
})

test_that("designs without replicates list their field grids under both stackings", {
  designs <- list(CRD(t = 6, reps = 4, seed = 1),
                  full_factorial(setfactors = c(2, 3), reps = 4, type = 1, seed = 1),
                  split_plot(wp = 3, sp = 2, reps = 4, type = 1, seed = 1))
  for (design in designs) {
    vertical <- layout_choice_labels(design, include_grids = TRUE)
    expect_true("Field grid" %in% names(vertical))
    expect_identical(layout_choice_labels(design, stacked = "horizontal", include_grids = TRUE)[["Field grid"]],
                     vertical[["Field grid"]])
    grid <- vertical[["Field grid"]][[1L]]
    dims <- as.numeric(strsplit(grid, ":", fixed = TRUE)[[1L]][-1L])
    expect_identical(app_selected_layout_view(design, grid, "serpentine", 1, "horizontal")$allSitesFieldbook,
                     field_layout(design, nrows = dims[1L], ncols = dims[2L]))
  }
})
