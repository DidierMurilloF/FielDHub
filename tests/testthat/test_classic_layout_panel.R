# Plain choice calculations, HTML construction, and static bindings only.
test_that("replicate stacking choices preserve the supported lattice grid rule", {
  base <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal")
  for (reps in seq_len(100)) {
    expected <- base
    if (reps >= 4 && (reps %% 2 == 0 || sqrt(reps) %% 1 == 0)) {
      expected <- c(expected, "Grid Panel" = "grid_panel")
    }
    expect_identical(classic_stacking_choices(reps, grid = TRUE), expected)
    expect_identical(classic_stacking_choices(reps), base)
  }
  for (bad in list(NULL, NA_real_, NaN, Inf, 0, -1, 1.5, "4", c(4, 6), matrix(4))) {
    expect_error(classic_stacking_choices(bad, grid = TRUE), class = "fieldhub_input_error")
  }
  expect_identical(classic_stacking_choices(stop("unused replication count")), base)
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
    expect_identical(spec$layout$grid,
                     module %in% c("Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice"))
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
    expect_identical(grepl("Grid Panel", html, fixed = TRUE), spec$layout$grid)
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
  design <- RCBD(t = 6, reps = 6, seed = 1)
  numbered <- layout_choice_labels(design)
  choices <- layout_choice_labels(design, include_grids = TRUE)
  expect_identical(choices[seq_along(numbered)], as.list(numbered))
  expect_identical(names(choices)[-seq_along(numbered)], "Field grid")
  expect_identical(choices[["Field grid"]][["Grid 4 \u00d7 9"]], "grid:4:9")

  spec <- design_app_spec("RCBD")$workflow
  html <- as.character(app_classic_layout_panel(identity, spec, design))
  for (i in seq_along(numbered)) {
    expect_true(grepl(paste0('<option value="', numbered[[i]], '"',
                             if (i == 1L) " selected", ">", names(numbered)[i], "</option>"),
                      html, fixed = TRUE))
  }
  expect_true(grepl('<optgroup label="Field grid">', html, fixed = TRUE))
  expect_true(grepl('<option value="grid:4:9">Grid 4 \u00d7 9</option>', html, fixed = TRUE))

  # Two Latin squares fit only 4 x 8 and 8 x 4, the shapes of the two
  # stackings: no field grid is offered, and the menu is the numbered one
  squares <- latin_square(t = 4, reps = 2, seed = 1)
  for (stacked in c("vertical", "horizontal")) {
    expect_identical(layout_choice_labels(squares, stacked = stacked, include_grids = TRUE),
                     layout_choice_labels(squares, stacked = stacked))
  }

  stored <- RCBD(t = 6, reps = 6, nrows = 4, ncols = 9, seed = 1)
  expect_identical(layout_choice_labels(stored, include_grids = TRUE), layout_choice_labels(stored))
})

test_that("the field grids of a menu never repeat a stacking's layouts", {
  # Regression test: the grid group listed the field shapes of the other
  # stackings, so a vertical menu offered the horizontal shapes as grids,
  # numbered differently. A grid is now only a shape no stacking offers.
  designs <- list(
    RCBD(t = 6, reps = 4, seed = 1),
    RCBD(t = 6, reps = 6, seed = 1),
    CRD(t = 6, reps = 4, seed = 1),
    full_factorial(setfactors = c(2, 3), reps = 4, type = 2, seed = 1),
    latin_square(t = 4, reps = 2, seed = 7),
    split_plot(wp = 3, sp = 2, reps = 4, seed = 1),
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 4, seed = 1),
    strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 1),
    alpha_lattice(t = 12, k = 4, reps = 4, seed = 1),
    incomplete_blocks(t = 12, k = 4, reps = 4, seed = 1),
    square_lattice(t = 16, k = 4, reps = 4, seed = 1),
    rectangular_lattice(t = 12, k = 3, reps = 4, seed = 1)
  )
  shape <- function(labels) sub(" · .*| \\([0-9]+\\)$", "", sub("^Grid ", "", labels))
  for (design in designs) {
    for (planter in c("serpentine", "cartesian")) {
      numbered <- character()
      grids <- list()
      for (stacked in c("vertical", "horizontal", "grid_panel")) {
        choices <- layout_choice_labels(design, planter = planter, stacked = stacked,
                                        include_grids = TRUE)
        group <- if (is.list(choices)) choices[["Field grid"]]
        numbered <- c(numbered, shape(setdiff(names(choices), "Field grid")))
        for (label in names(group)) {
          # A grid label gives one field book whatever the stacking
          book <- app_selected_layout_view(design, group[[label]], planter, 1,
                                           stacked)$allSitesFieldbook
          if (is.null(grids[[label]])) grids[[label]] <- book
          expect_identical(book, grids[[label]], info = paste(class(design)[1L], label))
        }
      }
      expect_length(intersect(shape(names(grids)), numbered), 0L)
    }
  }
  # The RCBD of the report: four reps of six plots stack as 4 x 6 or 6 x 4,
  # neither is a grid; 1 x 24 and 24 x 1 are offered once each
  rcbd <- RCBD(t = 6, reps = 4, seed = 1)
  for (stacked in c("vertical", "horizontal")) {
    group <- layout_choice_labels(rcbd, stacked = stacked, include_grids = TRUE)[["Field grid"]]
    expect_false(any(c("Grid 6 × 4", "Grid 4 × 6", "Grid 3 × 8",
                       "Grid 8 × 3") %in% names(group)), info = stacked)
  }
})
