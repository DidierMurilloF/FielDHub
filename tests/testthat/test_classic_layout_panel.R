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
    expected <- c("layout", if (module != "CRD") "stacked",
                  if (!module %in% c("CRD", "LSD")) "location")
    expect_identical(names(ids), expected, info = module)
    expect_identical(anyDuplicated(c(spec$ids, spec$simulation_ids, ids, spec$layout$output)), 0L)
    expect_true(all(spec$layout$widths %in% 1:12))
    expect_identical(spec$layout$grid,
                     module %in% c("Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice"))
    code <- body(get(paste0("mod_", module, "_server"), asNamespace("FielDHub")))
    expect_identical(sum(all.names(code) == "app_classic_layout"), 1L, info = module)
    expect_false(any(c("app_layout_selection", "layout_choices", "sqrt") %in% all.names(code)), info = module)
  }
  binding <- body(app_classic_layout)
  expect_identical(sum(all.names(binding) == "app_layout_selection"), 1L)
  expect_identical(sum(all.names(binding) == "app_classic_layout_panel"), 1L)
})

test_that("shared layout panels retain control labels, namespacing, and options", {
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
    for (label in c("Type of Plot:", "Entries/Treatments", "Plots", "Heatmap", "Layout option:")) {
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
