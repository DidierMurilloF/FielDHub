# Plain layout services and static component bindings; no Shiny sessions.
test_that("checked layout views reject unavailable choices instead of returning NULL", {
  design <- RCBD(t = 6, reps = 3, seed = 7)
  for (selection in list(NULL, NA_real_, NaN, Inf, -1, 0, 1.5, "1", c(1, 2), 100)) {
    expect_error(checked_layout_view(design, layout = selection), class = "fieldhub_input_error")
  }
  expect_error(checked_layout_view(design, stacked = "grid_panel"), class = "fieldhub_input_error")
  expect_error(checked_layout_view(design, location = 2), class = "fieldhub_input_error")
  expect_error(checked_layout_view(design, location = NA_real_), class = "fieldhub_input_error")
  expect_error(checked_layout_view(design, planter = "other"), class = "fieldhub_input_error")
  expect_error(checked_layout_view(NULL), class = "fieldhub_input_error")
})

test_that("checked views retain complete layout field books and reconstruction settings", {
  design <- RCBD(t = 6, reps = 4, l = 2, plotNumber = c(101, 1001), seed = 8)
  set.seed(416)
  before <- .Random.seed
  for (stacked in c("vertical", "horizontal")) {
    view <- checked_layout_view(design, planter = "cartesian", stacked = stacked, location = 2)
    expect_identical(view$allSitesFieldbook, field_layout(design, planter = "cartesian", stacked = stacked))
    expect_identical(view$layout_metadata,
                     list(parameters = list(layout = 1, planter = "cartesian", stacked = stacked), selected = 2))
    expect_true(all(as.character(view$fieldBookXY$LOCATION) == "loc2"))
    expect_silent(ggplot2::ggplot_build(view$out_layout))
    expect_silent(ggplot2::ggplot_build(view$out_layoutPlots))
  }
  expect_identical(.Random.seed, before)
})

test_that("classic modules delegate layout selection and stop swallowing plot errors", {
  find_binding <- function(code) {
    if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(list())
    if (is.call(code) && identical(code[[1]], as.name("app_classic_layout"))) return(list(code))
    unlist(lapply(as.list(code), find_binding), recursive = FALSE)
  }
  modules <- names(fieldhub_classic_workflows())
  for (module in modules) {
    code <- design_server_body(module)
    expect_identical(sum(all.names(code) == "app_classic_layout"), 1L)
    expect_false("plot_layout" %in% all.names(code))
    expect_false("reset_selection" %in% all.names(code))
    binding <- find_binding(code)
    expect_length(binding, 1L)
    # the generic page hands its own workflow spec to the layout lifecycle
    expect_identical(binding[[1L]][["spec"]], as.name("workflow"))
    ids <- design_app_spec(module)$workflow$layout$ids
    expect_identical(anyDuplicated(ids), 0L)
    expected <- c("layout", if (!module %in% c("CRD", "Latin_Rectangle")) "stacked",
                  if (!module %in% c("CRD", "LSD")) "location")
    expect_identical(names(ids), expected)
  }
})

test_that("selecting a grid in Shiny sends one field layout to plots and downloads", {
  design <- RCBD(t = 6, reps = 6, l = 2, plotNumber = c(101, 1001), seed = 1)
  server <- function(input, output, session) {
    view <- app_layout_selection(input, session, function() design, function() "cartesian",
                                 c(layout = "layout", stacked = "stacked", location = "location"))
  }
  shiny::testServer(server, {
    session$setInputs(stacked = "vertical", layout = "1", location = "2")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, layout = 1, planter = "cartesian"))
    session$setInputs(layout = "grid:4:9")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, nrows = 4, ncols = 9, planter = "cartesian"))
    expect_true(all(as.character(view()$fieldBookXY$LOCATION) == "loc2"))
    expect_identical(view()$layout_metadata,
                     list(parameters = list(planter = "cartesian", nrows = 4L, ncols = 9L),
                          selected = 2))
    expect_silent(ggplot2::ggplot_build(view()$out_layout))
    session$setInputs(stacked = "horizontal")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, layout = as.numeric(layout_choice_labels(
                       design, planter = "cartesian", stacked = "horizontal")[1L]),
                       planter = "cartesian", stacked = "horizontal"))
    session$setInputs(layout = "grid:4:9")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, nrows = 4, ncols = 9, planter = "cartesian"))
  })
})

test_that("field grids of every classic design reach the Shiny layout view", {
  designs <- list(
    CRD = CRD(t = 4, reps = 3, seed = 1),
    LSD = latin_square(t = 3, reps = 4, seed = 1),
    FD = full_factorial(setfactors = c(2, 3), reps = 2, l = 2, type = 2, seed = 9),
    SPD = split_plot(wp = 3, sp = 2, reps = 2, seed = 11),
    SSPD = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, seed = 13),
    STRIPD = strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 15),
    IBD = incomplete_blocks(t = 12, k = 4, reps = 2, seed = 17),
    Alpha = alpha_lattice(t = 12, k = 4, reps = 2, seed = 19),
    Square = square_lattice(t = 9, k = 3, reps = 2, seed = 20),
    Rectangular = rectangular_lattice(t = 12, k = 3, reps = 2, seed = 21)
  )
  current <- NULL
  server <- function(input, output, session) {
    view <- app_layout_selection(input, session, function() current, function() "serpentine",
                                 c(layout = "layout", stacked = "stacked", location = "location"))
  }
  for (name in names(designs)) {
    current <- designs[[name]]
    grid <- layout_choice_labels(current, include_grids = TRUE)[["Field grid"]][[1L]]
    dims <- as.numeric(strsplit(grid, ":", fixed = TRUE)[[1L]][-1L])
    shiny::testServer(server, {
      session$setInputs(stacked = "vertical", layout = "1", location = "1")
      expect_identical(view()$allSitesFieldbook, field_layout(current), info = name)
      session$setInputs(layout = grid)
      book <- field_layout(current, nrows = dims[1L], ncols = dims[2L])
      expect_identical(view()$allSitesFieldbook, book, info = name)
      expect_identical(dim(table(view()$fieldBookXY$ROW, view()$fieldBookXY$COLUMN)),
                       as.integer(dims), info = name)
      expect_silent(ggplot2::ggplot_build(view()$out_layout))
      expect_silent(ggplot2::ggplot_build(view()$out_layoutPlots))
      # A grid that does not fit this design is a message, not a crash
      session$setInputs(layout = "grid:7:7")
      expect_error(view(), class = "shiny.silent.error", info = name)
      session$setInputs(layout = "1")
      expect_identical(view()$allSitesFieldbook, field_layout(current), info = name)
    })
  }
})

test_that("a design built on a field grid shows its grid under every Shiny selection", {
  design <- RCBD(t = 6, reps = 6, l = 2, plotNumber = c(101, 1001), seed = 1, nrows = 4)
  planter <- "serpentine"
  server <- function(input, output, session) {
    view <- app_layout_selection(input, session, function() design, function() planter,
                                 c(layout = "layout", stacked = "stacked", location = "location"))
  }
  for (planter in c("serpentine", "cartesian")) {
    shiny::testServer(server, {
      for (stacked in c("vertical", "horizontal")) {
        session$setInputs(stacked = stacked, layout = "1", location = "2")
        expect_identical(view()$allSitesFieldbook, design$fieldBook)
        expect_silent(ggplot2::ggplot_build(view()$out_layout))
      }
    })
  }
})
