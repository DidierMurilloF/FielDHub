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
    if (is.call(code) && identical(code[[1]], as.name("app_layout_selection"))) return(list(code))
    unlist(lapply(as.list(code), find_binding), recursive = FALSE)
  }
  modules <- c("CRD", "RCBD", "LSD", "FD", "SPD", "SSPD", "STRIPD", "IBD",
               "RowCol", "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")
  for (module in modules) {
    code <- body(get(paste0("mod_", module, "_server"), asNamespace("FielDHub")))
    expect_identical(sum(all.names(code) == "app_layout_selection"), 1L)
    expect_false("plot_layout" %in% all.names(code))
    expect_false("reset_selection" %in% all.names(code))
    binding <- find_binding(code)
    expect_length(binding, 1L)
    ids <- eval(binding[[1L]][["ids"]])
    expect_identical(anyDuplicated(ids), 0L)
    expected <- c("layout", if (module != "CRD") "stacked",
                  if (!module %in% c("CRD", "LSD")) "location")
    expect_identical(names(ids), expected)
  }
})
