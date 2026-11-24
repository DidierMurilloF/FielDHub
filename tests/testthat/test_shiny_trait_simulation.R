test_that("classic and spatial Shiny workflows export the package simulation", {
  classic <- classic_workflow_spec("RCBD")
  spatial <- spatial_workflow_spec("Diagonal")
  classic_design <- RCBD(6, 3, nrows = 3, seed = 12)
  spatial_design <- diagonal_arrangement(15, 20, 270, 4, seed = 12)
  view <- checked_layout_view(classic_design, layout = 1, location = 1)
  expected <- function(design) {
    stats::simulate(design, seed = 12, mean = 6, residual_sd = 0.4, spatial = TRUE)$field_book
  }
  simulation_inputs <- function(spec) {
    ids <- spec$simulation_ids
    stats::setNames(list("YIELD", 6, 0.4, TRUE, 1L),
      c(ids["trait"], ids["minimum"], ids["maximum"], simulation_spatial_id(ids), ids["submit"]))
  }

  shiny::testServer(function(input, output, session) {
    state <- app_classic_workflow(input, output, session,
      design = function() classic_design, layout = function() view,
      seed = function() 12, selected = function() 1L, spec = classic,
      simulation_ready = function() TRUE)
  }, {
    session$flushReact()
    do.call(session$setInputs, c(simulation_inputs(classic),
      stats::setNames(list("3"), classic$ids[["plot_type"]])))
    reference <- expected(classic_design)
    book <- state$book()$df
    expect_identical(book$YIELD, reference$YIELD[match(book$ID, reference$ID)])
    expect_true(is.data.frame(state$layout_data()$file))
    expect_true(file.exists(output[[classic$ids[["field_book_download"]]]]))
  })

  shiny::testServer(function(input, output, session) {
    state <- app_spatial_workflow(input, output, session,
      design = function() spatial_design, seed = function() 12,
      selected = function() 1L, visible = function() TRUE,
      simulation_ready = function() TRUE, book_ready = function() TRUE, spec = spatial)
  }, {
    session$flushReact()
    do.call(session$setInputs, simulation_inputs(spatial))
    expect_identical(state$book()$df$YIELD, expected(spatial_design)$YIELD)
    expect_true(file.exists(output[[spatial$ids[["download"]]]]))
    expect_true(file.exists(output[[paste0(spatial$ids[["heatmap"]], "_csv")]]))
  })
})
