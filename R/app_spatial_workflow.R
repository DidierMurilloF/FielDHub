#' Shared lifecycle for spatial-design results
#'
#' Design generation and dimensions remain injected services. This component
#' owns simulation controls, heatmaps, tables, exports, and reproduction.
#' @noRd
app_spatial_workflow <- function(input, output, session, design, seed, dimensions,
                                  selected, visible, simulation_ready,
                                  book_ready, spec) {
  settings <- app_simulation_controls(input, session,
    ids = spec$simulation_ids, field_book = function() design()$fieldBook,
    correlation_ids = spec$correlation_ids)
  shiny::observeEvent(input[[spec$ids[["simulate"]]]], {
    if (isTRUE(simulation_ready())) {
      shiny::showModal(app_spatial_simulation_modal(session$ns, spec), session = session)
    }
  })
  book <- shiny::reactive({
    book_ready()
    field_book <- design()$fieldBook
    shape <- if (!is.null(settings())) dimensions(field_book)
    validate_design(spatial_workflow_book(field_book, settings(), shape$nrows, shape$ncols,
      workflow_seed(seed(), design()), renumber_display = spec$renumber_display, coerce_book = spec$coerce_book))
  })
  heatmap_state <- shiny::reactiveValues(visible = FALSE)
  shiny::observeEvent(settings(), {
    heatmap_state$visible <- TRUE
  })
  shiny::observeEvent(heatmap_state$visible, {
    if (heatmap_state$visible) {
      shiny::showTab(inputId = spec$ids[["tabset"]], target = "Heatmap", session = session)
    } else {
      shiny::hideTab(inputId = spec$ids[["tabset"]], target = "Heatmap", session = session)
    }
  })
  output[[spec$ids[["table"]]]] <- DT::renderDT({
    if (!visible()) return(NULL)
    shiny::req(book()$df)
    validate_design(app_field_book_table(book()$df, factor_columns = spec$table_columns,
      height = spec$table_height, collapse = spec$table_collapse))
  })
  heatmap <- shiny::reactive({
    app_plot_state(if (visible()) app_design_state(design), settings(), "heatmap")
    shiny::req(book()$simulation$simulations)
    if (!is.null(spec$heatmap_checkbox)) shiny::req(input[[spec$heatmap_checkbox]])
    validate_design(app_spatial_heatmap(book()$simulation$simulations,
      response_name = as.character(settings()$response_name), selected = selected(),
      height = spec$heatmap_height, show_title = spec$heatmap_title))
  })
  app_plot_outputs(input, output, session,
    panel = list(id = spec$ids[["heatmap"]], title = "Heatmap"),
    plot = heatmap, design = design, location = selected,
    ready = function() inherits(app_design_state(heatmap), "ggplot"),
    csv_data = function() app_plot_grid_data(heatmap(), as.character(settings()$response_name)),
    kind = function() "heatmap")
  output[[spec$ids[["download"]]]] <- app_csv_download(
    filename = function() csv_export_filename(design()),
    data = function() as.data.frame(book()$df)
  )
  app_reproduction_outputs(output, design)
  invisible(list(book = book, settings = settings))
}
