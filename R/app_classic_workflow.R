#' Shared lifecycle for classic-design results
#'
#' One registry-driven workflow owns accepted simulation settings, rendering,
#' tables, metadata-bearing downloads, and reproduction. Design generation and
#' layout selection are injected services; scientific work stays in plain R.
#' @noRd
app_classic_workflow <- function(input, output, session, design, layout, seed,
                                  selected, spec, simulation_ready) {
  field_book <- shiny::reactive({
    view <- layout()
    shiny::req(view)
    view[[spec$book_component]]
  })
  settings <- app_simulation_controls(input, session,
    ids = spec$simulation_ids, field_book = field_book)
  shiny::observeEvent(input[[spec$ids[["simulate"]]]], {
    simulation_ready()
    shiny::showModal(app_simulation_modal(session$ns, ids = spec$simulation_ids),
                     session = session)
  })
  book <- shiny::reactive({
    shiny::req(field_book())
    validate_design(classic_workflow_book(field_book(), settings(), seed(), spec$order_by_id))
  })
  heatmap <- shiny::reactive({
    data <- book()$df
    shiny::req(data)
    response <- as.character(settings()$response_name)
    if (length(response) == 1L && response %in% names(data)) {
      validate_design(do.call(app_field_heatmap,
        c(list(field_book = data, response_name = response, selected = selected()), spec$heatmap)))
    } else {
      shiny::showModal(shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"), easyClose = TRUE
      ), session = session)
      NULL
    }
  })
  output[[spec$ids[["plot"]]]] <- plotly::renderPlotly({
    shiny::req(design(), input[[spec$ids[["plot_type"]]]])
    type <- input[[spec$ids[["plot_type"]]]]
    if (type == 1) {
      layout()$out_layout
    } else if (type == 2) {
      layout()$out_layoutPlots
    } else {
      shiny::req(heatmap())
      heatmap()
    }
  })
  render_table <- if (identical(spec$table_renderer, "renderDT")) DT::renderDT else DT::renderDataTable
  output[[spec$ids[["table"]]]] <- render_table({
    columns <- spec$table_columns
    if (spec$table_extensions) columns <- c(columns, field_book_extension_columns(design()))
    validate_design(app_field_book_table(book()$df, factor_columns = columns,
                                          height = spec$table_height))
  })
  layout_data <- shiny::reactive({
    shiny::req(book()$df, input[[spec$ids[["plot_type"]]]])
    validate_design(classic_workflow_layout(book()$df, selected(),
                                            input[[spec$ids[["plot_type"]]]], spec))
  })
  output[[spec$ids[["field_book_download"]]]] <- app_csv_archive(
    filename = function() paste0(spec$export_prefixes[["field_book"]], Sys.Date(), ".csv"),
    data = function() as.data.frame(book()$df), design = design,
    field_book = function() book()$df, simulation = function() book()$simulation,
    layout = function() layout()$layout_metadata, kind = "field_book"
  )
  output[[spec$ids[["layout_download"]]]] <- app_csv_archive(
    filename = function() paste0(spec$export_prefixes[["layout"]], Sys.Date(), ".csv"),
    data = function() as.data.frame(layout_data()$file), design = design,
    field_book = function() book()$df, simulation = function() book()$simulation,
    layout = function() layout()$layout_metadata, kind = "layout"
  )
  app_reproduction_outputs(output, design)
  invisible(list(book = book, settings = settings, layout_data = layout_data))
}
