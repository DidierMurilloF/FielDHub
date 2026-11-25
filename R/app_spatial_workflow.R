#' Shared lifecycle for spatial-design results
#'
#' Design generation remains an injected service. This component
#' owns simulation controls, heatmaps, tables, exports, and reproduction.
#' @noRd
app_spatial_workflow <- function(input, output, session, design, seed,
                                  selected, visible, simulation_ready,
                                  book_ready, spec) {
  settings <- app_simulation_controls(input, session,
    ids = spec$simulation_ids, field_book = function() design()$fieldBook,
    correlation_suffix = spec$correlation_suffix)
  shiny::observeEvent(input[[spec$ids[["simulate"]]]], {
    if (isTRUE(simulation_ready())) {
      shiny::showModal(app_spatial_simulation_modal(session$ns, spec,
        spatial_available = all(c("ROW", "COLUMN") %in% names(design()$fieldBook))), session = session)
    }
  })
  book <- shiny::reactive({
    book_ready()
    field_book <- design()$fieldBook
    if (spec$coerce_book && !is.null(settings())) field_book <- as.data.frame(field_book)
    validate_design(app_trait_simulation(design(), field_book, settings(),
      workflow_seed(seed(), design()), renumber_display = spec$renumber_display))
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
    shiny::req(book()$simulation)
    if (!is.null(spec$heatmap_checkbox)) shiny::req(input[[spec$heatmap_checkbox]])
    trait <- settings()$trait
    data <- validate_design(field_book_heatmap_data(book()$df, trait, selected(),
      label_column = "ENTRY", label_title = "Entry", include_site = FALSE))
    app_tile_heatmap(data, trait, spec$heatmap_height,
      title = if (spec$heatmap_title) paste("Heat map for", trait))
  })
  app_plot_outputs(input, output, session,
    panel = list(id = spec$ids[["heatmap"]], title = "Heatmap"),
    plot = heatmap, design = design, location = selected,
    ready = function() inherits(app_design_state(heatmap), "ggplot"),
    csv_data = function() app_plot_grid_data(heatmap(), as.character(settings()$trait)),
    kind = function() "heatmap")
  output[[spec$ids[["download"]]]] <- app_csv_download(
    filename = function() csv_export_filename(design()),
    data = function() as.data.frame(book()$df)
  )
  app_reproduction_outputs(output, design)
  invisible(list(book = book, settings = settings))
}
