#' Registry-driven classic layout controls without a reactive session
#' @noRd
app_classic_layout_panel <- function(ns, spec, x, planter = "serpentine", locations = NULL) {
  layout <- spec$layout
  choices <- layout_choices(x, planter = planter, stacked = "vertical")
  controls <- list(shiny::radioButtons(ns(spec$ids[["plot_type"]]), "Type of Plot:",
    c("Entries/Treatments" = 1, "Plots" = 2, "Heatmap" = 3)))
  if ("stacked" %in% names(layout$ids)) {
    stacking <- classic_stacking_choices(length(levels(as.factor(x$fieldBook$REP))), grid = layout$grid)
    controls <- c(controls, list(shiny::selectInput(ns(layout$ids[["stacked"]]), "Reps layout:", stacking)))
  }
  controls <- c(controls, list(shiny::selectInput(ns(layout$ids[["layout"]]), "Layout option:", choices,
    selected = if (layout$select_layout) 1 else NULL)))
  if ("location" %in% names(layout$ids)) {
    if (is.null(locations)) locations <- seq_len(length(levels(as.factor(x$fieldBook$LOCATION))))
    controls <- c(controls, list(shiny::selectInput(ns(layout$ids[["location"]]), "Location:", locations,
      selected = if (layout$select_location) 1 else NULL)))
  }
  columns <- Map(function(width, control) shiny::column(width, control), layout$widths, controls)
  if (layout$container == "page") {
    shiny::wellPanel(do.call(shiny::fluidPage, columns))
  } else if (layout$container == "row") {
    shiny::wellPanel(do.call(shiny::fluidRow, columns))
  } else {
    shiny::wellPanel(columns[[1L]], do.call(shiny::fluidRow, columns[-1L]))
  }
}

#' Shared panel and selection lifecycle for classic design modules
#' @noRd
app_classic_layout <- function(input, output, session, design, planter, spec,
                                locations = function() NULL) {
  output[[spec$layout$output]] <- shiny::renderUI({
    shiny::req(design()$fieldBook)
    validate_design(app_classic_layout_panel(session$ns, spec, design(),
      planter = if (spec$layout$current_planter) planter() else "serpentine", locations = locations()))
  })
  app_layout_selection(input, session, design = design, planter = planter, ids = spec$layout$ids)
}
