#' Shared response dialog retaining each module's input identifiers
#' @noRd
app_simulation_modal <- function(ns, ids, spatial_available = FALSE,
                                  correlation_suffix = ".simulation", heatmap_checkbox = NULL) {
  ids <- simulation_control_ids(ids)
  spatial_id <- simulation_spatial_id(ids)
  shiny::modalDialog(
    shiny::selectInput(ns(ids[["trait"]]), label = "Trait",
                       choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
    shiny::conditionalPanel(paste0("input['", ids[["trait"]], "'] == 'Other'"), ns = ns,
      shiny::textInput(ns(ids[["other"]]), label = "Trait name", value = NULL)),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns(ids[["minimum"]]), "Trait mean", value = 100)),
      shiny::column(6, shiny::numericInput(ns(ids[["maximum"]]), "Plot-to-plot SD",
                                          value = 8, min = 0))
    ),
    if (spatial_available) shiny::tagList(
      shiny::checkboxInput(ns(spatial_id), label = "Add spatial correlation", value = FALSE),
      shiny::conditionalPanel(paste0("input['", spatial_id, "']"), ns = ns,
        app_spatial_correlations(ns, correlation_suffix))
    ),
    if (!is.null(heatmap_checkbox)) {
      shiny::checkboxInput(ns(heatmap_checkbox), label = "Include a heatmap", value = TRUE)
    },
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns(ids[["submit"]]), "Simulate"))
  )
}

#' Shared spatial-page dialog with per-module correlation identifiers
#' @noRd
app_spatial_simulation_modal <- function(ns, spec, spatial_available = TRUE) {
  app_simulation_modal(ns, spec$simulation_ids, spatial_available,
                       spec$correlation_suffix, spec$heatmap_checkbox)
}
