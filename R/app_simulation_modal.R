#' Shared classic-response dialog retaining each module's input identifiers
#' @noRd
app_simulation_modal <- function(ns, ids, failed = FALSE, introduction = NULL) {
  ids <- simulation_control_ids(ids)
  validate_flag(failed, "failed")
  shiny::modalDialog(
    if (!is.null(introduction)) shiny::h4(introduction),
    shiny::selectInput(ns(ids[["trait"]]), label = "Select One:",
                       choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
    shiny::conditionalPanel(paste0("input.", ids[["trait"]], " == 'Other'"), ns = ns,
      shiny::textInput(ns(ids[["other"]]), label = "Trait name:", value = NULL)),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns(ids[["minimum"]]), "Input the min value", value = NULL)),
      shiny::column(6, shiny::numericInput(ns(ids[["maximum"]]), "Input the max value", value = NULL))
    ),
    if (failed) shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns(ids[["submit"]]), "GO"))
  )
}

#' Shared spatial-response dialog with per-module correlation controls
#' @noRd
app_spatial_simulation_modal <- function(ns, spec, failed = FALSE) {
  ids <- simulation_control_ids(spec$simulation_ids)
  validate_flag(failed, "failed")
  shiny::modalDialog(
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(ns(ids[["trait"]]), label = "Select One:",
        choices = c("YIELD", "MOISTURE", "HEIGHT", "Other"))),
      if (!is.null(spec$heatmap_checkbox)) shiny::column(6,
        shiny::checkboxInput(ns(spec$heatmap_checkbox), label = "Include a Heatmap", value = TRUE))
    ),
    shiny::conditionalPanel(paste0("input.", ids[["trait"]], " == 'Other'"), ns = ns,
      shiny::textInput(ns(ids[["other"]]), label = "Trait name:", value = NULL)),
    app_spatial_correlations(ns, spec$correlation_suffix),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns(ids[["minimum"]]), spec$numeric_labels[[1L]], value = NULL)),
      shiny::column(6, shiny::numericInput(ns(ids[["maximum"]]), spec$numeric_labels[[2L]], value = NULL))
    ),
    if (failed) shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
    footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns(ids[["submit"]]), "GO"))
  )
}
