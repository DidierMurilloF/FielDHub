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
