#' One loading appearance for tasks and individual outputs
#' @param message Description of the work in progress.
#' @noRd
app_loading_indicator <- function(message) {
  shiny::tagList(
    shiny::icon("spinner", class = "fa-spin fieldhub-task-spinner"),
    shiny::div(class = "fieldhub-task-message", message))
}

#' Tab-local output feedback driven by Shiny's browser events
#'
#' @param ui_element The output to wrap, retaining its dimensions while busy.
#' @noRd
app_output_feedback <- function(ui_element) {
  shiny::div(class = "fieldhub-output-loader", `aria-busy` = "false",
    shiny::div(class = "fieldhub-output-content", ui_element),
    shiny::div(class = "fieldhub-output-feedback", hidden = "hidden",
      role = "status", `aria-live` = "polite", `aria-atomic` = "true",
      app_loading_indicator("Loading results...")))
}
