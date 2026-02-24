#' Run the Shiny Application
#'
#' @return A shiny app object
#' @param ... Unused, for extensibility
#' @param launch.browser Logical. If `TRUE`, the application is launched in the system's default web browser.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_app <- function(
  ...,
  launch.browser = TRUE
) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  with_golem_options(
    app = shinyApp(
      options = list(launch.browser = launch.browser),
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list()
  )
}
