#' Run the Shiny Application
#'
#' @details Constructing the app does not change R options. On startup,
#' uploads are limited to 100 MiB (100 * 1024^2 bytes); when the app stops,
#' the previous upload-limit option is restored, including an unset option.
#' Shiny uses a process-wide upload limit, shared by the app's sessions while
#' it is running. Closing one session does not restore the limit for others.
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
  with_golem_options(
    app = shinyApp(
      options = list(launch.browser = launch.browser),
      onStart = app_upload_limit,
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list()
  )
}
