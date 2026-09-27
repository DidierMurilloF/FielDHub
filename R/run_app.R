#' Run the Shiny Application
#'
#' @details Constructing the app does not change R options. On startup,
#' uploads are limited to 100 MiB (100 * 1024^2 bytes); when the app stops,
#' the previous upload-limit option is restored, including an unset option.
#' Shiny uses a process-wide upload limit, shared by the app's sessions while
#' it is running. Closing one session does not restore the limit for others.
#'
#' The app packages are optional for R-script users. If any are unavailable,
#' this function signals a \code{fieldhub_dependency_error} with an installation
#' command and the missing package names in its \code{packages} field. It never
#' installs packages automatically. See the README for the full app package list.
#'
#' @return A shiny app object
#' @param ... Unused, for extensibility
#' @param launch.browser Logical. If `TRUE`, the application is launched in the system's default web browser.
#'
#' @export

run_app <- function(
  ...,
  launch.browser = TRUE
) {
  check_app_dependencies()
  golem::with_golem_options(
    app = shiny::shinyApp(
      options = list(launch.browser = launch.browser),
      onStart = app_upload_limit,
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list()
  )
}
