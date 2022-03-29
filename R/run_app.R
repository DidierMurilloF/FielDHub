#' Run the Shiny Application
#'
#' @return A shiny app object
#' @param ... Unused, for extensibility
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  options(shiny.maxRequestSize=100*1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(
      launch.browser = TRUE
      #print('Hi, share and enjoy')
    )
  )
}
