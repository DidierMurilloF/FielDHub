#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#'     
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]     
#'     
#' @import shiny
#' @noRd
fieldhub_theme <- function() {
  bslib::bs_theme(version = 3, bootswatch = "flatly")
}

#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      theme = fieldhub_theme(),
      do.call(navbarPage, c(
        list(
          title = fieldhub_app_title(),
          tabPanel(
            " Welcome!", icon = icon("home", lib = "glyphicon"),
            suppressWarnings(htmltools::includeHTML(
              system.file("app/www/home.html", package = "FielDHub")
            ))
          )
        ),
        fieldhub_design_menus(),
        list(navbarMenu(
          "More",
          tabPanel(
            "Help",
            suppressWarnings(htmltools::includeHTML(
              system.file("app/www/Help.html", package = "FielDHub")
            ))
          ),
          tabPanel(
            "About Us",
            suppressWarnings(htmltools::includeHTML(
              system.file("app/www/aboutUs.html", package = "FielDHub")
            ))
          )
        ))
      ))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'FielDHub'
    )
  )
}
