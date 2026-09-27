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
#' @noRd
fieldhub_theme <- function() {
  bslib::bs_theme(version = 3, bootswatch = "flatly")
}

#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      theme = fieldhub_theme(),
      do.call(shiny::navbarPage, c(
        list(
          title = fieldhub_app_title(),
          shiny::tabPanel(
            " Welcome!", icon = shiny::icon("home", lib = "glyphicon"),
            suppressWarnings(htmltools::includeHTML(
              system.file("app/www/home.html", package = "FielDHub")
            ))
          )
        ),
        fieldhub_design_menus(),
        list(shiny::navbarMenu(
          "More",
          shiny::tabPanel(
            "Help",
            suppressWarnings(htmltools::includeHTML(
              system.file("app/www/Help.html", package = "FielDHub")
            ))
          ),
          shiny::tabPanel(
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
#' @noRd
golem_add_external_resources <- function(){
  
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
 
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'FielDHub'
    )
  )
}
