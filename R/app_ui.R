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
    shiny::tags$div(
      id = "fieldhub-app",
      `aria-busy` = "false",
      shiny::fluidPage(
        theme = fieldhub_theme(),
        do.call(shiny::navbarPage, c(
          list(
            title = fieldhub_app_title(),
            shiny::tabPanel(
              " Welcome!", icon = shiny::icon("home", lib = "glyphicon"),
              htmltools::includeHTML(
                system.file("app/www/home.html", package = "FielDHub")
              )
            )
          ),
          fieldhub_design_menus(),
          list(shiny::navbarMenu(
            "More",
            shiny::tabPanel("Help", app_help_ui()),
            shiny::tabPanel("About Us", app_about_ui())
          ))
        ))
      )
    ),
    # Keep announcements outside the busy region so they are not deferred.
    shiny::tags$div(id = "fieldhub-status", role = "status", `aria-live` = "polite",
                    `aria-atomic` = "true")
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
    golem::activate_js(),
    htmltools::htmlDependency(
      name = "fieldhub-resources",
      version = as.character(utils::packageVersion("FielDHub")),
      src = app_sys("app/www"),
      script = c("corner.js", "shinybusy.js"),
      stylesheet = c("style.css", "mobile.css"),
      all_files = TRUE
    ),
    shiny::tags$title("FielDHub")
  )
}
