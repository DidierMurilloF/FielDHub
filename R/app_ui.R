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
    app_add_external_resources(),
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
              app_home_ui()
            )
          ),
          app_design_menus(),
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
app_add_external_resources <- function(){
  scripts <- c("corner.js", "shinybusy.js", "task-feedback.js", "output-feedback.js", "layout-images.js",
               "home-links.js", "theme.js", "layout-menu.js", "design-summary.js")
  stylesheets <- c("style.css", "mobile.css", "design.css", "look-b.css", "look-c.css", "look-d.css")
  
  shiny::addResourcePath(
    'www', app_sys('app/www')
  )
 
  shiny::tags$head(
    # Set the colour theme before first paint (see theme.js)
    shiny::tags$script(shiny::HTML(paste0(
      "(function(){var t;try{t=localStorage.getItem('fieldhub-theme')}catch(e){}",
      "var q=/[?&]theme=(dark|light)/.exec(location.search);if(q)t=q[1];",
      "if(t!=='dark'&&t!=='light')t=window.matchMedia&&matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light';",
      "var r=document.documentElement;r.setAttribute('data-theme',t);",
      # Prototype only: which of the looks (a, b, c or d)
      "var k;try{k=localStorage.getItem('fieldhub-look')}catch(e){}",
      "q=/[?&]look=([abcd])/.exec(location.search);if(q)k=q[1];",
      "r.setAttribute('data-look',/^[abcd]$/.test(k)?k:'a')})();"))),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon.ico"),
    htmltools::htmlDependency(
      name = "fieldhub-resources",
      version = app_resources_version(c(scripts, stylesheets)),
      src = app_sys("app/www"),
      script = scripts,
      stylesheet = stylesheets,
      all_files = TRUE
    ),
    shiny::tags$title("FielDHub")
  )
}

#' Version of the app's own scripts and styles, for the browser cache
#'
#' The package version plus a fingerprint of the files' contents: the
#' resources are served under a path with this version, so a browser fetches
#' them again whenever a file changes, not only on a new release.
#' @param files File names in \code{app/www}.
#' @return A version string such as \code{"1.7.0.123456789"}.
#' @noRd
app_resources_version <- function(files) {
  sums <- tools::md5sum(file.path(app_sys("app/www"), files))
  combined <- tempfile()
  on.exit(unlink(combined), add = TRUE)
  writeLines(unname(sums), combined)
  fingerprint <- unname(tools::md5sum(combined))
  paste0(utils::packageVersion("FielDHub"), ".", strtoi(substr(fingerprint, 1L, 7L), 16L))
}
