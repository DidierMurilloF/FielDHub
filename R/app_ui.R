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
app_ui <- function(request) {
  options(spinner.color="#2c7da3", spinner.color.background="#ffffff", spinner.size = 2)
  tagList(
    golem_add_external_resources(),
    fluidPage(theme = shinythemes::shinytheme("flatly"),
              navbarPage(title = "FielDHub v1.4.2", 
                         tabPanel(
                           " Welcome!", icon = icon("home", lib = "glyphicon"),
                            suppressWarnings(
                              htmltools::includeHTML(
                                system.file("app/www/home.html", package = "FielDHub")
                              )
                            )
                         ),
                         navbarMenu("Unreplicated Designs",
                                    tabPanel("Single Diagonal Arrangement",
                                             mod_Diagonal_ui("Diagonal_ui_1")
                                    ),
                                    tabPanel("Multiple Diagonal Arrangement",
                                             mod_diagonal_multiple_ui("diagonal_multiple_ui_1")
                                    ),
                                    tabPanel("Optimized Arrangement",
                                             mod_Optim_ui("Optim_ui_1")
                                    ),
                                    tabPanel("Augmented RCBD",
                                             mod_RCBD_augmented_ui("RCBD_augmented_ui_1")
                                    ),
                                    tabPanel("New - Sparse Allocation",
                                             mod_sparse_allocation_ui("sparse_allocation_ui_1")
                                    )
                         ),
                         navbarMenu("Partially Replicated Designs",
                            tabPanel("Single and Multi-Location p-rep",
                                    mod_pREPS_ui("pREPS_ui_1")
                            ),
                            tabPanel("New - Optimized Multi-Location p-rep",
                                    mod_multi_loc_preps_ui("multi_loc_preps_ui_1")
                            )
                         ),
                         navbarMenu("Lattice Designs",
                                    tabPanel("Square Lattice",
                                             mod_Square_Lattice_ui("Square_Lattice_ui_1")
                                    ),
                                    tabPanel("Rectangular Lattice",
                                             mod_Rectangular_Lattice_ui("Rectangular_Lattice_ui_1")
                                    ),
                                    tabPanel("Alpha Lattice: alpha(0,1)",
                                             mod_Alpha_Lattice_ui("Alpha_Lattice_ui_1")
                                    )
                         ),
                         navbarMenu("Other Designs",
                                    tabPanel("Completely Randomized Design (CRD)",
                                             mod_CRD_ui("CRD_ui_1")
                                    ),
                                    tabPanel("Randomized Complete Block Designs (RCBD)",
                                             mod_RCBD_ui("RCBD_ui_1")
                                    ),
                                    tabPanel("Latin Square Design (LSD)",
                                             mod_LSD_ui("LSD_ui_1")
                                    ),
                                    tabPanel("Factorial Designs",
                                             mod_FD_ui("FD_ui_1")
                                    ),
                                    tabPanel("Split-Plot Design",
                                             mod_SPD_ui("SPD_ui_1")
                                    ),
                                    tabPanel("Split-Split-Plot Design",
                                             mod_SSPD_ui("SSPD_ui_1")
                                    ),
                                    tabPanel("Strip-Plot Design",
                                             mod_STRIPD_ui("STRIPD_ui_1")
                                    ),
                                    tabPanel("Incomplete Blocks Design (IBD)",
                                             mod_IBD_ui("IBD_ui_1")
                                    ),
                                    tabPanel("Resolvable Row-Column Design (RRCD)",
                                             mod_RowCol_ui("RowCol_ui_1")
                                    )
                         ),
                         navbarMenu("More", 
                           tabPanel(
                             "Help",
                             suppressWarnings(
                                htmltools::includeHTML(
                                  system.file("app/www/Help.html", package = "FielDHub")
                                )
                             )
                           ),
                           tabPanel(
                             "About Us",
                            suppressWarnings(
                             htmltools::includeHTML(
                               system.file("app/www/aboutUs.html", package = "FielDHub")
                             )
                            )
                           ),
                        ),
              ),
        )
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

