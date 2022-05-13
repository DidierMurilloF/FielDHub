#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
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
app_server <- function( input, output, session ) {
  ################## Unreplicated Module #########################################
  ### Diagonal Arrangement
  mod_Diagonal_server("Diagonal_ui_1")
  ### Optimized Arrangement
  mod_Optim_server("Optim_ui_1")
  ################## Augmented Designs ###########################################
  ### Diagonal Augmented
  ### RCBD Augmented
  mod_RCBD_augmented_server("RCBD_augmented_ui_1")
  ################## Partially Replicated Module #################################
  mod_pREPS_server("pREPS_ui_1")
  ################## Lattice Designs #############################################
  ### Square Lattice
  mod_Square_Lattice_server("Square_Lattice_ui_1")
  ### Rectangular Lattice
  mod_Rectangular_Lattice_server("Rectangular_Lattice_ui_1")
  ### Alpha Lattice
  mod_Alpha_Lattice_server("Alpha_Lattice_ui_1")
  ################## Classic Designs #############################################
  mod_CRD_server("CRD_ui_1")
  mod_RCBD_server("RCBD_ui_1")
  mod_LSD_server("LSD_ui_1")
  mod_FD_server("FD_ui_1")
  mod_SPD_server("SPD_ui_1")
  mod_SSPD_server("SSPD_ui_1")
  mod_IBD_server("IBD_ui_1")
  mod_RowCol_server("RowCol_ui_1")
  mod_STRIPD_server("STRIPD_ui_1")
}
