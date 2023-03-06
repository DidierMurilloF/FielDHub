#' sparse_allocation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sparse_allocation_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sparse_allocation Server Functions
#'
#' @noRd 
mod_sparse_allocation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sparse_allocation_ui("sparse_allocation_1")
    
## To be copied in the server
# mod_sparse_allocation_server("sparse_allocation_1")
