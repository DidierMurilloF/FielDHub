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
  registry <- fieldhub_app_registry()
  registration_order <- order(vapply(registry, `[[`, integer(1), "server_order"))
  for (entry in registry[registration_order]) {
    server <- get(entry$server, mode = "function")
    server(entry$id)
  }
}
