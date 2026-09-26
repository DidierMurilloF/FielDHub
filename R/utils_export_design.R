export_design <- function(G, movement_planter = NULL, location = NULL, Year = NULL,
                          data_file = NULL, reps = FALSE){
  
  if (all(c("serpentine", "cartesian") != movement_planter)) {
    fieldhub_abort("Input movement_planter is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  if (is.null(Year)) Year <- format(Sys.Date(), "%Y")
  H <- G[[3]]
  # The field book lists the plots in planting order, starting at the bottom
  # row of the maps
  planting <- planting_path(nrow(H), ncol(H), movement_planter)
  path <- field_path(nrow(H), ncol(H), movement_planter)
  asExport_cordenates <- function(){
    
    if (reps == FALSE){ 
      
      my_output_cord <- matrix(data = NA, nrow = dim(H)[1]*dim(H)[2], ncol = 9)
      names_exp <- c("ROW", "COLUMN", "ENTRY", "PLOT", "CHECKS", 
                     "EXPT", "LOCATION", "LOC", "YEAR")
      colnames(my_output_cord) <- names_exp
      
    } else{
      
      my_output_cord <- matrix(data = NA, nrow = dim(H)[1]*dim(H)[2], ncol = 10)
      names_exp <- c("ROW", "COLUMN", "ENTRY", "PLOT", "CHECKS", 
                     "EXPT", "LOCATION", "LOC", "YEAR", "REP")
      colnames(my_output_cord) <- names_exp
      
    }
    
    my_output_cord <- as.data.frame(my_output_cord)
    
    DATA_LOCATIONS <- list(Locations = c("PROSPER","BERTHOLD","CARRINGTON",
                                         "CASSELTON","LANGDON","OSNABROCK",
                                         "HETTINGER","MINOT","POLK CO","WILLISTON",
                                         "WOLVERTON","MANDAN","HEBRON","FARGO"),
                           LOC = c("PRO","BER","CAR","CAS","LAN","HOS","HET","MNT",
                                   "POL","WIL","WOL","MAN", "HEB","FAR")
    )
    
    LOCATIONS <- data.frame(DATA_LOCATIONS)
    
    location <- toupper(location)
    
    my_output_cord[,1] <- planting[, "ROW"]
    my_output_cord[,7] <- rep(location, dim(H)[1]*dim(H)[2])
    
    
    if (location %in% LOCATIONS$Locations){
      
      my_output_cord[,8] <- subset(LOCATIONS, LOCATIONS[,1] == location)[,2]
      
    }else my_output_cord[,8] <- rep(substr(location, start = 1, stop = 3), dim(H)[1]*dim(H)[2])
    
    
    my_output_cord[,9] <- rep(Year, dim(H)[1]*dim(H)[2])
    
    my_output_cord[,2] <- planting[, "COLUMN"]
    return(my_output_cord)
  }
  asExport <- function(H) {
    # The cells of the map in planting order
    cells <- numeric()
    for (k in seq_len(nrow(path))) {
      cells[k] <- H[path[k, "row"], path[k, "col"]]
    }
    return(cells)
  }
  
  my_final_export <- asExport_cordenates()
  for (m in 1:4){
    my_final_export[, m + 2] <- asExport(G[[m]])
  }
  
  if(reps == TRUE) {
    my_final_export[, 10] <- asExport(G[[5]])
    colnames(my_final_export)[10] <- "BLOCK"
  }
  
  datos_names <- data_file
  datos_names_merge <- datos_names |> dplyr::distinct(ENTRY, .keep_all = TRUE)
  export_full <- merge(my_final_export, datos_names_merge,
                       by.x = 3, by.y = 1, sort = F)
  my_final_export_full <-  export_full[order(export_full$ROW, export_full$PLOT),]
  
  return(my_final_export_full)
  
}
