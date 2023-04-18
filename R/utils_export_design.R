export_design <- function(G, movement_planter = NULL, location = NULL, Year = NULL,
                          data_file = NULL, reps = FALSE){
  
  if (all(c("serpentine", "cartesian") != movement_planter)) {
    stop("Input movement_planter is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  Year <- year <- format(Sys.Date(), "%Y")
  H <- G[[3]]
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
    
    my_output_cord[,1] <- rep(1:nrow(H), each = ncol(H))
    my_output_cord[,7] <- rep(location, dim(H)[1]*dim(H)[2])
    
    
    if (location %in% LOCATIONS$Locations){
      
      my_output_cord[,8] <- subset(LOCATIONS, LOCATIONS[,1] == location)[,2]
      
    }else my_output_cord[,8] <- rep(substr(location, start = 1, stop = 3), dim(H)[1]*dim(H)[2])
    
    
    my_output_cord[,9] <- rep(Year, dim(H)[1]*dim(H)[2])
    
    if (movement_planter == "cartesian"){
      my_output_cord[,2] <- rep(1:ncol(H), times = nrow(H))
    }else if (movement_planter == "serpentine"){
      if (nrow(H) %% 2 == 0){ ## depend on the numbers of cols 
        div <- nrow(my_output_cord) / (ncol(H)*2)
        my_output_cord[,2] <- rep(c(1:ncol(H),ncol(H):1), times = div)
      }else{
        remi <- ncol(H)
        div <- (nrow(my_output_cord) - remi) / (ncol(H)*2)
        my_output_cord[,2] <- c(rep(c(1:ncol(H),ncol(H):1), times = div), c(1:ncol(H)))
      }
    }
    return(my_output_cord)
  }
  asExport <- function(H) {
    ###### Cartesian movement ############
    if (movement_planter == "cartesian"){
      my_output_Carte <- numeric()
      k = 1
      for (i in nrow(H):1){
        for (j in 1:ncol(H)){
          my_output_Carte[k] <- H[i,j]
          k = k + 1
        }
      }
      return(my_output_Carte)
    }else if (movement_planter == "serpentine"){
      ###### Serpentine movement ############
      my_temp <- matrix(data = NA, nrow = ncol(H), ncol = nrow(H), byrow = F)
      for (i in seq(nrow(H), 1, by = -2)){
        for (j in 1:ncol(H)){
          my_temp[j,i] <- H[i,j]
        }
      }
      sy <- numeric()
      l = 1
      for (i in seq((nrow(H) - 1), 1, by = -2)){
        for (j in ncol(H):1){
          sy[l] <- H[i,j]
          l = l + 1
        }
      }
      my_temp[ ,seq((ncol(my_temp) - 1),1, by = - 2)] <- sy
      my_output_Serpe <- numeric()
      s = 1
      for (j in ncol(my_temp):1){
        for (i in 1:(nrow(my_temp))){
          my_output_Serpe[s] <- my_temp[i,j]
          s = s + 1
        }
      }
      return(my_output_Serpe)
    }
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
  datos_names_merge <- datos_names %>% dplyr::distinct(ENTRY, .keep_all = TRUE)
  export_full <- merge(my_final_export, datos_names_merge,
                       by.x = 3, by.y = 1, sort = F)
  my_final_export_full <-  export_full[order(export_full$ROW, export_full$PLOT),]
  
  return(my_final_export_full)
  
}
