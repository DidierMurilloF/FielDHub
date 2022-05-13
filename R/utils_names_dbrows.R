names_dbrows <- function(w_map = NULL, myWay = "By Row", kindExpt = "DBUDC", data_dim_each_block = NULL,
                         planter = "serpentine", w_map_letters = NULL, expt_name = NULL, Checks = NULL) {
  checks <- Checks
  if (kindExpt == "DBUDC") {
    if (myWay == "By Row") {
      my_row_sets <- automatically_cuts(data = w_map, planter_mov = planter,
                                        way = "By Row", dim_data = data_dim_each_block)[[1]]
      blocks <- length(my_row_sets)
      w_map_letters1 <- w_map_letters
      Index_block <- LETTERS[1:blocks]
      Name_expt <- expt_name
      if (length(Name_expt) == blocks || !is.null(Name_expt)) {
        name_blocks <- Name_expt
      }else {
        name_blocks <- paste(rep("Expt", blocks), 1:blocks, sep = "")
      }
      z <- 1
      for(i in Index_block){ 
        w_map_letters1[w_map_letters1 == i] <- name_blocks[z] 
        z <- z + 1 
      } 
      checks_ch <- as.character(checks) 
      for(i in nrow(w_map_letters1):1) { 
        for(j in 1:ncol(w_map_letters1)) { 
          if (any(checks_ch %in% w_map_letters1[i, j]) && w_map_letters1[i,j] != "Filler") {
            if (j != ncol(w_map_letters1)){
              if(w_map_letters1[i, j + 1] == "Filler") {
                w_map_letters1[i, j] <- w_map_letters1[i, j - 1]
              }else w_map_letters1[i, j] <- w_map_letters1[i, j + 1]
            }else if (j == ncol(w_map_letters1)) {
              w_map_letters1[i, j] <- w_map_letters1[i, j - 1]
            }
          }
        }
      }
      split_names <- w_map_letters1
    }else{
      return(NULL)
    }
  }
  return(list(my_names = split_names))
}