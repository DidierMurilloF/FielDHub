NO_Random <- function(checksMap = NULL, data_Entry = NULL, planter = "serpentine") {
  
  w_map <- checksMap
  dim_w <- dim(w_map)[1]*dim(w_map)[2]
  #target <- rep(LETTERS[1:length(dim_each_block)], dim_each_block)
  #target <- paste(rep("B", dim_w), rep(1:ncol(w_map), each = ncol(w_map)), sep = "")
  target <- as.vector(data_Entry)
  v <- 1
  if(planter == "serpentine") {
    if (nrow(w_map) %% 2 == 0){
      for(i in nrow(w_map):1){
        if (i %% 2 == 0){
          A <- 1:ncol(w_map)
        }else A <- ncol(w_map):1
        for(j in A){
          if (w_map[i,j] == 0){
            w_map[i,j] <- target[v]
            v <- v + 1
          }else{
            w_map[i,j] <-  w_map[i,j]
            v <- v
          }
        }
      }
    }else{
      for(i in nrow(w_map):1){
        if (i %% 2 == 0){
          A <- ncol(w_map):1
        }else A <- 1:ncol(w_map)
        for(j in A){
          if (w_map[i,j] == 0){
            w_map[i,j] <- target[v]
            v <- v + 1
          }else{
            w_map[i,j] <-  w_map[i,j]
            v <- v
          }
        }
      }
    }
  }else{
    for(i in nrow(w_map):1){
      for(j in 1:ncol(w_map)){
        if (w_map[i,j] == 0){
          w_map[i,j] <- target[v]
          v <- v + 1
        }else{
          w_map[i,j] <-  w_map[i,j]
          v <- v
        }
      }
    }
    v <- 1
  }
  w_map_letters <- w_map
  # levels_target <- levels(as.factor(target))
  # data_entries <- as.vector(data_entries$ENTRY)
  dim_each_block <- rep(ncol(w_map), nrow(w_map))
  # entries <- split_vectors(data_entries, dim_each_block)
  # z <- 1
  # for(k in 1:length(entries)) {
  #   w_map[w_map == levels_target[z]] <- sentries[[k]])
  #   z <- z + 1
  # }
  return(list(rand = w_map, len_cut = dim_each_block, w_map_letters = w_map_letters))
}