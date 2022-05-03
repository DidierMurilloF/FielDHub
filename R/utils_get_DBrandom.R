get_DBrandom <- function(binaryMap = NULL, data_dim_each_block = NULL, data_entries = NULL,
                         planter = "serpentine") {
  
  w_map <- binaryMap
  target <- rep(LETTERS[1:length(data_dim_each_block)], data_dim_each_block)
  v <- 1
  if(planter == "serpentine"){
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
  levels_target <- levels(as.factor(target))
  data_entries <- as.vector(data_entries$ENTRY)
  entries <- split_vectors(data_entries, data_dim_each_block)
  z <- 1
  for(k in 1:length(entries)) {
    w_map[w_map == levels_target[z]] <- sample(entries[[k]])
    z <- z + 1
  }
  return(list(rand = w_map, Entries = entries, 
              Lines = data_dim_each_block, 
              w_map_letters = w_map_letters))
}
