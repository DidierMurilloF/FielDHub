plot_number_fillers <- function(movement_planter = "serpentine", 
                                plot_n_start = NULL, 
                                datos = NULL,
                                expe_names = NULL,
                                ByRow = TRUE,
                                my_row_sets = NULL,
                                ByCol = FALSE,
                                my_col_sets = NULL, 
                                which.blocks = NULL,
                                n_blocks = NULL,
                                data.dim.each = NULL) {
  
  if (all(c("serpentine", "cartesian") != movement_planter)) {
    stop("Input movement_planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  blocks <- length(data.dim.each)
  Name_expt <- expe_names
  if (length(Name_expt) == blocks || !is.null(Name_expt)) {
    name_blocks <- Name_expt
  } else {
    name_blocks <- paste(rep("Expt", blocks), 1:blocks, sep = "")
  }
  
  w_map_letters1 <- datos
  data_dim_each_block <- data.dim.each
  
  if (plot_n_start == "" || is.character(plot_n_start)) return(NULL)
  
  plot_numbers_len <- numeric()
  
  for (i in 1:n_blocks){
    
    plot_numbers_len[i] <- sum(w_map_letters1 == expe_names[i])
    
  }
  
  if (length(plot_n_start) == 1){
    
    if(plot_n_start == 1){
      
      target_num <- plot_n_start[1]:sum(plot_numbers_len) 
      
      target_num1 <- split_vectors(target_num, plot_numbers_len)
      
    }else{
      
      target_num <- plot_n_start[1]:(sum(plot_numbers_len) + plot_n_start[1] - 1)
      
      target_num1 <- split_vectors(target_num, plot_numbers_len)
      
    }
    
  }else if (length(plot_n_start) < length(plot_numbers_len)){
    
    return(NULL)
    
  }else if (length(plot_n_start) == length(plot_numbers_len)) {
    
    target_num1 <- list()
    s <- 0
    for(i in 1:length(plot_numbers_len)){
      
      target_num1[[i]] <- (plot_n_start[i]):((plot_n_start[i] - 1) + plot_numbers_len[i])
      
    }
    
    target_num <- as.vector(unlist(target_num1))
    
  }
  
  if (is.null(which.blocks)) {
    stop("which.blocks is NULL")
  }else if (length(which.blocks) == 1 && which.blocks == length(data_dim_each_block)){
    
    v <- 1
    
    if(movement_planter == "serpentine"){
      
      if (nrow(w_map_letters1) %% 2 == 0){
        
        for(i in nrow(w_map_letters1):1){
          
          if (i %% 2 == 0){
            
            A <- 1:ncol(w_map_letters1)
            
          }else A <- ncol(w_map_letters1):1
          
          for(j in A){
            
            w_map_letters1[i,j] <- target_num[v]
            
            v <- v + 1
            
          }
          
        }
        
      }else{
        
        for(i in nrow(w_map_letters1):1){
          
          if (i %% 2 == 0){
            
            A <- ncol(w_map_letters1):1
            
          }else A <- 1:ncol(w_map_letters1)
          
          for(j in A){
            
            w_map_letters1[i,j] <- target_num[v]
            
            v <- v + 1
            
          }
          
        }
        
      }
      
    }else{
      
      for(i in nrow(w_map_letters1):1){
        
        for(j in 1:ncol(w_map_letters1)){
          
          w_map_letters1[i,j] <- target_num[v]
          
          v <- v + 1
          
        }
        
      }
      
      v <- 1
      
    }
    
  }else if(length(which.blocks) <= length(data_dim_each_block)){
    
    r_sets <- my_row_sets
    which_b <- sort(which.blocks,decreasing = FALSE)
    
    if(length(r_sets) %in% which_b){
      
      which_b <-  which_b
      
    }else{
      
      which_b <- c(which_b, length(r_sets))
      
    }
    
    cuts_max <- numeric()
    
    for (i in 1:length(r_sets)) {
      
      cuts_max[i] <- max(r_sets[[i]])
      
    }
    
    new_r_sets <- list()
    
    s <- 0
    v <- 1
    for (i in which_b) {
      
      new_r_sets[[v]] <- (1 + s):cuts_max[i]
      
      v <- v + 1
      
      s <- cuts_max[i]
      
    }
    
    row_sets <- new_r_sets
    
    W_SPLIT <- turner::matrix_to_blocks(w_map_letters1, blocks = rev(row_sets), byrow = TRUE)
    
    W_SPLIT <- rev(W_SPLIT)
    
    
    dim_each_block_without_Fillers_and_checks <- numeric()
    
    for (i in 1:length(W_SPLIT)){
      
      dim_each_block_without_Fillers_and_checks[i] <- nrow(W_SPLIT[[i]]) * ncol(W_SPLIT[[i]]) - sum(W_SPLIT[[i]] == "Filler")
      
    }
    
    split_target_num <- split_vectors(target_num, dim_each_block_without_Fillers_and_checks)
    
    s <- 1;v <- 1
    
    z <- 1:length(W_SPLIT)
    
    if(movement_planter == "serpentine"){
      
      for (s in z){
        
        if (nrow(W_SPLIT[[s]]) %% 2 == 0){
          
          for(i in nrow(W_SPLIT[[s]]):1){
            
            if (i %% 2 == 0){
              
              A <- 1:ncol(w_map_letters1)
              
            }else A <- ncol(w_map_letters1):1
            
            for(j in A){
              
              W_SPLIT[[s]][i,j] <- split_target_num[[s]][v]
              
              v <- v + 1
              
            }
            
          }
          
        }else{
          
          for(i in nrow(W_SPLIT[[s]]):1){
            
            if (i %% 2 == 0){
              
              A <- ncol(w_map_letters1):1
              
            }else A <- 1:ncol(w_map_letters1)
            
            for(j in A){
              
              W_SPLIT[[s]][i,j] <- split_target_num[[s]][v]
              
              v <- v + 1
              
            }
            
          }
          
        }
        
        v <- 1
        
      }
      
    }else{
      
      for (s in z){
        
        for(i in nrow(W_SPLIT[[s]]):1){
          
          for(j in 1:ncol(W_SPLIT[[s]])){
            
            W_SPLIT[[s]][i,j] <- split_target_num[[s]][v]
            
            v <- v + 1
            
          }
          
        }
        
        v <- 1
        
      }
      
    }
    
    len_list <- length(W_SPLIT)
    
    if (len_list > 2){
      
      w_map_letters1 <- rbind(W_SPLIT[[len_list]], W_SPLIT[[len_list - 1]])
      
      for (d in (len_list - 2):1){
        
        w_map_letters1 <- rbind(w_map_letters1, W_SPLIT[[d]])
        
        w_map_letters1 <- w_map_letters1
        
      }
      
    }else if (length(W_SPLIT) == 2){ 
      
      w_map_letters1 <- rbind(W_SPLIT[[2]], W_SPLIT[[1]])
      
    }else{
      
      w_map_letters1 <-  W_SPLIT[[1]]
    }
    
  }
  
  w_map_letters1[is.na(w_map_letters1)] <- 0
  
  w_map_letters1 <- apply(w_map_letters1, 2 ,as.numeric)
  
  return(list(w_map_letters1 = w_map_letters1, target_num1 = target_num1)) 
}