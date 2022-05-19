plot_number <- function(movement_planter = "serpentine", n_blocks = NULL, n_rows = NULL, n_cols = NULL, 
                        plot_n_start = NULL, datos = NULL, expe_name = NULL, ByRow = FALSE,
                        my_row_sets = NULL, ByCol = TRUE, my_col_sets = NULL ) {
  
  if (all(c("serpentine", "cartesian") != movement_planter)) {
    stop("Input movement_planter unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  if (is.null(plot_n_start) || is.character(plot_n_start)) return(NULL)
  
  my_split_plot_nub <- matrix(data = datos, nrow = n_rows, ncol = n_cols, byrow = F)
  
  lengt_my_expt <- length(my_split_plot_nub)
  
  if (n_blocks > 1) {
    
    if (ByCol == TRUE) {
      
      my_split_plot_nub_into <- turner::matrix_to_blocks(my_split_plot_nub, blocks = my_col_sets, byrow = FALSE)
      
      if (length(plot_n_start) == n_blocks){
        
        list_m <- list()
        
        for (h in 1:n_blocks){
          
          if (plot_n_start[h] > 1){
            
            list_m[[h]] <- plot_n_start[h]:(length(my_split_plot_nub_into[[h]]) + (plot_n_start[h] - 1))
            
          }else{
            
            list_m[[h]] <- plot_n_start[h]:(length(my_split_plot_nub_into[[h]])) 
            
          }
          
        }
        
        new_list_m <- list()
        
        for (j in 1:n_blocks){
          
          new_list_m[[j]] <- rev(list_m[[j]])
          
        }
        
        plot_num <- list()
        
        for (b in 1:length(new_list_m)){
          
          plot_num[[b]] <- matrix(new_list_m[[b]], nrow = nrow(my_split_plot_nub), byrow = T)
          
        }
        
        if (movement_planter == "serpentine") {
          
          if (nrow(plot_num[[1]]) %% 2 == 0) {
            for (k in 1:length(plot_num)){
              plot_num[[k]][seq(2,nrow(plot_num[[k]]),2),] <- rev(plot_num[[k]][seq(nrow(plot_num[[k]]),2,-2),])
            }
          } else{
            for (k in 1:length(plot_num)){
              plot_num[[k]][seq(1,nrow(plot_num[[k]]),2),] <- rev(plot_num[[k]][seq(nrow(plot_num[[k]]),1,-2),])
            }
          }
          v = 1
          for (a in 1:n_blocks){
            my_split_plot_nub[my_split_plot_nub == expe_name[v]] <- plot_num[[a]]
            v = v + 1
          }
          
          
        }else{
          for (k in 1:length(plot_num)){
            plot_num[[k]][n_rows:1,] <- rev(plot_num[[k]][,])
          }
          v = 1
          for (a in 1:n_blocks){
            my_split_plot_nub[my_split_plot_nub == expe_name[v]] <- plot_num[[a]]
            v = v + 1
          }
          
        }
        
        my_split_plot_nub <- apply(my_split_plot_nub, 2 ,as.numeric)
        
        return(list(w_map_letters1 = my_split_plot_nub, l = plot_num))
        
      }else if (length(plot_n_start) == 1){
        
        my_split_plot_nub <- matrix(data = 0, nrow = n_rows, ncol = n_cols, byrow = TRUE)
        
        my_split_plot_nub <- turner::matrix_to_blocks(my_split_plot_nub, blocks = my_col_sets, byrow = FALSE)
        
        B <- length(my_split_plot_nub)
        
        info_blocks <- matrix(data = NA, nrow = B, ncol = 4, byrow = TRUE)
        s <- 0
        for (i in 1:B){
          
          info_blocks[i,] <- c(plot_n_start[1] + s, nrow(my_split_plot_nub[[i]]), ncol(my_split_plot_nub[[i]]),
                               dim(my_split_plot_nub[[i]])[1]*dim(my_split_plot_nub[[i]])[2])
          
          s <- s + dim(my_split_plot_nub[[i]])[1]*dim(my_split_plot_nub[[i]])[2]
          
        }
        
        M_list <- list()
        
        for (n in 1:B){
          
          if (movement_planter == "serpentine"){
            
            M_list[[n]] <- continue_plot_Serp(datos = matrix(data = (info_blocks[n,1] + (info_blocks[n,4] - 1)):info_blocks[n,1],
                                                             nrow = info_blocks[n,2], ncol = info_blocks[n,3], byrow = T))
            
          }else{
            
            M_list[[n]] <- continue_plot_Cart(datos = matrix(data = (info_blocks[n,1] + (info_blocks[n,4] - 1)):info_blocks[n,1],
                                                             nrow = info_blocks[n,2], ncol = info_blocks[n,3], byrow = T))
            
          }
          
        }
        
        if (B > 2){
          my_split_plot_nub <- cbind(M_list[[1]], M_list[[2]])
          for (d in 3:B){
            my_split_plot_nub <- cbind(my_split_plot_nub, M_list[[d]])
            my_split_plot_nub <- my_split_plot_nub
          }
        }else if (B == 2){
          my_split_plot_nub <- cbind(M_list[[1]], M_list[[2]])
        }else{
          my_split_plot_nub <- M_list[[1]]
        }
        
      }
      
      plot_num <- M_list
      
      my_split_plot_nub <- apply(my_split_plot_nub, 2 ,as.numeric)
      
      return(list(w_map_letters1 = my_split_plot_nub, l = plot_num))
      
    }else if(ByRow == TRUE){
      
      my_split_plot_nub_into <- turner::matrix_to_blocks(my_split_plot_nub, blocks = my_row_sets, byrow = TRUE) #06/30/2020
      
      #my_split_plot_nub_into <- rev(my_split_plot_nub_into) #06/30/2020
      
      if (length(plot_n_start) == n_blocks){
        
        list_m <- list()
        
        for (h in 1:n_blocks){
          
          if (plot_n_start[h] > 1){
            
            list_m[[h]] <- plot_n_start[h]:(length(my_split_plot_nub_into[[h]]) + (plot_n_start[h] - 1))
            
          }else{
            
            list_m[[h]] <- plot_n_start[h]:(length(my_split_plot_nub_into[[h]])) 
            
          }
          
        }
        
        new_list_m <- list()
        
        for (j in 1:n_blocks){
          
          new_list_m[[j]] <- rev(list_m[[j]])
          
        }
        
        plot_num <- list()
        
        d <- 1
        
        for (b in 1:length(new_list_m)){
          
          plot_num[[b]] <- matrix(new_list_m[[b]], nrow = nrow(my_split_plot_nub_into[[d]]), byrow = T)
          
          d <- d + 1
          
        }
        
        if (movement_planter == "serpentine"){
          
          for (k in 1:length(plot_num)){
            
            if (nrow(plot_num[[k]]) %% 2 == 0){
              
              plot_num[[k]][seq(2,nrow(plot_num[[k]]),2),] <- rev(plot_num[[k]][seq(nrow(plot_num[[k]]),2,-2),])
              
            }else{
              
              plot_num[[k]][seq(1,nrow(plot_num[[k]]),2),] <- rev(plot_num[[k]][seq(nrow(plot_num[[k]]),1,-2),])
              
            }
            
          }
          
          v = 1
          
          for (a in 1:n_blocks){
            
            my_split_plot_nub[my_split_plot_nub == expe_name[v]] <- plot_num[[a]]
            
            v = v + 1
            
          }
          
        }else{
          
          lili <- my_row_sets
          new_lili <- list()
          
          for (i in 1:length(lili)){
            
            new_lili[[i]] <- rev(lili[[i]])
            
          }
          
          for (t in 1:length(plot_num)){
            
            plot_num[[t]][,] <- rev(plot_num[[t]][,])
            
          }
          
          v = 1
          
          for (a in 1:n_blocks){
            
            my_split_plot_nub[my_split_plot_nub == expe_name[v]] <- rev(plot_num[[a]])
            
            v = v + 1
            
          }
          
          for (j in 1:nrow(my_split_plot_nub)){
            
            my_split_plot_nub[j,] <- rev(my_split_plot_nub[j,])
            
          }
          
        }
        
      }else if (length(plot_n_start) == 1){
        
        M <- matrix(data = (plot_n_start[1] + (lengt_my_expt - 1)):plot_n_start[1], ncol = n_cols,
                    nrow = n_rows, byrow = T)
        
        if (movement_planter == "serpentine"){
          
          my_split_plot_nub <- continue_plot_Serp(datos = M)
          
          plot_num <- turner::matrix_to_blocks(my_split_plot_nub, blocks = rev(my_row_sets), byrow = TRUE)
          
          plot_num <- rev(plot_num)
          
        }else{
          
          my_split_plot_nub <- continue_plot_Cart(datos = M)
          
          plot_num <- turner::matrix_to_blocks(my_split_plot_nub, blocks = rev(my_row_sets), byrow = TRUE)
          
          plot_num <- rev(plot_num)
          
        }
        
      }
      
    }
    
    my_split_plot_nub <- apply(my_split_plot_nub, 2 ,as.numeric)
    
    return(list(w_map_letters1 = my_split_plot_nub, l = plot_num))
    
  }else if (n_blocks == 1){
    
    if (plot_n_start > 1){
      
      list_m <- plot_n_start:(length(my_split_plot_nub) + (plot_n_start - 1))
      
    }else{
      
      list_m <- plot_n_start:(length(my_split_plot_nub))
    }
    
    list_m <- list(list_m)
    
    
    new_list_m <- list()
    new_list_m[[1]] <- rev(list_m[[1]])
    
    plot_num <- list()
    
    for (b in 1:length(new_list_m)){
      
      plot_num[[b]] <- matrix(new_list_m[[b]], nrow = nrow(my_split_plot_nub), byrow = T)
      
    }
    
    if (movement_planter == "serpentine") {
      
      if (nrow(plot_num[[1]]) %% 2 == 0){
        
        for (k in 1:length(plot_num)){
          
          plot_num[[k]][seq(2,nrow(plot_num[[k]]),2),] <- rev(plot_num[[k]][seq(nrow(plot_num[[k]]),2,-2),])
        }
        
      }else{
        
        for (k in 1:length(plot_num)){
          
          plot_num[[k]][seq(1,nrow(plot_num[[k]]),2),] <- rev(plot_num[[k]][seq(nrow(plot_num[[k]]),1,-2),])
        }
      }
      
      v = 1
      for (a in 1:n_blocks){
        
        my_split_plot_nub[my_split_plot_nub == expe_name[v]] <- plot_num[[a]]
        
        v = v + 1
      }
      
      my_split_plot_nub <- apply(my_split_plot_nub, 2 ,as.numeric)
      
      return(list(w_map_letters1 = my_split_plot_nub, l = plot_num))
      
    }else{
      
      for (k in 1:length(plot_num)){
        
        plot_num[[k]][n_rows:1,] <- rev(plot_num[[k]][,])
        
      }
      
      v = 1
      for (a in 1:n_blocks){
        
        my_split_plot_nub[my_split_plot_nub == expe_name[v]] <- plot_num[[a]]
        
        v = v + 1
        
      }
      
      return(list(w_map_letters1 = my_split_plot_nub, l = plot_num))
      
    }
    
  }
  
  if ("Filler" %in% my_split_plot_nub){
    
    my_split_plot_nub[my_split_plot_nub == "Filler"] <- 0
    
  } 
  
  my_split_plot_nub <- apply(my_split_plot_nub, 2 ,as.numeric)
  
  return(list(w_map_letters1 = my_split_plot_nub, l = plot_num))
  
}