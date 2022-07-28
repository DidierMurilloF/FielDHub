get_random <- function(n_rows = NULL, 
                       n_cols = NULL, 
                       d_checks = NULL,
                       Fillers = FALSE,
                       row_sets = NULL, 
                       checks = NULL, 
                       data = NULL,
                       planter_mov = "serpentine", 
                       Multi.Fillers = FALSE, 
                       which.blocks = NULL,
                       data_dim_each_block = NULL) {
  if (all(c("serpentine", "cartesian") != planter_mov)) {
    stop("Input planter_mov is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  } 
  my_split_r <- d_checks
  n_rows <- as.numeric(n_rows)
  n_cols <- as.numeric(n_cols)
  data_entries <- as.vector(data[,1])
  if ("Filler" %in% my_split_r) Fillers <- TRUE else Fillers <- FALSE
    if (!is.null(row_sets)) {
      if (Multi.Fillers == FALSE){
        li_my_split_r <- turner::matrix_to_blocks(my_split_r, 
                                                  rev(row_sets), 
                                                  byrow = TRUE)
        li_my_split_r <- rev(li_my_split_r)
        any_check <- numeric()
        for (z in 1:length(li_my_split_r)){
          any_check[z] <- length(table(as.vector(li_my_split_r[[z]]))) - 2
        }
        lines <- numeric()                                                              
        for (n in 1:length(li_my_split_r)){
          lines[n] <- sum(li_my_split_r[[n]] == 0)
        }
        test_equ <- logical()
        for (v in 1:length(lines)){
          test_equ[v] <- (lines[v] != data_dim_each_block[v])
        }
        Blocks <- 1:length(lines)
        val <- data.frame(Blocks, lines, data_dim_each_block)
        colnames(val) <- c("Block", "Available Plots", "Your Entries")
        if (any(test_equ == TRUE)) return(NULL)
        w_map <- my_split_r
        data_entry <- data
        data_entry1 <- data_entry[(length(checks) + 1):nrow(data_entry), ]
        Block_levels <- as.numeric(levels(as.factor(data_entry1$BLOCK)))
        data_dim_each_block <- numeric()
        for (i in Block_levels) { 
          data_dim_each_block[i] <- nrow(subset(data_entry, 
                                                data_entry$BLOCK == i))
        }
        target <- rep(LETTERS[1:length(data_dim_each_block)], data_dim_each_block)
        W_SPLIT <- turner::matrix_to_blocks(my_split_r, 
                                            blocks = rev(row_sets), 
                                            byrow = TRUE)
        W_SPLIT <- rev(W_SPLIT)
        z <- 1:length(W_SPLIT)
        dim_each_block_without_Fillers_and_checks <- numeric()
        for (i in z){
          dim_each_block_without_Fillers_and_checks[i] <- nrow(W_SPLIT[[i]]) * 
            ncol(W_SPLIT[[i]]) - sum(W_SPLIT[[i]] != 0)
        }
        split_target <- split_vectors(target, dim_each_block_without_Fillers_and_checks)
        v <- 1
        for (s in z){
          for(i in 1:nrow(W_SPLIT[[s]])){
            for(j in 1:ncol(W_SPLIT[[s]])){
              if (W_SPLIT[[s]][i,j] == 0){
                W_SPLIT[[s]][i,j] <- split_target[[s]][v]
                v <- v + 1
              }else{
                W_SPLIT[[s]][i,j] <-  W_SPLIT[[s]][i,j]
                v <- v
              }
            }
          }
          v <- 1
        }
        len_list <- length(W_SPLIT)
        if (len_list > 2) {
          w_map <- rbind(W_SPLIT[[len_list]], W_SPLIT[[len_list - 1]])
          for (d in (len_list - 2):1){
            w_map <- rbind(w_map, W_SPLIT[[d]])
            w_map <- w_map
          }
        }else if (len_list == 2) { 
          w_map <- rbind(W_SPLIT[[2]], W_SPLIT[[1]])
        }else{
          w_map <-  W_SPLIT[[1]]
        }
        levels_target <- levels(as.factor(target))
        data_entries <- as.vector(data_entry1$ENTRY)
        entries <- split_vectors(data_entries, data_dim_each_block)
        z <- 1
        for(k in 1:length(entries)){
          w_map[w_map == levels_target[z]] <- sample(entries[[k]])
          z <- z + 1
        }
        return(list(rand = w_map, Entries = entries, Lines = lines))
      }else if (Multi.Fillers == TRUE) {
        w_map <- my_split_r
        data_entry <- data
        data_entry1 <- data_entry[(length(checks) + 1):nrow(data_entry), ]
        Block_levels <- as.numeric(levels(as.factor(data_entry1$BLOCK)))
        data_dim_each_block <- numeric()
        for (i in Block_levels){ 
          data_dim_each_block[i] <- nrow(subset(data_entry, 
                                                data_entry$BLOCK == i))
        }
        target <- rep(LETTERS[1:length(data_dim_each_block)], 
                      data_dim_each_block)
        if(length(which.blocks) == 1 && 
           which.blocks == length(data_dim_each_block)) {
          v <- 1
          if( planter_mov == "serpentine") {
            if (nrow(w_map) %% 2 == 0) {
              for (i in nrow(w_map):1) {
                if (i %% 2 == 0) {
                  A <- 1:ncol(w_map)
                } else A <- ncol(w_map):1
                for (j in A) {
                  if (w_map[i,j] == 0){
                    w_map[i,j] <- target[v]
                    v <- v + 1
                  }else{
                    w_map[i,j] <-  w_map[i,j]
                    v <- v
                  }
                }
              }
            } else {
              for (i in nrow(w_map):1) {
                if (i %% 2 == 0){
                  A <- ncol(w_map):1
                } else A <- 1:ncol(w_map)
                for (j in A) { 
                  if (w_map[i,j] == 0) {
                    w_map[i,j] <- target[v]
                    v <- v + 1
                  } else {
                    w_map[i,j] <- w_map[i,j]
                    v <- v
                  }
                }
              }
            }
          } else {
            for (i in nrow(w_map):1) {
              for (j in 1:ncol(w_map)) {
                if (w_map[i,j] == 0) {
                  w_map[i,j] <- target[v]
                  v <- v + 1
                } else {
                  w_map[i,j] <- w_map[i,j]
                  v <- v
                }
              }
            }
            v <- 1
          }
        }else if (length(which.blocks) <= length(data_dim_each_block)) {
          r_sets <- row_sets
          which_b <- sort(which.blocks,decreasing = FALSE)
          if (length(r_sets) %in% which_b) {
            which_b <-  which_b
          } else {
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
          W_SPLIT <- turner::matrix_to_blocks(my_split_r, 
                                              blocks = rev(row_sets), 
                                              byrow = TRUE)
          W_SPLIT <- rev(W_SPLIT)
          dim_each_block_without_Fillers_and_checks <- numeric()
          for (i in 1:length(W_SPLIT)){
           dim_each_block_without_Fillers_and_checks[i] <- nrow(W_SPLIT[[i]]) * 
             ncol(W_SPLIT[[i]]) - sum(W_SPLIT[[i]] != 0)
          }
          split_target <- split_vectors(target, 
                                        dim_each_block_without_Fillers_and_checks)
          s <- 1;v <- 1
          z <- 1:length(W_SPLIT)
          if (planter_mov == "serpentine") {
            for (s in z){
              if (nrow(W_SPLIT[[s]]) %% 2 == 0) {
                for (i in nrow(W_SPLIT[[s]]):1) {
                  if (i %% 2 == 0) {
                    A <- 1:ncol(w_map)
                  } else A <- ncol(w_map):1
                  for (j in A) {
                    if (W_SPLIT[[s]][i,j] == 0) {
                      W_SPLIT[[s]][i,j] <- split_target[[s]][v]
                      v <- v + 1
                    } else {
                      W_SPLIT[[s]][i,j] <-  W_SPLIT[[s]][i,j]
                      v <- v
                    }
                  }
                }
              } else {
                for (i in nrow(W_SPLIT[[s]]):1) {
                  if (i %% 2 == 0) {
                    A <- ncol(w_map):1
                  } else A <- 1:ncol(w_map)
                  for (j in A) {
                    if (W_SPLIT[[s]][i,j] == 0) {
                      W_SPLIT[[s]][i,j] <- split_target[[s]][v]
                      v <- v + 1
                    } else {
                      W_SPLIT[[s]][i,j] <-  W_SPLIT[[s]][i,j] 
                      v <- v
                    }
                  }
                }
              }
              v <- 1
            }
          } else {
            for (s in z) {
              for (i in nrow(W_SPLIT[[s]]):1) {
                for (j in 1:ncol(W_SPLIT[[s]])) {
                  if (W_SPLIT[[s]][i,j] == 0) {
                    W_SPLIT[[s]][i,j] <- split_target[[s]][v]
                    v <- v + 1
                  } else {
                    W_SPLIT[[s]][i,j] <-  W_SPLIT[[s]][i,j]
                    v <- v
                  }
                }
              }
              v <- 1
            }
          }
          len_list <- length(W_SPLIT)
          if (len_list > 2) {
            w_map <- rbind(W_SPLIT[[len_list]], W_SPLIT[[len_list - 1]])
            for (d in (len_list - 2):1){
              w_map <- rbind(w_map, W_SPLIT[[d]])
              w_map <- w_map
            }
          }else if (length(W_SPLIT) == 2) { 
            w_map <- rbind(W_SPLIT[[2]], W_SPLIT[[1]])
          }else{
            w_map <-  W_SPLIT[[1]]
          }
        }
        w_map_letters <- w_map
        levels_target <- levels(as.factor(target))
        data_entries <- as.vector(data_entry1$ENTRY)
        entries <- split_vectors(data_entries, data_dim_each_block)
        z <- 1
        for(k in 1:length(entries)){
          w_map[w_map == levels_target[z]] <- sample(entries[[k]])
          z <- z + 1
        }
      }
      data_entries_no_checks <- as.vector(data_entry1$ENTRY)
      len_entries_to_random <- length(data_entries_no_checks)
      treatments_random <- sum(data_entries_no_checks %in% w_map)
      if (treatments_random == len_entries_to_random) {
        w_map_ok <- w_map
        # print("Randomization was successful. It passed all tests!")
        # print(c(treatments_random, len_entries_to_random))
      } else stop("Some entries are missing in the randomization!!")
      return(list(rand = w_map_ok, 
                  Entries = entries, 
                  Lines = data_dim_each_block, 
                  w_map_letters = w_map_letters))
  } 
}