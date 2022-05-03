get_random_stacked <- function(stacked = "By Column",
                               n_rows, 
                               n_cols, 
                               matrix_checks = NULL,
                               Fillers = FALSE, 
                               checks = NULL, 
                               data = NULL, 
                               data_dim_each_block = NULL) {
  data_entries <- as.vector(data[,1])
  data_entries_no_checks <- data_entries[!(data_entries %in% checks)]
  b <- length(data_dim_each_block)
  target <- rep(paste0("B", 1:b), times = data_dim_each_block)
  if (sum(matrix_checks == 0) != sum(data_dim_each_block)) {
    stop("Block dimensions do not fit to the matrix")
  }
  target <- rep(paste0("B", 1:b), times = data_dim_each_block)
  v <- 1
  for(j in 1:ncol(matrix_checks)) {
    for (i in nrow(matrix_checks):1) {
      if (matrix_checks[i,j] == 0){
        matrix_checks[i,j] <- target[v]
        v <- v + 1
      }else{
        matrix_checks[i,j] <- matrix_checks[i,j]
        v <- v
      }
    }
  }
  w_map_letters <- matrix_checks
  levels_target <- levels(as.factor(target))
  split_entries <- split_vectors(data_entries_no_checks, data_dim_each_block)
  z <- 1
  for(k in 1:b){
    matrix_checks[matrix_checks == levels_target[z]] <- sample(split_entries[[k]])
    z <- z + 1
  }
  return(list(rand = matrix_checks, 
              Entries = split_entries, 
              Lines = data_dim_each_block, 
              w_map_letters = w_map_letters))
}

# M <- diagonals_checks(n_rows = n_rows, n_cols = n_cols, jump_by_cols = W[l,3],
#                       jump_by_rows = W[l,4], checks = c(1,1,1,1), 
#                       p_start = 0)[[2]]
# matrix_checks <- M
# rand_checks <- sample(1:4, 33, replace = TRUE)
# matrix_checks[matrix_checks == 1] <- sample(rand_checks)
# matrix_checks
# owndataDIAGONALS <- "No"
# blocks <- c(100, 100, 127)
# lines.db <- 327
# checks <- 4
# kindExpt <- "DBUDC"
# sameEntries <- FALSE
# 
# df <- getData()
# df
# 
# test <- get_random_stacked(stacked = "By Column", 
#                            n_rows = 24, 
#                            n_cols = 15, 
#                            matrix_checks = matrix_checks, 
#                            Fillers = FALSE, 
#                            checks = 1:4, 
#                            data = getData()$data_entry, 
#                            data_dim_each_block = blocks)
# 
# test$rand
# test$Entries
# test$Lines
# test$w_map_letters














