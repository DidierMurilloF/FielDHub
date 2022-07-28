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
  
  treatments_random <- sum(data_entries_no_checks %in% matrix_checks)
  len_entries_to_random <- length(data_entries_no_checks)
  if (treatments_random == len_entries_to_random) {
    matrix_checks_random_entries <- matrix_checks
    # print("Randomization was successful. It passed all tests! Great!!")
    # print(c(treatments_random, len_entries_to_random))
  } else stop("Some entries are missing in the randomization!!")
  return(list(rand = matrix_checks_random_entries, 
              Entries = split_entries, 
              Lines = data_dim_each_block, 
              w_map_letters = w_map_letters))
}















