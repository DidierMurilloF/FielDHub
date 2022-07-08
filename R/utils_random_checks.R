random_checks <- function(dt = NULL, d_checks = NULL, p = NULL, percent = NULL,
                          exptlines = NULL, kindExpt = NULL, planter_mov = NULL,
                          Checks = NULL, stacked = NULL, data = NULL, 
                          data_dim_each_block = NULL, n_reps = NULL, 
                          Option_NCD = FALSE,
                          seed = NULL) { 
  if (is.null(seed) || is.character(seed) || is.factor(seed)) {
    seed <- runif(1, min = -50000, max = 50000)
  } 
  set.seed(seed)
  if (all(c("serpentine", "cartesian") != planter_mov)) {
    stop("Input planter_mov choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  my_P <- p
  if (!is.null(percent) && is.null(exptlines)) {
    my_index <- subset(my_P, my_P[,1] == percent)[1,2]
  }else if (is.null(percent) && !is.null(exptlines)) {
    if (Option_NCD == FALSE) {
      my_index <- subset(my_P, my_P[,7] == exptlines)[1,2]
    }
    if (Option_NCD == TRUE) {
      d_checks <- d_checks[!sapply(d_checks,is.null)]
      k <- 1
      my_index <- k
    }
  }
  my_index <- as.numeric(my_index)
  w_map <- d_checks[[my_index]]
  if(is.null(w_map)) stop("Input w_map is NULL.")
  n_cols <- ncol(w_map)
  n_rows <- nrow(w_map)
  shiny::req(w_map)
  multi <- kindExpt == "DBUDC"
  
  if (multi == TRUE) {
    req(data_dim_each_block)
    req(data)
    if (stacked == "By Row"){
      data_dim_each_block <- data_dim_each_block
      my_row_sets <- automatically_cuts(data = w_map, 
                                        planter_mov = planter_mov,
                                        way = "By Row",
                                        dim_data = data_dim_each_block)[[1]]
      if(is.null(my_row_sets)) return(NULL)
      blocks <- length(my_row_sets)
    }else {
      data_dim_each_block <- data_dim_each_block
      cuts_by_c <- automatically_cuts(data = w_map, 
                                      planter_mov = planter_mov, 
                                      way = "By Column",
                                      dim_data = data_dim_each_block)
      if(is.null(cuts_by_c)) return(NULL)
      blocks <- length(cuts_by_c)
      m = diff(cuts_by_c)
      my_col_sets = c(cuts_by_c[1], m)
    }
    
    if (stacked == "By Column") {
      w_map_split <- turner::matrix_to_blocks(w_map, 
                                              blocks = my_col_sets,
                                              byrow = FALSE)
      Total_checks <- numeric()                                                              
      for (n in 1:length(w_map_split)) {
        Total_checks[n] <- sum(w_map_split[[n]] == 1)
      }
      checks <- Checks
      rand_checks <- list()
      for (j in 1:length(w_map_split)) {
        res <- Total_checks[j] %% length(checks)
        if (res == 0) {
          s <- rep(checks, Total_checks[j]/length(checks))
          rand_checks[[j]] <- sample(s)
        } else {
          if (res > 1) {
            v <- c(rep(checks,(Total_checks[j]-res)/length(checks)), 
                   checks[sample(1:length(checks), size = res)])
          } else {
            v <- c(rep(checks,(Total_checks[j]-res)/length(checks)), 
                   checks[sample(1:length(checks), size = 1)])
          }
          rand_checks[[j]] <- sample(v)
        }
      }
      
      w_map[w_map == 1] <- unlist(rand_checks)
      col_checks <- ifelse(w_map != 0, w_map, 0) 
    } else if (stacked == "By Row") {
      w_map_split <- turner::matrix_to_blocks(w_map, 
                                              blocks = my_row_sets, 
                                              byrow = TRUE)
      Total_checks <- numeric()                                                              
      for (n in 1:length(w_map_split)) {
        Total_checks[n] <- sum(w_map_split[[n]] == 1)
      }
      
      checks <- Checks
      rand_checks <- list()
      for (j in 1:length(w_map_split)) {
        if (Total_checks[j] >= length(checks)) {
          res <- Total_checks[j] %% length(checks)
          if (res == 0) {
            s <- rep(checks, Total_checks[j]/length(checks))
            rand_checks[[j]] <- sample(s)
          } else {
            if (res == 1) {
              v <- c(rep(checks,(Total_checks[j]-res)/length(checks)),
                     sample(checks, size = 1))
            } else if (res > 1) {
              v <- c(rep(checks,(Total_checks[j]-res)/length(checks)),
                     sample(checks, size = res))
            }
            rand_checks[[j]] <- sample(v)
          }
        } else { 
          rand_checks[[j]] <- sample(checks, 
                                     size = Total_checks[j], 
                                     replace = FALSE)
        }
      }

      rand_checks_rev <- rev(rand_checks)
      
      w_map <- t(w_map)
      w_map[w_map == 1] <- unlist(rand_checks_rev)
      w_map <- t(w_map)
      col_checks <- ifelse(w_map != 0, w_map, 0) 
    }
  } else if (multi == FALSE) {
    w_map_split <- list(w_map)
    Total_checks <- numeric()                                                              
    for (n in 1:length(w_map_split)){
      Total_checks[n] <- sum(w_map_split[[n]] == 1)
    }
    checks = Checks
    rand_checks <- list()
    for (j in 1:length(w_map_split)){
      res <- Total_checks[j] %% length(checks)
      if (res == 0){
        s <- rep(checks, Total_checks[j]/length(checks))
        rand_checks[[j]] <- sample(s)
      }else{
        v <- c(rep(checks,Total_checks[j]/length(checks)), sample(checks,res))
        rand_checks[[j]] <- sample(v)
      }
    }
    w_map[w_map == 1] <- unlist(rand_checks)
    col_checks <- ifelse(w_map != 0, w_map, 0) 
  }
  list(map_checks = w_map, col_checks = col_checks)
}
