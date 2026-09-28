#' @importFrom stats  na.omit
available_percent <- function(n_rows, 
                              n_cols, 
                              checks, 
                              Option_NCD = FALSE, 
                              Visual_ch = NULL, 
                              visualCheck = FALSE,
                              kindExpt = NULL, 
                              stacked = "By Row", 
                              planter_mov1 = "serpentine", 
                              data = NULL,
                              dim_data = NULL, 
                              dim_data_1 = NULL, 
                              Block_Fillers = NULL) {
  if(n_rows < 5 || n_cols < 5) return(NULL)
  n_rows <- n_rows; n_cols = n_cols
  checks <- checks
  dim_expt <- n_rows * n_cols
  P <- matrix(c(rep(NA,10),c(1:10), rep(c(14:8,7,6,5),1),
                c(3,3,7,7,3,2,5,2,7,2)), ncol = 4, byrow = F)
  W <- matrix(c(rep(0,50),c(1:50), 
                rep(c(14:8,7,6,5),5),
                rep(c(3,3,7,7,3,2,5,2,7,2),5), 
                rep(c(0,1,2,3,4), each = 10), 
                rep(NA,50)),
              ncol = 6, byrow = F)
  w_map_list <- list()
  range_in_W <- 1:nrow(W)
  for (l in range_in_W) {
    if (n_cols < W[l,3]) next
    p_start <-  W[l,5]
    w_map_checks <- diagonals_checks(n_rows = n_rows, 
                                     n_cols = n_cols, 
                                     jump_by_cols = W[l,3],
                                     jump_by_rows = W[l,4], 
                                     checks = c(1,1,1,1), 
                                     p_start = p_start)
    w_map <- w_map_checks[[2]]
    W[l,6] <- sum(w_map != 0)
    w_map_list[[l]] <- w_map
  }
  W <- as.data.frame(W)
  W <- na.omit(W)
  W <- rbind(subset(W, W$V5 >= 2), subset(W, W$V5 < 2))
  W <- W[!duplicated(W$V6),]
  W <- W[order(W$V6),]
  G <- as.vector(W[,2])
  w_map_engage <- w_map_list[G]
  ncols_W <- nrow(W)
  multi <- kindExpt == "DBUDC"
  if (multi) {
    data_entry <- data
    data_entry1 <- data_entry[(length(checks) + 1):nrow(data_entry), ]
    Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
    Block_levels <- na.omit(Block_levels)
    data_dim_each_block <- numeric()
    for (i in Block_levels) { 
      data_dim_each_block[i] <- nrow(subset(data_entry, data_entry$BLOCK == i))
    }
    dim_data <- sum(data_dim_each_block)
  }
  if (multi == FALSE) {
    if (Option_NCD == TRUE) {
      M <- matrix(data = NA, ncol = 6, nrow = ncols_W, byrow = T)
      colnames(M) <- c("Options", "Percentage of Checks", 
                       "Total # of Check Plots",
                       "Total # of Fillers",
                       "Total # of Experimental Plots", 
                       "Total # of Plots")
    }
  } else if (multi == TRUE) {
    if (Option_NCD == TRUE) {
      M <- matrix(data = NA, ncol = 6, nrow = ncols_W, byrow = T)
      colnames(M) <- c("Options", "Percentage of Checks", 
                       "Total # of Check Plots", 
                       "Total # of Fillers",
                       "Total # of Experimental Plots",
                       "Total #r of Plots")
    }
  } 
  opts <- 1:length(w_map_engage)
  d_checks <- list()
  
  vis <- 0
  for (m in opts) {
    w_map <- w_map_engage[[m]]
    if (sum(w_map == 0) < dim_data_1) next
    n_Checks <- sum(w_map != 0)
    if (kindExpt == "SUDC") {
      if (Option_NCD == TRUE) {
        dim_data_entry <- dim_data
        real_dim_data_entry <- dim_data_1
        Fillers <- dim_expt - real_dim_data_entry - n_Checks
        limit_out <- checks + 1
        checks_in_first_row <- sum(w_map[1, ] != "0")
        if ((Fillers + checks_in_first_row) >= n_cols) next
        if (Fillers > 0 && Fillers < n_cols) {
          w_map <- top_row_fillers(w_map, Fillers, planter_mov1)
        }
        n_Checks <- sum(w_map == 1)
        pots <- nrow(w_map) * ncol(w_map)
        per <- round((n_Checks/pots)*100,2)
        expt_lines <- pots - n_Checks
        Fillers_t <- sum(w_map == "Filler")
        f_expt_lines <- expt_lines - Fillers_t
        M[m, c(1,2,3,4,5,6)] <- c(m, per, n_Checks, Fillers_t, f_expt_lines, pots)
      }
    } else if (kindExpt == "DBUDC") {
      if(dim_data > sum(w_map == 0) || dim_data < sum(w_map[n_rows:2,] == 0)) {
        next
      }
      if (stacked == "By Row") {
        if (Option_NCD == TRUE) {
          dim_data_entry <- dim_data
          real_dim_data_entry <- dim_data_1
          Fillers <- dim_expt - real_dim_data_entry - n_Checks
          if (diff(c(Fillers, (n_cols - 5))) <= 2) next 
          if (Fillers > 0 && Fillers < n_cols) {
            if (sum(w_map == 1) <= (length(checks) * 3)) next
            checks_in_first_row <- sum(w_map[1, ] != "0")
            if ((Fillers + checks_in_first_row) >= n_cols) next
            if (Fillers > ceiling(n_cols/2)) next
            w_map <- top_row_fillers(w_map, Fillers, planter_mov1)
          }
          n_Checks <- length(which(w_map == 1))
          Fillers <- length(which(w_map == "Filler"))
          pots <- nrow(w_map) * ncol(w_map)
          per <- round((n_Checks/pots)*100,1)
          expt_lines <- pots - n_Checks - Fillers
          if(dim_data_1 != expt_lines){
            Fillers <- NA
          }else Fillers <- Fillers
          M[m, c(1,2,3,4,5,6)] <- c(m, per, n_Checks, Fillers, expt_lines, pots)
        }
      } else if (stacked == "By Column") {
        if (Option_NCD == TRUE) {
          dim_data_entry <- dim_data
          real_dim_data_entry <- dim_data_1 
          Fillers <- dim_expt - real_dim_data_entry - n_Checks
          limit_out <- checks + 1
          if (diff(c(Fillers, (n_rows - 5))) <= 2) next
          if (Fillers > 0) {
            empty_rows <- which(w_map[, n_cols] == 0)
            if (Fillers > length(empty_rows)) next
            # Preserve the existing top-down placement, including checks
            # passed on the way to the last required empty cell.
            w_map[seq_len(empty_rows[Fillers]), n_cols] <- "Filler"
          }
          if (Fillers < 0 || Fillers > n_rows){
            Fillers <- 0
          }
          n_Checks <- sum(w_map == 1)
          pots <- nrow(w_map) * ncol(w_map)
          per <- round((n_Checks/pots)*100,2)
          expt_lines <- pots - n_Checks
          Fillers_t <- length(which(w_map == "Filler"))
          f_expt_lines <- expt_lines - Fillers_t
          M[m, c(1,2,3,4,5,6)] <- c(m, per, n_Checks, Fillers_t, f_expt_lines, pots)
        }
      }
    }
    d_checks[[m]] <- w_map
    W[m,1] <- per
  }
  realData <- dim_data_1
  if (Option_NCD == TRUE) {
    M <- subset(M, M[,4] >= 0 & M[,5] >= realData)
  }
  W[,2] <- 1:nrow(W)
  M <- na.omit(M)
  if (length(M) == 0) return(NULL)
  dt <- as.data.frame(M)
  dt <- dt[!duplicated(dt[,3]),]
  dt[,1] <- 1:nrow(dt)
  if (multi && Option_NCD == TRUE) {
    list(dt = dt, P = W, d_checks = d_checks, 
         data_dim_each_block = data_dim_each_block)
  } else if (multi) {
    list(dt = dt, P = W, d_checks = d_checks, 
         data_dim_each_block = data_dim_each_block)
  } else list(dt = dt, P = W, d_checks = d_checks)
}

#' @importFrom utils capture.output
#' @noRd
diagonals_checks <- function(n_rows = NULL, n_cols = NULL, jump_by_cols = 5, jump_by_rows = 4,
                             name_file = "Car2020", checks = c(1,2,3,4), p_start = NULL){
  
  di <- jump_by_cols - n_rows
  
  if (n_rows < jump_by_cols){
    
    n_rows <- n_rows + di
    
  }
  w_map <- matrix(data = 0, ncol = n_cols, nrow = n_rows, byrow = F)
  
  breaks_points <- seq(1, (jump_by_cols + p_start)*jump_by_rows, jump_by_rows)
  breaks_points <- rev(breaks_points)
  min_breaks_points <- numeric(length = jump_by_cols)
  
  for (i in 1:jump_by_cols){
    min_breaks_points[i] <- min(seq(breaks_points[i], 1, by = -jump_by_cols))
  }
  
  star_break_points <- list()
  for (l in 1:length(min_breaks_points)) {
    star_break_points[[l]] <- seq(min_breaks_points[l], n_rows, by = jump_by_cols)
  }
  
  ################ some test ####################
  rem <- (n_cols) %% jump_by_cols
  divs <- (n_cols - rem) / jump_by_cols
  ###############################################
  
  for(i in 1:length(star_break_points)) {
    w_map[star_break_points[[i]], i] <- 1
  }
  
  if ( divs > 1){
    new_divs <- 1:divs
    for (k in 2:length(new_divs)) {
      w_map[, (jump_by_cols + 1):(new_divs[k]*jump_by_cols)] <- w_map[, 1:jump_by_cols]
    }
  }
  ######## Final cheking ###################################### 
  if (rem > 0){
    w_map[, (divs*jump_by_cols + 1):n_cols] <- w_map[, 1:rem]
  }
  ############################################################
  
  if (di > 0){
    w_map <- w_map[-((n_rows-di+1):n_rows),]
  }
  n_Checks <- length(which(w_map == 1))
  pots <- dim(w_map)[1] * dim(w_map)[2]
  a <- capture.output(print(paste("You have",  n_Checks,"(",round((n_Checks/pots)*100,2),
                                  "%) pots available for Check of the", pots)))
  b <- capture.output(print(c(jump_by_cols, jump_by_rows)))
  
  ## Get random the checks into the whole experiment.
  res <- n_Checks %% length(checks)
  divv <- (n_Checks - res) / length(checks)
  if (res == 0){
    s <- rep(checks, n_Checks/length(checks))
    random_checks <- sample(s)
  }else{
    v <- c(rep(checks,divv), sample(checks,res))
    random_checks <- sample(v)
  }
  
  w_map[w_map == 1] <- random_checks
  output_list <- list(a, w_map, table(random_checks), b)
  
  return(output_list)
}

#' @noRd 
#' 
#' 
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
  validate_planter(planter_mov)
  my_P <- p
  if (!is.null(percent) && is.null(exptlines)) {
    my_index <- subset(my_P, my_P[,1] == percent)[1,2]
  } else if (is.null(percent) && !is.null(exptlines)) {
    if (Option_NCD == TRUE) {
      d_checks <- d_checks[!sapply(d_checks, is.null)]
      k <- 1
      my_index <- k
    }
  }
  my_index <- as.numeric(my_index)
  w_map <- d_checks[[my_index]]
  if(is.null(w_map)) fieldhub_abort("Input w_map is NULL.")
  n_cols <- ncol(w_map)
  n_rows <- nrow(w_map)
  if (is.null(w_map)) {
    fieldhub_abort("Internal error: the checks map is missing.",
                   class = "fieldhub_internal_error")
  }
  multi <- kindExpt == "DBUDC"
  
  if (multi == TRUE) {
    if (stacked == "By Row"){
      data_dim_each_block <- data_dim_each_block
      my_row_sets <- automatically_cuts(
        data = w_map, 
        planter_mov = planter_mov,
        stacked = "By Row",
        dim_data = data_dim_each_block)[[1]]
      if(is.null(my_row_sets)) return(NULL)
      blocks <- length(my_row_sets)
    }else {
      data_dim_each_block <- data_dim_each_block
      cuts_by_c <- automatically_cuts(
        data = w_map, 
        planter_mov = planter_mov, 
        stacked = "By Column",
        dim_data = data_dim_each_block)
      if(is.null(cuts_by_c)) return(NULL)
      blocks <- length(cuts_by_c)
      m = diff(cuts_by_c)
      my_col_sets = c(cuts_by_c[1], m)
    }
    
    if (stacked == "By Column") {
      w_map_split <- split_matrix_into_blocks(w_map, 
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
      w_map_split <- split_matrix_into_blocks(w_map, 
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

#' Turn the empty cells of the top row of a checks map into fillers, from the
#' end of the planting path backward, until there are n; the checks passed on
#' the way become fillers too
#' @noRd
top_row_fillers <- function(w_map, n, planter) {
  for (col in path_end(nrow(w_map), ncol(w_map), planter, ncol(w_map))[, "col"]) {
    w_map[1, col] <- ifelse(w_map[1, col] == 0, "Filler", "-9")
    if (sum(w_map[1, ] == "Filler") == n) break
  }
  w_map[w_map == "-9"] <- "Filler"
  w_map
}
