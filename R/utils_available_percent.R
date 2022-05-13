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
                rep(c(0,1,2,3,4),each = 10), 
                rep(NA,50)),
              ncol = 6, byrow = F)
  w_map_list <- list()
  for (l in 1:nrow(W)) {
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
  multi <- kindExpt == "RDC" || kindExpt == "DBUDC"
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
  if(multi == FALSE) {
    if (Option_NCD == TRUE) {
    M <- matrix(data = NA, ncol = 6, nrow = ncols_W, byrow = T)
    colnames(M) <- c("Options", "Percentage of Checks", 
                     "Total # of Check Plots",
                     "Total # of Fillers",
                     "Total # of Experimental Plots", 
                     "Total # of Plots")
    } else{
      M <- matrix(data = NA, ncol = 5, nrow = ncols_W, byrow = T)
      colnames(M) <- c("Options", "% of Diagonal Checks", 
                       "Total # of Check Plots",
                       "Total # of Experimental Plots", 
                       "Total # of Plots")
    }
  }else if(multi == TRUE){
    if(Option_NCD == TRUE){
      M <- matrix(data = NA, ncol = 6, nrow = ncols_W, byrow = T)
      colnames(M) <- c("Options", "Percentage of Checks", 
                       "Total # of Check Plots", 
                       "Total # of Fillers",
                       "Total # of Experimental Plots",
                       "Total #r of Plots")
    }else{
      M <- matrix(data = NA, ncol = 5, nrow = ncols_W, byrow = T)
      colnames(M) <- c("Options", "% of Diagonal Checks", 
                       "Total # of Check Plots",
                       "Total # of Experimental Plots", 
                       "Total # of Plots")
    }
  } 
  opts <- 1:length(w_map_engage)
  d_checks <- list()
  vis <- 0
  for (m in opts) {
    w_map <- w_map_engage[[m]] 
    n_Checks <- sum(w_map != 0)
    if (kindExpt == "SUDC") {
      if (Option_NCD == TRUE) {
        dim_data_entry <- dim_data
        real_dim_data_entry <- dim_data_1
        Fillers <- dim_expt - real_dim_data_entry - n_Checks
        limit_out <- checks + 1
        if (diff(c(Fillers, (n_cols - 5))) <= 2) next 
        if (Fillers > 0 && Fillers < n_cols) {
          checks_in_first_row <- sum(w_map[1, ] != "0")
          if ((Fillers + checks_in_first_row) >= n_cols) next
          if (Fillers > ceiling(n_cols/2)) next
          if (n_rows %% 2 == 0) {
            if(planter_mov1 == "serpentine") {
              i <- 1
              repeat {
                w_map[1,i] <- ifelse(w_map[1,i] == 0, "Filler", "-9")
                if (sum(w_map[1, ] == "Filler") == Fillers) break
                i <- i + 1
              }
              w_map[w_map == "-9"] <- "Filler"
            }else{
              i <- n_cols
              repeat{
                w_map[1,i] <- ifelse(w_map[1,i] == 0, "Filler", "-9")
                if (sum(w_map[1, ] == "Filler") == Fillers) break
                i <- i - 1
              }
              w_map[w_map == "-9"] <- "Filler"
            }
          }else{
            i <- 0
            repeat{
              w_map[1, n_cols - i] <- ifelse(w_map[1, n_cols - i] == 0, "Filler", "-9")
              if (sum(w_map[1, ] == "Filler") == Fillers) break
              i <- i + 1
            }
            w_map[w_map == "-9"] <- "Filler"
          }
        }
        n_Checks <- sum(w_map == 1)
        pots <- nrow(w_map) * ncol(w_map)
        per <- round((n_Checks/pots)*100,2)
        expt_lines <- pots - n_Checks
        
        Fillers_t <- length(which(w_map == "Filler"))
        f_expt_lines <- expt_lines - Fillers_t
        M[m, c(1,2,3,4,5,6)] <- c(m, per, n_Checks, Fillers_t, f_expt_lines, pots)
      } else {
        n_Checks <- length(which(w_map == 1))
        pots <- nrow(w_map) * ncol(w_map)
        per <- round((n_Checks/pots)*100,1)
        expt_lines <- pots - n_Checks
        f_expt_lines <- expt_lines
        M[m, c(1,2,3,4,5)] <- c(m, per, n_Checks, f_expt_lines, pots)
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
            if (n_rows %% 2 == 0) {
              if(planter_mov1 == "serpentine") {
                i <- 1
                repeat {
                  w_map[1,i] <- ifelse(w_map[1,i] == 0, "Filler", "-9")
                  if (sum(w_map[1, ] == "Filler") == Fillers) break
                  i <- i + 1
                }
                w_map[w_map == "-9"] <- "Filler"
              } else {
                i <- n_cols
                repeat {
                  w_map[1,i] <- ifelse(w_map[1,i] == 0, "Filler", "-9")
                  if (sum(w_map[1, ] == "Filler") == Fillers) break
                  i <- i - 1
                }
                w_map[w_map == "-9"] <- "Filler"
              }
            } else {
              i <- 0
              repeat {
                w_map[1, n_cols - i] <- ifelse(w_map[1, n_cols - i] == 0, "Filler", "-9")
                if (sum(w_map[1, ] == "Filler") == Fillers) break
                i <- i + 1
              }
              w_map[w_map == "-9"] <- "Filler"
            }
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
        } else if (Option_NCD == FALSE) {
          n_Checks <- length(which(w_map == 1))
          pots <- nrow(w_map) * ncol(w_map)
          per <- round((n_Checks/pots)*100,1)
          expt_lines <- pots - n_Checks
          M[m, c(1,2,3,4,5)] <- c(m, per, n_Checks, expt_lines, pots)
        }
      } else if (stacked == "By Column") {
        if (Option_NCD == TRUE) {
          dim_data_entry <- dim_data
          real_dim_data_entry <- dim_data_1 
          Fillers <- dim_expt - real_dim_data_entry - n_Checks
          limit_out <- checks + 1
          if (diff(c(Fillers, (n_rows - 5))) <= 2) next
          if (Fillers > 0) {
            next
            i <- 0
            repeat {
              w_map[1 + i, n_cols] <- ifelse(w_map[1 + i, n_cols] == 0, "Filler", "-9")
              if (sum(w_map[, n_cols] == "Filler") == Fillers) break
              i <- i + 1
            }
            w_map[w_map == "-9"] <- "Filler"
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
        } else if (Option_NCD == FALSE) {
          n_Checks <- length(which(w_map == 1))
          pots <- nrow(w_map) * ncol(w_map)
          per <- round((n_Checks/pots)*100,1)
          expt_lines <- pots - n_Checks
          M[m, c(1,2,3,4,5)] <- c(m, per, n_Checks, expt_lines, pots)
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