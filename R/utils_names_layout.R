#' @noRd 
names_layout <- function(w_map = NULL, 
                         stacked = "By Row", 
                         kindExpt = "DBUDC", 
                         data_dim_each_block = NULL,
                         planter = "serpentine", 
                         w_map_letters = NULL, 
                         expt_name = NULL, 
                         Checks = NULL) {
  myWay <- stacked
  checks <- Checks
  if (kindExpt == "DBUDC") {
    if (myWay == "By Row") {
      blocks <- length(data_dim_each_block)
      w_map_letters1 <- w_map_letters
      Index_block <- LETTERS[1:blocks]
      name_blocks <- expt_name
      z <- 1
      for(i in Index_block) { 
        w_map_letters1[w_map_letters1 == i] <- name_blocks[z] 
        z <- z + 1 
      } 
      checks_ch <- as.character(checks) 
      for(i in nrow(w_map_letters1):1) { 
        for(j in 1:ncol(w_map_letters1)) { 
          if (any(checks_ch %in% w_map_letters1[i, j]) & w_map_letters1[i,j] != "Filler") {
            if (j != ncol(w_map_letters1)){
              if (w_map_letters1[i, j + 1] == "Filler") {
                w_map_letters1[i, j] <- w_map_letters1[i, j - 1]
              } else w_map_letters1[i, j] <- w_map_letters1[i, j + 1]
            } else if (j == ncol(w_map_letters1)) {
              w_map_letters1[i, j] <- w_map_letters1[i, j - 1]
            }
          }
        }
      }
      split_names <- w_map_letters1
    } else {
      blocks <- length(data_dim_each_block)
      w_map_letters1 <- w_map_letters
      Name_expt <- expt_name
      if (length(Name_expt) == blocks || !is.null(Name_expt)) {
        name_blocks <- Name_expt
      }else {
        name_blocks <- paste(rep("Expt", blocks), 1:blocks, sep = "")
      }
      Index_block <- paste0("B", 1:blocks)
      Name_expt <- expt_name
      if (length(Name_expt) == blocks || !is.null(Name_expt)) {
        name_blocks <- Name_expt
      }else {
        name_blocks <- paste(rep("Expt", blocks), 1:blocks, sep = "")
      }
      z <- 1
      for(i in Index_block){ 
        w_map_letters1[w_map_letters1 == i] <- name_blocks[z] 
        z <- z + 1 
      } 
      checks_ch <- as.character(checks) 
      for(j in 1:ncol(w_map_letters1)) {
        for(i in nrow(w_map_letters1):1) { 
          if (any(checks_ch %in% w_map_letters1[i, j]) & w_map_letters1[i,j] != "Filler") {
            if (i != 1) {
              if (w_map_letters1[i - 1, j] == "Filler") {
                w_map_letters1[i, j] <- w_map_letters1[i + 1, j]
              } else {
                w_map_letters1[i, j] <- w_map_letters1[i - 1, j]
              }
            } else {
              w_map_letters1[i, j] <- w_map_letters1[i + 1, j]
            }
          } 
        }
      }
      split_names <- w_map_letters1
    }
  } else {
    split_names <- matrix(data = expt_name, ncol = dim(w_map)[2], nrow = dim(w_map)[1])
    Fillers <- sum(w_map == "Filler")
    n_cols <- ncol(w_map)
    n_rows <- nrow(w_map)
    if (Fillers > 0) {
      if (n_rows %% 2 == 0) {
        if (planter == "serpentine") {
          split_names[1, 1:Fillers] <- "Filler"
        } else {
          split_names[1,((n_cols + 1) - Fillers):n_cols] <- "Filler"
        }
      } else {
        split_names[1,((n_cols + 1) - Fillers):n_cols] <- "Filler"
      }
    }
  }
  return(list(my_names = split_names))
}
#' @noRd 
no_random_arcbd <- function(checksMap = NULL, 
                            data_Entry = NULL, 
                            planter = "serpentine") {
  w_map <- checksMap
  dim_w <- dim(w_map)[1]*dim(w_map)[2]
  target <- as.vector(data_Entry)
  v <- 1
  if(planter == "serpentine") {
    if (nrow(w_map) %% 2 == 0) {
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
    } else {
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
  } else {
    for(i in nrow(w_map):1) {
      for(j in 1:ncol(w_map)) {
        if (w_map[i,j] == 0) {
          w_map[i,j] <- target[v]
          v <- v + 1
        } else {
          w_map[i,j] <-  w_map[i,j]
          v <- v
        }
      }
    }
    v <- 1
  }
  w_map_letters <- w_map
  dim_each_block <- rep(ncol(w_map), nrow(w_map))
  return(list(rand = w_map, 
              len_cut = dim_each_block, 
              w_map_letters = w_map_letters))
}
#' @noRd 
order_ls <- function(S = NULL, data = NULL) {
  cindex <- ncol(S)
  rindex <- nrow(S)
  if (is.null(data)) {
    r <- paste("Row", 1:rindex, sep = " ")
    c <- paste("Column", 1:cindex, sep = " ")
  }else {
    r <- factor(data[,1], levels = as.character(unique(data[,1])))
    c <- factor(data[,2], levels = as.character(unique(data[,2])))
  }
  rnames <- rownames(S)
  cnames <- colnames(S)
  rOrder <- vector(mode = "numeric")
  cOrder <- vector(mode = "numeric")
  for (i in r) {rOrder[i] <- which(rnames == i)}
  for (j in c) {cOrder[j] <- which(cnames == j)}
  rOrder <- as.numeric(rOrder)
  cOrder <- as.numeric(cOrder)
  new_s <- S[,cOrder]
  new_s <- new_s[rOrder,]
  return(new_s)
}
#' @noRd 
paste_by_col <- function(files_list){
  len_list <- length(files_list)
  file_range <- 2:len_list
  if (len_list >= 2) {
    data_output <- files_list[[1]]
    for (d in file_range){
      data_output  <- cbind(data_output, files_list[[d]])
      data_output <- data_output 
    }
  }else{
    data_output <- files_list[[1]]
  }
  return(data_output)
}
#' @noRd 
#' 
#' 
paste_by_row <- function(files_list){
  len_list <- length(files_list)
  file_range <- 2:len_list
  if (len_list >= 2) {
    data_output <- files_list[[1]]
    for (d in file_range){
      data_output  <- rbind(data_output, files_list[[d]])
      data_output <- data_output 
    }
  }else{
    data_output <- files_list[[1]]
  }
  return(data_output)
}
#' planter_transform 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
planter_transform <- function(plots = NULL, planter = "serpentine", cols = NULL,
                              reps = NULL, mode = NULL, units = NULL) {
  PLOTS <- plots
  n_Reps <- reps
  if (!is.null(mode)) {
    if (mode == "Grid") {
      repCols <- units
    }else repCols <- cols/n_Reps
    nt <- length(PLOTS)/n_Reps
    RPLOTS <- split_vectors(x = PLOTS, len_cuts = rep(rep(nt, n_Reps), each = 1))
    rep_breaks <- vector(mode = "list", length = n_Reps)
    for (nreps in 1:n_Reps) {
      nCuts <- length(RPLOTS[[nreps]]) / repCols
      rep_breaks[[nreps]] <- split_vectors(x = RPLOTS[[nreps]], len_cuts = rep(repCols, each = nCuts))
    }
    lngt1 <- 1:length(rep_breaks)
    lngt2 <- 1:length(rep_breaks[[1]])
    new_breaks <- list()
    k <- 1
    for (n in lngt1) {
      for (m in lngt2) {
        if (m %% 2 == 0) {
          new_breaks[[k]] <- rev(unlist(rep_breaks[[n]][m]))
        } else new_breaks[[k]] <- rep_breaks[[n]][m]
        k <- k + 1
      }
    }
  } else {
    nCuts <- length(PLOTS) / cols
    breaks <- split_vectors(x = PLOTS, len_cuts = rep(cols, each = nCuts))
    lngt <- 1:length(breaks)
    new_breaks <- vector(mode = "list", length = nCuts)
    for (n in lngt) {
      if (n %% 2 == 0) {
        new_breaks[[n]] <- rev(breaks[[n]])
      } else new_breaks[[n]] <- breaks[[n]]
    }
  }
  PLOTS_serp <- as.vector(unlist(new_breaks))
  if (planter == "serpentine") {
    New_PLOTS <- as.vector(PLOTS_serp)
  } else New_PLOTS <- as.vector(PLOTS)
  
  return(PLOTS = New_PLOTS)
}

#' plot_CRD 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_CRD <- function(x = NULL, n_TrtGen = NULL, n_Reps = NULL, layout = 1, 
                     planter = "serpentine", l = 1) {
  
  site <- l
  locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
  nlocs <- length(levels(locations))
  newBooksLocs <- vector(mode = "list", length = nlocs)
  countLocs <- 1
  books0 <- list(NULL)
  books1 <- list(NULL)
  for (locs in levels(locations)) {
    NewBook <- x$fieldBook |>
      dplyr::filter(LOCATION == locs)
    
    cols <- rep(1:n_TrtGen, times = n_Reps)
    breaks <- split_vectors(x = cols, len_cuts = rep(n_TrtGen, each = n_Reps))
    lngt <- 1:length(breaks)
    new_breaks <- vector(mode = "list", length = n_Reps)
    for (n in lngt) {
      if (n %% 2 == 0) {
        new_breaks[[n]] <- rev(breaks[[n]])
      } else new_breaks[[n]] <- breaks[[n]]
    }
    COLUMN_serp <- unlist(new_breaks)
    if (planter == "serpentine") {
      COLUMN <- COLUMN_serp
    } else COLUMN <- rep(1:n_TrtGen, times = n_Reps)
    books0[[1]] <- NewBook |> 
      dplyr::mutate(ROW = rep(1:n_Reps, each = n_TrtGen),
                    COLUMN = COLUMN)
    cols <- rep(1:n_Reps, times = n_TrtGen)
    breaks <- split_vectors(x = cols, len_cuts = rep(n_Reps, each = n_TrtGen))
    lngt <- 1:length(breaks)
    new_breaks <- vector(mode = "list", length = n_TrtGen)
    for (n in lngt) {
      if (n %% 2 == 0) {
        new_breaks[[n]] <- rev(breaks[[n]])
      } else new_breaks[[n]] <- breaks[[n]]
    }
    COLUMN_serp <- unlist(new_breaks)
    if (planter == "serpentine") {
      COLUMN <- COLUMN_serp
    } else COLUMN <- rep(1:n_Reps, times = n_TrtGen)
    books1[[1]] <- NewBook |> 
      dplyr::mutate(ROW = rep(1:n_TrtGen, each = n_Reps),
                    COLUMN = COLUMN)
    books_crd <- c(books0, books1)
    newBooks <- books_crd[!sapply(books_crd,is.null)]
    newBooksLocs[[countLocs]] <- newBooks
    countLocs <- countLocs + 1
  }
  
  opt <- layout
  newBooksSelected <- newBooksLocs[[site]]
  opt_available <- 1:length(newBooksSelected)
  if (all(opt_available != opt)) {
    message(cat("\n",
                " Option for layout is not available!", "\n", "\n",
                "*********************************************", "\n",
                "*********************************************", "\n", "\n",
                "Layout options available for this design are:", "\n", "\n",
                opt_available, "\n", "\n",
                "*********************************************", "\n",
                "*********************************************"))
    return(NULL)
  }
  df1 <- newBooksSelected[opt]
  df <- as.data.frame(df1)
  
  if (x$infoDesign$id_design == 1) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,6,7,4:5)]
    df <- df[,c(1:3,6,7,4:5)]
    # Plot field layout
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    ds <- "Completely Randomized Design " 
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    p1 <- desplot::desplot(TREATMENT ~ COLUMN + ROW, flip = FALSE,
                           out2.gpar = list(col = "black", lty = 3), 
                           text = TREATMENT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE, 
                           gg=TRUE)
    p1 <- add_gg_features(p1)
    # Plot number layout
    df$REP <- as.factor(df$REP)
    df$PLOT <- as.character(df$PLOT)
    p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                           text = PLOT, 
                           cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE, 
                           gg=TRUE)
    p2 <- add_gg_features(p2)
  } else if (x$infoDesign$id_design == 4) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    nc <- ncol(df)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,(nc-1),nc,4:(nc-2))]
    df <- df[, c(1:3,(nc-1),nc,4:(nc-2))]
    if (x$infoDesign$kind == "CRD") {
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Full Factorial Design (CRD) " 
      main <- paste0(ds, rows, "X", cols)
      # Plot field layout
      p1 <- desplot::desplot(TRT_COMB ~ COLUMN + ROW, flip = FALSE,
                             out2.gpar = list(col = "black", lty = 3), 
                             text = TRT_COMB, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
      p1 <- add_gg_features(p1)
      
      # Plot number layout
      df$REP <- as.factor(df$REP)
      df$PLOT <- as.factor(df$PLOT)
      p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
      p2 <- add_gg_features(p2)
    }
  } else if (x$infoDesign$id_design == 5) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    nc <- ncol(df)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,(nc-1),nc,4:(nc-2))]
    df <- df[,c(1:3,(nc-1),nc,4:(nc-2))]
    if (x$infoDesign$typeDesign == "CRD") {
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Split Plot Design (CRD) " 
      main <- paste0(ds, rows, "X", cols)
      df$REP <- as.factor(df$REP)
      p1 <- desplot::desplot(TRT_COMB ~ COLUMN + ROW, flip = FALSE,
                             out1 = REP,
                             out2.gpar=list(col = "gray50", lwd = 1, lty = 1),
                             text = TRT_COMB, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = TRUE, 
                             gg=TRUE)
      p1 <- add_gg_features(p1)
      
      # Plot number layout
      df$REP <- as.factor(df$REP)
      df$PLOT <- as.factor(df$PLOT)
      p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
      p2 <- add_gg_features(p2)
    }
  } else if (x$infoDesign$id_design == 6) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    nc <- ncol(df)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,9,10,4:8)]
    df <- df[,c(1:3,9,10,4:8)]
    if (x$infoDesign$typeDesign == "CRD") {
      # df <- df[,c(1:3,9,10,4:8)]
      df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
      df$SUB_PLOT <- as.factor(df$SUB_PLOT)
      df$SUB_SUB_PLOT <- as.factor(df$SUB_SUB_PLOT)
      df$REP <- as.factor(df$REP)
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Split-Split Plot Design (CRD) " 
      main <- paste0(ds, rows, "X", cols)
      p1 <- desplot::desplot(REP ~ COLUMN + ROW, flip = FALSE,
                             out1 = REP,
                             out2 = WHOLE_PLOT,
                             out2.gpar=list(col = "gray50", lwd = 1, lty = 1),
                             text = WHOLE_PLOT, cex = 1, shorten = "no",
                             col = SUB_PLOT,
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = TRUE, 
                             gg=TRUE)
      p1 <- add_gg_features(p1)
      
      # PLot number layout
      df$PLOT <- as.factor(df$PLOT)
      p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
      p2 <- add_gg_features(p2)
    }
  }
  return(list(p1 = p1, p2 = p2, df = df, newBooks = newBooksSelected, 
              allSitesFieldbook = allSitesFieldbook))
}


#' @noRd 
#' 
#' 
plot_number_splits <- function(plot.number = NULL, reps = NULL, l = NULL, t = NULL, crd = FALSE) {
  b <- reps
  wp <- t
  if (!is.null(plot.number)) {
    if (any(plot.number < 1)) stop ("Plot numbers should be positive values.")
    if (any(plot.number %% 1 != 0)) stop ("Plot numbers should be integer values.")
    if (length(plot.number) == l) {
      plot.number <- plot.number[1:l]
      plot.number_serie <- seriePlot.numbers(plot.number = plot.number, reps = b, l = l, t = wp)
      plot.random <- matrix(data = NA, nrow = wp * b, ncol = l)
      if(crd) {
        for (k in 1:l) {
          D <- plot.number_serie[[k]]
          plots <- D[1]:(D[1] + (wp * b) - 1)
          plot.random[,k] <- replicate(1, sample(plots))
        }
      }else {
        p.number.loc <- vector(mode = "list", length = l) #b*l
        for (k in 1:l) {
          plot.random <- matrix(data = NA, nrow = wp, ncol = b)
          for(s in 1:b) {
            D <- plot.number_serie[[k]]
            plots <- D[s]:(D[s] + (wp) - 1)
            plot.random[,s] <- plots
          }
          p.number.loc[[k]] <- as.vector(plot.random)
        }
      }
    }else if (length(plot.number) < l) {
      plot.number <- seq(1001, 1000*(l+1), 1000)
      plot.number_serie <- seriePlot.numbers(plot.number = plot.number, reps = b, l = l, t = wp)
      plot.random <- matrix(data = NA, nrow = wp * b, ncol = l)
      if(crd) {
        for (k in 1:l) {
          D <- plot.number_serie[[k]]
          plots <- D[1]:(D[1] + (wp * b) - 1)
          plot.random[,k] <- replicate(1, sample(plots))
          
        }
      }else {
        p.number.loc <- vector(mode = "list", length = b*l)
        for (k in 1:l) {
          plot.random <- matrix(data = NA, nrow = wp, ncol = b)
          for(s in 1:b) {
            D <- plot.number_serie[[k]]
            plots <- D[s]:(D[s] + (wp) - 1)
            plot.random[,s] <- plots
          }
          p.number.loc[[k]] <- as.vector(plot.random)
        }
      }
      warning("The length of plot numbers is less than the number of locations.")
    }else if (length(plot.number) > l) {
      warning("The length of plot numbers is greater than the number of locations.")
      plot.number <- plot.number[1:l]
      plot.number_serie <- seriePlot.numbers(plot.number = plot.number, reps = b, l = l, t = wp)
      plot.random <- matrix(data = NA, nrow = wp * b, ncol = l)
      if(crd == TRUE) {
        for (k in 1:l) {
          D <- plot.number_serie[[k]]
          plots <- D[1]:(D[1] + (wp * b) - 1)
          plot.random[,k] <- replicate(1, sample(plots))
          
        }
      }else {
        p.number.loc <- vector(mode = "list", length = b*l)
        for (k in 1:l) {
          plot.random <- matrix(data = NA, nrow = wp, ncol = b)
          for(s in 1:b) {
            D <- plot.number_serie[[k]]
            plots <- D[s]:(D[s] + (wp) - 1)
            plot.random[,s] <- plots
          }
          p.number.loc[[k]] <- as.vector(plot.random)
        }
      }
      warning("Length of plot numbers is greater than location numbers.")
    }
  }else {
    plot.number <- seq(1001, 1000*(l+1), 1000)
    plot.number_serie <- seriePlot.numbers(plot.number = plot.number, reps = b, l = l, t = wp)
    plot.random <- matrix(data = NA, nrow = wp * b, ncol = l)
    for (k in 1:l) {
      D <- plot.number_serie[[k]]
      plots <- D[1]:(D[1] + (wp * b) - 1)
      plot.random[,k] <- replicate(1, sample(plots))
    }
    warning("Since plot numbers are NULL, they were generated automatically.")
  }
  if (crd == TRUE) {
    return(list(plots = plot.random))
  }else {
    return(list(plots = plot.random, plots_loc = p.number.loc))
  }
}

#' @noRd 
#' 
#' 
scrollY <- function(n_rows){
  scrollY <- "500px"
  if (n_rows >= 15 && n_rows < 30){
    scrollY <- "800px"
  }else if (n_rows >= 30 && n_rows < 40){
    scrollY <- "1200px"
  }else if(n_rows >= 40){
    scrollY <- "1500px"
  }
  return(scrollY)
}
#' @noRd 
#' 
#' 
seriePlot.numbers <- function(plot.number = NULL, reps = NULL, l = NULL, t = NULL) {
  overlap <- FALSE
  if (t >= 100) overlap <- TRUE
  if (!is.null(plot.number)) {
    if (any(plot.number < 1)) stop ("Plot numbers should be possitive values.")
    if (any(plot.number %% 1 != 0)) stop ("Plot numbers should be integer values.")
    if (length(plot.number) == l) {
      plot.number <- plot.number[1:l]
    }else if (length(plot.number) < l) {
      plot.number <- rep(plot.number[1], l)
    }else if (length(plot.number) > l) {
      plot.number <- plot.number[1:l]
    }
  }else {
    plot.number <- seq(1001, 1000*(l+1), 1000)
    warning("'plotNumber' was set up to its default values for each site.")
  }
  plot.numbs <- list()
  if (overlap == FALSE) {
    for (k in 1:l) {
      if (plot.number[k] == 1) {
        plot.numbs[[k]] <- seq(1, (100)*(reps), 100)[1:reps]
      }else if (plot.number[k] > 1) {
        # && plot.number[k] < 1000
        #plot.numbs[[k]] <- seq(plot.number[k], (101)*reps, 100)[1:reps]
        plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)[1:reps]
      }
    }
  }else {
    for (k in 1:l) {
      if (plot.number[k] == 1) {
        plot.numbs[[k]] <- seq(1, t*reps, t)
      }else if (plot.number[k] > 1 && plot.number[k] < 1000) {
        if (reps == 1) B <- 1 else B <- 0
        if (t == 100) R <- 1 else R <- 0
        #plot.numbs[[k]] <- seq(plot.number[k], (t+R)*reps + B, t)
        plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(t*(reps-1)), t)[1:reps]
      }else if (plot.number[k] >= 1000 && plot.number[k] < 10000) {
        plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(t*(reps-1)), t)[1:reps]
      }else if (plot.number[k] >= 10000 && plot.number[k] < 100000) {
        plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(t*(reps-1)), t)[1:reps]
      }else if (plot.number[k] >= 100000) {
        plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(t*(reps-1)), t)[1:reps]
      }
    }
  }
  return(plot.numbs)
}

#' @noRd 
#' 
#' 
split_vectors <- function(x, len_cuts){
  
  if (length(x) != sum(len_cuts)) {
    return(NULL)
  } 
  cut_test <- x; dim_each_split <- len_cuts
  entris <- list()
  v = 1; s = 1
  h <- dim_each_split[v]
  for (j in 1:length(dim_each_split)) {
    entris[[v]] <- cut_test[s:h]
    s = s + dim_each_split[v]
    v = v + 1
    h <- h + dim_each_split[v]
  }
  
  return(entris)
}
#' @noRd 
#' 
#' 
serpentinelayout <-  function(datos, opt = 1){
  if (opt == 1) {
    if (nrow(datos) %% 2 == 0) {
      for (i in seq(2,nrow(datos),2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }else{
      for (i in seq(1,nrow(datos),2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }
  }else if (opt == 2) {
    if (nrow(datos) %% 2 == 0) {
      for (i in seq(nrow(datos),1,-2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }else{
      for (i in seq(2, nrow(datos), 2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }
  }
  return(datos)
}

#' @noRd
plot_number <- function(planter = "serpentine", 
                        plot_number_start = NULL, 
                        layout_names = NULL,
                        expe_names, 
                        fillers) {
  plot_number <- plot_number_start
  names_plot <- as.matrix(layout_names)
  Fillers <- FALSE
  if (fillers > 0) Fillers <- TRUE
  plots <- prod(dim(names_plot))
  expts <- as.vector(names_plot)
  if (Fillers) {
    expts <- expts[!expts %in% "Filler"]
  }
  dim_each_block <- as.vector(table(expts))
  b <- length(expe_names)
  expts_ft <- factor(expe_names, levels = unique(expe_names))
  expt_levels <- levels(expts_ft)
  if (length(plot_number) != b) {
    start_plot <-  as.numeric(plot_number)
    serie_plot_numbers <- start_plot:(plots + start_plot - fillers)
    max_len_plots <- sum(dim_each_block)
    serie_plot_numbers <- serie_plot_numbers[1:max_len_plots]
    plot_number_blocks <- split_vectors(x = serie_plot_numbers, 
                                        len_cuts = dim_each_block)
  } else {
    plot_number_blocks <- vector(mode = "list", length = b)
    for (i in 1:b) {
      w <- 0
      # if (i == b) w <- fillers
      serie <- plot_number[i]:(plot_number[i] + dim_each_block[i] - 1 - w)
      if (length(serie) == dim_each_block[i]) {
        plot_number_blocks[[i]] <- serie
      } else stop("problem in length of the current serie")
    }
  }
  plot_number_layout <- names_plot
  if(planter == "cartesian") {
    for (blocks in 1:b) {
      v <- 1
      for (i in nrow(plot_number_layout):1) {
        for (j in 1:ncol(plot_number_layout)) {
          if (plot_number_layout[i,j] == expt_levels[blocks]) {
            plot_number_layout[i,j] <- plot_number_blocks[[blocks]][v]
            v <- v + 1
          }
        }
      }
    }
  } else if (planter == "serpentine") {
    for (blocks in 1:b) {
      v <- 1
      if (nrow(plot_number_layout) %% 2 == 0) {
        for(i in nrow(plot_number_layout):1) {
          if (i %% 2 == 0) {
            A <- 1:ncol(plot_number_layout)
          } else A <- ncol(plot_number_layout):1
          for (j in A) {
            if (plot_number_layout[i,j] == expt_levels[blocks]) {
              plot_number_layout[i,j] <- plot_number_blocks[[blocks]][v]
              v <- v + 1
            }
          }
        }
      } else {
        for (i in nrow(plot_number_layout):1){
          if (i %% 2 == 0) {
            A <- ncol(plot_number_layout):1
          } else A <- 1:ncol(plot_number_layout)
          for (j in A) {
            if (plot_number_layout[i,j] == expt_levels[blocks]) {
              plot_number_layout[i,j] <- plot_number_blocks[[blocks]][v]
              v <- v + 1
            }
          }
        }
      }
    }
  }
  if (Fillers) {
    plot_number_layout[plot_number_layout == "Filler"] <- 0
  }
  plot_number_layout <- apply( plot_number_layout, c(1,2) , as.numeric)
  return(list(
    w_map_letters1 = plot_number_layout, 
    target_num1 = plot_number_blocks
    )
  )
}

#' @noRd
get.levels <- function(k = NULL) {
  newlevels <- list();s <- 1
  for (i in k) {
    newlevels[[s]] <- rep(0:(i-1), 1)
    s <- s + 1
  }
  return(newlevels)
}
