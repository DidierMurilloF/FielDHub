names_diagonal <- function(nrows = NULL, 
                           ncols = NULL, 
                           randomChecksMap = NULL, 
                           kindExpt = NULL, 
                           checks = NULL,
                           planter = "serpentine",
                           myWay = "By Column",
                           Option_NCD = FALSE, 
                           expt_name = NULL,
                           data_entry = NULL, 
                           reps = NULL,
                           data_dim_each_block = NULL, 
                           w_map_letters1 = NULL) {
  w_map <- randomChecksMap
  n_rows <- nrows; n_cols <- ncols
  
  multi <- kindExpt == "RDC" || kindExpt == "DBUDC"
  
  if (multi == TRUE && Option_NCD == FALSE) {
    if (kindExpt == "DBUDC") {
      #req(available_percent1()$data_dim_each_block)
      if (myWay == "By Row") {
        #data_dim_each_block <- available_percent1()$data_dim_each_block
        my_row_sets <- automatically_cuts(data = w_map, planter_mov = planter,
                                          way = "By Row", dim_data = data_dim_each_block)[[1]]
        blocks <- length(my_row_sets)
      }else {
        #data_dim_each_block <- available_percent1()$data_dim_each_block
        cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                        dim_data = data_dim_each_block)  
        blocks <- length(cuts_by_c)
        m = diff(cuts_by_c)
        my_col_sets = c(cuts_by_c[1], m)
      }
      Name_expt <- expt_name
      if (length(Name_expt) == blocks || !is.null(Name_expt)) {
        name_expt <- Name_expt
      }else{
        name_expt = paste0(rep("Expt", times = blocks), 1:blocks)
      }
      if (myWay == "By Row"){
        split_names <- split_name(n_rows = n_rows, n_cols = n_cols, Name_expt = name_expt,
                                  by_row = TRUE, col_sets = NULL, row_sets = my_row_sets)
      }else{
        split_names <- split_name(n_rows = n_rows, n_cols = n_cols, Name_expt = name_expt,
                                  by_row = FALSE, col_sets = my_col_sets, row_sets = NULL)
      }
      list(my_names = split_names)
    }else if(kindExpt == "RDC") {
      reps <- as.numeric(reps)
      Name_expt <- as.character(expt_name)[1]
      if (!is.null(Name_expt)){
        Name_expt <- paste(Name_expt, "rep", sep = ".")
        expe_names = paste0(rep(Name_expt, times = reps), 1:reps)
      }else {
        expe_names = paste0(rep("EXPT1_Rep", times = reps), 1:reps)
      }
      if (myWay == "By Row"){
        v <- as.numeric(nrows)/as.numeric(reps)
        reps <- as.numeric(reps)
        s <- 0
        cuts <- numeric() 
        for (i in 1:reps) {
          cuts[i] <- v + s
          s <- s + v
        }
        lili <- list()
        s <- 1
        for (i in 1:length(cuts)) {
          lili[[i]] <- s:(cuts[i])
          s <- (cuts[i] + 1)
        }
        my_row_sets <- lili
        split_names <- split_name(n_rows = n_rows, n_cols = n_cols, Name_expt = expe_names,
                                  by_row = TRUE, col_sets = NULL, row_sets = my_row_sets)
        expt_reps <- 1:reps
        split_reps <- split_name(n_rows = n_rows, n_cols = n_cols, Name_expt = expt_reps,
                                 by_row = TRUE, col_sets = NULL, row_sets = my_row_sets)
      }else if (myWay == "By Column") {
        
        x <- as.numeric(ncols)/as.numeric(reps)
        reps <- as.numeric(reps)
        my_col_sets <- rep(x, reps)
        split_names <- split_name(n_rows = n_rows, n_cols = n_cols, Name_expt = expe_names,
                                  by_row = FALSE, col_sets = my_col_sets, row_sets = NULL)
        expt_reps <- 1:reps
        split_reps <- split_name(n_rows = n_rows, n_cols = n_cols, Name_expt = expt_reps,
                                 by_row = FALSE, col_sets = my_col_sets, row_sets = NULL)
      }
      list(my_names = split_names, my_reps = split_reps)
    }
  }else if(multi == TRUE && Option_NCD == TRUE){

    if (kindExpt == "DBUDC") {
      if (myWay == "By Row") {
        #data_dim_each_block <- available_percent1()$data_dim_each_block
        my_row_sets <- automatically_cuts(data = w_map, planter_mov = planter,
                                          way = "By Row", dim_data = data_dim_each_block)[[1]]
        blocks <- length(my_row_sets)
        #w_map_letters1 <- rand_lines()$w_map_letters
        Index_block <- LETTERS[1:blocks]
        Name_expt <- as.character(expt_name)
        if (length(Name_expt) == blocks) {
          name_blocks <- Name_expt
        }else {
          name_blocks <- paste(rep("Block", blocks), 1:blocks, sep = "")
        }
        z <- 1
        for(i in Index_block){ 
          w_map_letters1[w_map_letters1 == i] <- name_blocks[z] 
          z <- z + 1 
        } 
        checks_ch <- as.character(checks) 
        for(i in nrow(w_map_letters1):1) { 
          for(j in 1:ncol(w_map_letters1)) { 
            if (any(checks_ch %in% w_map_letters1[i, j]) && w_map_letters1[i,j] != "Filler") {
              if (j != ncol(w_map_letters1)){
                if(w_map_letters1[i, j + 1] == "Filler") {
                  w_map_letters1[i, j] <- w_map_letters1[i, j - 1]
                }else w_map_letters1[i, j] <- w_map_letters1[i, j + 1]
              }else if (j == ncol(w_map_letters1)) {
                w_map_letters1[i, j] <- w_map_letters1[i, j - 1]
              }
            }
          }
        }
        split_names <- w_map_letters1
      }else{
        return(NULL)
      }
    }else if(kindExpt == "RDC"){
      return(NULL)
    }
    list(my_names = split_names)
  }else{
    blocks <- 1
    if (!is.null(expt_name)) {
      Name_expt <- expt_name 
    }else Name_expt = paste0(rep("Expt", times = blocks), 1:blocks)
    split_names <- matrix(data = Name_expt, ncol = n_cols, nrow = n_rows)
    list(my_names = split_names)
  }
}