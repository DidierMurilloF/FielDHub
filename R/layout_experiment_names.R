ARCBD_name <- function(Fillers = NULL, b = NULL, layout = NULL, name.expt = NULL,
                       planter = NULL) {
  validate_planter(planter)
  r_map <- layout
  nrows <- b
  ncols <- ncol(r_map)
  if (!is.null(name.expt)) {
    Name_expt <- name.expt 
  }else Name_expt = paste0(rep("ARCBD", times = 1), 1)
  split_names <- matrix(data = Name_expt, ncol = ncols, nrow = nrows)
  if (Fillers > 0) {
    split_names1 <- matrix(data = Name_expt, ncol = ncols, nrow = nrows)
    split_names1[1, filler_columns(nrows, ncols, planter, Fillers)] <- "Filler"
  }
  if (Fillers == 0) {
    return(list(my_names = split_names))
  }else {
    return(list(my_names = split_names, my_names_Filles = split_names1))
  }
}

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
    if (Fillers > 0) {
      split_names[1, filler_columns(nrow(w_map), ncol(w_map), planter, Fillers)] <- "Filler"
    }
  }
  return(list(my_names = split_names))
}
