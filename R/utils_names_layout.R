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
#' @noRd 
no_random_arcbd <- function(checksMap = NULL, 
                            data_Entry = NULL, 
                            planter = "serpentine") {
  w_map <- fill_along_path(checksMap, as.vector(data_Entry), planter)
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
#' @noRd
#'
#'
plot_number_splits <- function(plot.number = NULL, reps = NULL, l = NULL, t = NULL, crd = FALSE,
                               supplied = TRUE) {
  b <- reps
  wp <- t
  if (!is.null(plot.number)) {
    validate_plot_starts(plot.number)
    if (any(plot.number < 1)) fieldhub_abort("Plot numbers should be positive values.")
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
      default_plots <- default_plot_starts(l, 1001)
      warn_default_plot_numbers(plot.number, l, default_plots, caller_supplied = supplied)
      plot.number <- default_plots
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
    }else if (length(plot.number) > l) {
      default_plots <- plot.number[1:l]
      warn_default_plot_numbers(plot.number, l, default_plots, caller_supplied = supplied)
      plot.number <- default_plots
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
    }
  }else {
    default_plots <- default_plot_starts(l, 1001)
    warn_default_plot_numbers(plot.number, l, default_plots, caller_supplied = supplied)
    plot.number <- default_plots
    plot.number_serie <- seriePlot.numbers(plot.number = plot.number, reps = b, l = l, t = wp)
    plot.random <- matrix(data = NA, nrow = wp * b, ncol = l)
    for (k in 1:l) {
      D <- plot.number_serie[[k]]
      plots <- D[1]:(D[1] + (wp * b) - 1)
      plot.random[,k] <- replicate(1, sample(plots))
    }
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
seriePlot.numbers <- function(plot.number = NULL, reps = NULL, l = NULL, t = NULL,
                              supplied = TRUE) {
  overlap <- FALSE
  if (t >= 100) overlap <- TRUE
  if (!is.null(plot.number)) {
    validate_plot_starts(plot.number)
    if (any(plot.number < 1)) fieldhub_abort("Plot numbers should be possitive values.")
    if (length(plot.number) == l) {
      plot.number <- plot.number[1:l]
    }else if (length(plot.number) < l) {
      plot.number <- rep(plot.number[1], l)
    }else if (length(plot.number) > l) {
      plot.number <- plot.number[1:l]
    }
  }else {
    default_plots <- default_plot_starts(l, 1001)
    warn_default_plot_numbers(plot.number, l, default_plots, caller_supplied = supplied)
    plot.number <- default_plots
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
  b <- length(expe_names)
  expts_ft <- factor(expe_names, levels = unique(expe_names))
  expt_levels <- levels(expts_ft)
  # Count plots per experiment in the order of expe_names, not alphabetically
  dim_each_block <- as.vector(table(factor(expts, levels = expt_levels)))
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
      } else fieldhub_abort("problem in length of the current serie")
    }
  }
  plot_number_layout <- names_plot
  # Number the plots of each experiment along the planting path
  path <- field_path(nrow(plot_number_layout), ncol(plot_number_layout), planter)
  for (blocks in 1:b) {
    cells <- path[which(plot_number_layout[path] == expt_levels[blocks]), , drop = FALSE]
    plot_number_layout[cells] <- plot_number_blocks[[blocks]][seq_len(nrow(cells))]
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
