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


