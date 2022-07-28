names_plot <- read.csv("inst/test_names_plot.csv", header = TRUE)
# View(names_plot)

planter <- "serpentine"
planter <- "cartesian"
plot_number <- 101
plot_number <- c(1, 1001, 4005)
plots <- prod(dim(names_plot))
expts <- as.vector(unlist(names_plot))
dim_each_block <- as.vector(table(expts))
b <- length(dim_each_block)
expts_ft <- factor(expts, levels = unique(expts))
expt_levels <- levels(expts_ft)

if (length(plot_number) != b) {
  serie_plot_numbers <- 1:plots
  plot_number_blocks <- FielDHub:::split_vectors(x = serie_plot_numbers, 
                                                 len_cuts = dim_each_block)
} else {
  plot_number_blocks <- vector(mode = "list", length = b)
  for (i in 1:b) {
    serie<- plot_number[i]:(plot_number[i] + dim_each_block[i] - 1)
    if (length(serie) == dim_each_block[i]) {
      plot_number_blocks[[i]] <- serie
    } else stop("problem in length of the current serie")
  }
}
plot_number_blocks

plot_number_layout <- names_plot
planter <- "serpentine"
if(planter == "cartesian") {
  if (nrow(plot_number_layout) %% 2 != 0) {
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


plot_number_by_col <- function(planter = "serpentine", 
                               plot_number_start = NULL, 
                               layout_names = NULL) {
  plot_number <- plot_number_start
  names_plot <- layout_names
  plots <- prod(dim(names_plot))
  expts <- as.vector(unlist(names_plot))
  dim_each_block <- as.vector(table(expts))
  b <- length(dim_each_block)
  expts_ft <- factor(expts, levels = unique(expts))
  expt_levels <- levels(expts_ft)
  
  if (length(plot_number) != b) {
    serie_plot_numbers <- 1:plots
    plot_number_blocks <- FielDHub:::split_vectors(x = serie_plot_numbers, 
                                                   len_cuts = dim_each_block)
  } else {
    plot_number_blocks <- vector(mode = "list", length = b)
    for (i in 1:b) {
      serie<- plot_number[i]:(plot_number[i] + dim_each_block[i] - 1)
      if (length(serie) == dim_each_block[i]) {
        plot_number_blocks[[i]] <- serie
      } else stop("problem in length of the current serie")
    }
  }
  plot_number_layout <- names_plot
  if(planter == "cartesian") {
    if (nrow(plot_number_layout) %% 2 != 0) {
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
  plot_number_layout <- apply( plot_number_layout, c(1,2) , as.numeric)
  return(list(
    w_map_letters1 = plot_number_layout, 
    target_num1 = plot_number_blocks
    )
  )
}


plot_number_by_col(planter = "serpentine", plot_number_start = 101, layout_names = names_plot)











