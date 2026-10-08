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
  # plot.number holds the effective per-location starting plot numbers used
  # above (whichever branch ran), for callers that need to record what was
  # actually built rather than the raw argument they passed in.
  if (crd == TRUE) {
    return(list(plots = plot.random, plot_number = plot.number))
  }else {
    return(list(plots = plot.random, plots_loc = p.number.loc, plot_number = plot.number))
  }
}

#' @noRd 
#' 
#' 
seriePlot.numbers <- function(plot.number = NULL, reps = NULL, l = NULL, t = NULL) {
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
    # Every current caller pre-normalizes plot.number to length l before
    # calling seriePlot.numbers(), so this branch is unreachable in
    # practice; kept as a defensive fallback (no supplied/caller_supplied
    # gate needed since it never fires).
    default_plots <- default_plot_starts(l, 1001)
    warn_default_plot_numbers(plot.number, l, default_plots)
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
