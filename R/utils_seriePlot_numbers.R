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
      # else if (plot.number[k] >= 1000 && plot.number[k] < 10000) {
      #   plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)[1:reps]
      # }else if (plot.number[k] >= 10000 && plot.number[k] < 100000) {
      #   plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)[1:reps]
      # }else if (plot.number[k] >= 100000) {
      #   plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)[1:reps]
      # }
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