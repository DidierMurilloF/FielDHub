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