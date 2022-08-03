ibd_plot_numbers <- function(nt = NULL, plot.number = NULL, r = NULL, l = NULL) {
  
  if (!is.null(plot.number)) {
    if (any(plot.number < 1)) stop ("Plot numbers should be possitive values.")
    if (any(plot.number %% 1 != 0)) stop ("Plot numbers should be integer values.")
    if (!is.numeric(plot.number)) stop ("Plot numbers should be integer values.")
    
    if (length(plot.number) == l) {
      plot.number <- plot.number[1:l]
      plot.number <- seriePlot.numbers(plot.number = plot.number, reps = r, l = l, t = nt)
      p.number.loc <- vector(mode = "list", length = l)
      for (k in 1:l) {
        plotsDesign <- matrix(data = NA, nrow = nt, ncol = r)
        for(s in 1:r) {
          D <- plot.number[[k]]
          plots <- D[s]:(D[s] + (nt) - 1)
          plotsDesign[,s] <- plots
        }
        p.number.loc[[k]] <- as.vector(plotsDesign)
      }
    }else if (length(plot.number) < l) {
      plot.number <- seq(1001, 1000*(l+1), 1000)
      plot.number <- seriePlot.numbers(plot.number = plot.number, reps = r, l = l, t = nt)
      p.number.loc <- vector(mode = "list", length = l)
      for (k in 1:l) {
        plotsDesign <- matrix(data = NA, nrow = nt, ncol = r)
        for(s in 1:r) {
          D <- plot.number[[k]]
          plots <- D[s]:(D[s] + (nt) - 1)
          plotsDesign[,s] <- plots
        }
        p.number.loc[[k]] <- as.vector(plotsDesign)
      }
      warning("Length of plot numbers is lower than the number of locations.")
    }else if (length(plot.number) > l) {
      plot.number <- plot.number[1:l]
      plot.number <- seriePlot.numbers(plot.number = plot.number, reps = r, l = l, t = nt)
      plotsDesign <- matrix(data = NA, nrow = nt, ncol = l)
      p.number.loc <- vector(mode = "list", length = l)
      for (k in 1:l) {
        plotsDesign <- matrix(data = NA, nrow = nt, ncol = r)
        for(s in 1:r) {
          D <- plot.number[[k]]
          plots <- D[s]:(D[s] + (nt) - 1)
          plotsDesign[,s] <- plots
        }
        p.number.loc[[k]] <- as.vector(plotsDesign)
      }
      warning("Length of plot numbers is larger than number of locations.")
    }
  }else {
    plot.number <- seq(1001, 1000*(l+1), 1000)
    plot.number <- seriePlot.numbers(plot.number = plot.number, reps = r, l = l, t = nt)
    p.number.loc <- vector(mode = "list", length = l)
    for (k in 1:l) {
      plotsDesign <- matrix(data = NA, nrow = nt, ncol = r)
      for(s in 1:r) {
        D <- plot.number[[k]]
        plots <- D[s]:(D[s] + (nt) - 1)
        plotsDesign[,s] <- plots
      }
      p.number.loc[[k]] <- as.vector(plotsDesign)
    }
    warning("Since plot numbers are NULL, these will be generated automatically.")
  }
  
  return(plot.number = p.number.loc)
  
}
