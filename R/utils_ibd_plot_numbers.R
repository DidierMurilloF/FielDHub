ibd_plot_numbers <- function(nt = NULL, plot.number = NULL, r = NULL, l = NULL) {
  
  if (!is.null(plot.number)) {
    validate_plot_starts(plot.number)
    if (any(plot.number < 1)) fieldhub_abort("Plot numbers should be possitive values.")
    
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
      default_plots <- seq(1001, 1000*(l+1), 1000)
      warn_default_plot_numbers(plot.number, l, default_plots)
      plot.number <- seriePlot.numbers(plot.number = default_plots, reps = r, l = l, t = nt)
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
    }else if (length(plot.number) > l) {
      default_plots <- plot.number[1:l]
      warn_default_plot_numbers(plot.number, l, default_plots)
      plot.number <- seriePlot.numbers(plot.number = default_plots, reps = r, l = l, t = nt)
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
    }
  }else {
    default_plots <- seq(1001, 1000*(l+1), 1000)
    warn_default_plot_numbers(plot.number, l, default_plots)
    plot.number <- seriePlot.numbers(plot.number = default_plots, reps = r, l = l, t = nt)
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
  }
  
  return(plot.number = p.number.loc)
  
}
