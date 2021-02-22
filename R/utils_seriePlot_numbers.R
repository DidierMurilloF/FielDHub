seriePlot.numbers <- function(plot.number = NULL, reps = NULL, l = NULL) {
  
  if (!is.null(plot.number)) {
    
    if (any(plot.number < 1)) stop ("Plot numbers should be possitive values.")
    if (any(plot.number %% 1 != 0)) stop ("Plot numbers should be integer values.")
    
    if (length(plot.number) == l) {
      
      plot.number <- plot.number[1:l]
      
    }else if (length(plot.number) < l) {
      
      plot.number <- rep(plot.number[1], l)
      
      warning("Length of plot numbers is lower than number of locations. Only the first is used.")
      
    }else if (length(plot.number) > l) {
      
      plot.number <- plot.number[1:l]
      
      warning("Length of plot numbers is greater than number of locations.")
      
    }
    
  }else {
    
    stop ("Please, input the plot number(s).")
    
  }
  
  plot.numbs <- list()
  
  for (k in 1:l) {
    
    if (plot.number[k] == 1) {
      plot.numbs[[k]] <- seq(1, (101)*(reps-1), 100)
    }else if (plot.number[k] > 1 && plot.number[k] < 1000) {
      plot.numbs[[k]] <- seq(plot.number[k], (101)*reps, 100)
    }else if (plot.number[k] >= 1000 && plot.number[k] < 10000) {
      plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)
    }else if (plot.number[k] >= 10000 && plot.number[k] < 100000) {
      plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)
    }else if (plot.number[k] >= 100000) {
      plot.numbs[[k]] <- seq(plot.number[k], plot.number[k]+(100*(reps-1)), 100)
    }
    
  }
  
  return(plot.numbs)
  
}