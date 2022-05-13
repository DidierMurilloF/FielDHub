continue_plot_Cart <- function(datos){
  for (i in 1:nrow(datos)){
    datos[i,] <- rev(datos[i, ])
  }
  return(datos)
}

