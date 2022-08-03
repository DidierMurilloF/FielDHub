continue_plot_Serp <- function(datos){
  if (nrow(datos) %% 2 == 0) {
    for (i in seq(2,nrow(datos),2)) {
      datos[i,] <- rev(datos[i, ])
    }
  }else{
    for (i in seq(1,nrow(datos),2)) {
      datos[i,] <- rev(datos[i, ])
    }
  }
  return(datos)
} 