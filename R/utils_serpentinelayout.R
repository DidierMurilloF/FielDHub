serpentinelayout <-  function(datos, opt = 1){
  if (opt == 1) {
    if (nrow(datos) %% 2 == 0) {
      for (i in seq(2,nrow(datos),2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }else{
      for (i in seq(1,nrow(datos),2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }
  }else if (opt == 2) {
    if (nrow(datos) %% 2 == 0) {
      for (i in seq(nrow(datos),1,-2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }else{
      for (i in seq(2, nrow(datos), 2)) {
        datos[i,] <- rev(datos[i, ])
      }
    }
  }
  return(datos)
}