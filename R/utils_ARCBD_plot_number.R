ARCBD_plot_number <- function(plot.number = NULL, 
                              planter = "serpentine", 
                              b = NULL, 
                              name.expt = NULL, 
                              Fillers = NULL,
                              nameEXPT = NULL) {
  if (all(c("serpentine", "cartesian") != planter)) {
    stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  datos_name <- nameEXPT
  nrows <- b
  ncols <- ncol(datos_name)
  datos_name = as.matrix(datos_name) 
  movement_planter <- planter
  plot_n_start <- plot.number
  if (!is.null(name.expt)) { 
    Name_expt <- name.expt  
  }else Name_expt = paste0(rep("ARCBD", times = 1), 1)
  for (i in plot_n_start) {
    my_split_plot_nub <- plot_number(
      planter = planter,
      plot_number_start = i,
      layout_names = datos_name,
      expe_names = Name_expt,
      fillers = 0
    )[[1]]
  }

  plot_num1 <- my_split_plot_nub
  if (Fillers > 0) {
    if (nrows %% 2 == 0) {
      if(planter == "serpentine") {
        plot_num1[1, 1:Fillers] <- 0
      }else{
        plot_num1[1,((ncols + 1) - Fillers):ncols] <- 0
      }
    }else {
      plot_num1[1,((ncols + 1) - Fillers):ncols] <- 0
    }
  }
  return(list(plot_num = plot_num1))
}



