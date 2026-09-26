ARCBD_name <- function(Fillers = NULL, b = NULL, layout = NULL, name.expt = NULL,
                       planter = NULL) {
  if (all(c("serpentine", "cartesian") != planter)) {
    fieldhub_abort("Input planter is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  r_map <- layout
  nrows <- b
  ncols <- ncol(r_map)
  if (!is.null(name.expt)) {
    Name_expt <- name.expt 
  }else Name_expt = paste0(rep("ARCBD", times = 1), 1)
  split_names <- matrix(data = Name_expt, ncol = ncols, nrow = nrows)
  if (Fillers > 0) {
    split_names1 <- matrix(data = Name_expt, ncol = ncols, nrow = nrows)
    split_names1[1, filler_columns(nrows, ncols, planter, Fillers)] <- "Filler"
  }
  if (Fillers == 0) {
    return(list(my_names = split_names))
  }else {
    return(list(my_names = split_names, my_names_Filles = split_names1))
  }
}
