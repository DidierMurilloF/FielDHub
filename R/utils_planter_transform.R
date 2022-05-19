#' planter_transform 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
planter_transform <- function(plots = NULL, planter = "serpentine", cols = NULL,
                              reps = NULL, mode = NULL, units = NULL) {
  PLOTS <- plots
  n_Reps <- reps
  if (!is.null(mode)) {
    if (mode == "Grid") {
      repCols <- units
    }else repCols <- cols/n_Reps
    nt <- length(PLOTS)/n_Reps
    RPLOTS <- split_vectors(x = PLOTS, len_cuts = rep(rep(nt, n_Reps), each = 1))
    rep_breaks <- vector(mode = "list", length = n_Reps)
    for (nreps in 1:n_Reps) {
      nCuts <- length(RPLOTS[[nreps]]) / repCols
      rep_breaks[[nreps]] <- split_vectors(x = RPLOTS[[nreps]], len_cuts = rep(repCols, each = nCuts))
    }
    lngt1 <- 1:length(rep_breaks)
    lngt2 <- 1:length(rep_breaks[[1]])
    new_breaks <- list()
    k <- 1
    for (n in lngt1) {
      for (m in lngt2) {
        if (m %% 2 == 0) {
          new_breaks[[k]] <- rev(unlist(rep_breaks[[n]][m]))
        } else new_breaks[[k]] <- rep_breaks[[n]][m]
        k <- k + 1
      }
    }
  } else {
    nCuts <- length(PLOTS) / cols
    breaks <- split_vectors(x = PLOTS, len_cuts = rep(cols, each = nCuts))
    lngt <- 1:length(breaks)
    new_breaks <- vector(mode = "list", length = nCuts)
    for (n in lngt) {
      if (n %% 2 == 0) {
        new_breaks[[n]] <- rev(breaks[[n]])
      } else new_breaks[[n]] <- breaks[[n]]
    }
  }
  PLOTS_serp <- as.vector(unlist(new_breaks))
  if (planter == "serpentine") {
    New_PLOTS <- as.vector(PLOTS_serp)
  } else New_PLOTS <- as.vector(PLOTS)
  
  return(PLOTS = New_PLOTS)
}
