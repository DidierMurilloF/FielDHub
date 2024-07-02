#' Function to estimate the efficiencies of a IBD design
#' This function is sourced from the `blocksdesign` package.
#' @param TF Treatment factor levels
#' @param BF Block factor levels
#' @author Rodney Edmondson <rodney.edmondson at gmail.com>
#' @return List containing design and A efficiencies
#' @noRd
blockEstEffics = function(TF, BF) {
  TM = qr.Q(qr(scale(stats::model.matrix(~TF))[, -1]))
  BM = qr.Q(qr(scale(stats::model.matrix(~BF))[, -1]))
  if (nlevels(TF) <= nlevels(BF)) 
    E = eigen(diag(ncol(TM)) - tcrossprod(crossprod(TM, 
                                                    BM)), symmetric = TRUE, only.values = TRUE)
  else E = eigen(diag(ncol(BM)) - tcrossprod(crossprod(BM, 
                                                       TM)), symmetric = TRUE, only.values = TRUE)
  Deff = exp(sum(log(E$values))/ncol(TM))
  if (nlevels(TF) <= nlevels(BF)) 
    Aeff = ncol(TM)/sum(1/E$values)
  else Aeff = ncol(TM)/(ncol(TM) - ncol(BM) + sum(1/E$values))
  return(list(Deffic = round(Deff, 7), Aeffic = round(Aeff, 
                                                      7)))
}

#' Function to estimate the A bound and report the efficiencies of a IBD design
#' This function is sourced from the `blocksdesign` package.
#' @param Design The IBD design matrix
#' @return Data frame of efficiencies and bounds
#' @author Rodney Edmondson <rodney.edmondson at gmail.com>
#' @noRd
BlockEfficiencies = function(Design) {
  TF = Design[, ncol(Design)]
  regreps = table(TF)
  regReps = isTRUE(max(regreps) == min(regreps))
  sizes = lapply(1:(ncol(Design) - 2), function(i) {
    table(Design[, i])
  })
  regBlocks = sapply(1:length(sizes), function(i) {
    max(sizes[[i]]) == min(sizes[[i]])
  })
  bounds = sapply(1:(ncol(Design) - 2), function(i) {
    if (regBlocks[i] & regReps) 
      blocksdesign::A_bound(length(TF), nlevels(TF), nlevels(Design[, 
                                                      i]))
    else 1
  })
  blocklevs = unlist(lapply(1:(ncol(Design) - 2), function(j) {
    nlevels(Design[, j])
  }))
  Effics = t(sapply(1:(ncol(Design) - 2), function(i) {
    if (nlevels(Design[, i]) > 1) 
      blockEstEffics(TF, Design[, i])
    else list(Deffic = 1, Aeffic = 1)
  }))
  efficiencies = data.frame(1:(ncol(Design) - 2), blocklevs, 
                            as.numeric(Effics[, 1]), as.numeric(Effics[, 2]), 
                            round(bounds, 7))
  colnames(efficiencies) = c("Level", "Blocks", "D-Efficiency", 
                             "A-Efficiency", "A-Bound")
  return(efficiencies)
}

#' Function to re-randomize IBD design
#' 
#' @param ibd_design Input design from the `blocksdesign` package
#' @return Modified IBD design with re-randomized treatments
#' @author Didier Murillo 
#' @noRd
rerandomize_ibd <- function(ibd_design) {
  mydes <- ibd_design
  tretments <- sort(unique(mydes$Design$treatments))
  new_order <- data.frame(
    treatments = tretments, 
    new_order_treatments = sample(tretments, replace = FALSE)
  )
  
  newDesign <- mydes$Design |> 
    dplyr::left_join(new_order, by = "treatments") 
  
  mydes$Design_new <- newDesign |>
    dplyr::select(-treatments) |> 
    dplyr::rename(treatments = new_order_treatments)
  
  mydes$Blocks_model_new <- BlockEfficiencies(mydes$Design_new)
  
  return(mydes)
}
