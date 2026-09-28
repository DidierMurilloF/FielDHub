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
