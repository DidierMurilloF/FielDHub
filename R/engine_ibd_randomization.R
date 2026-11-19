#' Function to re-randomize IBD design
#'
#' Relabelling the treatments does not change the eigenvalues of the
#' information matrix, so the efficiency report of an engine design is the
#' engine's own (nested-engine spec 6.2b, decision 5): `BlockEfficiencies()`
#' reports them, rounded to seven decimals with its A-bound, without its
#' singular value decomposition. A design without engine efficiencies (the
#' unoptimised design above the block limit) is still measured in full.
#'
#' @param ibd_design A resolvable design from `blocks_engine(resolvable = TRUE)`:
#'   `Design` with `Level_1`, `Level_2`, `plots` and `treatments`
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

  model <- mydes$Blocks_model
  known <- if (is.data.frame(model) && !anyNA(model[c("D-Efficiency", "A-Efficiency")])) model
  mydes$Blocks_model_new <- BlockEfficiencies(mydes$Design_new, known = known)

  return(mydes)
}
