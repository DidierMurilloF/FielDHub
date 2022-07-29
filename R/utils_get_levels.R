#' @noRd
get.levels <- function(k = NULL) {
  newlevels <- list();s <- 1
  for (i in k) {
    newlevels[[s]] <- rep(0:(i-1), 1)
    s <- s + 1
  }
  return(newlevels)
}