#' infoPrint 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
infoPrint <- function(n, nr) {
  nhead <- min(n, nr)
  if (n < 0) {
    nhead_print <- nr + n
  }else {
    nhead_print <- nhead
  }
  return(nhead_print)
}