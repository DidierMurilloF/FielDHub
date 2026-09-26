#' Access files in the current app
#' 
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#' 
#' @noRd
app_sys <- function(...){
  system.file(..., package = "FielDHub")
}
