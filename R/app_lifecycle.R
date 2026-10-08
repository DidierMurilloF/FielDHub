#' Scope the upload limit to the running application's lifetime
#'
#' Shiny reads a process-wide option for uploads. Register its restoration
#' with the application, not with any individual user session. Accepting a
#' registration function keeps the resource lifecycle testable in plain R.
#' @noRd
app_upload_limit <- function(register_stop = function(callback) {
  shiny::onStop(callback, session = NULL)
}) {
  previous <- options(shiny.maxRequestSize = 100 * 1024^2)
  active <- TRUE
  restore <- function() {
    if (active) {
      options(previous)
      active <<- FALSE
    }
    invisible(NULL)
  }
  registered <- FALSE
  on.exit(if (!registered) restore(), add = TRUE)
  register_stop(restore)
  registered <- TRUE
  invisible(NULL)
}
