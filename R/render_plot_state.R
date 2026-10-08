#' Explain why a plot panel has nothing to draw yet
#'
#' @description The app shows this text where a layout or heatmap would be,
#' instead of a blank panel: the design has not been run, it failed, or no
#' data has been simulated for the heatmap.
#'
#' @param design The design result, \code{NULL} when it has not been run, or
#'   the condition it failed with.
#' @param settings The accepted simulation settings, or \code{NULL} when no
#'   data has been simulated.
#' @param view \code{"layout"} or \code{"heatmap"}.
#' @return A single string, or \code{NULL} when the plot can be drawn.
#' @noRd
plot_state_message <- function(design, settings, view) {
  if (!is.character(view) || length(view) != 1L || !view %in% c("layout", "heatmap")) {
    fieldhub_abort("view must be \"layout\" or \"heatmap\".")
  }
  if (is.null(design)) {
    return(if (identical(view, "heatmap")) "Run the design to see the heatmap."
           else "Run the design to see the field layout.")
  }
  if (inherits(design, "condition")) return(problem_message(design))
  if (identical(view, "heatmap") && is.null(settings)) {
    return("Simulate data to see the heatmap.")
  }
  NULL
}
