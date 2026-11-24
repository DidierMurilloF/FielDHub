#' The "Reps layout" choices of a classic design
#'
#' @description The stackings that have a layout option for the design: a
#' numbered layout or a field grid (\code{layout_choice_labels()}). A design
#' whose replicates can lie in rows and columns of replicates has a "Grid
#' Panel"; a design without replicates to arrange, or built on a field of
#' given dimensions, keeps the two stackings of its numbered layouts.
#' @inheritParams layout_choices
#' @noRd
layout_stacking_choices <- function(x, planter = "serpentine") {
  x <- with_design_class(x)
  choices <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal",
               if (!is.null(grid_replicate_unit(x)) && !stores_design_grid(x)) {
                 c("Grid Panel" = "grid_panel")
               })
  offered <- vapply(choices, function(stacked) {
    length(layout_choice_labels(x, planter = planter, stacked = stacked, include_grids = TRUE)) > 0L
  }, logical(1))
  choices[offered]
}
