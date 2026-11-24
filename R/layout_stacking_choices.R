#' The "Replicate layout" choices of a classic design
#'
#' @description The stackings that have a layout option for the design
#' (\code{layout_choice_labels()}). "Vertical" and "Horizontal" hold the
#' numbered layouts of each; "Grid panel" is offered when the design has a
#' field grid, of any stacking, or a numbered grid panel. A design without
#' replicates to arrange (\code{layout_arranges_replicates()}) has one list
#' of fields: a single choice, which a page does not show.
#' @inheritParams layout_choices
#' @noRd
layout_stacking_choices <- function(x, planter = "serpentine") {
  x <- with_design_class(x)
  arranged <- layout_arranges_replicates(x)
  choices <- c("Vertical" = "vertical", "Horizontal" = "horizontal",
               if (!is.null(arranged_replicate_unit(x))) c("Grid panel" = "grid_panel"))
  offered <- vapply(choices, function(stacked) {
    length(layout_choice_labels(x, planter = planter, stacked = stacked, include_grids = TRUE)) > 0L
  }, logical(1))
  choices <- choices[offered]
  if (arranged) choices else utils::head(choices, 1L)
}
