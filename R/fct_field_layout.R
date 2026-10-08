#' Field layout of a design
#'
#' @description Returns the field book of a design with the \code{ROW} and
#'   \code{COLUMN} of every plot, for the layout, planter and stacking chosen.
#'   This is the field book that \code{plot()} draws and that the app
#'   exports.
#'
#'   The field books of \code{CRD()}, \code{RCBD()}, \code{latin_square()},
#'   \code{full_factorial()}, \code{split_plot()}, \code{split_split_plot()},
#'   \code{strip_plot()}, \code{incomplete_blocks()}, \code{row_column()} and
#'   the lattice designs have no field coordinates: their plots can be
#'   arranged in several ways, and the layouts that renumber the plots along
#'   the planting path also change \code{PLOT}. The other designs place their
#'   plots when they are built; their field book already has \code{ROW} and
#'   \code{COLUMN}, and it is returned as it is.
#'
#' @param x A design created by a FielDHub function.
#' @param layout Layout option, a whole number. The options depend on the
#'   design, the planter and the stacking; an unavailable option is an error
#'   that lists the available ones.
#' @param planter Order in which the plots are planted: \code{"serpentine"}
#'   (by default) or \code{"cartesian"}.
#' @param stacked How the reps are arranged: \code{"vertical"} (by default),
#'   \code{"horizontal"}, or \code{"grid_panel"} for designs in incomplete
#'   blocks and split plots in complete blocks with more than two reps.
#'
#' @return A data frame: the field book of every location with the columns
#'   \code{ID}, \code{LOCATION}, \code{PLOT}, \code{ROW} and \code{COLUMN}
#'   first.
#'
#' @author Didier Murillo [aut]
#'
#' @examples
#' rcbd <- RCBD(t = 6, reps = 3, plotNumber = 101, seed = 1)
#' head(field_layout(rcbd))
#' # Blocks side by side, plots planted row by row
#' head(field_layout(rcbd, stacked = "horizontal", planter = "cartesian"))
#'
#' @export
field_layout <- function(x, layout = 1, planter = "serpentine", stacked = "vertical") {
  if (!inherits(x, "FielDHub")) {
    fieldhub_abort("'x' must be a design created by FielDHub.")
  }
  x <- with_design_class(x)
  check_layout_arguments(planter, stacked)
  options <- layout_options(x, planter = planter, stacked = stacked)
  available <- seq_along(options[[1]])
  if (length(available) == 0) {
    fieldhub_abort(paste0("Stacking \"", stacked, "\" is not available for this design."))
  }
  if (length(layout) != 1 || !layout %in% available) {
    fieldhub_abort(
      paste0("Layout option ", paste(layout, collapse = ", "),
             " is not available for this design. Options: ",
             paste(available, collapse = ", "), "."),
      data = list(options = available)
    )
  }
  all_locations_layout(x, options, layout)
}
