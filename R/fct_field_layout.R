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
#'   the lattice designs have no field coordinates by default: their plots can be
#'   arranged in several ways, and the layouts that renumber the plots along
#'   the planting path also change \code{PLOT}. The other designs place their
#'   plots when they are built; their field book already has \code{ROW} and
#'   \code{COLUMN}, and it is returned as it is.
#'
#'   For supported designs, dimensions select a rectangular grid instead of a numbered
#'   layout or stacking. Each location must have exactly as many cells as
#'   plots, including repeated checks. For RCBD, complete blocks occupy equal rectangles.
#'   The layout prefers blocks stacked vertically, then blocks side by side,
#'   then the arrangement with the most rows of blocks. Blocks are placed
#'   across each row of blocks, and the planting path restarts in each block.
#'   Plot numbers may move within a block to follow that path.
#'
#'   CRD and factorials in CRD can fill any exact rectangle. Factorials in
#'   RCBD keep complete blocks together. Incomplete blocks and lattices keep
#'   each replicate and incomplete block intact. Split plots keep whole plots
#'   intact and split-split plots also keep the subplots within them intact.
#'   Their RCBD variants keep replicate blocks intact. Latin squares and strip
#'   plots tile intact squares or strip grids with their original planting order.
#'   Original Latin square labels are retained as \code{ROW_SQ} and \code{COLUMN_SQ}. These designs keep
#'   each plot's original number and treatment assignment. Invalid grids
#'   report suggested dimensions. For a design with a stored grid, supplied
#'   dimensions must match the stored grid.
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
#' @param nrows,ncols Optional positive whole numbers specifying the field
#'   dimensions per location for a supported design. If only one is given, the other is
#'   inferred from the plot count.
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
#' # Six complete blocks arranged two down and three across
#' head(field_layout(RCBD(t = 6, reps = 6, seed = 1), nrows = 4, ncols = 9))
#'
#' @export
field_layout <- function(x, layout = 1, planter = "serpentine", stacked = "vertical",
                         nrows = NULL, ncols = NULL) {
  if (!inherits(x, "FielDHub")) {
    fieldhub_abort("'x' must be a design created by FielDHub.")
  }
  x <- with_design_class(x)
  check_layout_arguments(planter, stacked)
  if (!is.null(nrows) || !is.null(ncols)) {
    if (inherits(x, "fieldhub_rcbd")) return(rcbd_grid_layout(x, nrows, ncols, planter))
    spec <- design_grid_spec(x)
    if (is.null(spec)) fieldhub_abort("Field dimensions are not supported for this design.")
    return(design_grid_layout(x, spec, nrows, ncols, planter))
  }
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

# Store the coordinates of a grid chosen when the design is built
store_design_grid <- function(x, nrows, ncols, planter = "serpentine") {
  if (!is.null(nrows) || !is.null(ncols)) {
    x$fieldBook <- field_layout(x, nrows = nrows, ncols = ncols, planter = planter)
  }
  x
}
