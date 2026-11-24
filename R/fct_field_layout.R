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
#'   With \code{nrows} or \code{ncols}, the design is placed in a field of
#'   those dimensions instead of a numbered layout. Each location must have
#'   exactly as many cells as plots, including repeated checks, and the units
#'   of the design are equal rectangles of the field:
#'   \itemize{
#'   \item CRD and factorials in CRD fill any rectangle that holds the plots.
#'   \item RCBD and factorials in RCBD keep each complete block together.
#'   \item Incomplete blocks and lattices keep each replicate and each
#'   incomplete block together.
#'   \item Split plots keep each whole plot together, split-split plots also
#'   each subplot, and their RCBD variants each replicate.
#'   \item Latin squares and strip plots tile their squares and strip grids
#'   whole, in their own orientation. The row and column labels of a Latin
#'   square are kept as \code{ROW_SQ} and \code{COLUMN_SQ}.
#'   }
#'   The plots are then numbered along the planting path of the whole field,
#'   serpentine or cartesian as \code{planter} says, as in the numbered
#'   layouts: the plot numbers of the location are handed out in increasing
#'   order as the planter reaches each plot, and the path does not restart in
#'   each block or replicate. With a serpentine planter, consecutive plot
#'   numbers are therefore neighbours in the field. Subplots that share a plot
#'   number keep sharing one; whole plots that tile the field as equal
#'   rectangles are numbered along the planter's path over the grid of whole
#'   plots, so consecutive whole plots are neighbours too. The randomization
#'   does not change: every row of
#'   the field book keeps its treatment, replicate and block, and only
#'   \code{PLOT} differs from the design's own field book. Where the plot
#'   numbers restart at each replicate (101, 201, ...), the hundreds name the
#'   replicate only where the replicates lie one above the other.
#'
#'   In a field of given dimensions, \code{stacked} arranges the replicates:
#'   the complete blocks of an RCBD or of a factorial in blocks, the
#'   replicates of split, split-split and strip plots, of incomplete blocks
#'   and of the lattices, and the squares of a Latin square.
#'   \itemize{
#'   \item \code{"vertical"}: one above another. Each replicate spans all the
#'   columns of the field and \code{nrows / reps} of its rows.
#'   \item \code{"horizontal"}: side by side. Each replicate spans all the
#'   rows of the field and \code{ncols / reps} of its columns.
#'   \item \code{"grid_panel"}: in rows and columns of replicates, more than
#'   one of each.
#'   }
#'   When \code{stacked} is not given with dimensions, the stacking is the one
#'   whose replicate is the most square (the smallest difference between its
#'   rows and its columns) among those that fit the dimensions; this also
#'   chooses among the panels of \code{"grid_panel"}. A tie goes to
#'   \code{"vertical"}, then to \code{"horizontal"}, then to the panel with the
#'   most rows of replicates. CRD, and factorials and split plots in CRD, have
#'   no replicates to stack: \code{stacked} with their dimensions is an error.
#'
#'   Dimensions that do not fit, or do not fit the stacking asked for, are an
#'   error that suggests dimensions that do. \code{layout} selects a numbered
#'   layout: giving it together with dimensions is an error.
#'
#'   A design built with dimensions (\code{RCBD(..., nrows = 4)}) has one
#'   layout: the grid stored in its field book, numbered along the path of the
#'   planter it was built with, its replicates stacked as its
#'   \code{metadata$parameters$stacked} records. \code{field_layout()} returns
#'   that field book when it is called with no other argument, or with the
#'   same \code{planter}, \code{stacked} or dimensions. Another
#'   \code{planter}, another \code{stacked} or other dimensions are an error:
#'   for another layout, build the design again with other dimensions or
#'   another stacking, or without dimensions.
#'
#' @param x A design created by a FielDHub function.
#' @param layout Layout option, a whole number. The options depend on the
#'   design, the planter and the stacking; an unavailable option is an error
#'   that lists the available ones.
#' @param planter Order in which the plots are planted: \code{"serpentine"}
#'   (by default) or \code{"cartesian"}.
#' @param stacked How the reps are arranged: \code{"vertical"} (by default),
#'   \code{"horizontal"}, or \code{"grid_panel"} for designs in incomplete
#'   blocks and split plots in complete blocks with more than two reps. With
#'   \code{nrows} or \code{ncols} it arranges the replicates in the field of
#'   those dimensions, for every design with replicates, and its default is
#'   the stacking whose replicate is the most square (see the description).
#' @param nrows,ncols Optional positive whole numbers: the rows and columns
#'   of the field at each location, for the designs listed above. If only one
#'   is given, the other follows from the number of plots.
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
#' # Six complete blocks arranged two down and three across in a field of
#' # 4 rows and 9 columns, numbered along the serpentine path of the field
#' head(field_layout(RCBD(t = 6, reps = 6, seed = 1), nrows = 4, ncols = 9))
#' # Four blocks of 1 x 6 plots one above another, or 2 x 3 blocks in a panel
#' # of two by two, in the same field of 4 rows and 6 columns
#' blocks <- RCBD(t = 6, reps = 4, seed = 1)
#' head(field_layout(blocks, stacked = "vertical", nrows = 4, ncols = 6))
#' head(field_layout(blocks, stacked = "grid_panel", nrows = 4, ncols = 6))
#'
#' @export
field_layout <- function(x, layout = 1, planter = "serpentine", stacked = "vertical",
                         nrows = NULL, ncols = NULL) {
  if (!inherits(x, "FielDHub")) {
    fieldhub_abort("'x' must be a design created by FielDHub.")
  }
  x <- with_design_class(x)
  check_layout_arguments(planter, stacked)
  check_fixed_grid_request(x, planter, stacked, planter_given = !missing(planter),
                           stacked_given = !missing(stacked))
  if (!is.null(nrows) || !is.null(ncols)) {
    check_dimensions_alone(layout_given = !missing(layout))
    return(design_field_grid(x, nrows, ncols, planter, if (!missing(stacked)) stacked))
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

# A design built with dimensions has one layout: its grid, numbered along the
# path of the planter it was built with, its replicates stacked as they were
# then. Another planter or another stacking is refused; the same or none
# returns the grid.
check_fixed_grid_request <- function(x, planter, stacked, planter_given, stacked_given) {
  if (!stores_design_grid(x)) return(invisible(x))
  if (stacked_given && is.null(grid_replicate_unit(x))) abort_no_replicates()
  if ((planter_given && !identical(planter, stored_grid_planter(x))) ||
      (stacked_given && !identical(stacked, x$metadata$parameters$stacked))) {
    abort_fixed_grid(x)
  }
  invisible(x)
}

# A numbered layout option cannot be combined with dimensions
check_dimensions_alone <- function(layout_given) {
  if (layout_given) {
    fieldhub_abort("'layout' selects a numbered layout: it cannot be combined with ",
                   "'nrows' or 'ncols'.")
  }
  invisible(TRUE)
}

# Store the coordinates of a grid chosen when the design is built, and the
# stacking of its replicates. The elements named in `maps` hold plot numbers
# (matrices, in lists by location when `by_location`), which follow the plots
# to their numbers in the grid.
store_design_grid <- function(x, nrows, ncols, planter = "serpentine", maps = character(),
                              by_location = TRUE, stacked = NULL) {
  if (is.null(nrows) && is.null(ncols)) {
    if (!is.null(stacked)) {
      fieldhub_abort("'stacked' arranges the replicates in a field of given dimensions: ",
                     "give 'nrows' or 'ncols' with it.")
    }
    return(x)
  }
  plan <- field_grid_plan(x, nrows, ncols, stacked)
  old <- x$fieldBook
  x$fieldBook <- place_field_grid(x, plan, planter)
  # The stacking chosen rebuilds the same field; a design without replicates
  # has none
  stacking <- if (!is.na(plan$stacked)) plan$stacked
  x$metadata$parameters$stacked <- stacking
  x$infoDesign$stacked <- stacking
  site <- match(old$LOCATION, unique(old$LOCATION))
  if (!by_location) site[] <- 1L
  renumber <- function(map, at) {
    if (is.list(map)) return(lapply(map, renumber, at = at))
    here <- site == at
    # A table of the plots holds their numbers in its PLOT column
    table <- "PLOT" %in% colnames(map)
    new <- x$fieldBook$PLOT[here][match(if (table) map[, "PLOT"] else map, old$PLOT[here])]
    if (table) map[, "PLOT"] <- new else map[] <- new
    map
  }
  for (name in maps) {
    if (by_location) {
      for (at in seq_along(x[[name]])) x[[name]][[at]] <- renumber(x[[name]][[at]], at)
    } else {
      x[[name]] <- renumber(x[[name]], 1L)
    }
  }
  x
}
