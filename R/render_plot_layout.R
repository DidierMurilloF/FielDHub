#' Generates field layouts plots
#'
#' @description It generates field layout plots for experimental designs.
#'
#' @param x A FielDHub object.
#' @param layout Type of layout field.
#' @param planter option to order of planter
#' @param l a specific location
#' @param stacked order of reps in the field layout
#'
#'
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#'         
#'
#' @return A list with the map of entries or treatments \code{out_layout}, the
#'   map of plot numbers \code{out_layoutPlots} (NULL for some designs), the
#'   drawn field book of the location \code{fieldBookXY}, the layout options
#'   of the location \code{newBooks}, and the field book with coordinates of
#'   every location \code{allSitesFieldbook}, and the selected layout settings
#'   \code{layout_metadata}. An unavailable location, stacking or layout
#'   option raises a classed \code{fieldhub_input_error} that lists the valid
#'   options, instead of warning and returning NULL.
#'
#' @references
#' Kevin Wright (2020). desplot: Plotting Field Plans for Agricultural Experiments. R package version 1.8.
#' https://CRAN.R-project.org/package=desplot
#'
#'
#' @noRd
plot_layout <- function(
    x = NULL,
    layout = 1,
    planter = "serpentine",
    l = 1,
    stacked = "vertical",
    nrows = NULL,
    ncols = NULL,
    ...) {
    # The same rules as field_layout(): a design built with dimensions draws
    # only its grid, and dimensions select a grid, not a numbered option. The
    # replicates of a grid lie as `stacked` says or, when it is not given, as
    # the most square replicate does. `nrows` and `ncols` are arguments of
    # their own so that they may be abbreviated as in field_layout().
    if (inherits(x, "FielDHub")) {
      check_layout_arguments(planter, stacked)
      check_fixed_grid_request(with_design_class(x), planter, stacked,
                               planter_given = !missing(planter),
                               stacked_given = !missing(stacked))
      # A planter that was not given is that of the grid the design was built on
      if (missing(planter) && stores_design_grid(with_design_class(x))) {
        planter <- stored_grid_planter(x)
      }
    }
    if (!is.null(nrows) || !is.null(ncols)) {
      check_dimensions_alone(layout_given = !missing(layout))
      return(checked_layout_view(x, planter = planter, location = l,
                                 stacked = if (!missing(stacked)) stacked, ...,
                                 nrows = nrows, ncols = ncols))
    }
    checked_layout_view(x, layout = layout, planter = planter, location = l,
                        stacked = stacked, ...)
}
