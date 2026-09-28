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
    ...) {
    checked_layout_view(x, layout = layout, planter = planter, location = l,
                        stacked = stacked, ...)
}
