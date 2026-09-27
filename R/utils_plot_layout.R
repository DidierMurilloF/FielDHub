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
#'   \code{layout_metadata}. When the location, stacking or
#'   layout option is not available, a warning and NULL.
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
    if (!inherits(x,"FielDHub")) fieldhub_abort("x is not a FielDHub class object")
    if (length(l) != 1 || l < 1 || l %% 1 != 0) fieldhub_abort("l must be a positive integer!")
    x <- with_design_class(x)
    check_layout_arguments(planter, stacked)
    options <- layout_options(x, planter = planter, stacked = stacked)
    locs_available <- length(options)
    if (l > locs_available) {
        fieldhub_warn(
            "Location ", l, " is not available: the design has ", locs_available, " ",
            if (locs_available > 1) "locations!" else "location!",
            class = "fieldhub_layout_warning", call = NULL
        )
        return(NULL)
    }
    site_options <- options[[l]]
    if (length(site_options) == 0) {
        fieldhub_warn(
            "Stacking \"", stacked, "\" is not available for this design.",
            class = "fieldhub_layout_warning", call = NULL
        )
        return(NULL)
    }
    if (length(layout) != 1 || !layout %in% seq_along(site_options)) {
        fieldhub_warn(
            "Layout option ", layout, " is not available for this design. Options: ",
            paste(seq_along(site_options), collapse = ", "), ".",
            class = "fieldhub_layout_warning", call = NULL
        )
        return(NULL)
    }
    render_layout_view(x, options, layout, planter, l, stacked, ...)
}
