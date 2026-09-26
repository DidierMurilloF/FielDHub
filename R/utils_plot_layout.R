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
#'   every location \code{allSitesFieldbook}. When the location, stacking or
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
    if (!inherits(x,"FielDHub")) stop("x is not a FielDHub class object") 
    if (length(l) != 1 || l < 1 || l %% 1 != 0) stop("l must be a positive integer!")
    x <- with_design_class(x)
    check_layout_arguments(planter, stacked)
    options <- layout_options(x, planter = planter, stacked = stacked)
    locs_available <- length(options)
    if (l > locs_available) {
        warning("Location ", l, " is not available: the design has ", locs_available, " ",
                if (locs_available > 1) "locations!" else "location!", call. = FALSE)
        return(NULL)
    }
    site_options <- options[[l]]
    if (length(site_options) == 0) {
        warning("Stacking \"", stacked, "\" is not available for this design.", call. = FALSE)
        return(NULL)
    }
    if (length(layout) != 1 || !layout %in% seq_along(site_options)) {
        warning("Layout option ", layout, " is not available for this design. Options: ",
                paste(seq_along(site_options), collapse = ", "), ".", call. = FALSE)
        return(NULL)
    }
    drawn <- draw_layout(x, site_options[[layout]], ...)
    list(out_layout = drawn$p1,
         out_layoutPlots = drawn$p2,
         fieldBookXY = drawn$data,
         newBooks = site_options,
         allSitesFieldbook = all_locations_layout(x, options, layout))
}

#' Draw a FielDHub field-layout map with desplot
#'
#' @description
#' Internal helper that wraps [desplot::desplot()] with the styling shared by all
#' FielDHub layout plots: no legend, a tick at every integer coordinate, no panel
#' border, and the FielDHub title/axis text sizes. Design-specific arguments (the
#' formula, the outline/text/colour columns, the title) are supplied by the calling
#' \code{plot_*()} function; a \code{plot()} caller can override or extend any
#' desplot argument via \code{extra_args}.
#'
#' This replaces the former \code{add_gg_features()} post-processor. Integer ticks
#' and the borderless look are now requested natively through desplot's \code{ticks}
#' and \code{panel.border} arguments (desplot >= 1.11) instead of being patched onto
#' the finished ggplot, so a single desplot call fully describes the plot.
#'
#' @param form A desplot formula, e.g. \code{TREATMENT ~ COLUMN + ROW}.
#' @param data The field-book data frame.
#' @param ... desplot arguments set by the calling \code{plot_*()} function. Column
#'   arguments must use desplot's string form (\code{out1.string}, \code{text.string},
#'   \code{col.string}, ...) so they survive being assembled with \code{do.call()}.
#' @param extra_args A named list of desplot arguments forwarded from \code{plot()};
#'   these win over both the shared defaults and the \code{...} arguments.
#'
#' @return A ggplot object.
#'
#' @noRd
plot_desplot <- function(form, data, ..., extra_args = list()) {
    defaults <- list(
        flip = FALSE,
        cex = 1,
        shorten = "no",
        xlab = "COLUMNS",
        ylab = "ROWS",
        show.key = FALSE,
        gg = TRUE,
        ticks = "all",
        panel.border = FALSE
    )
    args <- utils::modifyList(utils::modifyList(defaults, list(...)), extra_args)
    args$form <- form
    args$data <- data
    do.call(desplot::desplot, args) + fieldhub_layout_theme()
}

#' Title and axis text sizes shared by all FielDHub layout plots
#'
#' @description
#' The one piece of the former \code{add_gg_features()} that has no desplot
#' equivalent: a bold title and slightly larger axis text. Applied by
#' \code{plot_desplot()} and, where a plot is assembled by hand, added directly.
#'
#' @return A ggplot2 theme object.
#' @noRd
fieldhub_layout_theme <- function() {
    ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12),
        axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 10)
    )
}