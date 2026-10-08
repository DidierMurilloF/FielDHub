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
    fill <- data[[all.vars(form)[1L]]]
    if (is.character(fill) || is.factor(fill)) defaults$col.regions <- fieldhub_layout_palette()
    args <- utils::modifyList(utils::modifyList(defaults, list(...)), extra_args)
    args$form <- form
    args$data <- data
    # desplot passes a missing outline width to ggplot2 as an empty linewidth,
    # which warns on every draw; ggplot2 would draw it at 0.5 anyway.
    for (gpar in c("out1.gpar", "out2.gpar")) {
        if (is.list(args[[gpar]]) && is.null(args[[gpar]]$lwd)) args[[gpar]]$lwd <- 0.5
    }
    do.call(desplot::desplot, args) + fieldhub_layout_theme()
}

#' Neutral cell fill shared by native layouts
#' @noRd
fieldhub_layout_neutral <- function() "#F2F2F2"

#' Established categorical layout colours, with a lighter neutral background
#' @noRd
fieldhub_layout_palette <- function() {
    c(fieldhub_layout_neutral(), "#FFD9D9", "#FFB2B2", "#FFD7B2", "#FDFFB2", "#D9FFB2",
      "#B2D6FF", "#C2B2FF", "#F0B2FF", "#A6FFC9", "#FF8C8C", "#B2B2B2", "#FFBD80",
      "#BFFF80", "#80BAFF", "#9980FF", "#E680FF", "#D0D192", "#59FF9C", "#FFA24D",
      "#FBFF4D", "#4D9FFF", "#704DFF", "#DB4DFF", "#808080", "#9FFF40", "#C9CC3D")
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
