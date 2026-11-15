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

#' Muted background tints for field structure
#'
#' @description
#' One tint per replication: the shared neutral grey, then pale blue, green,
#' sand, lilac and rose (the block backgrounds of the augmented RCBD map),
#' blended into more tints when a field has more than six. The white alleys
#' separate replications; dark labels keep AAA contrast on every tint.
#'
#' @param n Number of groups (replications).
#' @return A character vector of \code{n} colours.
#' @noRd
fieldhub_field_tints <- function(n) {
  tints <- c(fieldhub_layout_neutral(), "#E6EEF5", "#E9F2EC", "#F3EEE6", "#EDE7F2", "#F1E9E9")
  if (n > length(tints)) grDevices::colorRampPalette(tints)(n) else tints[seq_len(n)]
}

#' Neutral grey background of check plots
#'
#' @description
#' Every check plot of a field map gets this one grey, so checks stand out as
#' a set without a colour per check; the label says which check it is.
#'
#' @return A single colour.
#' @noRd
fieldhub_check_fill <- function() "#D5D9DD"

#' Draw a field map with white alleys between groups
#'
#' @description
#' The field-plan style: each group (a replication, or a block of an augmented
#' design) gets a single muted background from \code{fieldhub_field_tints()},
#' white alleys separate the groups, and grey lines outline the incomplete
#' blocks. Blocks have no background of their own, so a colour never repeats
#' inside a group. Check plots share one grey (\code{fieldhub_check_fill()}),
#' filler plots are white with grey labels, and the other labels are dark.
#'
#' The map is drawn by a single \code{plot_desplot()} call (fill, \code{out1}
#' for the blocks or the plot grid, \code{out2} for the alleys,
#' \code{col.text} for the labels), so desplot arguments passed through
#' \code{plot()} override it. The one exception is the plot grid of maps with
#' incomplete blocks: desplot has no outline left for it, so it is a ggplot2
#' layer just above the fill (until desplot can draw a cell grid itself).
#' \code{plot(x, field_grid = FALSE)} switches the plot grid off.
#'
#' @param data The field book to draw.
#' @param text Column with the cell labels.
#' @param main Plot title.
#' @param group Optional column of the groups separated by alleys, in
#'   numeric order or, for a factor, in the order of its levels; without it
#'   the whole field is one group, with no alleys.
#' @param block Optional column of the incomplete blocks (or whole plots)
#'   within a group.
#' @param sub_block Optional column of the units nested in \code{block} (the
#'   sub-plots of a split-split-plot), drawn as lighter grey lines.
#' @param checks Optional logical vector along \code{data}: the check plots.
#' @param fillers Optional logical vector along \code{data}: the filler plots.
#' @param grid Draw a white line around every plot. It is thinner when there
#'   are alleys, so the alleys still read first.
#' @param extra_args desplot arguments forwarded from \code{plot()}, plus
#'   \code{field_grid = FALSE} to switch the plot grid off.
#' @return A ggplot object.
#' @noRd
plot_field_alleys <- function(data, text, main, group = NULL, block = NULL, checks = NULL,
                              fillers = NULL, grid = FALSE, sub_block = NULL, extra_args = list()) {
  if ("field_grid" %in% names(extra_args)) {
    validate_flag(extra_args$field_grid, "field_grid")
    grid <- grid && extra_args$field_grid
    extra_args$field_grid <- NULL
  }
  drawn <- data
  drawn[[text]] <- as.character(drawn[[text]])
  groups <- if (is.null(group)) rep("1", nrow(drawn)) else as.character(drawn[[group]])
  group_levels <- if (!is.null(group) && is.factor(drawn[[group]])) {
    # a factor gives the order of its groups
    levels(droplevels(drawn[[group]]))
  } else {
    unique(groups[order(suppressWarnings(as.numeric(groups)), groups)])
  }
  is_check <- if (is.null(checks)) rep(FALSE, nrow(drawn)) else checks %in% TRUE
  is_filler <- if (is.null(fillers)) rep(FALSE, nrow(drawn)) else fillers %in% TRUE
  fill <- paste0("group", match(groups, group_levels))
  fill[is_check] <- "check"
  fill[is_filler] <- "filler"
  colours <- c(stats::setNames(fieldhub_field_tints(length(group_levels)),
                               paste0("group", seq_along(group_levels))),
               check = fieldhub_check_fill(), filler = "#FFFFFF")
  used <- names(colours)[names(colours) %in% fill]
  drawn$FIELD_FILL <- factor(fill, levels = used)
  text_colours <- c(label = "gray10", filler = "gray60")
  labels <- ifelse(is_filler, "filler", "label")
  used_text <- names(text_colours)[names(text_colours) %in% labels]
  drawn$FIELD_TEXT <- factor(labels, levels = used_text)
  args <- list(form = FIELD_FILL ~ COLUMN + ROW, data = drawn, text.string = text, cex = 0.85,
               col.regions = unname(colours[used]), col.string = "FIELD_TEXT",
               col.text = unname(text_colours[used_text]), main = main)
  if (!is.null(block)) {
    args$data$FIELD_BLOCK <- factor(paste(groups, as.character(drawn[[block]]), sep = ":"))
    args$out1.string <- "FIELD_BLOCK"
    args$out1.gpar <- list(col = "gray50", lwd = 0.6, lty = 1)
  }
  grid_width <- if (length(group_levels) > 1L) 0.8 else 1.2
  if (isTRUE(grid) && is.null(block)) {
    # every plot is its own outline group, so every edge between plots is drawn
    args$data$FIELD_PLOT <- seq_len(nrow(drawn))
    args$out1.string <- "FIELD_PLOT"
    args$out1.gpar <- list(col = "white", lwd = grid_width, lty = 1)
  }
  if (length(group_levels) > 1L) {
    args$data$FIELD_GROUP <- factor(groups, levels = group_levels)
    args$out2.string <- "FIELD_GROUP"
    args$out2.gpar <- list(col = "white", lwd = 2.2, lty = 1)
  }
  p <- do.call(plot_desplot, c(args, list(extra_args = extra_args)))
  if (!is.null(sub_block)) p <- with_nested_lines(p, block, sub_block)
  if (isTRUE(grid) && !is.null(block)) p <- with_plot_grid(p, grid_width)
  p
}

#' Add lighter grey lines between units nested in a block
#'
#' @description
#' For a third level of nesting that desplot's two outlines cannot draw (the
#' sub-plots of a split-split-plot): a line between neighbouring plots of the
#' same \code{block} that belong to different \code{unit}s. Computed from the
#' drawn data, so desplot's coordinate changes apply, and placed just above
#' the fill, below desplot's outlines and the labels.
#'
#' @param p A desplot ggplot.
#' @param block Column of the enclosing blocks (whole plots).
#' @param unit Column of the nested units (sub-plots).
#' @return \code{p} with the line layer.
#' @noRd
with_nested_lines <- function(p, block, unit) {
  d <- p$data
  x <- as.numeric(d$COLUMN)
  y <- as.numeric(d$ROW)
  key <- paste(x, y)
  owner <- as.character(d[[block]])
  inner <- paste(owner, as.character(d[[unit]]), sep = ":")
  right <- match(paste(x + 1, y), key)
  up <- match(paste(x, y + 1), key)
  segments <- rbind(
    data.frame(from = seq_along(x), to = right, x = x + 0.5, y = y - 0.5, xend = x + 0.5, yend = y + 0.5),
    data.frame(from = seq_along(x), to = up, x = x - 0.5, y = y + 0.5, xend = x + 0.5, yend = y + 0.5))
  segments <- segments[!is.na(segments$to), , drop = FALSE]
  segments <- segments[owner[segments$from] == owner[segments$to] &
                       inner[segments$from] != inner[segments$to], , drop = FALSE]
  lines <- ggplot2::geom_segment(data = segments,
                                 ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                                 inherit.aes = FALSE, colour = "gray70", linewidth = 0.45)
  fill <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))[1L]
  p$layers <- append(p$layers, list(lines), after = fill)
  p
}

#' Add a white plot grid just above a desplot map's fill
#'
#' @description
#' For maps whose two desplot outlines are both in use (replication alleys and
#' incomplete-block lines). The grid is drawn from the drawn data, so desplot's
#' coordinate changes (such as \code{flip}) apply to it, and it sits below the
#' outlines and the labels.
#'
#' @param p A desplot ggplot.
#' @param width Line width of the grid.
#' @return \code{p} with the grid layer.
#' @noRd
with_plot_grid <- function(p, width) {
  grid <- ggplot2::geom_tile(data = p$data, ggplot2::aes(x = .data$COLUMN, y = .data$ROW),
                             inherit.aes = FALSE, fill = NA, colour = "white", linewidth = width)
  fill <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))[1L]
  p$layers <- append(p$layers, list(grid), after = fill)
  p
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
