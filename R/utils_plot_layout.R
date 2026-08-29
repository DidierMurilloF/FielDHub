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
#' @return A list with four elements.
#' \itemize{
#'   \item \code{out_layout} is the layout plot.
#'   \item \code{fieldBookXY} is a data frame with the field book design.
#'   \item \code{newBooks} is a list with the all field books combinations.
#' }
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
    if (missing(layout)) layout <- 1
    if (missing(planter)) planter <- "serpentine"
    if (missing(l)) l <- 1
    if (missing(stacked)) stacked <- "vertical"
    locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
    loc_levels <- levels(locations)
    locs_available <- length(loc_levels)
    if (!missing(l)) {
        if (l < 1 || l %% 1 != 0) {
        stop("l must be a positive integer!")
        } 
    } else stop(" l is missing!")
    if (locs_available > 1) {
        message <- "locations!"
    } else {
        message <- "location!"
    }
    if (l > locs_available) {
        message(cat("\n", " Option for location is not available!", "\n", "\n",
                    "***************************************************", "\n",
                    "***************************************************", "\n", "\n",
                    "The randomization was done only with:", locs_available, message, "\n", "\n",
                    "***************************************************", "\n",
                    "***************************************************"))
        return(NULL)
    }
    if (x$infoDesign$id_design %in% c(10, 11, 12, 8, 5, 6)) {
        if (x$infoDesign$id_design %in% c(10, 11, 12, 8)) {
            n_TrtGen <- dplyr::n_distinct(x$fieldBook$ENTRY)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            sizeIblocks <- dplyr::n_distinct(x$fieldBook$UNIT)
            iBlocks <- n_TrtGen / sizeIblocks
            return0 <- plot_iblocks_1(..., x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                                    sizeIblocks = sizeIblocks, iBlocks = iBlocks, 
                                    layout = layout, planter = planter,
                                    l = l, stacked = stacked)
        if (is.null(return0)) return(NULL)
        } else if (x$infoDesign$id_design == 5) {
            if (x$infoDesign$typeDesign == "RCBD") {
                wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
                sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
                n_TrtGen <- wp * sp
                n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
                sizeIblocks <- sp
                iBlocks <- wp
                return0 <- plot_splitPlots(..., x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                                            sizeIblocks = sizeIblocks, iBlocks = iBlocks, 
                                            layout = layout, planter = planter,
                                            l = l, stacked = stacked)
                if (is.null(return0)) return(NULL)
            } else {
                wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
                sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
                n_TrtGen <- wp * sp
                n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
                return0 <- plot_CRD(..., x = x, n_TrtGen = n_TrtGen, 
                                    n_Reps = n_Reps, 
                                    layout = layout, 
                                    planter = planter, l = l)
                if (is.null(return0)) return(NULL)
            }
        } else if (x$infoDesign$id_design == 6) {
            if (x$infoDesign$typeDesign == "RCBD") {
                wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
                sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
                ssp <- dplyr::n_distinct(x$fieldBook$SUB_SUB_PLOT)
                n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
                n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
                sizeIblocks <- ssp
                sizeIblocks <- as.numeric(sizeIblocks)
                iBlocks <- wp * sp
                return0 <- plot_splitPlots(..., x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                                            sizeIblocks = sizeIblocks, iBlocks = iBlocks, 
                                            layout = layout, planter = planter, 
                                            l = l)
                if (is.null(return0)) return(NULL)
            } else {
                wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
                sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
                ssp <- dplyr::n_distinct(x$fieldBook$SUB_SUB_PLOT)
                n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
                n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
                return0 <- plot_CRD(..., x = x, n_TrtGen = n_TrtGen, 
                                    n_Reps = n_Reps, 
                                    layout = layout, 
                                    planter = planter, l = l)
                if (is.null(return0)) return(NULL)
            }
        }
        return(list(out_layout = return0$p1, 
                    out_layoutPlots = return0$p2, 
                    fieldBookXY = return0$df, 
                    newBooks = return0$newBooks, 
                    allSitesFieldbook = return0$allSitesFieldbook))
    } else if (x$infoDesign$id_design %in% c(1, 2, 4)) {
        if (x$infoDesign$id_design == 1) {
            n_TrtGen <- dplyr::n_distinct(x$fieldBook$TREATMENT)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            return <- plot_CRD(..., x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, 
                            layout = layout, 
                            planter = planter, l = 1)
            if (is.null(return)) return(NULL)
            return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                        newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
        } else if (x$infoDesign$id_design == 2) {
            n_TrtGen <- dplyr::n_distinct(x$fieldBook$TREATMENT)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            return <- plot_RCBD(..., x = x, n_TrtGen = n_TrtGen, 
                                n_Reps = n_Reps, 
                                layout = layout, 
                                planter = planter, 
                                stacked = stacked,
                                l = l)
            if (is.null(return)) return(NULL)
            return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                        newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
        } else if (x$infoDesign$id_design == 4 & x$infoDesign$kind == "RCBD") {
            n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            return <- plot_RCBD(..., x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, layout = layout, 
                            planter = planter, stacked = stacked, l = l)
            if (is.null(return)) return(NULL)
            return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                        newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
        } else if (x$infoDesign$id_design == 4 & x$infoDesign$kind == "CRD") {
            n_TrtGen <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            return <- plot_CRD(..., x = x, n_TrtGen = n_TrtGen, n_Reps = n_Reps, layout = layout, 
                            planter = planter, l = l)
            if (is.null(return)) return(NULL)
            return(list(out_layout = return$p1, out_layoutPlots = return$p2, fieldBookXY = return$df, 
                        newBooks = return$newBooks, allSitesFieldbook = return$allSitesFieldbook))
        }

    } else if (x$infoDesign$id_design %in% c(3, 7, 9)) {
        if (x$infoDesign$id_design == 3) {
            rsRep <- dplyr::n_distinct(x$fieldBook$COLUMN)
            csRep <- dplyr::n_distinct(x$fieldBook$ROW)
            dims <- c(rsRep, csRep)
            n_Reps <- dplyr::n_distinct(x$fieldBook$SQUARE)
            return2 <- plot_latinSQ(..., 
                x = x, 
                dims = dims, 
                n_Reps = n_Reps, 
                layout = layout, 
                planter = planter, 
                l = l, 
                stacked = stacked
        )
        if (is.null(return2)) return(NULL)
        } else if (x$infoDesign$id_design == 7) {
            rsRep <- dplyr::n_distinct(x$fieldBook$HSTRIP)
            csRep <- dplyr::n_distinct(x$fieldBook$VSTRIP)
            dims <- c(rsRep, csRep)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            return2 <- plot_latinSQ(..., x = x, dims = dims, 
                                    n_Reps = n_Reps, 
                                    layout = layout,
                                    planter = planter, 
                                    l = l, 
                                    stacked = stacked)
            if (is.null(return2)) return(NULL)
        } else if (x$infoDesign$id_design == 9) {
            rsRep <- dplyr::n_distinct(x$fieldBook$ROW)
            csRep <- dplyr::n_distinct(x$fieldBook$COLUMN)
            dims <- c(rsRep, csRep)
            n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
            return2 <- plot_latinSQ(..., x = x, dims = dims, 
                                    n_Reps = n_Reps, 
                                    layout = layout,
                                    planter = planter, 
                                    l = l, 
                                    stacked = stacked)
            if (is.null(return2)) return(NULL)
        }
        return(list(out_layout = return2$p1, out_layoutPlots = return2$p2, fieldBookXY = return2$df, 
                    newBooks = return2$newBooks, allSitesFieldbook = return2$allSitesFieldbook))
    } else if (x$infoDesign$id_design == 13 || x$infoDesign$id_design == "MultiPrep") {
        return_prep <- plot_prep(..., x = x, l = l)
        return(list(out_layout = return_prep$p1, 
                    allSitesFieldbook = return_prep$allSitesFieldbook))
    } else if (x$infoDesign$id_design == 14) {
        return_arcbd <- plot_augmented_RCBD(..., x = x, l = l)
        return(list(out_layout = return_arcbd$p1, out_layoutPlots = return_arcbd$p2,
                    allSitesFieldbook = return_arcbd$allSitesFieldbook))
    } else if (x$infoDesign$id_design == 15 || x$infoDesign$id_design == "Sparse") {
        return_diagonal <- plot_diagonal_arrangement(..., x = x, l = l)
        return(
            list(
                out_layout = return_diagonal$p1, 
                allSitesFieldbook = return_diagonal$allSitesFieldbook
            )
        )
    } else if (x$infoDesign$id_design == 16) {
        return_optim <- plot_optim(..., x = x, l = l)
        return(list(out_layout = return_optim$p1, 
                    allSitesFieldbook = return_optim$allSitesFieldbook))
    } 
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