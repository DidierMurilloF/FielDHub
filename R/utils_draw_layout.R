#-----------------------------------------------------------------------
# Drawing field layouts. draw_layout() draws the field book of one location,
# with its coordinates, as a map of the entries or treatments (p1) and, for
# most designs, a map of the plot numbers (p2).
#-----------------------------------------------------------------------

#' Draw the field layout of one location
#'
#' @param x A design result.
#' @param df The field book of one location with ROW and COLUMN.
#' @param ... Arguments passed to \code{desplot::desplot()}.
#' @return A list with the plots \code{p1} and \code{p2} (or NULL) and the
#'   drawn data frame \code{data}.
#' @noRd
draw_layout <- function(x, df, ...) UseMethod("draw_layout")

# Title of a layout map, with its dimensions
layout_title <- function(title, df) {
  paste0(title, max(as.numeric(df$ROW)), "X", max(as.numeric(df$COLUMN)))
}

#' @method draw_layout fieldhub_crd
#' @export
#' @noRd
draw_layout.fieldhub_crd <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Completely Randomized Design ", df)
  p1 <- plot_desplot(TREATMENT ~ COLUMN + ROW,
                     data = df,
                     out2.gpar = list(col = "black", lty = 3),
                     text.string = "TREATMENT",
                     main = main,
                     extra_args = dots)
  df$REP <- as.factor(df$REP)
  df$PLOT <- as.character(df$PLOT)
  p2 <- plot_desplot(PLOT ~ COLUMN + ROW,
                     data = df,
                     text.string = "PLOT",
                     main = main,
                     extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_rcbd
#' @export
#' @noRd
draw_layout.fieldhub_rcbd <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Randomized Complete Block Design ", df)
  p1_args <- list(
    form        = TREATMENT ~ COLUMN + ROW,
    data        = df,
    out1.string = "REP",
    out2.gpar   = list(col = "black", lwd = 1, lty = 3),
    text.string = "TREATMENT",
    main        = main,
    extra_args  = dots
  )
  if ("CHECKS" %in% names(df)) {
    # Colour the plot text by check status and outline the check plots
    p1_args$data$CHECKS <- as.character(p1_args$data$CHECKS)
    p1_args$col.string  <- "CHECKS"
    p1_args$out2.string <- "CHECKS"
  }
  p1 <- do.call(plot_desplot, p1_args)
  df$REP <- as.factor(df$REP)
  p2 <- plot_desplot(REP ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     text.string = "PLOT",
                     main = main,
                     extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_full_factorial
#' @export
#' @noRd
draw_layout.fieldhub_full_factorial <- function(x, df, ...) {
  dots <- list(...)
  if (x$infoDesign$kind == "RCBD") {
    main <- layout_title("Full Factorial Design (RCBD) ", df)
    p1 <- plot_desplot(TRT_COMB ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       out2.gpar = list(col = "black", lty = 3),
                       text.string = "TRT_COMB",
                       main = main,
                       extra_args = dots)
    df$PLOT <- as.factor(df$PLOT)
    df$REP <- as.factor(df$REP)
    p2 <- plot_desplot(REP ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       text.string = "PLOT",
                       main = main,
                       extra_args = dots)
  } else {
    main <- layout_title("Full Factorial Design (CRD) ", df)
    p1 <- plot_desplot(TRT_COMB ~ COLUMN + ROW,
                       data = df,
                       out2.gpar = list(col = "black", lty = 3),
                       text.string = "TRT_COMB",
                       main = main,
                       extra_args = dots)
    df$REP <- as.factor(df$REP)
    df$PLOT <- as.factor(df$PLOT)
    p2 <- plot_desplot(PLOT ~ COLUMN + ROW,
                       data = df,
                       text.string = "PLOT",
                       main = main,
                       extra_args = dots)
  }
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_split_plot
#' @export
#' @noRd
draw_layout.fieldhub_split_plot <- function(x, df, ...) {
  dots <- list(...)
  if (x$infoDesign$typeDesign == "RCBD") {
    df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
    df$SUB_PLOT <- as.factor(df$SUB_PLOT)
    df$TRT_COMB <- factor(df$TRT_COMB, levels = unique(df$TRT_COMB))
    df$REP <- as.factor(df$REP)
    main <- layout_title("Split Plot Design (RCBD) ", df)
    p1 <- plot_desplot(TRT_COMB ~ COLUMN + ROW,
                       data = df,
                       text.string = "TRT_COMB",
                       out1.string = "REP",
                       out1.gpar = list(col = "grey"),
                       main = main,
                       extra_args = dots)
    p2 <- plot_desplot(REP ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       out2.gpar = list(col = "gray50", lwd = 1, lty = 1),
                       text.string = "PLOT",
                       key.cex = 0.7,
                       main = main,
                       extra_args = dots)
  } else {
    main <- layout_title("Split Plot Design (CRD) ", df)
    df$REP <- as.factor(df$REP)
    p1 <- plot_desplot(TRT_COMB ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       out2.gpar = list(col = "gray50", lwd = 1, lty = 1),
                       text.string = "TRT_COMB",
                       main = main,
                       extra_args = dots)
    df$REP <- as.factor(df$REP)
    df$PLOT <- as.factor(df$PLOT)
    p2 <- plot_desplot(PLOT ~ COLUMN + ROW,
                       data = df,
                       text.string = "PLOT",
                       main = main,
                       extra_args = dots)
  }
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_split_split_plot
#' @export
#' @noRd
draw_layout.fieldhub_split_split_plot <- function(x, df, ...) {
  dots <- list(...)
  df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
  df$SUB_PLOT <- as.factor(df$SUB_PLOT)
  df$SUB_SUB_PLOT <- as.factor(df$SUB_SUB_PLOT)
  if (x$infoDesign$typeDesign == "RCBD") {
    df$TRT_COMB <- as.factor(df$TRT_COMB)
    df$REP <- as.factor(df$REP)
    main <- layout_title("Split-Split Plot Design (RCBD) ", df)
    p1 <- plot_desplot(TRT_COMB ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       out2.string = "WHOLE_PLOT",
                       col.string = "WHOLE_PLOT",
                       out1.gpar = list(col = "black", lwd = 1, lty = 3),
                       text.string = "TRT_COMB",
                       main = main,
                       extra_args = dots)
    p2 <- plot_desplot(REP ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       out2.gpar = list(col = "gray50", lwd = 1, lty = 1),
                       text.string = "PLOT",
                       key.cex = 0.7,
                       main = main,
                       extra_args = dots)
  } else {
    df$REP <- as.factor(df$REP)
    main <- layout_title("Split-Split Plot Design (CRD) ", df)
    p1 <- plot_desplot(REP ~ COLUMN + ROW,
                       data = df,
                       out1.string = "REP",
                       out2.string = "WHOLE_PLOT",
                       out2.gpar = list(col = "gray50", lwd = 1, lty = 1),
                       text.string = "WHOLE_PLOT",
                       col.string = "SUB_PLOT",
                       main = main,
                       extra_args = dots)
    df$PLOT <- as.factor(df$PLOT)
    p2 <- plot_desplot(PLOT ~ COLUMN + ROW,
                       data = df,
                       text.string = "PLOT",
                       main = main,
                       extra_args = dots)
  }
  list(p1 = p1, p2 = p2, data = df)
}

# Designs in incomplete blocks: the entries, with the reps and blocks outlined
draw_iblock_layout <- function(x, df, title, ...) {
  dots <- list(...)
  df$ENTRY <- as.factor(df$ENTRY)
  main <- layout_title(title, df)
  p1 <- plot_desplot(ENTRY ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     out2.string = "IBLOCK",
                     out2.gpar = list(col = "black", lty = 3),
                     text.string = "ENTRY",
                     main = main,
                     extra_args = dots)
  df$REP <- as.factor(df$REP)
  p2 <- plot_desplot(REP ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     text.string = "PLOT",
                     main = main,
                     extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_incomplete_blocks
#' @export
#' @noRd
draw_layout.fieldhub_incomplete_blocks <- function(x, df, ...) {
  draw_iblock_layout(x, df, "Incomplete Blocks Design Field Layout ", ...)
}

#' @method draw_layout fieldhub_square_lattice
#' @export
#' @noRd
draw_layout.fieldhub_square_lattice <- function(x, df, ...) {
  draw_iblock_layout(x, df, "Square Lattice Design Field Layout ", ...)
}

#' @method draw_layout fieldhub_rectangular_lattice
#' @export
#' @noRd
draw_layout.fieldhub_rectangular_lattice <- function(x, df, ...) {
  draw_iblock_layout(x, df, "Rectangular Lattice Design Field Layout ", ...)
}

#' @method draw_layout fieldhub_alpha_lattice
#' @export
#' @noRd
draw_layout.fieldhub_alpha_lattice <- function(x, df, ...) {
  draw_iblock_layout(x, df, "Alpha Lattice Design Field Layout ", ...)
}

#' @method draw_layout fieldhub_latin_square
#' @export
#' @noRd
draw_layout.fieldhub_latin_square <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Latin Square Design ", df)
  df$TREATMENT <- as.factor(df$TREATMENT)
  # Outline the squares when there are several
  squares <- if (dplyr::n_distinct(x$fieldBook$SQUARE) > 1) list(out1.string = "SQUARE") else list()
  p1 <- do.call(plot_desplot, c(list(
    form = TREATMENT ~ COLUMN + ROW,
    data = df,
    out2.gpar = list(col = "black", lty = 3),
    text.string = "TREATMENT",
    main = main,
    extra_args = dots
  ), squares))
  df$SQUARE <- as.factor(df$SQUARE)
  p2 <- do.call(plot_desplot, c(list(
    form = SQUARE ~ COLUMN + ROW,
    data = df,
    text.string = "PLOT",
    main = main,
    extra_args = dots
  ), squares))
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_strip_plot
#' @export
#' @noRd
draw_layout.fieldhub_strip_plot <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Strip-Plot Design ", df)
  df$TRT_COMB <- as.factor(df$TRT_COMB)
  p1 <- plot_desplot(TRT_COMB ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     out2.gpar = list(col = "black", lty = 3),
                     text.string = "TRT_COMB",
                     main = main,
                     extra_args = dots)
  df$REP <- as.factor(df$REP)
  p2 <- plot_desplot(REP ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     text.string = "PLOT",
                     main = main,
                     extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_row_column
#' @export
#' @noRd
draw_layout.fieldhub_row_column <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Row-Column Design ", df)
  df$ENTRY <- as.factor(df$ENTRY)
  p1 <- plot_desplot(ENTRY ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     out2.gpar = list(col = "black", lty = 3),
                     text.string = "ENTRY",
                     main = main,
                     extra_args = dots)
  df$REP <- as.factor(df$REP)
  p2 <- plot_desplot(REP ~ COLUMN + ROW,
                     data = df,
                     out1.string = "REP",
                     text.string = "PLOT",
                     main = main,
                     extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

# Title of the maps of designs placed when they are built
field_title <- function(title, df) {
  paste0(title, max(as.numeric(df$ROW)), " x ", max(as.numeric(df$COLUMN)))
}

# Unreplicated designs in diagonal arrangements: the experiments, with the
# checks outlined and coloured
draw_diagonal_layout <- function(x, df, ...) {
  df$ENTRY <- as.numeric(df$ENTRY)
  p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = df,
    form = EXPT ~ COLUMN + ROW,
    text.string = "ENTRY",
    col.string = "CHECKS",
    cex = 1,
    shorten = "no",
    out1.string = "EXPT",
    out2.string = "CHECKS",
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = field_title("Un-replicated Diagonal Arrangement ", df),
    show.key = FALSE,
    gg = TRUE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  ), list(...)))
  list(p1 = p1, p2 = NULL, data = df)
}

#' @method draw_layout fieldhub_diagonal_arrangement
#' @export
#' @noRd
draw_layout.fieldhub_diagonal_arrangement <- draw_diagonal_layout
#' @method draw_layout fieldhub_sparse_allocation
#' @export
#' @noRd
draw_layout.fieldhub_sparse_allocation <- draw_diagonal_layout

# Partially replicated designs: the replicated entries in green
draw_prep_layout <- function(x, df, ...) {
  df$ENTRY <- as.character(df$ENTRY)
  df$ENTRY[df$TREATMENT == "Filler"] <- "Filler"
  df$binay_checks <- ifelse(df$CHECKS != 0, 1, 0)
  p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = df,
    form = binay_checks ~ COLUMN + ROW,
    text.string = "ENTRY",
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = field_title("Partially Replicated Design ", df),
    cex = 1,
    shorten = "no",
    show.key = FALSE,
    gg = TRUE,
    col.regions = c("gray", "seagreen")
  ), list(...)))
  list(p1 = p1, p2 = NULL, data = df)
}

#' @method draw_layout fieldhub_partially_replicated
#' @export
#' @noRd
draw_layout.fieldhub_partially_replicated <- draw_prep_layout
#' @method draw_layout fieldhub_multi_location_prep
#' @export
#' @noRd
draw_layout.fieldhub_multi_location_prep <- draw_prep_layout

#' @method draw_layout fieldhub_optimized_arrangement
#' @export
#' @noRd
draw_layout.fieldhub_optimized_arrangement <- function(x, df, ...) {
  df$ENTRY <- as.character(df$ENTRY)
  df$CHECKS <- as.character(df$CHECKS)
  p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = df,
    form = CHECKS ~ COLUMN + ROW,
    text.string = "ENTRY",
    cex = 1,
    shorten = "no",
    main = field_title("Un-replicated Optimized Arrangement ", df),
    show.key = FALSE,
    xlab = "COLUMNS",
    ylab = "ROWS",
    gg = TRUE
  ), list(...)))
  list(p1 = p1, p2 = NULL, data = df)
}

#' @method draw_layout fieldhub_rcbd_augmented
#' @export
#' @noRd
draw_layout.fieldhub_rcbd_augmented <- function(x, df, ...) {
  dots <- list(...)
  df$ENTRY <- as.character(df$ENTRY)
  df$CHECKS <- as.character(df$CHECKS)
  df$BLOCK <- as.character(df$BLOCK)
  # plot numbers as plain integers, whatever PLOT is stored as
  df$PLOT_TXT <- sprintf("%d", as.integer(df$PLOT))
  # text color groups for p1: checks are highlighted, test lines are not;
  # filler plots have CHECKS = NA and are drawn like test lines
  df$CHECK_TEXT <- ifelse(!is.na(df$CHECKS) & df$CHECKS == "1", "check", "test")
  check_text_cols <- c(check = "red3", test = "gray10")
  # Muted palette for the block background
  block_levels <- sort(unique(df$BLOCK))
  muted6 <- c("#F2F2F2", "#E6EEF5", "#E9F2EC", "#F3EEE6", "#EDE7F2", "#F1E9E9")
  if (length(block_levels) > length(muted6)) {
    muted6 <- grDevices::colorRampPalette(muted6)(length(block_levels))
  } else {
    muted6 <- muted6[seq_along(block_levels)]
  }
  fill_vals <- stats::setNames(muted6, block_levels)
  augmented_map <- function(text, main, ...) {
    do.call(desplot::ggdesplot, utils::modifyList(list(
      data = df,
      form = BLOCK ~ COLUMN + ROW,
      text.string = text,
      cex = 0.8,
      shorten = "no",
      out1.string = "EXPT",
      out2.string = "BLOCK",
      xlab = "COLUMNS",
      ylab = "ROWS",
      main = main,
      show.key = FALSE,
      gg = TRUE,
      ticks = "all",
      panel.border = FALSE,
      out2.gpar = list(col = "gray50", lwd = 1, lty = 1),
      ...
    ), dots)) +
      ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
      fieldhub_layout_theme()
  }
  # p1: entries, with the checks highlighted; p2: plot numbers
  p1 <- augmented_map("ENTRY", field_title("Augmented RCBD Layout ", df),
                      col.string = "CHECK_TEXT", col.text = check_text_cols)
  p2 <- augmented_map("PLOT_TXT", field_title("Augmented RCBD Plot Number Layout ", df),
                      col.text = "gray10")
  list(p1 = p1, p2 = p2, data = df)
}
