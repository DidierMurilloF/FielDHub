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

#' @method draw_layout FielDHub
#' @export
#' @noRd
draw_layout.FielDHub <- function(x, df, ...) {
  handler <- registered_design_entry(x)$render
  if (is.null(handler)) fieldhub_abort("This result has no registered layout renderer.")
  get(handler, envir = asNamespace("FielDHub"), inherits = FALSE)(x, df, ...)
}

#' Treatment and plot maps for registered fixed-coordinate designs
#' @noRd
draw_registered_layout <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title(paste0(registered_design_entry(x)$title, " "), df)
  # Field alleys without alleys: rows and columns are the blocks, shown by the
  # white plot grid and the axes on one background.
  p1 <- plot_field_alleys(df, "TREATMENT", main, grid = TRUE, extra_args = dots)
  numbers <- df
  numbers$PLOT <- as.character(numbers$PLOT)
  p2 <- plot_field_alleys(numbers, "PLOT", main, grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' Entry and plot maps of augmented row-column designs
#'
#' The field-alleys style without alleys (the field has no blocks): every
#' plot on the shared neutral inside a white plot grid, check plots on the
#' check grey, labelled by entry number. The plot-number map keeps the grid
#' and does not highlight checks.
#' @noRd
draw_augmented_row_column_layout <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title(paste0(registered_design_entry(x)$title, " "), df)
  is_check <- !is.na(df$CHECKS) & df$CHECKS != 0
  p1 <- plot_field_alleys(df, "ENTRY", main, checks = is_check, grid = TRUE, extra_args = dots)
  numbers <- df
  numbers$PLOT <- sprintf("%d", as.integer(numbers$PLOT))
  p2 <- plot_field_alleys(numbers, "PLOT", main, grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

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
  # No blocks: one background and a white plot grid.
  p1 <- plot_field_alleys(df, "TREATMENT", main, grid = TRUE, extra_args = dots)
  # The drawn field book is returned as before.
  df$REP <- as.factor(df$REP)
  df$PLOT <- as.character(df$PLOT)
  p2 <- plot_field_alleys(df, "PLOT", main, grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_rcbd
#' @export
#' @noRd
draw_layout.fieldhub_rcbd <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Randomized Complete Block Design ", df)
  # Field alleys: one muted background per block, white alleys, a white plot
  # grid, and the checks (if any) on the check grey in the treatment map.
  is_check <- if ("CHECKS" %in% names(df)) !is.na(df$CHECKS) & df$CHECKS != 0 else NULL
  p1 <- plot_field_alleys(df, "TREATMENT", main, group = "REP", checks = is_check, grid = TRUE,
                          extra_args = dots)
  # The drawn field book is returned as before: REP as a factor.
  df$REP <- as.factor(df$REP)
  p2 <- plot_field_alleys(df, "PLOT", main, group = "REP", grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_full_factorial
#' @export
#' @noRd
draw_layout.fieldhub_full_factorial <- function(x, df, ...) {
  dots <- list(...)
  rcbd <- x$infoDesign$kind == "RCBD"
  main <- layout_title(if (rcbd) "Full Factorial Design (RCBD) " else "Full Factorial Design (CRD) ", df)
  # RCBD: a background per rep and alleys; CRD: one background. Both get a
  # white plot grid.
  group <- if (rcbd) "REP" else NULL
  p1 <- plot_field_alleys(df, "TRT_COMB", main, group = group, grid = TRUE, extra_args = dots)
  # The drawn field book is returned as before.
  df$PLOT <- as.factor(df$PLOT)
  df$REP <- as.factor(df$REP)
  p2 <- plot_field_alleys(df, "PLOT", main, group = group, grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_split_plot
#' @export
#' @noRd
draw_layout.fieldhub_split_plot <- function(x, df, ...) {
  dots <- list(...)
  rcbd <- x$infoDesign$typeDesign == "RCBD"
  # The drawn field book is returned as before.
  if (rcbd) {
    df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
    df$SUB_PLOT <- as.factor(df$SUB_PLOT)
    df$TRT_COMB <- factor(df$TRT_COMB, levels = unique(df$TRT_COMB))
  }
  df$REP <- as.factor(df$REP)
  main <- layout_title(if (rcbd) "Split Plot Design (RCBD) " else "Split Plot Design (CRD) ", df)
  # Reps (RCBD) get a background and alleys; whole plots (one PLOT number
  # each) grey lines; sub-plots the white plot grid.
  group <- if (rcbd) "REP" else NULL
  p1 <- plot_field_alleys(df, "TRT_COMB", main, group = group, block = "PLOT", grid = TRUE,
                          extra_args = dots)
  if (!rcbd) df$PLOT <- as.factor(df$PLOT)
  p2 <- plot_field_alleys(df, "PLOT", main, group = group, block = "PLOT", grid = TRUE,
                          extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_split_split_plot
#' @export
#' @noRd
draw_layout.fieldhub_split_split_plot <- function(x, df, ...) {
  dots <- list(...)
  rcbd <- x$infoDesign$typeDesign == "RCBD"
  # The drawn field book is returned as before.
  df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
  df$SUB_PLOT <- as.factor(df$SUB_PLOT)
  df$SUB_SUB_PLOT <- as.factor(df$SUB_SUB_PLOT)
  if (rcbd) df$TRT_COMB <- as.factor(df$TRT_COMB)
  df$REP <- as.factor(df$REP)
  main <- layout_title(if (rcbd) "Split-Split Plot Design (RCBD) " else "Split-Split Plot Design (CRD) ", df)
  # Reps (RCBD) get a background and alleys; whole plots grey lines; sub-plots
  # lighter grey lines; sub-sub-plots the white plot grid.
  group <- if (rcbd) "REP" else NULL
  p1 <- plot_field_alleys(df, "TRT_COMB", main, group = group, block = "PLOT",
                          sub_block = "SUB_PLOT", grid = TRUE, extra_args = dots)
  if (!rcbd) df$PLOT <- as.factor(df$PLOT)
  p2 <- plot_field_alleys(df, "PLOT", main, group = group, block = "PLOT",
                          sub_block = "SUB_PLOT", grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

# Designs in incomplete blocks (alpha, square and rectangular lattices,
# incomplete blocks): field-alleys maps of the entries and of the plot
# numbers: one muted background per replication, white alleys between
# replications, grey outlines around the incomplete blocks and a white grid
# around every plot.
draw_iblock_layout <- function(x, df, title, ...) {
  dots <- list(...)
  # The drawn field book is returned as before: ENTRY and REP as factors.
  df$ENTRY <- as.factor(df$ENTRY)
  df$REP <- as.factor(df$REP)
  main <- layout_title(title, df)
  p1 <- plot_field_alleys(df, "ENTRY", main, group = "REP", block = "IBLOCK", grid = TRUE,
                          extra_args = dots)
  p2 <- plot_field_alleys(df, "PLOT", main, group = "REP", block = "IBLOCK", grid = TRUE,
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
  # Rows and columns are the blocks, shown by the white plot grid and the axes.
  # Several squares get a background each and alleys between them.
  group <- if (dplyr::n_distinct(x$fieldBook$SQUARE) > 1) "SQUARE" else NULL
  p1 <- plot_field_alleys(df, "TREATMENT", main, group = group, grid = TRUE, extra_args = dots)
  # The drawn field book is returned as before.
  df$SQUARE <- as.factor(df$SQUARE)
  p2 <- plot_field_alleys(df, "PLOT", main, group = group, grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_strip_plot
#' @export
#' @noRd
draw_layout.fieldhub_strip_plot <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Strip-Plot Design ", df)
  # Every plot edge inside a replication is a strip edge (rows are horizontal
  # strips, columns vertical strips), so the white plot grid shows the strips.
  df$TRT_COMB <- as.factor(df$TRT_COMB)
  df$REP <- as.factor(df$REP)
  p1 <- plot_field_alleys(df, "TRT_COMB", main, group = "REP", grid = TRUE, extra_args = dots)
  p2 <- plot_field_alleys(df, "PLOT", main, group = "REP", grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

#' @method draw_layout fieldhub_row_column
#' @export
#' @noRd
draw_layout.fieldhub_row_column <- function(x, df, ...) {
  dots <- list(...)
  main <- layout_title("Row-Column Design ", df)
  # Rows and columns are the blocks inside each replication: the white plot
  # grid and the axes show them, white alleys separate the replications.
  df$ENTRY <- as.factor(df$ENTRY)
  df$REP <- as.factor(df$REP)
  p1 <- plot_field_alleys(df, "ENTRY", main, group = "REP", grid = TRUE, extra_args = dots)
  p2 <- plot_field_alleys(df, "PLOT", main, group = "REP", grid = TRUE, extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}

# Title of the maps of designs placed when they are built
field_title <- function(title, df) {
  paste0(title, max(as.numeric(df$ROW)), " x ", max(as.numeric(df$COLUMN)))
}

# Unreplicated entry maps fill the checks, while experiment and plot-number
# maps keep experiment colours without highlighting checks.
draw_diagonal_layout <- function(x, df, ...) {
  dots <- list(...)
  labels <- if (is.null(dots$text.string)) "ENTRY" else dots$text.string
  entry_view <- !labels %in% c("PLOT", "EXPT")
  experiments <- unique(as.character(df$EXPT))
  has_blocks <- length(setdiff(experiments, "Filler")) > 1L
  if (!entry_view && !has_blocks) {
    dots <- utils::modifyList(list(col.regions = rep(fieldhub_layout_neutral(), length(experiments))), dots)
  }
  df$ENTRY <- as.character(df$ENTRY)
  df$ENTRY[df$TREATMENT == "Filler"] <- "Filler"
  drawn <- df
  drawn$CHECKS <- as.character(drawn$CHECKS)
  form <- if (entry_view) CHECKS ~ COLUMN + ROW else EXPT ~ COLUMN + ROW
  colours <- fieldhub_layout_palette()
  if (entry_view && has_blocks) {
    # Keep experiment backgrounds and boundaries; only check cells get new fills.
    expts <- sort(unique(as.character(drawn$EXPT)))
    check <- !is.na(drawn$CHECKS) & drawn$CHECKS != "0"
    checks <- sort(unique(drawn$CHECKS[check]))
    drawn$LAYOUT_FILL <- ifelse(check, paste0("check:", drawn$CHECKS), paste0("experiment:", drawn$EXPT))
    groups <- c(paste0("experiment:", expts), paste0("check:", checks))
    colours <- stats::setNames(rep(colours, length.out = length(groups)), groups)
    if ("Filler" %in% expts) colours[["experiment:Filler"]] <- fieldhub_layout_neutral()
    form <- LAYOUT_FILL ~ COLUMN + ROW
  }
  p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = drawn,
    form = form,
    text.string = "ENTRY",
    col.regions = colours,
    cex = 1,
    shorten = "no",
    out1.string = if (entry_view || has_blocks) "EXPT" else NULL,
    out2.string = if (entry_view) "CHECKS" else NULL,
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = field_title("Un-replicated Diagonal Arrangement ", df),
    show.key = FALSE,
    gg = TRUE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  ), dots))
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
  dots <- list(...)
  plot_numbers <- identical(dots$text.string, "PLOT")
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
    col.regions = c(fieldhub_layout_neutral(), if (plot_numbers) fieldhub_layout_neutral() else "seagreen")
  ), dots))
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
  dots <- list(...)
  plot_numbers <- identical(dots$text.string, "PLOT")
  if (plot_numbers) {
    dots <- utils::modifyList(list(col.regions = rep(fieldhub_layout_neutral(), length(unique(df$CHECKS)))), dots)
  }
  df$ENTRY <- as.character(df$ENTRY)
  df$CHECKS <- as.character(df$CHECKS)
  drawn <- df
  if (!plot_numbers) {
    # Give adjacent copies of a check separate cell borders, not a merged outline.
    drawn$CHECK_OUTLINE <- ifelse(!is.na(df$CHECKS) & df$CHECKS != "0", seq_len(nrow(df)), 0L)
  }
  p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = drawn,
    form = CHECKS ~ COLUMN + ROW,
    col.regions = fieldhub_layout_palette(),
    text.string = "ENTRY",
    out2.string = if (!plot_numbers) "CHECK_OUTLINE" else NULL,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1),
    cex = 1,
    shorten = "no",
    main = field_title("Un-replicated Optimized Arrangement ", df),
    show.key = FALSE,
    xlab = "COLUMNS",
    ylab = "ROWS",
    gg = TRUE
  ), dots))
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
  # The returned field book keeps these columns, as before: plot numbers as
  # plain integers and the check status (fillers have CHECKS = NA).
  df$PLOT_TXT <- sprintf("%d", as.integer(df$PLOT))
  df$CHECK_TEXT <- ifelse(!is.na(df$CHECKS) & df$CHECKS == "1", "check", "test")
  # Block numbers restart in each experiment: every experiment's block is a group.
  drawn <- df
  drawn$FIELD_BLOCK_GROUP <- paste(match(drawn$EXPT, unique(drawn$EXPT)), drawn$BLOCK, sep = ".")
  drawn$FIELD_BLOCK_GROUP <- factor(drawn$FIELD_BLOCK_GROUP,
    levels = unique(drawn$FIELD_BLOCK_GROUP[order(match(drawn$EXPT, unique(drawn$EXPT)),
                                                  as.integer(drawn$BLOCK))]))
  is_check <- df$CHECK_TEXT == "check"
  is_filler <- is.na(df$CHECKS)
  p1 <- plot_field_alleys(drawn, "ENTRY", field_title("Augmented RCBD Layout ", df),
                          group = "FIELD_BLOCK_GROUP", checks = is_check, fillers = is_filler,
                          grid = TRUE, extra_args = dots)
  p2 <- plot_field_alleys(drawn, "PLOT_TXT", field_title("Augmented RCBD Plot Number Layout ", df),
                          group = "FIELD_BLOCK_GROUP", fillers = is_filler, grid = TRUE,
                          extra_args = dots)
  list(p1 = p1, p2 = p2, data = df)
}
