#-----------------------------------------------------------------------
# Field grids: the layout of a design in a field of given dimensions.
#
# The units of the design (blocks, incomplete blocks, whole plots, squares)
# tile the field as equal rectangles, and the plots are then numbered along
# the planting path of the whole field.
#-----------------------------------------------------------------------

#' One field dimension: a single positive whole number
#' @noRd
validate_field_dimension <- function(value, name) {
  if (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L ||
      !is.finite(value) || value < 1 || value != trunc(value) ||
      value > .Machine$integer.max) {
    fieldhub_abort("`", name, "` must be one positive whole number.",
                   data = list(argument = name))
  }
  invisible(value)
}

#' The planter of a design built with field dimensions: its own argument,
#' or a serpentine for the designs that have none
#' @noRd
stored_grid_planter <- function(x) {
  planter <- x$metadata$parameters$planter
  if (is.null(planter)) "serpentine" else planter
}

#' A design built with field dimensions has that one layout
#' @noRd
abort_fixed_grid <- function(x) {
  fieldhub_abort(
    "This design was built on a fixed ", max(x$fieldBook$ROW), " x ", max(x$fieldBook$COLUMN),
    " field with a ", stored_grid_planter(x), " planter, and that is its only layout. ",
    "For another layout, build the design again with other 'nrows' and 'ncols', ",
    "or without them."
  )
}

#' Whether the dimensions asked for are those of the grid the design was
#' built on; other dimensions are an error
#' @noRd
matches_stored_grid <- function(x, nrows, ncols) {
  rows <- max(x$fieldBook$ROW)
  cols <- max(x$fieldBook$COLUMN)
  if ((is.null(nrows) || nrows == rows) && (is.null(ncols) || ncols == cols)) return(TRUE)
  abort_fixed_grid(x)
}

#' Number the plots of every location along the planting path of its field
#'
#' The plot numbers of a location are kept and handed out in increasing
#' order as the planter first reaches each plot. Rows that share a plot
#' number (the subplots of a whole plot) keep sharing one.
#' @param book A field book with ROW and COLUMN.
#' @noRd
number_along_field <- function(book, planter) {
  for (rows in split(seq_len(nrow(book)), match(book$LOCATION, unique(book$LOCATION)))) {
    plots <- book$PLOT[rows]
    unit <- match(plots, unique(plots))
    # Whole plots that tile the field as equal rectangles are planted along
    # the path over the grid of whole plots, so consecutive numbers are
    # neighbours; a plot of one cell is its own tile, the path of the field
    tile <- unit_tiling(book$ROW[rows], book$COLUMN[rows], unit)
    path <- planting_path(tile$nrows, tile$ncols, planter)
    step <- matrix(0L, tile$nrows, tile$ncols)
    step[path] <- seq_len(nrow(path))
    reached <- tapply(step[cbind(tile$row, tile$col)], unit, min)
    book$PLOT[rows] <- sort(unique(plots))[rank(reached)][unit]
  }
  book
}

#' The grid of plot units in a field
#'
#' When every unit (the cells that share one plot number) is a full rectangle
#' of the same height and width and the units tile the field, each cell is
#' mapped to its unit's place in the grid of units. Otherwise every cell is
#' its own place, the path of the field.
#' @noRd
unit_tiling <- function(row, col, unit) {
  top <- tapply(row, unit, min)
  left <- tapply(col, unit, min)
  height <- tapply(row, unit, max) - top + 1L
  width <- tapply(col, unit, max) - left + 1L
  h <- max(height)
  w <- max(width)
  tiles <- all(height == h) && all(width == w) && all(tabulate(unit) == h * w) &&
    all((top - 1L) %% h == 0L) && all((left - 1L) %% w == 0L) &&
    max(row) %% h == 0L && max(col) %% w == 0L
  if (!tiles || (h == 1L && w == 1L)) {
    return(list(row = row, col = col, nrows = max(row), ncols = max(col)))
  }
  list(row = (top[unit] - 1L) %/% h + 1L, col = (left[unit] - 1L) %/% w + 1L,
       nrows = max(row) %/% h, ncols = max(col) %/% w)
}

grid_factor_pairs <- function(n) {
  divisors <- seq_len(floor(sqrt(n)))
  divisors <- divisors[n %% divisors == 0]
  rows <- sort(unique(c(divisors, n / divisors)))
  cbind(rows, cols = n / rows)
}

valid_field_grids <- function(book, axes = NULL) {
  plots <- unique(tabulate(match(book$LOCATION, unique(book$LOCATION))))
  if (length(plots) != 1L) return(grid_factor_pairs(0))
  base <- field_grid_unit(book[book$LOCATION == book$LOCATION[1L], axes, drop = FALSE], axes)
  sweep(grid_factor_pairs(plots / prod(base)), 2L, base, "*")
}

field_grid_unit <- function(book, axes) {
  if (!length(axes)) return(c(1L, 1L))
  vapply(book[axes], dplyr::n_distinct, integer(1))
}

suggest_field_grid <- function(grids, nrows, ncols) {
  distance <- numeric(nrow(grids))
  if (!is.null(nrows)) distance <- distance + abs(log(grids[, "rows"] / nrows))
  if (!is.null(ncols)) distance <- distance + abs(log(grids[, "cols"] / ncols))
  candidates <- grids[utils::head(order(distance), 5L), , drop = FALSE]
  paste(paste(candidates[, "rows"], candidates[, "cols"], sep = " x "), collapse = ", ")
}

# Take as many of the remaining row factors as possible at this level.
# The remaining factors are assigned to columns and to inner groups.
grid_common_divisor <- function(a, b) {
  while (b != 0) {
    remainder <- a %% b
    a <- b
    b <- remainder
  }
  a
}

grouped_grid_coordinates <- function(book, groups, axes, dims, base, planter) {
  if (!length(groups)) {
    if (!length(axes)) return(planting_path(dims[1L], dims[2L], planter))
    return(cbind(
      ROW = match(book[[axes[1L]]], unique(book[[axes[1L]]])),
      COLUMN = match(book[[axes[2L]]], unique(book[[axes[2L]]]))
    ))
  }

  group <- book[[groups[1L]]]
  pieces <- split(seq_len(nrow(book)), match(group, unique(group)))
  tile_rows <- grid_common_divisor(dims[1L] / base[1L], length(pieces))
  tile_cols <- length(pieces) / tile_rows
  cell <- dims / c(tile_rows, tile_cols)
  coordinates <- matrix(0, nrow(book), 2L, dimnames = list(NULL, c("ROW", "COLUMN")))
  for (i in seq_along(pieces)) {
    rows <- pieces[[i]]
    offset <- c((i - 1L) %/% tile_cols, (i - 1L) %% tile_cols) * cell
    local <- grouped_grid_coordinates(book[rows, , drop = FALSE], groups[-1L],
                                      axes, cell, base, planter)
    coordinates[rows, ] <- sweep(local, 2L, offset, "+")
  }
  coordinates
}

design_grid_layout <- function(x, spec, nrows, ncols, planter) {
  if (!is.null(nrows)) validate_field_dimension(nrows, "nrows")
  if (!is.null(ncols)) validate_field_dimension(ncols, "ncols")
  if (has_design_grid(x)) {
    matches_stored_grid(x, nrows, ncols)
    return(x$fieldBook)
  }

  book <- x$fieldBook
  sites <- split(seq_len(nrow(book)), match(book$LOCATION, unique(book$LOCATION)))
  plot_counts <- lengths(sites)
  if (length(unique(plot_counts)) != 1L) {
    fieldhub_abort("A common field grid requires the same number of plots at every location.")
  }
  base <- field_grid_unit(book[book$LOCATION == book$LOCATION[1L], , drop = FALSE], spec$axes)
  grids <- valid_field_grids(book, spec$axes)
  matches <- rep(TRUE, nrow(grids))
  if (!is.null(nrows)) matches <- matches & grids[, "rows"] == nrows
  if (!is.null(ncols)) matches <- matches & grids[, "cols"] == ncols
  if (!any(matches)) {
    fieldhub_abort("The field grid must fit exactly ", plot_counts[1L],
                   " plots per location and preserve the design structure. ",
                   "Valid dimensions (nrows x ncols) include: ",
                   suggest_field_grid(grids, nrows, ncols), ".")
  }

  dims <- grids[which(matches)[1L], ]
  coords <- matrix(0, nrow(book), 2L, dimnames = list(NULL, c("ROW", "COLUMN")))
  for (rows in sites) {
    placement <- book[rows, , drop = FALSE]
    if (".WHOLE_UNIT" %in% spec$groups) {
      placement$.WHOLE_UNIT <- interaction(placement$REP, placement$WHOLE_PLOT, drop = TRUE)
    }
    coords[rows, ] <- grouped_grid_coordinates(placement, spec$groups, spec$axes, dims,
                                                base, planter)
  }
  if (inherits(x, "fieldhub_latin_square")) {
    names(book)[match(spec$axes, names(book))] <- c("ROW_SQ", "COLUMN_SQ")
  }
  book$ROW <- as.integer(coords[, "ROW"])
  book$COLUMN <- as.integer(coords[, "COLUMN"])
  number_along_field(layout_columns(book), planter)
}

# Fit a full field with congruent rectangular complete blocks.
rcbd_grid_layout <- function(x, nrows, ncols, planter) {
  if (!is.null(nrows)) validate_field_dimension(nrows, "nrows")
  if (!is.null(ncols)) validate_field_dimension(ncols, "ncols")

  book <- x$fieldBook
  if (all(c("ROW", "COLUMN") %in% names(book))) {
    matches_stored_grid(x, nrows, ncols)
    return(book)
  }

  sites <- split(seq_len(nrow(book)), match(book$LOCATION, unique(book$LOCATION)))
  reps <- dplyr::n_distinct(book$REP)
  plots <- nrow(book) / length(sites)
  rows <- if (is.null(nrows)) plots / ncols else nrows
  cols <- if (is.null(ncols)) plots / nrows else ncols
  if (rows > plots || cols > plots || rows %% 1 != 0 || cols %% 1 != 0 ||
      rows * as.double(cols) != plots) {
    # Every factor pair of the plot count can be partitioned into complete
    # blocks by distributing the factors of reps between the two axes.
    fieldhub_abort(
      "The RCBD field dimensions must fit exactly ", plots, " plots per location. ",
      "Valid dimensions (nrows x ncols) include: ",
      suggest_field_grid(grid_factor_pairs(plots), nrows, ncols), "."
    )
  }

  # Prefer a vertical stack, then a horizontal stack, then the most
  # block rows that fit. The blocks lie across each row of blocks.
  block_rows <- unique(c(reps, 1L, rev(seq_len(reps))))
  block_rows <- block_rows[reps %% block_rows == 0]
  block_cols <- reps / block_rows
  fit <- which(rows %% block_rows == 0 & cols %% block_cols == 0)[1L]
  across <- block_cols[fit]
  height <- rows / block_rows[fit]
  width <- cols / across
  path <- planting_path(height, width, "cartesian")

  book$ROW <- 0L
  book$COLUMN <- 0L
  for (site in sites) {
    block <- match(book$REP[site], sort(unique(book$REP[site]))) - 1L
    cell <- stats::ave(block, block, FUN = seq_along)
    book$ROW[site] <- as.integer(path[cell, "ROW"] + (block %/% across) * height)
    book$COLUMN[site] <- as.integer(path[cell, "COLUMN"] + (block %% across) * width)
  }
  number_along_field(layout_columns(book), planter)
}
