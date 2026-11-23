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
  if (!is.null(nrows)) validate_iteration_budget(nrows, "nrows")
  if (!is.null(ncols)) validate_iteration_budget(ncols, "ncols")
  if (has_design_grid(x)) {
    rows <- max(x$fieldBook$ROW)
    cols <- max(x$fieldBook$COLUMN)
    if ((is.null(nrows) || nrows == rows) && (is.null(ncols) || ncols == cols)) {
      return(x$fieldBook)
    }
    fieldhub_abort("This design already has a stored field layout. Valid dimensions: ",
                   rows, " x ", cols, ".")
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
  book$ROW <- coords[, "ROW"]
  book$COLUMN <- coords[, "COLUMN"]
  layout_columns(book)
}

# Fit a full field with congruent rectangular complete blocks.
rcbd_grid_layout <- function(x, nrows, ncols, planter) {
  if (!is.null(nrows)) validate_iteration_budget(nrows, "nrows")
  if (!is.null(ncols)) validate_iteration_budget(ncols, "ncols")

  book <- x$fieldBook
  if (all(c("ROW", "COLUMN") %in% names(book))) {
    rows <- max(book$ROW)
    cols <- max(book$COLUMN)
    if ((is.null(nrows) || nrows == rows) && (is.null(ncols) || ncols == cols)) return(book)
    fieldhub_abort("This RCBD already has a stored ", rows, " x ", cols, " field layout.")
  }

  reps <- dplyr::n_distinct(book$REP)
  plots <- nrow(book) / dplyr::n_distinct(book$LOCATION)
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
  # block rows that fit. Number the blocks across each row of blocks.
  block_rows <- unique(c(reps, 1L, rev(seq_len(reps))))
  block_rows <- block_rows[reps %% block_rows == 0]
  block_cols <- reps / block_rows
  fit <- which(rows %% block_rows == 0 & cols %% block_cols == 0)[1L]
  across <- block_cols[fit]
  height <- rows / block_rows[fit]
  width <- cols / across
  path <- planting_path(height, width, "cartesian")

  books <- lapply(location_books(x), function(site) {
    blocks <- split(site, site$REP)
    for (i in seq_along(blocks)) {
      block <- blocks[[i]]
      block$ROW <- path[, "ROW"] + ((i - 1L) %/% across) * height
      block$COLUMN <- path[, "COLUMN"] + ((i - 1L) %% across) * width
      block$PLOT <- plots_along_grid(block$PLOT, width, planter)
      blocks[[i]] <- block
    }
    layout_columns(dplyr::bind_rows(blocks))
  })
  dplyr::bind_rows(books)
}
