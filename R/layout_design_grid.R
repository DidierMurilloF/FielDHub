# Dimensions of a new field layout preserve the units at each level.
design_grid_spec <- function(x) {
  design <- sub("^fieldhub_", "", class(x)[1L])
  switch(design,
    crd = list(),
    full_factorial = list(groups = if (identical(x$infoDesign$kind, "RCBD")) "REP"),
    incomplete_blocks = ,
    alpha_lattice = ,
    square_lattice = ,
    rectangular_lattice = list(groups = c("REP", "IBLOCK")),
    split_plot = ,
    split_split_plot = list(groups = c(
      if (identical(x$infoDesign$typeDesign, "RCBD")) "REP",
      # REP and WHOLE_PLOT identify a whole plot even when whole plots
      # were randomized together across replicates in a CRD.
      if (identical(x$infoDesign$typeDesign, "RCBD")) "WHOLE_PLOT" else ".WHOLE_UNIT",
      if (design == "split_split_plot") "SUB_PLOT"
    )),
    latin_square = list(groups = "SQUARE", axes = c("ROW", "COLUMN")),
    strip_plot = list(groups = "REP", axes = c("HSTRIP", "VSTRIP")),
    NULL
  )
}

has_design_grid <- function(x) {
  all(c("ROW", "COLUMN") %in% names(x$fieldBook)) &&
    !is.null(design_grid_spec(x)) &&
    (!inherits(x, "fieldhub_latin_square") ||
       all(c("ROW_SQ", "COLUMN_SQ") %in% names(x$fieldBook)))
}

store_design_grid <- function(x, nrows, ncols, planter = "serpentine") {
  if (!is.null(nrows) || !is.null(ncols)) {
    x$fieldBook <- field_layout(x, nrows = nrows, ncols = ncols, planter = planter)
  }
  x
}

grid_factor_pairs <- function(n) {
  divisors <- seq_len(floor(sqrt(n)))
  divisors <- divisors[n %% divisors == 0]
  rows <- sort(unique(c(divisors, n / divisors)))
  cbind(rows, cols = n / rows)
}

suggest_field_grid <- function(grids, nrows, ncols) {
  distance <- numeric(nrow(grids))
  if (!is.null(nrows)) distance <- distance + abs(log(grids[, 1L] / nrows))
  if (!is.null(ncols)) distance <- distance + abs(log(grids[, 2L] / ncols))
  candidates <- grids[utils::head(order(distance), 5L), , drop = FALSE]
  paste(paste(candidates[, 1L], candidates[, 2L], sep = " x "), collapse = ", ")
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
  base <- if (length(spec$axes)) {
    vapply(book[sites[[1L]], spec$axes, drop = FALSE], dplyr::n_distinct, integer(1))
  } else c(1L, 1L)
  grids <- sweep(grid_factor_pairs(plot_counts[1L] / prod(base)), 2L, base, "*")
  matches <- rep(TRUE, nrow(grids))
  if (!is.null(nrows)) matches <- matches & grids[, 1L] == nrows
  if (!is.null(ncols)) matches <- matches & grids[, 2L] == ncols
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
