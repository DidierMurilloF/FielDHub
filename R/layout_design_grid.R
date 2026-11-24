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
  stacking <- x$metadata$parameters$stacked
  fieldhub_abort(
    "This design was built on a fixed ", max(x$fieldBook$ROW), " x ", max(x$fieldBook$COLUMN),
    " field with a ", stored_grid_planter(x), " planter",
    if (!is.null(stacking)) paste0(" and the \"", stacking, "\" stacking"),
    ", and that is its only layout. ",
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

#-----------------------------------------------------------------------
# Stacking: how the replicates lie in a field of given dimensions.
#
# "vertical" puts the replicates one above another, each across all the
# columns of the field; "horizontal" side by side, each down all its rows;
# "grid_panel" in rows and columns of replicates, more than one of each.
#-----------------------------------------------------------------------

#' The unit that a stacking moves: the replicate (the complete block) of a
#' design, or the square of a Latin square. NULL for a design without one.
#' @noRd
grid_replicate_unit <- function(x) {
  if (inherits(x, "fieldhub_rcbd")) return("REP")
  unit <- design_grid_spec(x)$groups[1L]
  if (is.null(unit) || !unit %in% c("REP", "SQUARE")) NULL else unit
}

abort_no_replicates <- function() {
  fieldhub_abort("This design has no replicates to stack: 'stacked' does not apply to its ",
                 "field dimensions.")
}

check_grid_stacking <- function(stacked) {
  if (!is.character(stacked) || length(stacked) != 1L || !stacked %in% stacking_choices) {
    fieldhub_abort("'stacked' must be \"vertical\", \"horizontal\" or \"grid_panel\".")
  }
  invisible(stacked)
}

#' Rows of replicates of each arrangement of a stacking
#' @noRd
replicate_rows <- function(reps, stacked) {
  divisors <- seq_len(reps)[reps %% seq_len(reps) == 0L]
  switch(stacked,
    vertical = reps,
    horizontal = 1L,
    grid_panel = divisors[divisors > 1L & divisors < reps])
}

#' The fields a design fits in, by stacking
#'
#' @param stacked The stackings to list; all of them by default. Ignored for
#'   a design without replicates, whose fields have no stacking (NA).
#' @return A data frame with one row per arrangement: the stacking, the rows
#'   of replicates (\code{down}) and the replicates in each (\code{across}),
#'   the rows and columns of a replicate (\code{height}, \code{width}) and of
#'   the field (\code{rows}, \code{cols}). No row when the locations differ
#'   in their number of plots.
#' @noRd
field_grid_candidates <- function(x, stacked = stacking_choices) {
  book <- x$fieldBook
  none <- data.frame(stacked = character(), down = numeric(), across = numeric(),
                     height = numeric(), width = numeric())
  plots <- unique(tabulate(match(book$LOCATION, unique(book$LOCATION))))
  if (length(plots) != 1L) stacked <- character()
  site <- book[book$LOCATION == book$LOCATION[1L], , drop = FALSE]
  unit <- grid_replicate_unit(x)
  axes <- design_grid_spec(x)$axes
  base <- field_grid_unit(site, axes)
  reps <- if (is.null(unit)) 1L else dplyr::n_distinct(site[[unit]])
  # A replicate is any rectangle that holds its plots in whole units
  shapes <- sweep(grid_factor_pairs(plots / reps / prod(base)), 2L, base, "*")
  if (is.null(unit) && length(stacked)) stacked <- NA_character_
  fields <- lapply(stacked, function(stacking) {
    down <- if (is.na(stacking)) 1L else replicate_rows(reps, stacking)
    if (!length(down)) return(none)
    arrangement <- rep(seq_along(down), each = nrow(shapes))
    shape <- rep(seq_len(nrow(shapes)), times = length(down))
    data.frame(stacked = stacking, down = down[arrangement], across = reps / down[arrangement],
               height = shapes[shape, "rows"], width = shapes[shape, "cols"])
  })
  fields <- do.call(rbind, c(list(none), fields))
  fields$rows <- fields$down * fields$height
  fields$cols <- fields$across * fields$width
  fields
}

#' The dimensions of the candidate fields, each once
#' @noRd
field_grid_dimensions <- function(fields) {
  grids <- unique(cbind(rows = fields$rows, cols = fields$cols))
  grids[order(grids[, "rows"]), , drop = FALSE]
}

#' The field of a design for the dimensions and the stacking asked for
#'
#' Without a stacking, the one whose replicate is the most square among those
#' that fit the dimensions; a tie goes to the vertical stack, then to the
#' horizontal one, then to the panel with the most rows of replicates.
#' @return One row of \code{field_grid_candidates()}.
#' @noRd
field_grid_plan <- function(x, nrows, ncols, stacked = NULL) {
  if (!is.null(nrows)) validate_field_dimension(nrows, "nrows")
  if (!is.null(ncols)) validate_field_dimension(ncols, "ncols")
  if (!is.null(stacked)) {
    check_grid_stacking(stacked)
    if (is.null(grid_replicate_unit(x))) abort_no_replicates()
  }
  book <- x$fieldBook
  plots <- unique(tabulate(match(book$LOCATION, unique(book$LOCATION))))
  if (length(plots) != 1L) {
    fieldhub_abort("A common field grid requires the same number of plots at every location.")
  }
  fields <- field_grid_candidates(x, if (is.null(stacked)) stacking_choices else stacked)
  if (!nrow(fields)) {
    fieldhub_abort("The \"grid_panel\" stacking needs rows and columns of replicates, more ",
                   "than one of each: the replicates of this design cannot be arranged so.")
  }
  fits <- rep(TRUE, nrow(fields))
  if (!is.null(nrows)) fits <- fits & fields$rows == nrows
  if (!is.null(ncols)) fits <- fits & fields$cols == ncols
  if (!any(fits)) {
    how <- if (is.null(stacked)) " and preserve the design structure" else switch(stacked,
      vertical = " with the replicates one above another (stacked = \"vertical\")",
      horizontal = " with the replicates side by side (stacked = \"horizontal\")",
      grid_panel = " with the replicates in rows and columns of replicates (stacked = \"grid_panel\")")
    fieldhub_abort("The field grid must fit exactly ", plots, " plots per location", how, ". ",
                   "Valid dimensions (nrows x ncols) include: ",
                   suggest_field_grid(field_grid_dimensions(fields), nrows, ncols), ".")
  }
  fields <- fields[fits, , drop = FALSE]
  fields[order(abs(fields$height - fields$width), match(fields$stacked, stacking_choices),
               -fields$down)[1L], , drop = FALSE]
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

grouped_grid_coordinates <- function(book, groups, axes, dims, base, planter, down = NULL) {
  if (!length(groups)) {
    if (!length(axes)) return(planting_path(dims[1L], dims[2L], planter))
    return(cbind(
      ROW = match(book[[axes[1L]]], unique(book[[axes[1L]]])),
      COLUMN = match(book[[axes[2L]]], unique(book[[axes[2L]]]))
    ))
  }

  group <- book[[groups[1L]]]
  pieces <- split(seq_len(nrow(book)), match(group, unique(group)))
  # The replicates lie as their stacking says (`down` rows of them); the
  # units inside take as many of the remaining row factors as they can
  tile_rows <- if (is.null(down)) grid_common_divisor(dims[1L] / base[1L], length(pieces)) else down
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

#' The field book of a design in a field of given dimensions
#'
#' A design built with dimensions returns its stored grid, for the same
#' dimensions only.
#' @param stacked The stacking of the replicates, or NULL for the most square
#'   replicate (\code{field_grid_plan()}).
#' @noRd
design_field_grid <- function(x, nrows, ncols, planter, stacked = NULL) {
  if (!is.null(nrows)) validate_field_dimension(nrows, "nrows")
  if (!is.null(ncols)) validate_field_dimension(ncols, "ncols")
  if (stores_design_grid(x)) {
    matches_stored_grid(x, nrows, ncols)
    return(x$fieldBook)
  }
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) {
    fieldhub_abort("Field dimensions are not supported for this design.")
  }
  place_field_grid(x, field_grid_plan(x, nrows, ncols, stacked), planter)
}

#' The stacking of the replicates in the field of \code{design_field_grid()}:
#' the one asked for, the one a design was built with, or the one of the most
#' square replicate. NULL for a design without replicates.
#' @noRd
field_grid_stacking <- function(x, nrows, ncols, stacked = NULL) {
  if (!is.null(nrows)) validate_field_dimension(nrows, "nrows")
  if (!is.null(ncols)) validate_field_dimension(ncols, "ncols")
  if (stores_design_grid(x)) return(x$metadata$parameters$stacked)
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) {
    fieldhub_abort("Field dimensions are not supported for this design.")
  }
  stacking <- field_grid_plan(x, nrows, ncols, stacked)$stacked
  if (!is.na(stacking)) stacking
}

#' Place the plots of a design in the field of a plan
#' @param plan One row of \code{field_grid_candidates()}.
#' @noRd
place_field_grid <- function(x, plan, planter) {
  if (inherits(x, "fieldhub_rcbd")) return(rcbd_grid_layout(x, plan, planter))
  design_grid_layout(x, design_grid_spec(x), plan, planter)
}

design_grid_layout <- function(x, spec, plan, planter) {
  book <- x$fieldBook
  sites <- split(seq_len(nrow(book)), match(book$LOCATION, unique(book$LOCATION)))
  base <- field_grid_unit(book[book$LOCATION == book$LOCATION[1L], , drop = FALSE], spec$axes)
  dims <- c(rows = plan$rows, cols = plan$cols)
  coords <- matrix(0, nrow(book), 2L, dimnames = list(NULL, c("ROW", "COLUMN")))
  for (rows in sites) {
    placement <- book[rows, , drop = FALSE]
    if (".WHOLE_UNIT" %in% spec$groups) {
      placement$.WHOLE_UNIT <- interaction(placement$REP, placement$WHOLE_PLOT, drop = TRUE)
    }
    coords[rows, ] <- grouped_grid_coordinates(placement, spec$groups, spec$axes, dims,
                                                base, planter,
                                                down = if (!is.na(plan$stacked)) plan$down)
  }
  if (inherits(x, "fieldhub_latin_square")) {
    names(book)[match(spec$axes, names(book))] <- c("ROW_SQ", "COLUMN_SQ")
  }
  book$ROW <- as.integer(coords[, "ROW"])
  book$COLUMN <- as.integer(coords[, "COLUMN"])
  number_along_field(layout_columns(book), planter)
}

# Fit a full field with congruent rectangular complete blocks. The blocks
# lie across each row of blocks.
rcbd_grid_layout <- function(x, plan, planter) {
  book <- x$fieldBook
  sites <- split(seq_len(nrow(book)), match(book$LOCATION, unique(book$LOCATION)))
  across <- plan$across
  height <- plan$height
  width <- plan$width
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
