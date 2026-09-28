#' Order in which a planter visits the cells of a field
#'
#' @param nrows,ncols Dimensions of the field.
#' @param planter \code{"serpentine"} or \code{"cartesian"}.
#' @return An integer matrix with the columns ROW and COLUMN, one row per
#'   cell in planting order. The path starts at row 1, column 1 and goes
#'   along the rows; with \code{"serpentine"} every second row goes back.
#' @noRd
planting_path <- function(nrows, ncols, planter = "serpentine") {
  nrows <- as.integer(nrows)
  ncols <- as.integer(ncols)
  ROW <- rep(seq_len(nrows), each = ncols)
  COLUMN <- rep(seq_len(ncols), times = nrows)
  if (planter == "serpentine") {
    back <- ROW %% 2L == 0L
    COLUMN[back] <- ncols + 1L - COLUMN[back]
  }
  cbind(ROW = ROW, COLUMN = COLUMN)
}

#' Cells of a field map in planting order
#'
#' @description Field maps are matrices whose last row is planted first: the
#' planter starts at the bottom-left cell and moves up one row at a time. This
#' is \code{planting_path()} with the rows of a matrix.
#'
#' @return An integer matrix with the columns row and col, which indexes the
#'   cells of the map in planting order.
#' @noRd
field_path <- function(nrows, ncols, planter = "serpentine") {
  path <- planting_path(nrows, ncols, planter)
  cbind(row = as.integer(nrows) + 1L - path[, "ROW"], col = path[, "COLUMN"])
}

#' Extract map values in an existing planting path's order
#'
#' Keep the export convention of double storage for numeric/logical maps,
#' character storage for labels, and no names on the returned vector.
#' @param map A field-map matrix.
#' @param path A two-column matrix of row and column indices.
#' @noRd
values_along_path <- function(map, path) {
  if (nrow(path) == 0L) return(numeric())
  values <- numeric(nrow(path))
  values[] <- map[path]
  values
}

#' Fill the empty cells of a field map in planting order
#'
#' @param map A matrix.
#' @param values Values for the empty cells in planting order. When there are
#'   fewer values than empty cells, the last cells get NA.
#' @param empty Value of the empty cells.
#' @noRd
fill_along_path <- function(map, values, planter, empty = 0) {
  path <- field_path(nrow(map), ncol(map), planter)
  free <- path[which(map[path] == empty), , drop = FALSE]
  map[free] <- values[seq_len(nrow(free))]
  map
}

#' The last n cells of the planting path of a field map, from the end
#' backward; fillers go there
#' @noRd
path_end <- function(nrows, ncols, planter, n) {
  path <- field_path(nrows, ncols, planter)
  path[rev(seq_len(nrow(path)))[seq_len(n)], , drop = FALSE]
}

#' Columns of the top row of a field map, the last one planted, that hold n
#' fillers at the end of the planting path (n at most the number of columns)
#' @noRd
filler_columns <- function(nrows, ncols, planter, n) {
  if (n > ncols) {
    fieldhub_abort("Internal error: more fillers than columns in the last row.",
                   class = "fieldhub_internal_error")
  }
  sort(path_end(nrows, ncols, planter, n)[, "col"])
}

#' Place a sequence along the rows of a matrix in planting order
#'
#' @param M A matrix whose rows are planted in order, row 1 first, holding the
#'   sequence row by row.
#' @return M with every row that the planter goes back along reversed.
#' @noRd
along_rows <- function(M, planter = "serpentine") {
  path <- planting_path(nrow(M), ncol(M), planter)
  M[path] <- as.vector(t(M))
  M
}

#' Plot numbers of a grid in planting order
#'
#' Fills an \code{nrows x ncols} grid with \code{plots}, row by row, then
#' reads the grid back along the planting path: unchanged for a cartesian
#' planter, with every second row reversed for a serpentine one.
#'
#' @param plots Plot numbers, row 1 of the grid first.
#' @param ncols Number of columns of the grid.
#' @param planter \code{"serpentine"} or \code{"cartesian"}.
#' @noRd
plots_along_grid <- function(plots, ncols, planter = "serpentine") {
  M <- matrix(plots, ncol = ncols, byrow = TRUE)
  as.vector(t(along_rows(M, planter)))
}

#' Plot numbers of a grid in planting order, one rep at a time
#'
#' Some layouts lay each rep's plots out in its own grid of \code{grid_cols}
#' columns; the planter starts each rep's grid afresh, so the serpentine
#' direction restarts at row 1 of every rep instead of continuing across
#' reps. \code{plots} holds every rep's plots back to back.
#'
#' @param plots Plot numbers of every rep, rep by rep.
#' @param grid_cols Columns of each rep's grid.
#' @param reps Number of reps.
#' @param planter \code{"serpentine"} or \code{"cartesian"}.
#' @noRd
plots_along_grid_by_rep <- function(plots, grid_cols, reps, planter = "serpentine") {
  chunks <- split_vectors(x = plots, len_cuts = rep(length(plots) / reps, reps))
  unlist(lapply(chunks, plots_along_grid, ncols = grid_cols, planter = planter))
}
