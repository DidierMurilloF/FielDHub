#' Tables the spatial design pages show
#'
#' @description Plain functions that shape a spatial design (or its
#' allocation) into the data frames the result tabs of its page show: the
#' field grids, the entry lists and the allocation table. They only read
#' the result.
#' @name spatial_views
#' @noRd
NULL

#' Colours of highlighted values in a field grid
#'
#' @param kind \code{"checks"} (a colour per check), \code{"replicated"}
#'   (one colour for every replicated entry) or \code{"experiments"} (a
#'   colour per experiment).
#' @param n Number of values.
#' @return A character vector of \code{n} colours (\code{NA} past the
#'   palette).
#' @noRd
spatial_highlight_colours <- function(kind, n) {
  palette <- switch(kind,
    checks = c("royalblue", "salmon", "green", "orange", "orchid", "slategrey",
               "greenyellow", "blueviolet", "deepskyblue", "gold", "blue", "red"),
    replicated = rep("green", n),
    experiments = c("snow", "cadetblue", "lightgreen", "grey", "tan", "lightcyan",
                    "violet", "thistle"),
    fieldhub_abort("Unknown highlight: ", kind, class = "fieldhub_internal_error")
  )
  palette[seq_len(n)]
}

#' A field grid as the page shows it
#'
#' @description Columns \code{V1..Vn}, and rows numbered from the bottom of
#' the field, as the grids are stored (the first matrix row is the last
#' field row).
#' @param grid A matrix or data frame of one location.
#' @param fillers Optional logical matrix of the filler plots, shown as
#'   "Filler".
#' @param highlight Values to colour (checks, replicated entries or
#'   experiments).
#' @param colours Their colours (\code{spatial_highlight_colours()}).
#' @return A list with \code{data}, \code{highlight} and \code{colours}.
#' @noRd
field_grid_view <- function(grid, fillers = NULL, highlight = NULL, colours = NULL) {
  if (is.null(grid) || length(dim(grid)) != 2L) fieldhub_abort("This location has no field layout.")
  grid <- as.matrix(grid)
  if (!is.null(fillers)) grid[fillers] <- "Filler"
  data <- as.data.frame(unname(grid), stringsAsFactors = FALSE)
  colnames(data) <- paste0("V", seq_len(ncol(data)))
  rownames(data) <- rev(seq_len(nrow(data)))
  list(data = data, highlight = highlight, colours = colours)
}

#' An entry list as the page shows it
#' @param data A data frame.
#' @param factors Columns shown as factors (filterable by level).
#' @return \code{data} with those columns as factors.
#' @noRd
entry_list_view <- function(data, factors = c("ENTRY", "NAME")) {
  data <- as.data.frame(data)
  for (column in intersect(factors, names(data))) data[[column]] <- as.factor(data[[column]])
  data
}

#' The checks of one location of a diagonal arrangement and their plots
#' @param design A \code{diagonal_arrangement()} or
#'   \code{sparse_allocation()} result.
#' @param location The location.
#' @return A data frame with ENTRY, NAME and TIMES.
#' @noRd
diagonal_checks_view <- function(design, location) {
  info <- design$infoDesign
  entries <- design$data_entry[[location]]
  checks <- info$entry_checks[[location]]
  data.frame(ENTRY = checks, NAME = entries$NAME[match(checks, entries$ENTRY)],
             TIMES = info$rep_checks[[location]])
}
