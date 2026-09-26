#-----------------------------------------------------------------------
# Field layouts
#
# The field books of CRD, RCBD, latin square, factorial, split-plot, strip-
# plot, incomplete-block, lattice and row-column designs have no field
# coordinates: the layout is chosen when the design is drawn or exported,
# among the options that fit its reps and blocks. layout_options() computes
# those options for each location, field_layout() returns one of them and
# plot_layout() draws it. The other designs place their plots when they are
# built, so their field books already have ROW and COLUMN.
#-----------------------------------------------------------------------

planter_choices <- c("serpentine", "cartesian")
stacking_choices <- c("vertical", "horizontal", "grid_panel")

#' Check the planter and the stacking of a layout
#' @noRd
check_layout_arguments <- function(planter, stacked, call = sys.call(-1)) {
  if (!is.character(planter) || length(planter) != 1 || !planter %in% planter_choices) {
    fieldhub_abort("'planter' must be \"serpentine\" or \"cartesian\".", call = call)
  }
  if (!is.character(stacked) || length(stacked) != 1 || !stacked %in% stacking_choices) {
    fieldhub_abort("'stacked' must be \"vertical\", \"horizontal\" or \"grid_panel\".",
                   call = call)
  }
  invisible(TRUE)
}

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
  if (n > ncols) stop("Internal error: more fillers than columns in the last row.")
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

#' Columns of a field book with coordinates: the identifiers, the
#' coordinates, then the other columns in their order
#' @noRd
layout_columns <- function(book) {
  first <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN")
  book[, c(first, setdiff(names(book), first))]
}

#' Field books of each location, in the order of the field book
#' @noRd
location_books <- function(x) {
  locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
  lapply(levels(locations), function(loc) {
    as.data.frame(dplyr::filter(x$fieldBook, LOCATION == loc))
  })
}

#' Layout options of a design
#'
#' @param x A design result.
#' @param planter \code{"serpentine"} or \code{"cartesian"}.
#' @param stacked \code{"vertical"}, \code{"horizontal"} or
#'   \code{"grid_panel"}: how the reps are arranged.
#' @return A list with one element per location: the list of field books with
#'   ROW and COLUMN of each layout option. The list is empty when the
#'   stacking has no layout for the design.
#' @noRd
layout_options <- function(x, planter = "serpentine", stacked = "vertical") {
  UseMethod("layout_options")
}

#' @method layout_options default
#' @export
#' @noRd
layout_options.default <- function(x, planter = "serpentine", stacked = "vertical") {
  fieldhub_abort("This result has no field layout.")
}

#' @method layout_options fieldhub_split_families
#' @export
#' @noRd
layout_options.fieldhub_split_families <- function(x, planter = "serpentine", stacked = "vertical") {
  fieldhub_abort("split_families() results have no field layout to plot.")
}

# Designs whose field book has coordinates have one layout per location
field_book_layouts <- function(x, planter = "serpentine", stacked = "vertical") {
  lapply(location_books(x), list)
}

#' @method layout_options fieldhub_partially_replicated
#' @export
#' @noRd
layout_options.fieldhub_partially_replicated <- field_book_layouts
#' @method layout_options fieldhub_multi_location_prep
#' @export
#' @noRd
layout_options.fieldhub_multi_location_prep <- field_book_layouts
#' @method layout_options fieldhub_rcbd_augmented
#' @export
#' @noRd
layout_options.fieldhub_rcbd_augmented <- field_book_layouts
#' @method layout_options fieldhub_diagonal_arrangement
#' @export
#' @noRd
layout_options.fieldhub_diagonal_arrangement <- field_book_layouts
#' @method layout_options fieldhub_sparse_allocation
#' @export
#' @noRd
layout_options.fieldhub_sparse_allocation <- field_book_layouts
#' @method layout_options fieldhub_optimized_arrangement
#' @export
#' @noRd
layout_options.fieldhub_optimized_arrangement <- field_book_layouts

#' @method layout_options fieldhub_crd
#' @export
#' @noRd
layout_options.fieldhub_crd <- function(x, planter = "serpentine", stacked = "vertical") {
  crd_layouts(x,
              n_trt = dplyr::n_distinct(x$fieldBook$TREATMENT),
              n_reps = dplyr::n_distinct(x$fieldBook$REP),
              planter = planter)
}

#' @method layout_options fieldhub_rcbd
#' @export
#' @noRd
layout_options.fieldhub_rcbd <- function(x, planter = "serpentine", stacked = "vertical") {
  n_reps <- dplyr::n_distinct(x$fieldBook$REP)
  n_locs <- dplyr::n_distinct(x$fieldBook$LOCATION)
  rcbd_layouts(x,
               n_units = nrow(x$fieldBook) / (n_reps * n_locs),
               n_reps = n_reps,
               planter = planter,
               stacked = stacked)
}

#' @method layout_options fieldhub_full_factorial
#' @export
#' @noRd
layout_options.fieldhub_full_factorial <- function(x, planter = "serpentine", stacked = "vertical") {
  n_trt <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
  n_reps <- dplyr::n_distinct(x$fieldBook$REP)
  if (x$infoDesign$kind == "RCBD") {
    return(rcbd_layouts(x, n_units = n_trt, n_reps = n_reps, planter = planter,
                        stacked = stacked))
  }
  crd_layouts(x, n_trt = n_trt, n_reps = n_reps, planter = planter)
}

#' @method layout_options fieldhub_split_plot
#' @export
#' @noRd
layout_options.fieldhub_split_plot <- function(x, planter = "serpentine", stacked = "vertical") {
  wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
  sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
  n_reps <- dplyr::n_distinct(x$fieldBook$REP)
  if (x$infoDesign$typeDesign == "RCBD") {
    return(split_layouts(x, n_reps = n_reps, sizeIblocks = sp, iBlocks = wp,
                         planter = planter, stacked = stacked))
  }
  crd_layouts(x, n_trt = wp * sp, n_reps = n_reps, planter = planter)
}

#' @method layout_options fieldhub_split_split_plot
#' @export
#' @noRd
layout_options.fieldhub_split_split_plot <- function(x, planter = "serpentine", stacked = "vertical") {
  wp <- dplyr::n_distinct(x$fieldBook$WHOLE_PLOT)
  sp <- dplyr::n_distinct(x$fieldBook$SUB_PLOT)
  ssp <- dplyr::n_distinct(x$fieldBook$SUB_SUB_PLOT)
  n_trt <- dplyr::n_distinct(x$fieldBook$TRT_COMB)
  n_reps <- dplyr::n_distinct(x$fieldBook$REP)
  if (x$infoDesign$typeDesign == "RCBD") {
    return(split_layouts(x, n_reps = n_reps, sizeIblocks = as.numeric(ssp),
                         iBlocks = wp * sp, planter = planter, stacked = stacked))
  }
  crd_layouts(x, n_trt = n_trt, n_reps = n_reps, planter = planter)
}

#' @method layout_options fieldhub_incomplete_blocks
#' @export
#' @noRd
layout_options.fieldhub_incomplete_blocks <- function(x, planter = "serpentine", stacked = "vertical") {
  iblock_layouts(x, planter, stacked)
}
#' @method layout_options fieldhub_square_lattice
#' @export
#' @noRd
layout_options.fieldhub_square_lattice <- layout_options.fieldhub_incomplete_blocks
#' @method layout_options fieldhub_rectangular_lattice
#' @export
#' @noRd
layout_options.fieldhub_rectangular_lattice <- layout_options.fieldhub_incomplete_blocks
#' @method layout_options fieldhub_alpha_lattice
#' @export
#' @noRd
layout_options.fieldhub_alpha_lattice <- layout_options.fieldhub_incomplete_blocks

#' @method layout_options fieldhub_latin_square
#' @export
#' @noRd
layout_options.fieldhub_latin_square <- function(x, planter = "serpentine", stacked = "vertical") {
  fb <- x$fieldBook
  options <- square_layouts(x, rsRep = dplyr::n_distinct(fb$COLUMN),
                            csRep = dplyr::n_distinct(fb$ROW),
                            n_Reps = dplyr::n_distinct(fb$SQUARE),
                            planter = planter, stacked = stacked, from_book = FALSE)
  # The row and column within the square become ROW_SQ and COLUMN_SQ
  square_coordinates(options, own = c("ROW_SQ", "COLUMN_SQ"))
}

#' @method layout_options fieldhub_strip_plot
#' @export
#' @noRd
layout_options.fieldhub_strip_plot <- function(x, planter = "serpentine", stacked = "vertical") {
  fb <- x$fieldBook
  options <- square_layouts(x, rsRep = dplyr::n_distinct(fb$HSTRIP),
                            csRep = dplyr::n_distinct(fb$VSTRIP),
                            n_Reps = dplyr::n_distinct(fb$REP),
                            planter = planter, stacked = stacked, from_book = FALSE)
  square_coordinates(options)
}

#' @method layout_options fieldhub_row_column
#' @export
#' @noRd
layout_options.fieldhub_row_column <- function(x, planter = "serpentine", stacked = "vertical") {
  fb <- x$fieldBook
  options <- square_layouts(x, rsRep = dplyr::n_distinct(fb$ROW),
                            csRep = dplyr::n_distinct(fb$COLUMN),
                            n_Reps = dplyr::n_distinct(fb$REP),
                            planter = planter, stacked = stacked, from_book = TRUE)
  # The row and column within the rep give way to the field coordinates
  square_coordinates(options)
}

#-----------------------------------------------------------------------
# CRD-like designs: the plots, in field-book order, fill a grid along the
# planting path.
#-----------------------------------------------------------------------

#' Grids of the layouts of a CRD-like design with n plots
#'
#' @return A list of c(rows, columns): reps x treatments and treatments x
#'   reps when every treatment has every rep, else every grid that holds the
#'   n plots exactly, with one row or one column only when n is prime.
#' @noRd
crd_dimensions <- function(n, n_reps, n_trt) {
  if (n_reps * n_trt == n) return(list(c(n_reps, n_trt), c(n_trt, n_reps)))
  rows <- which(n %% seq_len(n) == 0)
  rows <- rows[rows > 1 & rows < n]
  if (length(rows) == 0) rows <- c(1, n)
  lapply(rows, function(r) c(r, n %/% r))
}

#' @noRd
crd_layouts <- function(x, n_trt, n_reps, planter) {
  lapply(location_books(x), function(book) {
    lapply(crd_dimensions(nrow(book), n_reps, n_trt), function(dims) {
      path <- planting_path(dims[1], dims[2], planter)
      book$ROW <- path[, "ROW"]
      book$COLUMN <- path[, "COLUMN"]
      layout_columns(book)
    })
  })
}

#-----------------------------------------------------------------------
# RCBD-like designs
#-----------------------------------------------------------------------

#' @noRd
rcbd_layouts <- function(x, n_units, n_reps, planter, stacked) {
  lapply(location_books(x), function(book) {
    layouts <- switch(stacked,
      vertical = generate_vertical_layout(book, book$PLOT, n_units, n_reps, planter),
      horizontal = generate_horizontal_layout(book, book$PLOT, n_units, n_reps, planter),
      list()
    )
    lapply(unname(layouts), layout_columns)
  })
}

#' Generate Vertical Layouts for RCBD Designs
#'
#' @description
#' This function creates a set of vertical layout options for a randomized complete block design (RCBD)
#' using a subset of field book data. It generates a basic vertical grid layout (with a specified number
#' of replicates and treatments per replicate), extended factor-based layouts when the number of treatments
#' has multiple prime factors, and a single-column layout.
#'
#' @param NewBook A data frame containing the field book data for a single location. Typically, this is a subset
#'   of the FielDHub fieldBook.
#' @param plots A numeric or character vector representing the plot identifiers.
#' @param n_units An integer specifying the number of plots in each block.
#' @param n_Reps An integer specifying the number of replicates (blocks).
#' @param planter A character string indicating the plot numbering scheme.
#' @noRd
generate_vertical_layout <- function(NewBook, plots, n_units, n_Reps, planter) {
  layouts <- list()

  # 1) Basic vertical layout: (n_Reps) rows, (n_units) columns
  basic_df <- NewBook |>
    dplyr::mutate(
      ROW = rep(1:n_Reps, each = n_units),
      COLUMN = rep(1:n_units, times = n_Reps)
    )
  layouts[["basic_vertical"]] <- basic_df

  # 2) Extended vertical factor-based layouts
  pf <- prime_factors(n_units)
  if (length(pf) >= 2) {
    factor_combos <- as.data.frame(
      factor_subsets(n_units, all_factors = TRUE)$comb_factors
    )
    for (i in seq_len(nrow(factor_combos))) {
      s1 <- as.numeric(factor_combos[i, 1])
      s2 <- as.numeric(factor_combos[i, 2])

      df <- NewBook |>
        dplyr::mutate(
          ROW = rep(1:(s1 * n_Reps), each = s2),
          COLUMN = rep(rep(1:s2, times = s1), times = n_Reps)
        )
      nCols <- max(df$COLUMN)

      df$PLOT <- planter_transform(
        plots   = plots,
        planter = planter,
        reps    = n_Reps,
        cols    = nCols,
        units   = NULL
      )
      layouts[[paste0("vertical_ext_", i)]] <- df
    }
  }

  # 3) Single-column layout (all plots in a single column) as the LAST option
  single_column_df <- NewBook |>
    dplyr::mutate(
      ROW = 1:(n_units * n_Reps),
      COLUMN = 1
    )
  layouts[["single_column"]] <- single_column_df

  return(layouts)
}

#' Generate Horizontal Layouts for RCBD Designs
#'
#' @description
#' This function creates a set of horizontal layout options for a randomized complete block design (RCBD)
#' using a subset of field book data. It produces a basic horizontal grid layout and, when applicable,
#' generates extended factor-based layouts.
#'
#' @param NewBook A data frame containing the field book data for a single location. Typically, this is a subset
#'   of the FielDHub fieldBook.
#' @param plots A numeric or character vector representing the plot identifiers.
#' @param n_units An integer specifying the number of plots in each block.
#' @param n_Reps An integer specifying the number of replicates (blocks).
#' @param planter A character string indicating the plot numbering scheme.
#' @noRd
generate_horizontal_layout <- function(NewBook, plots, n_units, n_Reps, planter) {
  layouts <- list()

  # B. Basic horizontal grid
  basic_horizontal_df <- NewBook |>
    dplyr::mutate(
      ROW    = rep(1:n_units, times = n_Reps),
      COLUMN = rep(1:n_Reps, each = n_units)
    )
  layouts[["basic_horizontal"]] <- basic_horizontal_df

  # C. Extended factor-based layouts
  factor_combos <- as.data.frame(
    factor_subsets(n_units, all_factors = TRUE)$comb_factors
  )
  if (nrow(factor_combos) > 0) {
    for (i in seq_len(nrow(factor_combos))) {
      s1 <- as.numeric(factor_combos[i, 1])
      s2 <- as.numeric(factor_combos[i, 2])

      # Assign columns per replication
      w <- 1:(s1 * n_Reps)
      u <- seq(1, length(w), by = s1)
      v <- seq(s1, length(w), by = s1)
      z <- unlist(lapply(1:n_Reps, function(j) rep(u[j]:v[j], times = s2)))

      df <- NewBook |>
        dplyr::mutate(
          ROW    = rep(rep(1:s2, each = s1), n_Reps),
          COLUMN = z
        )
      nCols <- max(df$COLUMN)

      df$PLOT <- planter_transform(
        plots   = plots,
        planter = planter,
        reps    = n_Reps,
        cols    = nCols,
        mode    = "Horizontal",
        units   = NULL
      )
      layouts[[paste0("horizontal_ext_", i)]] <- df
    }
  }

  return(layouts)
}

#-----------------------------------------------------------------------
# Designs in incomplete blocks and split plots in complete blocks: each rep
# is a panel of blocks, and the reps are stacked vertically, horizontally or
# in a grid of panels.
#-----------------------------------------------------------------------

#' Ways to write the prime factors f of a number as two factors: f and its
#' reverse when f has two factors, else the first factor against the rest
#' and the last factor against the rest, each in both orders
#' @noRd
factor_splits <- function(f) {
  if (length(f) == 2) {
    return(unique(data.frame(rbind(f, rev(f)))))
  }
  f1 <- c(f[1], prod(f[2:length(f)]))
  f2 <- c(prod(f[1:length(f) - 1]), f[length(f)])
  unique(data.frame(rbind(f1, rev(f1), f2, rev(f2))))
}

#' Rows of each rep when its blocks stand side by side
#' @noRd
block_panel_rows <- function(sizeIblocks, n_Reps, iBlocks) {
  w <- 1:(sizeIblocks * n_Reps)
  u <- seq(1, length(w), by = sizeIblocks)
  v <- seq(sizeIblocks, length(w), by = sizeIblocks)
  z <- vector(mode = "list", length = n_Reps)
  for (j in 1:n_Reps) {
    z[[j]] <- c(rep(u[j]:v[j], times = iBlocks))
  }
  unlist(z)
}

#' Coordinates of the vertical layouts that arrange the blocks of each rep
#' in a grid given by the factors of the number of blocks and of the block
#' size
#' @return A list of data frames with ROW and COLUMN.
#' @noRd
block_grid_coordinates <- function(sizeIblocks, n_Reps, iBlocks) {
  r <- prime_factors(iBlocks)
  if (length(r) > 2) {
    if (iBlocks %% 2 != 0) {
      r <- c(r[1], prod(r[2:length(r)]))
    } else {
      r <- c(prod(r[1:length(r) - 1]), r[length(r)])
    }
  }
  if (length(r) == 1) r <- c(1, r)
  y <- prime_factors(sizeIblocks)
  if (sizeIblocks == 2) y <- c(1, y)
  Y <- factor_splits(y)
  lapply(seq_len(nrow(Y)), function(k) {
    s1 <- as.numeric(Y[k, ][1])
    s2 <- as.numeric(Y[k, ][2])
    w_r <- 1:(r[1] * s1 * n_Reps)
    u_r <- seq(1, length(w_r), by = s1)
    v_r <- seq(s1, length(w_r), by = s1)
    z_rows <- vector(mode = "list", length = n_Reps * r[1])
    for (j in 1:(n_Reps * r[1])) {
      z_rows[[j]] <- rep(c(rep(u_r[j]:v_r[j], each = s2)), times = r[2])
    }
    w_c <- 1:(r[2] * s2)
    u_c <- seq(1, length(w_c), by = s2)
    v_c <- seq(s2, length(w_c), by = s2)
    z_cols <- vector(mode = "list", length = r[2])
    for (i in 1:r[2]) {
      z_cols[[i]] <- c(rep(u_c[i]:v_c[i], times = s1))
    }
    data.frame(ROW = unlist(z_rows), COLUMN = rep(unlist(z_cols), times = r[1] * n_Reps))
  })
}

#' Coordinates of the vertical layouts that split the blocks of each rep
#' into rows of blocks
#' @noRd
block_row_coordinates <- function(sizeIblocks, n_Reps, iBlocks) {
  r <- prime_factors(iBlocks)
  if (iBlocks == 2) r <- c(1, r)
  r <- rev(r)
  R <- factor_splits(r)
  lapply(seq_len(nrow(R)), function(k) {
    w1 <- as.numeric(R[k, ][1])
    w2 <- as.numeric(R[k, ][2])
    w_r <- 1:(w1 * n_Reps)
    u_r <- seq(1, length(w_r), by = w1)
    v_r <- seq(w1, length(w_r), by = w1)
    z_rows <- vector(mode = "list", length = n_Reps)
    for (j in 1:(n_Reps)) {
      z_rows[[j]] <- c(rep(u_r[j]:v_r[j], each = w2 * sizeIblocks))
    }
    w_c <- 1:(w2 * sizeIblocks)
    z_cols <- vector(mode = "list", length = n_Reps)
    for (i in 1:n_Reps) {
      z_cols[[i]] <- rep(w_c, times = w1)
    }
    data.frame(ROW = unlist(z_rows), COLUMN = as.vector(unlist(z_cols)))
  })
}

#' Coordinates of the grid-panel layouts, which arrange the reps in a grid
#' @return A list with one or two data frames with ROW and COLUMN, or an
#'   empty list when the number of reps does not allow a grid.
#' @noRd
panel_grid_coordinates <- function(sizeIblocks, n_Reps, iBlocks) {
  if (n_Reps <= 2 || !(n_Reps %% 2 == 0 || sqrt(n_Reps) %% 1 == 0)) return(list())
  t <- prime_factors(n_Reps)
  panel <- function(s, n0, nCols) {
    nROWs <- s * sizeIblocks
    w0 <- 1:(nROWs)
    u0 <- seq(1, length(w0), by = sizeIblocks)
    v0 <- seq(sizeIblocks, length(w0), by = sizeIblocks)
    z0 <- vector(mode = "list", length = s)
    for (j in 1:(s)) {
      z0[[j]] <- rep(c(rep(u0[j]:v0[j], times = iBlocks)), n0)
    }
    data.frame(ROW = unlist(z0), COLUMN = rep(rep(1:nCols, each = sizeIblocks), s))
  }
  s <- if (length(t) > 2) t[1] * t[2] else t[1]
  first <- panel(s, n0 = t[length(t)], nCols = t[length(t)] * iBlocks)
  # A square number of reps has one grid only
  if (sqrt(n_Reps) %% 1 == 0) return(list(first))
  s <- if (length(t) > 2) t[2] * t[3] else t[2]
  list(first, panel(s, n0 = t[1], nCols = t[1] * iBlocks))
}

#' Layouts of designs in incomplete blocks: the plots are renumbered along
#' the planting path
#' @noRd
iblock_layouts <- function(x, planter = "serpentine", stacked = "vertical") {
  n_TrtGen <- dplyr::n_distinct(x$fieldBook$ENTRY)
  n_Reps <- dplyr::n_distinct(x$fieldBook$REP)
  sizeIblocks <- dplyr::n_distinct(x$fieldBook$UNIT)
  iBlocks <- n_TrtGen / sizeIblocks
  lapply(location_books(x), function(NewBook) {
    plots <- NewBook$PLOT
    with_rows <- function(coordinates, order_by = "ROW", mode = NULL, units = NULL) {
      df <- NewBook |>
        dplyr::mutate(ROW = coordinates$ROW, COLUMN = coordinates$COLUMN)
      if (identical(order_by, "ROW")) df <- df[order(df$ROW, decreasing = FALSE), ]
      if (identical(order_by, "REP")) df <- df[order(df$REP, df$UNIT), ]
      df$PLOT <- planter_transform(plots = plots, planter = planter, reps = n_Reps,
                                   cols = max(df$COLUMN), mode = mode, units = units)
      df
    }
    z <- block_panel_rows(sizeIblocks, n_Reps, iBlocks)
    blocks_in_rows <- data.frame(
      ROW = rep(1:(iBlocks * n_Reps), each = sizeIblocks),
      COLUMN = rep(rep(1:sizeIblocks, times = iBlocks), n_Reps)
    )
    books <- list()
    if (stacked == "vertical") {
      grids <- list()
      if (sizeIblocks %% 2 == 0 || sqrt(sizeIblocks) %% 1 == 0) {
        grids <- block_grid_coordinates(sizeIblocks, n_Reps, iBlocks)
      }
      books <- c(
        lapply(grids, with_rows),
        list(with_rows(data.frame(ROW = z, COLUMN = rep(rep(1:iBlocks, each = sizeIblocks), n_Reps)))),
        if (iBlocks %% 2 == 0) lapply(block_row_coordinates(sizeIblocks, n_Reps, iBlocks), with_rows),
        if (sizeIblocks %% 2 != 0 & sqrt(sizeIblocks) %% 1 != 0) list(with_rows(blocks_in_rows)),
        if (sizeIblocks %% 2 == 0) list(with_rows(blocks_in_rows))
      )
    } else if (stacked == "horizontal") {
      horizontal <- function(coordinates) with_rows(coordinates, order_by = NULL, mode = "Horizontal")
      books <- list(horizontal(data.frame(ROW = rep(rep(1:iBlocks, each = sizeIblocks), n_Reps), COLUMN = z)))
      if (sizeIblocks %% 2 == 0 || sqrt(sizeIblocks) %% 1 == 0) {
        Y <- as.data.frame(factor_subsets(sizeIblocks, all_factors = TRUE)$comb_factors)
        books <- c(books, lapply(seq_len(nrow(Y)), function(h) {
          s1 <- as.numeric(Y[h, ][1])
          s2 <- as.numeric(Y[h, ][2])
          w_r <- 1:(iBlocks * s1)
          u_r <- seq(1, length(w_r), by = s1)
          v_r <- seq(s1, length(w_r), by = s1)
          z_rows <- vector(mode = "list", length = iBlocks * s1)
          for (j in 1:iBlocks) {
            z_rows[[j]] <- rep(u_r[j]:v_r[j], each = s2)
          }
          w_c <- 1:(s2 * n_Reps)
          u_c <- seq(1, length(w_c), by = s2)
          v_c <- seq(s2, length(w_c), by = s2)
          z_cols <- vector(mode = "list", length = n_Reps)
          for (i in 1:n_Reps) {
            z_cols[[i]] <- c(rep(u_c[i]:v_c[i], times = s1 * iBlocks))
          }
          horizontal(data.frame(ROW = rep(unlist(z_rows), times = n_Reps), COLUMN = unlist(z_cols)))
        }))
      }
      books <- c(books, list(horizontal(data.frame(
        ROW = rep(rep(1:sizeIblocks, times = iBlocks), n_Reps),
        COLUMN = rep(1:(iBlocks * n_Reps), each = sizeIblocks)
      ))))
    } else if (stacked == "grid_panel") {
      number_units <- length(levels(as.factor(NewBook$IBLOCK)))
      books <- lapply(panel_grid_coordinates(sizeIblocks, n_Reps, iBlocks), with_rows,
                      order_by = "REP", mode = "Grid", units = number_units)
    }
    lapply(unique(books), layout_columns)
  })
}

#' Layouts of split plots in complete blocks: the plots keep their numbers
#' @noRd
split_layouts <- function(x, n_reps, sizeIblocks, iBlocks, planter, stacked) {
  n_Reps <- n_reps
  lapply(location_books(x), function(NewBook) {
    with_coordinates <- function(coordinates) {
      NewBook |> dplyr::mutate(ROW = coordinates$ROW, COLUMN = coordinates$COLUMN)
    }
    z <- block_panel_rows(sizeIblocks, n_Reps, iBlocks)
    books <- list()
    if (stacked == "vertical") {
      books <- c(
        list(with_coordinates(data.frame(ROW = z, COLUMN = rep(rep(1:iBlocks, each = sizeIblocks), n_Reps)))),
        if (sizeIblocks %% 2 == 0 || sqrt(sizeIblocks) %% 1 == 0) {
          lapply(block_grid_coordinates(sizeIblocks, n_Reps, iBlocks), with_coordinates)
        },
        if (iBlocks %% 2 == 0) {
          lapply(block_row_coordinates(sizeIblocks, n_Reps, iBlocks), with_coordinates)
        }
      )
    } else if (stacked == "horizontal") {
      books <- list(with_coordinates(data.frame(
        ROW = rep(rep(1:iBlocks, each = sizeIblocks), n_Reps), COLUMN = z
      )))
    } else if (stacked == "grid_panel") {
      books <- lapply(panel_grid_coordinates(sizeIblocks, n_Reps, iBlocks), with_coordinates)
    }
    lapply(books, layout_columns)
  })
}

#-----------------------------------------------------------------------
# Latin squares, strip plots and row-column designs: each rep (or square)
# is a rows x columns grid, and the reps are stacked vertically or
# horizontally.
#-----------------------------------------------------------------------

#' @param rsRep,csRep Rows and columns of each rep.
#' @param from_book Whether the row and column of each plot within its rep
#'   come from the field book (row-column designs), or from the order of the
#'   field book, rep by rep and row by row.
#' @return The layout options of each location, with the field coordinates
#'   in NewROW and NewCOLUMNS.
#' @noRd
square_layouts <- function(x, rsRep, csRep, n_Reps, planter, stacked, from_book) {
  lapply(location_books(x), function(NewBook) {
    if (from_book) {
      plots <- NewBook$PLOT
      NewROWS1 <- rep(1:(rsRep * n_Reps), each = csRep)
      NewCOLUMNS1 <- NewBook$COLUMN
      NewROWS2 <- NewBook$ROW
    } else {
      plots <- sort(NewBook$PLOT)
      t <- split_vectors(x = 1:(rsRep * n_Reps), len_cuts = rep(rsRep, n_Reps))
      rows <- list()
      for (k in 1:n_Reps) {
        rows[[k]] <- rep(t[[k]], each = csRep)
      }
      NewROWS1 <- unlist(rows)
      NewCOLUMNS1 <- rep(rep(1:csRep, times = rsRep), times = n_Reps)
      NewROWS2 <- rep(rep(1:rsRep, each = csRep), times = n_Reps)
    }
    if (stacked == "vertical") {
      df1 <- NewBook |>
        dplyr::mutate(NewROW = NewROWS1, NewCOLUMNS = NewCOLUMNS1)
      df1 <- df1[order(df1$NewROW, decreasing = FALSE), ]
      nCols <- max(df1$NewCOLUMNS)
      df1$PLOT <- planter_transform(plots = plots, planter = planter, reps = n_Reps,
                                    cols = nCols, units = csRep)
      return(list(df1))
    }
    if (stacked == "horizontal") {
      w <- 1:(csRep * n_Reps)
      u <- seq(1, length(w), by = csRep)
      v <- seq(csRep, length(w), by = csRep)
      z <- vector(mode = "list", length = n_Reps)
      for (j in 1:n_Reps) {
        z[[j]] <- c(rep(u[j]:v[j], times = rsRep))
      }
      df2 <- NewBook |>
        dplyr::mutate(NewROW = NewROWS2, NewCOLUMNS = unlist(z))
      nCols <- max(df2$NewCOLUMNS)
      df2$PLOT <- planter_transform(plots = plots, planter = planter, reps = n_Reps,
                                    cols = nCols, units = NULL, mode = "horizontal")
      return(list(df2))
    }
    list()
  })
}

#' Name the field coordinates of square layouts ROW and COLUMN
#'
#' @param own Names for the row and column within the rep that the field
#'   book already has, or NULL to drop them.
#' @noRd
square_coordinates <- function(options, own = NULL) {
  lapply(options, function(books) {
    lapply(books, function(book) {
      within <- intersect(c("ROW", "COLUMN"), names(book))
      if (is.null(own)) {
        book <- book[setdiff(names(book), within)]
      } else {
        names(book)[match(within, names(book))] <- own
      }
      names(book)[names(book) == "NewROW"] <- "ROW"
      names(book)[names(book) == "NewCOLUMNS"] <- "COLUMN"
      layout_columns(book)
    })
  })
}

#-----------------------------------------------------------------------
# Choosing a layout
#-----------------------------------------------------------------------

#' Field book with the coordinates of one layout option at every location
#' @noRd
all_locations_layout <- function(x, options, layout) {
  if (identical(options, field_book_layouts(x))) return(x$fieldBook)
  dplyr::bind_rows(lapply(options, `[[`, layout))
}
