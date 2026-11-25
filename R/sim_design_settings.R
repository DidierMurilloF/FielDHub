#' Validate trait simulation scales and spatial coordinates
#' @noRd
simulation_number <- function(value, name, nonnegative = FALSE) {
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) || !is.null(names(value)) ||
      length(value) != 1L || !is.finite(value) || (nonnegative && value < 0)) {
    fieldhub_abort("'", name, "' must be one finite ",
                   if (nonnegative) "nonnegative " else "", "number.")
  }
  invisible(value)
}

simulation_location_values <- function(value, locations, name, nonnegative = FALSE) {
  if (is.null(names(value))) {
    simulation_number(value, name, nonnegative)
    return(rep(value, length(locations)))
  }
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
      any(!is.finite(value)) || (nonnegative && any(value < 0)) ||
      anyDuplicated(names(value)) || !setequal(names(value), locations)) {
    fieldhub_abort("'", name, "' must give one valid value named for each location.")
  }
  unname(value[locations])
}

simulation_design_sd <- function(value, terms, default) {
  if (is.null(value)) value <- default
  if (is.null(names(value))) {
    simulation_number(value, "design_sd", nonnegative = TRUE)
    return(stats::setNames(rep(value, length(terms)), terms))
  }
  if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
      any(!is.finite(value) | value < 0) || anyDuplicated(names(value)) ||
      !all(names(value) %in% terms)) {
    fieldhub_abort("'design_sd' must give nonnegative values named for design effects.",
                   if (length(terms)) paste0(" Available effects: ", paste(terms, collapse = ", "), "."))
  }
  result <- stats::setNames(rep(default, length(terms)), terms)
  result[names(value)] <- value
  result
}

simulation_spatial_spec <- function(spatial, default_sd) {
  if (identical(spatial, FALSE)) return(FALSE)
  defaults <- list(sd = default_sd, rho_row = 0.5, rho_col = 0.5)
  if (identical(spatial, TRUE)) return(defaults)
  if (!is.list(spatial) || is.data.frame(spatial) || is.null(names(spatial)) ||
      anyDuplicated(names(spatial)) || !all(names(spatial) %in% names(defaults))) {
    fieldhub_abort("'spatial' must be TRUE, FALSE, or a list with sd, rho_row and rho_col.")
  }
  defaults[names(spatial)] <- spatial
  simulation_number(defaults$sd, "spatial$sd", nonnegative = TRUE)
  check_spatial_grid(1, 1, defaults$rho_col, defaults$rho_row)
  defaults
}

check_simulation_axes <- function(design) {
  design <- with_design_class(design)
  experimental_axes <- if (inherits(design, "fieldhub_latin_square")) {
    c("ROW_SQ", "COLUMN_SQ")
  } else if (inherits(design, "fieldhub_row_column")) {
    c("ROW_REP", "COLUMN_REP")
  }
  if (!all(experimental_axes %in% names(design$fieldBook))) {
    fieldhub_abort("Spatial simulation of this design needs its physical layout. Run ",
                   "`x$fieldBook <- field_layout(x)` first, then `simulate(x, spatial = TRUE)`; ",
                   "the layout keeps the experimental axes as ",
                   paste(experimental_axes, collapse = " and "), ".")
  }
}

simulation_spatial_patches <- function(book, site, spatial) {
  axes <- c("ROW", "COLUMN")
  if (!all(axes %in% names(book))) {
    fieldhub_abort("Spatial simulation requires physical ROW and COLUMN in the field book.")
  }
  coords <- lapply(book[axes], function(axis) {
    if (is.factor(axis) || is.character(axis)) axis <- suppressWarnings(as.numeric(as.character(axis)))
    if (!is.numeric(axis) || is.complex(axis) || !is.null(dim(axis)) ||
        any(!is.finite(axis) | axis < 1 | axis %% 1 != 0)) {
      fieldhub_abort("Spatial ROW and COLUMN must contain positive whole-number coordinates.")
    }
    as.double(axis)
  })
  lapply(split(seq_len(nrow(book)), site), function(rows) {
    row <- coords$ROW[rows] - min(coords$ROW[rows]) + 1
    col <- coords$COLUMN[rows] - min(coords$COLUMN[rows]) + 1
    nrows <- max(row)
    ncols <- max(col)
    if (anyDuplicated(data.frame(row, col))) {
      fieldhub_abort("Spatial simulation requires unique physical ROW and COLUMN pairs within each location.")
    }
    size <- nrows * ncols
    if (size <= 4 * length(rows) && size <= .Machine$integer.max) {
      return(list(rows = rows, index = (row - 1) * ncols + col, size = size,
                  nrows = nrows, ncols = ncols))
    }
    # Retain distances but skip unobserved axes when the bounding rectangle
    # would allocate many empty cells, for example when plots span alleys.
    field_rows <- sort(unique(row))
    field_cols <- sort(unique(col))
    list(rows = rows, observed = split(seq_along(rows), match(row, field_rows)),
         row_rho = spatial$rho_row ^ diff(field_rows),
         col_rho = spatial$rho_col ^ diff(field_cols), column = match(col, field_cols),
         ncols = length(field_cols))
  })
}

#' Draw a spatial patch on the observed coordinates
#' @noRd
simulation_spatial_draw <- function(patch, spatial, draw_normal = stats::rnorm) {
  if (!is.null(patch$size)) {
    surface <- separable_ar1_patch(draw_normal(patch$size, sd = spatial$sd),
      patch$nrows, patch$ncols, spatial$rho_col, spatial$rho_row)
    return(surface[patch$index])
  }
  draw <- numeric(length(patch$rows))
  row <- numeric(patch$ncols)
  column_sd <- sqrt(1 - patch$col_rho^2)
  for (i in seq_along(patch$observed)) {
    innovation <- draw_normal(patch$ncols)
    for (j in seq_along(patch$col_rho)) {
      innovation[j + 1L] <- patch$col_rho[j] * innovation[j] + column_sd[j] * innovation[j + 1L]
    }
    rho <- if (i == 1L) 0 else patch$row_rho[i - 1L]
    row <- rho * row + sqrt(1 - rho^2) * innovation
    plots <- patch$observed[[i]]
    draw[plots] <- spatial$sd * row[patch$column[plots]]
  }
  draw
}
