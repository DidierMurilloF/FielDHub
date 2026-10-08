#' Validate a rectangular AR1 by AR1 field before allocating or drawing
#' @noRd
check_spatial_grid <- function(nrows, ncols, correlation_x, correlation_y) {
  for (value in list(nrows, ncols)) {
    if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) || length(value) != 1L ||
        !is.finite(value) || value < 1 || value %% 1 != 0) {
      fieldhub_abort("Spatial field dimensions must be positive whole numbers.")
    }
  }
  size <- as.double(nrows) * as.double(ncols)
  if (!is.finite(size) || size > .Machine$integer.max) {
    fieldhub_abort("The spatial field exceeds the supported number of plots.")
  }
  for (value in list(correlation_x, correlation_y)) {
    if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) || length(value) != 1L ||
        !is.finite(value) || abs(value) >= 1) {
      fieldhub_abort("Spatial correlations must be finite numbers strictly between -1 and 1.")
    }
  }
  size
}

#' Apply a separable AR1 covariance square root in linear space and time
#'
#' For a stationary AR1 series, z[1] = e[1] and
#' z[j] = rho * z[j - 1] + sqrt(1 - rho^2) * e[j]. Applying this recurrence
#' along columns and then rows is Lx E t(Ly), whose row-major vector has
#' covariance Ry %x% Rx. It is the same square root as the dense Cholesky
#' factor, up to floating-point rounding, without constructing either matrix.
#' Innovations and output use the field's row-major plot order.
#' @noRd
separable_ar1_patch <- function(innovations, nrows, ncols, correlation_x, correlation_y) {
  size <- check_spatial_grid(nrows, ncols, correlation_x, correlation_y)
  if (!is.numeric(innovations) || is.complex(innovations) ||
      !is.null(dim(innovations)) || length(innovations) != size ||
      any(!is.finite(innovations))) {
    fieldhub_abort("Spatial innovations must be one finite numeric value per plot.")
  }
  # R fills matrices column-first; the first index here is the field column.
  patch <- matrix(as.numeric(innovations), nrow = ncols, ncol = nrows)
  scale_x <- sqrt(1 - correlation_x^2)
  scale_y <- sqrt(1 - correlation_y^2)
  if (ncols > 1L) {
    for (column in seq.int(2L, ncols)) {
      patch[column, ] <- correlation_x * patch[column - 1L, ] + scale_x * patch[column, ]
    }
  }
  if (nrows > 1L) {
    for (row in seq.int(2L, nrows)) {
      patch[, row] <- correlation_y * patch[, row - 1L] + scale_y * patch[, row]
    }
  }
  as.vector(patch)
}
