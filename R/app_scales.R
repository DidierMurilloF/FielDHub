#' Continuous viridis scale used by the app heatmaps
#'
#' This keeps the 256-colour interpolation used by
#' `viridis::scale_fill_viridis(discrete = FALSE)` without depending on the
#' full `viridis` package.
#'
#' @return A continuous ggplot2 fill scale.
#' @noRd
fieldhub_viridis_scale <- function() {
  ggplot2::scale_fill_gradientn(
    colours = viridisLite::viridis(256),
    aesthetics = "fill"
  )
}
