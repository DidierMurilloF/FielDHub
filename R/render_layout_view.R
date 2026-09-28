#' Strict layout view for application consumers
#'
#' Unlike the legacy plot_layout() adapter, unavailable selections are classed
#' errors, not warning-and-NULL results that leave an unexplained blank panel.
#' Prepare coordinates once and share the same drawing/output constructor.
#' @noRd
checked_layout_view <- function(x, layout = 1, planter = "serpentine",
                                  location = 1, stacked = "vertical", ...) {
  if (!inherits(x, "FielDHub")) fieldhub_abort("'x' must be a design created by FielDHub.")
  x <- with_design_class(x)
  check_layout_arguments(planter, stacked)
  if (!is.numeric(location) || is.complex(location) || !is.null(dim(location)) ||
      length(location) != 1L || !is.finite(location) || location < 1 || location %% 1 != 0) {
    fieldhub_abort("Select one available location for the layout.")
  }
  options <- layout_options(x, planter = planter, stacked = stacked)
  if (location > length(options)) {
    fieldhub_abort("Location ", location, " is not available for this design.",
                   data = list(options = seq_along(options)))
  }
  available <- seq_along(options[[location]])
  if (!length(available)) {
    fieldhub_abort("Stacking '", stacked, "' is not available for this design.")
  }
  if (!is.numeric(layout) || is.complex(layout) || !is.null(dim(layout)) ||
      length(layout) != 1L || !is.finite(layout) || !layout %in% available) {
    fieldhub_abort("Select an available layout option: ", paste(available, collapse = ", "), ".",
                   data = list(options = available))
  }
  tryCatch(
    render_layout_view(x, options, layout, planter, location, stacked, ...),
    error = function(e) fieldhub_abort("Unable to draw the selected layout: ", conditionMessage(e),
                                      class = "fieldhub_render_error", data = list(parent = e))
  )
}

#' Draw prepared coordinates and retain the established layout-result fields
#' @noRd
render_layout_view <- function(x, options, layout, planter, location, stacked, ...) {
  site_options <- options[[location]]
  drawn <- draw_layout(x, site_options[[layout]], ...)
  list(out_layout = drawn$p1,
       out_layoutPlots = drawn$p2,
       fieldBookXY = drawn$data,
       newBooks = site_options,
       allSitesFieldbook = all_locations_layout(x, options, layout),
       layout_metadata = list(parameters = list(layout = layout, planter = planter, stacked = stacked),
                              selected = location))
}
