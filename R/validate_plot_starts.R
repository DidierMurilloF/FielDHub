#' Validate plot-start values without changing numbering or fallback policies
#'
#' Call at the existing argument evaluation point, before arithmetic or
#' coercion. Length, positivity, sorting, and NULL defaults remain the calling
#' engine's responsibility. Empty numeric vectors retain length-based fallbacks.
#' @noRd
validate_plot_starts <- function(values) {
  if (!is.numeric(values) || !is.null(dim(values)) ||
      any(!is.finite(values)) || any(values %% 1 != 0)) {
    fieldhub_abort("`plotNumber` must be a numeric vector of finite whole numbers.",
                   data = list(argument = "plotNumber"))
  }
  invisible(values)
}
