#' Select existing spatial simulation data without rounding or reordering it
#' @noRd
spatial_heatmap_data <- function(simulations, response_name, selected = 1L) {
  if (!is.list(simulations) || is.data.frame(simulations) || length(simulations) == 0L) {
    fieldhub_abort("The heatmap needs a list of per-location simulations.")
  }
  if (!is.numeric(selected) || is.complex(selected) || length(selected) != 1L ||
      !is.finite(selected) || !selected %in% seq_along(simulations)) {
    fieldhub_abort("Select one available simulation location for the heatmap.")
  }
  if (!is.character(response_name) || length(response_name) != 1L ||
      is.na(response_name) || !nzchar(response_name)) {
    fieldhub_abort("The heatmap response must have one non-empty name.")
  }
  data <- simulations[[selected]]
  required <- c("ROW", "COLUMN", response_name, "text")
  if (!is.data.frame(data) || nrow(data) == 0L || anyNA(names(data)) ||
      anyDuplicated(names(data)) > 0L || !all(required %in% names(data))) {
    fieldhub_abort("The simulation heatmap needs coordinates, the named response, and tooltips.")
  }
  for (name in unique(required)) {
    value <- data[[name]]
    if (!is.atomic(value) || !is.null(dim(value)) || is.complex(value)) {
      fieldhub_abort("Heatmap column '", name, "' must be an atomic vector.")
    }
    if (name %in% c("ROW", "COLUMN") && anyNA(value)) {
      fieldhub_abort("Heatmap coordinates cannot be missing.")
    }
  }
  if (!is.numeric(data[[response_name]]) || any(is.infinite(data[[response_name]]))) {
    fieldhub_abort("The heatmap response must be numeric, with finite or missing values.")
  }
  if (!is.character(data$text)) fieldhub_abort("Heatmap tooltips must be text.")
  data
}
