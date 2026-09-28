#' Prepare the shared classic workflow without a reactive session
#' @noRd
classic_workflow_book <- function(field_book, settings = NULL, seed = NULL,
                                   order_by_id = FALSE) {
  if (!is.data.frame(field_book) || nrow(field_book) == 0L) {
    fieldhub_abort("The workflow needs a nonempty field book.")
  }
  if (is.null(settings)) return(list(df = field_book, simulation = NULL))
  if (!is.list(settings) ||
      !all(c("min_value", "max_value", "response_name") %in% names(settings))) {
    fieldhub_abort("Simulation settings must include minimum, maximum, and response name.")
  }
  simulation <- simulate_classic_field_book(
    field_book, min_value = settings$min_value, max_value = settings$max_value,
    response_name = settings$response_name, seed = seed, order_by_id = order_by_id
  )
  list(df = simulation$field_book, simulation = simulation)
}

#' Export the selected classic layout using the same labels as its map
#' @noRd
classic_workflow_layout <- function(field_book, selected, plot_type, spec) {
  if ((!is.character(plot_type) && !is.numeric(plot_type)) ||
      !is.null(dim(plot_type)) || length(plot_type) != 1L || is.na(plot_type) ||
      !as.character(plot_type) %in% c("1", "2", "3")) {
    fieldhub_abort("Select entries, plot numbers, or a heatmap before exporting the layout.")
  }
  export_layout(field_book, selected, plotOn = as.character(plot_type) == "2",
                 type_pref = spec$export_label)
}
