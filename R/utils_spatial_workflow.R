#' Prepare spatial workflow data independently of a reactive session
#' @noRd
spatial_workflow_book <- function(field_book, settings = NULL, nrows, ncols,
                                   seed = NULL, renumber_display = FALSE,
                                   coerce_book = FALSE) {
  if (!is.data.frame(field_book) || nrow(field_book) == 0L) {
    fieldhub_abort("The workflow needs a nonempty field book.")
  }
  validate_flag(renumber_display, "renumber_display")
  validate_flag(coerce_book, "coerce_book")
  simulation <- NULL
  if (!is.null(settings)) {
    required <- c("min_value", "max_value", "response_name", "correlation_x", "correlation_y")
    if (!is.list(settings) || !all(required %in% names(settings))) {
      fieldhub_abort("Spatial simulation settings must include bounds, a response name, and both correlations.")
    }
    simulation <- simulate_spatial_field_book(
      field_book = if (coerce_book) as.data.frame(field_book) else field_book,
      nrows = nrows, ncols = ncols,
      correlation_x = as.numeric(settings$correlation_x),
      correlation_y = as.numeric(settings$correlation_y),
      min_value = as.numeric(settings$min_value), max_value = as.numeric(settings$max_value),
      response_name = as.character(settings$response_name), seed = seed
    )
    field_book <- simulation$field_book
  }
  # Display numbering must not replace the source IDs in the simulation record.
  if (renumber_display) field_book$ID <- seq_len(nrow(field_book))
  list(df = field_book, simulation = simulation)
}
