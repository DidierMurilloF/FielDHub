#' Simulate the exact field book displayed by either Shiny workflow
#' @noRd
app_trait_simulation <- function(design, field_book, settings = NULL, seed = NULL,
                                 order_by_id = FALSE, renumber_display = FALSE) {
  validate_flag(order_by_id, "order_by_id")
  validate_flag(renumber_display, "renumber_display")
  if (!is.data.frame(field_book) || nrow(field_book) == 0L) {
    fieldhub_abort("The workflow needs a nonempty field book.")
  }
  simulation <- NULL
  if (!is.null(settings)) {
    original <- design$fieldBook
    if (!identical(original, field_book)) {
      # Layouts can renumber plots or format their columns for display.
      # Carry over the randomization units by the stable location and plot ID.
      key_columns <- c("LOCATION", "ID")
      keys <- as.data.frame(field_book[key_columns])
      source <- as.data.frame(original[key_columns])
      keys$LOCATION <- as.character(keys$LOCATION)
      source$LOCATION <- as.character(source$LOCATION)
      if (anyDuplicated(source)) {
        fieldhub_abort("Plot IDs must be unique within each simulation location.")
      }
      source$.source_row <- seq_len(nrow(source))
      aligned <- dplyr::left_join(keys, source, by = key_columns)$.source_row
      if (length(aligned) != nrow(field_book) || anyNA(aligned) || anyDuplicated(aligned)) {
        fieldhub_abort("The displayed plots do not match the design's field book.")
      }

      design$fieldBook <- original[aligned, , drop = FALSE]
      axes <- intersect(c("ROW", "COLUMN", "ROW_REP", "COLUMN_REP", "ROW_SQ", "COLUMN_SQ"),
                        names(field_book))
      design$fieldBook[axes] <- field_book[axes]
      design$fieldBook$PLOT <- as.numeric(as.character(field_book$PLOT))
    }
    simulation <- do.call(stats::simulate, c(list(object = design, seed = seed), settings), quote = TRUE)
    field_book[[settings$trait]] <- simulation$responses[[1L]]
    if (order_by_id) field_book <- field_book[order(field_book$ID), , drop = FALSE]
  }
  if (renumber_display) field_book$ID <- seq_len(nrow(field_book))
  list(df = field_book, simulation = simulation)
}
