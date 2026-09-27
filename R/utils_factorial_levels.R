#' Check uniqueness of complete factor-level pairs
#'
#' A level label may be reused in different factors. Incomplete rows are
#' omitted by both the app and full_factorial() before generating a design.
#' @noRd
factorial_levels_unique <- function(data) {
  entries <- data[, 1:2, drop = FALSE]
  entries <- entries[stats::complete.cases(entries), , drop = FALSE]
  anyDuplicated(entries) == 0L
}
