#' Shape an uploaded entry list for a classic design
#'
#' @description Keeps the first \code{length(columns)} columns of the file
#' (\code{read_design_upload()} has already checked there are enough), names
#' them \code{columns}, and drops incomplete rows unless the design pairs
#' columns of different lengths (split-plot and strip-plot levels).
#'
#' @param data The data frame \code{read_design_upload()} returned.
#' @param columns Names the design gives the columns it reads.
#' @param omit_na Whether to drop rows with a missing value.
#' @return A data frame.
#' @noRd
shape_design_upload <- function(data, columns, omit_na = TRUE) {
  if (!is.data.frame(data) || ncol(data) < length(columns)) {
    fieldhub_abort("The uploaded file needs at least ", length(columns), " column",
                   if (length(columns) > 1L) "s", ": ", paste(columns, collapse = ", "), ".")
  }
  shaped <- as.data.frame(data[, seq_along(columns), drop = FALSE])
  if (omit_na) shaped <- stats::na.omit(shaped)
  colnames(shaped) <- columns
  shaped
}

#' Check an uploaded factorial list names at least two factors
#' @param data A shaped FACTOR/LEVEL data frame.
#' @return \code{data}, or a classed input error.
#' @noRd
check_factorial_upload <- function(data) {
  if (length(unique(data$FACTOR)) < 2L) {
    fieldhub_abort("More than one factor needs to be specified.")
  }
  data
}

#' The upload a design page runs with
#'
#' @description A failed upload has already been reported when it was read
#' (\code{app_read_upload()}); running then says why nothing happens.
#' @param data The shaped upload, or \code{NULL} when reading it failed.
#' @return \code{data}, or a classed input error.
#' @noRd
design_upload_data <- function(data) {
  if (is.null(data)) fieldhub_abort("Check the input file and try again.")
  data
}

#' Number of levels in each column of a paired upload (strip plots)
#' @param data A shaped data frame.
#' @return A numeric vector, one count of non-missing values per column.
#' @noRd
upload_level_counts <- function(data) {
  vapply(data, function(column) as.numeric(length(stats::na.omit(column))), numeric(1),
         USE.NAMES = FALSE)
}
