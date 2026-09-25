#' Treatment labels for designs that take `t` as a count or a vector
#'
#' @param t Number of treatments, or a vector with the treatment labels.
#' @param nt Number of treatments.
#' @param fun Name of the calling function, used in the error message.
#'
#' @return A character vector of length \code{nt}: the labels in \code{t}
#'   when it is a character or factor vector, otherwise
#'   \code{"G-1", ..., "G-nt"}.
#'
#' @noRd
treatment_labels <- function(t, nt, fun) {
  if (!is.character(t) && !is.factor(t)) return(paste0("G-", 1:nt))
  labels <- as.character(t)
  check_unique_labels(labels, fun)
  labels
}

#' Stop when treatment labels are duplicated
#'
#' @param labels Character vector with the treatment labels.
#' @param fun Name of the calling function, used in the error message.
#'
#' @noRd
check_unique_labels <- function(labels, fun) {
  if (anyDuplicated(labels) > 0) {
    stop(fun, "() requires unique treatment labels; duplicated: ",
         paste(unique(labels[duplicated(labels)]), collapse = ", "),
         call. = FALSE)
  }
  invisible(labels)
}
