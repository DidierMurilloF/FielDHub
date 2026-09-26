#' Signal an error from a FielDHub function
#'
#' @description The design functions signal their errors as conditions of
#' class \code{fieldhub_error}, with a more specific class first, so that
#' scripts can catch them with \code{tryCatch()} and the app can show them to
#' its users. \code{fieldhub_input_error} is for invalid arguments and
#' \code{fieldhub_dimension_error} for field dimensions that do not fit the
#' entries.
#'
#' @param message The error message.
#' @param class More specific classes of the condition.
#' @param data Named list of fields added to the condition, such as the valid
#'   options.
#' @param call The call reported with the error, by default the call to the
#'   function that signals it.
#'
#' @noRd
fieldhub_abort <- function(message, class = "fieldhub_input_error", data = list(),
                           call = sys.call(-1)) {
  condition <- structure(
    c(list(message = message, call = call), data),
    class = c(class, "fieldhub_error", "error", "condition")
  )
  stop(condition)
}

#' Signal that the field dimensions do not fit the entries
#'
#' @param message Why the dimensions do not fit.
#' @param options Data frame with the valid options, or NULL. It is kept in the
#'   \code{options} field of the condition.
#' @param labels Character vector describing each valid option, listed in the
#'   message.
#' @param no_options Explanation added to the message when there is no valid
#'   option.
#' @param call The call reported with the error.
#'
#' @noRd
stop_dimensions <- function(message, options = NULL, labels = NULL, no_options = NULL,
                            call = sys.call(-1)) {
  if (length(labels) > 0) {
    message <- paste0(message, "\nValid options:\n", paste0("  ", labels, collapse = "\n"))
  } else if (!is.null(no_options)) {
    message <- paste0(message, "\n", no_options)
  }
  fieldhub_abort(message, class = "fieldhub_dimension_error",
                 data = list(options = options), call = call)
}

#' Valid field dimensions as a data frame
#'
#' @param choices Character vector of dimensions such as "10 x 20".
#' @return A data frame with the columns rows and cols, sorted by rows, or NULL
#'   when there is no choice.
#' @noRd
dimension_options <- function(choices) {
  if (is.null(choices) || length(choices) == 0) return(NULL)
  dims <- do.call(rbind, lapply(choices, function(x) {
    parts <- as.integer(trimws(strsplit(x, "x")[[1]]))
    data.frame(rows = parts[1], cols = parts[2])
  }))
  dims <- unique(dims[order(dims$rows), ])
  rownames(dims) <- NULL
  dims
}
