#' Parse a comma-separated list of whole numbers typed in the app
#'
#' @description
#' Strictly parses text inputs such as a starting plot number ("101") or the
#' levels of each factor ("2,2,3"). A value that is not a whole number of 1
#' or more is reported by name, instead of becoming an NA that fails later
#' with a raw R error.
#'
#' @param text The raw text input value.
#' @param label Name of the input, used in the message.
#' @return A list with \code{ok} (logical), \code{value} (a numeric vector
#'   when \code{ok}), and \code{message} (a user-facing string when
#'   \code{!ok}).
#' @noRd
parse_whole_numbers <- function(text, label) {
  if (is.null(text) || length(text) == 0 || !nzchar(trimws(text))) {
    return(list(ok = FALSE, value = NULL,
                message = paste0(label, " cannot be blank.")))
  }
  tokens <- trimws(strsplit(as.character(text), ",")[[1]])
  if (length(tokens) == 0 || any(!nzchar(tokens))) {
    return(list(ok = FALSE, value = NULL,
                message = paste0(label, " has an empty value in \"", text, "\".")))
  }
  vals <- suppressWarnings(as.numeric(tokens))
  bad <- tokens[is.na(vals) | vals %% 1 != 0 | vals < 1]
  if (length(bad) > 0) {
    return(list(ok = FALSE, value = NULL,
                message = paste0(label, " could not read \"",
                                 paste(bad, collapse = "\", \""),
                                 "\" as a whole number of 1 or more.")))
  }
  list(ok = TRUE, value = vals, message = NULL)
}
