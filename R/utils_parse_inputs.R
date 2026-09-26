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

#' Parse the "Input # of Checks" numeric input
#'
#' @description
#' A single, shared validator for `n_checks_rcbd` so the app never hands a
#' negative or fractional count to `seq_len()`, `rep()`, or
#' `rcbd_resolve_entries()`.
#'
#' @param x The raw `n_checks_rcbd` input value.
#' @return A list with `ok` (logical), `value` (an integer when `ok`), and
#'   `message` (a user-facing string when `!ok`).
#' @noRd
parse_n_checks <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(list(ok = FALSE, value = NULL,
                message = "Input # of Checks cannot be blank."))
  }
  if (x %% 1 != 0 || x < 1) {
    return(list(ok = FALSE, value = NULL,
                message = "Input # of Checks must be a whole number of 1 or more."))
  }
  list(ok = TRUE, value = as.integer(x), message = NULL)
}

#' Parse the "Reps per Check" text input
#'
#' @description
#' Strictly parses a comma-separated list of reps-per-check. A single value is
#' legal and is recycled across `n_checks`; any other length must match
#' `n_checks` exactly. A token that does not parse as a number is reported by
#' name rather than silently dropped. Shared by the RCBD input parser and its
#' block-size preview so both give the same answer for the same text.
#'
#' @param text The raw `rep_checks_rcbd` input value.
#' @param n_checks Number of checks the value must ultimately cover.
#' @return A list with `ok` (logical), `value` (a numeric vector of length
#'   `n_checks` when `ok`), and `message` (a user-facing string when `!ok`).
#' @noRd
parse_rep_checks <- function(text, n_checks) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(ok = FALSE, value = NULL,
                message = "Reps per Check cannot be blank."))
  }
  tokens <- trimws(strsplit(text, ",")[[1]])
  if (length(tokens) == 0 || any(!nzchar(tokens))) {
    return(list(ok = FALSE, value = NULL,
                message = paste0(
                  "Reps per Check has an empty value in \"", text, "\". ",
                  "Use a single number, or one comma-separated number per check.")))
  }
  vals <- suppressWarnings(as.numeric(tokens))
  bad <- tokens[is.na(vals)]
  if (length(bad) > 0) {
    return(list(ok = FALSE, value = NULL,
                message = paste0(
                  "Reps per Check could not read \"",
                  paste(bad, collapse = "\", \""), "\" as a number.")))
  }
  if (length(vals) == 1) {
    return(list(ok = TRUE, value = rep(vals, n_checks), message = NULL))
  }
  if (length(vals) != n_checks) {
    return(list(ok = FALSE, value = NULL,
                message = sprintf(
                  "Reps per Check must have 1 value or %d values (one per check); got %d.",
                  n_checks, length(vals))))
  }
  list(ok = TRUE, value = vals, message = NULL)
}
