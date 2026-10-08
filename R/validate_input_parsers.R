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
#' @return A numeric vector, or a \code{fieldhub_input_error} for invalid
#'   input.
#' @noRd
parse_whole_numbers <- function(text, label) {
  if (length(text) == 0L) fieldhub_abort(label, " cannot be blank.")
  if (!(is.character(text) || is.numeric(text)) || length(text) != 1L) {
    fieldhub_abort(label, " must be one comma-separated text value.")
  }
  if (is.na(text) || !nzchar(trimws(as.character(text)))) {
    fieldhub_abort(label, " cannot be blank.")
  }
  text <- as.character(text)
  tokens <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  # strsplit() drops a trailing empty token; do not silently accept it.
  if (length(tokens) == 0L || any(!nzchar(tokens)) || grepl(",[[:space:]]*$", text)) {
    fieldhub_abort(label, " has an empty value in \"", text, "\".")
  }
  vals <- suppressWarnings(as.numeric(tokens))
  bad <- tokens[!is.finite(vals) | vals != trunc(vals) | vals < 1]
  if (length(bad) > 0) {
    fieldhub_abort(label, " could not read \"", paste(bad, collapse = "\", \""),
                   "\" as a whole number of 1 or more.")
  }
  vals
}

#' Parse the "Input # of Checks" numeric input
#'
#' @description
#' A single, shared validator for the checks-count inputs so the app never
#' hands a blank, negative or fractional count to \code{seq_len()},
#' \code{rep()}, or \code{rcbd_resolve_entries()}.
#'
#' @param x The raw checks-count input value.
#' @return An integer, or a \code{fieldhub_input_error} for invalid input.
#' @noRd
parse_n_checks <- function(x) {
  if (length(x) == 0L || (is.atomic(x) && length(x) == 1L && anyNA(x))) {
    fieldhub_abort("Input # of Checks cannot be blank.")
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x > .Machine$integer.max || x != trunc(x)) {
    fieldhub_abort("Input # of Checks must be a whole number from 1 to ",
                   .Machine$integer.max, ".")
  }
  as.integer(x)
}

#' Parse the "Reps per Check" text input
#'
#' @description
#' Strictly parses a comma-separated list of reps-per-check. A single value is
#' legal and is recycled across `n_checks`; any other length must match
#' `n_checks` exactly. Values must be finite positive whole numbers. Invalid
#' tokens are reported by name rather than silently dropped. Shared by the RCBD input parser and its
#' block-size preview so both give the same answer for the same text.
#'
#' @param text The raw `rep_checks_rcbd` input value.
#' @param n_checks Number of checks the value must ultimately cover.
#' @return A numeric vector of length `n_checks`, or a
#'   \code{fieldhub_input_error} for invalid input.
#' @noRd
parse_rep_checks <- function(text, n_checks) {
  n_checks <- parse_n_checks(n_checks)
  vals <- parse_whole_numbers(text, "Reps per Check")
  if (length(vals) == 1) return(rep(vals, n_checks))
  if (length(vals) != n_checks) {
    fieldhub_abort(sprintf(
      "Reps per Check must have 1 value or %d values (one per check); got %d.",
      n_checks, length(vals)))
  }
  vals
}

#' Parse the p-rep "entries per rep group" and "reps per group" inputs
#'
#' @description
#' Both inputs are comma-separated whole numbers of 1 or more, one value per
#' replication group, so they must have the same length. Text such as
#' "75,abc" is reported by name instead of reaching the design as an NA.
#'
#' @param entries The raw "# of Entries Per Rep Group" input.
#' @param reps The raw "# of Rep Per Group" input.
#' @return A list with \code{repGens}, \code{repUnits} and
#'   \code{total_plots}, or a \code{fieldhub_input_error}.
#' @noRd
parse_rep_groups <- function(entries, reps) {
  rep_gens <- parse_whole_numbers(entries, "# of Entries Per Rep Group")
  rep_units <- parse_whole_numbers(reps, "# of Rep Per Group")
  if (length(rep_gens) != length(rep_units)) {
    fieldhub_abort("# of Entries Per Rep Group and # of Rep Per Group must have the same ",
                   "number of values (one per group); got ", length(rep_gens), " and ",
                   length(rep_units), ".")
  }
  list(repGens = rep_gens, repUnits = rep_units, total_plots = sum(rep_gens * rep_units))
}

#' Read an optional app seed without drawing from the caller's RNG stream
#'
#' A cleared Shiny \code{numericInput} is delivered as logical \code{NA}
#' (\code{shiny:::inputHandlers$get("shiny.number")(NULL)}), which counts as
#' blank the same way as \code{NULL}, an empty vector, a numeric/character
#' \code{NA}, or an empty/whitespace string. Any other non-scalar-number
#' value is a classed input error.
#' @noRd
read_app_seed <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.logical(value) && length(value) == 1L && is.na(value)) return(NULL)
  if ((!is.numeric(value) && !is.character(value)) || !is.null(dim(value))) {
    fieldhub_abort("The random seed must be one number, or blank for an automatic seed.")
  }
  if (length(value) == 0L) return(NULL)
  if (length(value) != 1L) {
    fieldhub_abort("The random seed must be one number, or blank for an automatic seed.")
  }
  if (is.numeric(value) && is.na(value) && !is.nan(value)) return(NULL)
  if (is.character(value) && !is.na(value) && !nzchar(trimws(value))) return(NULL)
  resolve_seed(suppressWarnings(as.numeric(value)))
}
