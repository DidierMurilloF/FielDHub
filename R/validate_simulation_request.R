#' Validate a complete simulation submission without changing accepted settings
#'
#' Numeric inputs supply bounds; select inputs may supply correlations as text.
#' Construct all settings before the app updates its single reactive value.
#' @noRd
simulation_request <- function(min_value, max_value, trait, other = NULL,
                                 field_columns = character(), correlations = NULL) {
  finite_scalar <- function(x) {
    is.numeric(x) && !is.complex(x) && is.null(dim(x)) &&
      length(x) == 1L && is.finite(x)
  }
  if (!finite_scalar(min_value) || !finite_scalar(max_value) || min_value >= max_value ||
      !is.finite(max_value - min_value) || !is.finite(max_value + min_value)) {
    fieldhub_abort("Enter finite minimum and maximum values, with minimum < maximum.")
  }
  if (!is.character(trait) || !is.null(dim(trait)) || length(trait) != 1L ||
      is.na(trait) || !trait %in% c("YIELD", "MOISTURE", "HEIGHT", "Other")) {
    fieldhub_abort("Select a response trait, or choose Other to enter its name.")
  }
  response <- if (trait == "Other") other else trait
  if (!is.character(response) || !is.null(dim(response)) || length(response) != 1L ||
      is.na(response) || !nzchar(trimws(response))) {
    fieldhub_abort("Enter one non-empty trait name.")
  }
  if (!is.character(field_columns) || !is.null(dim(field_columns)) || anyNA(field_columns)) {
    fieldhub_abort("The field-book column names must be a complete character vector.")
  }
  reserved <- c(field_columns, "text", if (!is.null(correlations)) c("ZST", "genot"))
  if (response %in% reserved) {
    fieldhub_abort("The trait name '", response, "' is already present or reserved.")
  }
  settings <- list(min_value = as.numeric(min_value), max_value = as.numeric(max_value),
                   response_name = response)
  if (!is.null(correlations)) {
    if ((!is.numeric(correlations) && !is.character(correlations)) ||
        is.complex(correlations) || !is.null(dim(correlations)) ||
        !identical(names(correlations), c("x", "y"))) {
      fieldhub_abort("Supply both named spatial correlations, x and y.")
    }
    values <- suppressWarnings(as.numeric(correlations))
    if (anyNA(values) || any(!is.finite(values)) || any(abs(values) >= 1)) {
      fieldhub_abort("Each spatial correlation must be a finite number between -1 and 1.")
    }
    if (abs(values[1L] - values[2L]) >= 0.85) {
      fieldhub_abort("The two spatial correlations must differ by less than 0.85.")
    }
    settings$correlation_x <- values[1L]
    settings$correlation_y <- values[2L]
  }
  settings
}
