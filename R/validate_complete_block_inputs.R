#' Validate field-book size before generating labels or allocating matrices
#' @noRd
validate_design_size <- function(counts) {
  if (!is.numeric(counts) || !is.null(dim(counts)) || length(counts) == 0L ||
      any(!is.finite(counts)) || any(counts < 1 | counts != trunc(counts))) {
    fieldhub_abort("Design sizes must be positive finite whole numbers.")
  }
  size <- prod(as.double(counts))
  if (!is.finite(size) || size > .Machine$integer.max) {
    fieldhub_abort("The design must contain at most ", .Machine$integer.max, " experimental units.")
  }
  invisible(size)
}

#' Validate complete, nonblank entry labels without changing their values
#' @noRd
validate_entry_labels <- function(labels, argument) {
  if (!is.atomic(labels) || is.complex(labels) || !is.null(dim(labels)) ||
      length(labels) == 0L || anyNA(labels) ||
      any(!nzchar(trimws(as.character(labels))))) {
    fieldhub_abort("`", argument, "` requires nonmissing, nonblank entry labels.",
                   data = list(argument = argument))
  }
  invisible(labels)
}

#' Validate the established RCBD treatment count or character-label forms
#' @noRd
validate_rcbd_treatments <- function(t, reps, l) {
  if (is.numeric(t)) {
    validate_iteration_budget(t, "t", minimum = 2)
    count <- t
  } else if (is.character(t) && length(t) > 1L) {
    validate_entry_labels(t, "t")
    count <- length(t)
  } else {
    fieldhub_abort("RCBD() requires more than one treatment, supplied as a count or a character vector of labels.")
  }
  validate_design_size(c(count, reps, l))
  invisible(t)
}
