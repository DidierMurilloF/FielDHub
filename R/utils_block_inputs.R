#' Validate shared incomplete-block inputs before randomization or allocation
#'
#' Retain supported count/label representations and supplied-table processing.
#' Numeric vectors retain their established generated-label behavior; the
#' supplied-data path requires a scalar count matching the processed table.
#' @noRd
validate_block_design_inputs <- function(t, k, reps, l, data) {
  # Keep valid lazy argument expressions in their historical RNG scope and
  # evaluation order: supplied data, treatments, then block size. Replication
  # and location have already been resolved by the calling engine.
  force(data)
  force(t)
  force(k)
  validate_iteration_budget(k, "k", minimum = 2)
  validate_iteration_budget(reps, "reps")
  if (!is.null(data)) {
    validate_iteration_budget(t, "t", minimum = 2)
    if (!is.data.frame(data) || ncol(data) < 2L) {
      fieldhub_abort("Entry data must be a data frame with at least two columns: ENTRY and TREATMENT.")
    }
    treatments <- t
  } else {
    if ((!is.numeric(t) && !is.character(t) && !is.factor(t)) || is.complex(t) ||
        !is.null(dim(t)) || length(t) == 0L || anyNA(t) ||
        (is.numeric(t) && any(!is.finite(t)))) {
      fieldhub_abort("`t` must be a finite treatment count or a complete vector of treatment labels.",
                     data = list(argument = "t"))
    }
    if (is.numeric(t) && length(t) == 1L) {
      validate_iteration_budget(t, "t", minimum = 2)
      treatments <- t
    } else {
      if (length(t) < 2L || any(!nzchar(trimws(as.character(t))))) {
        fieldhub_abort("Supply at least two nonmissing, nonblank treatment labels.",
                       data = list(argument = "t"))
      }
      treatments <- length(t)
    }
  }
  size <- as.double(treatments) * as.double(reps) * as.double(l)
  if (!is.finite(size) || size > .Machine$integer.max) {
    fieldhub_abort("The block design must contain at most ", .Machine$integer.max, " experimental units.")
  }
  invisible(treatments)
}
