#' Resolve count or label inputs before expanding factorial units
#'
#' Preserve scalar count storage and the original label vectors. Prefixes are
#' used only for generated strip labels; split factors retain integer levels.
#' @noRd
resolve_design_factors <- function(values, reps, l, prefixes = NULL) {
  counts <- lapply(names(values), function(argument) {
    value <- values[[argument]]
    if (is.numeric(value) && length(value) == 1L) {
      validate_iteration_budget(value, argument)
      return(value)
    }
    if (length(value) < 2L) {
      fieldhub_abort("`", argument, "` requires a positive count or at least two level labels.")
    }
    validate_entry_labels(value, argument)
    if (is.numeric(value) && any(!is.finite(value))) {
      fieldhub_abort("`", argument, "` requires finite level labels.")
    }
    check_unique_labels(value, argument)
    length(value)
  })
  names(counts) <- names(values)
  validate_design_size(c(unlist(counts, use.names = FALSE), reps, l))
  result <- lapply(names(values), function(argument) {
    value <- values[[argument]]
    count <- counts[[argument]]
    levels <- if (is.numeric(value) && length(value) == 1L) {
      if (is.null(prefixes)) seq_len(value) else paste0(prefixes[[argument]], seq_len(value) - 1L)
    } else value
    list(levels = levels, count = count)
  })
  stats::setNames(result, names(values))
}

#' Validate the generated full-factorial count vector before expand.grid()
#' @noRd
validate_factor_counts <- function(counts, reps, l) {
  if (!is.numeric(counts) || !is.null(dim(counts)) || length(counts) < 2L ||
      length(counts) > length(LETTERS)) {
    fieldhub_abort("Generated factorial designs require a numeric vector of 2 to 26 factor counts.")
  }
  for (count in counts) validate_iteration_budget(count, "setfactors")
  validate_iteration_budget(reps, "reps")
  validate_design_size(c(counts, reps, l))
  invisible(counts)
}
