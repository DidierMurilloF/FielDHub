#' Construct a reproducible standalone pair-swap result
#' @noRd
new_fieldhub_optimization <- function(x, parameters) {
  new_fieldhub_result(x, "pair_swap", parameters$seed, parameters,
                     c("fieldhub_pair_swap", "fieldhub_optimization"), validate_fieldhub_optimization)
}

#' Validate a pair-swap result without running its optimizer again
#' @noRd
validate_fieldhub_optimization <- function(x) {
  fail <- function(message) {
    fieldhub_abort("Internal error: the optimization result ", message, ".",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  if (!is.list(x) || !identical(class(x), c("fieldhub_pair_swap", "fieldhub_optimization"))) {
    fail("has an inconsistent class")
  }
  meta <- x$metadata
  problems <- fieldhub_metadata_problems(meta)
  if (length(problems)) fail(paste(problems, collapse = ", "))
  if (!identical(meta$design, "pair_swap")) fail("does not name the pair-swap engine")
  parameters <- meta$parameters
  if (!is.list(parameters) || !identical(names(parameters), names(formals(swap_pairs)))) {
    fail("has incomplete input parameters")
  }
  input <- parameters$X
  output <- x$optim_design
  tryCatch({
    validate_swap_matrix(input)
    validate_swap_matrix(output)
    validate_swap_controls(parameters$starting_dist, parameters$stop_iter, parameters$lambda,
                            parameters$dist_method, parameters$candidate_sample_size)
  }, fieldhub_error = function(e) fail(conditionMessage(e)))
  if (!identical(dim(input), dim(output)) || !identical(is.na(input), is.na(output)) ||
      !identical(table(input, dnn = NULL), table(output, dnn = NULL))) {
    fail("does not preserve field geometry and entry counts")
  }
  if (!is.list(x$designs) || !is.list(x$distances) || length(x$designs) == 0L ||
      length(x$designs) != length(x$distances) || !identical(x$designs[[1L]], input) ||
      !identical(x$designs[[length(x$designs)]], output) ||
      !identical(x$distances[[length(x$distances)]], x$pairwise_distance)) {
    fail("has inconsistent retained search steps")
  }
  distances <- x$pairwise_distance
  if (!is.data.frame(distances) || !is.numeric(distances$DIST) || nrow(distances) == 0L ||
      any(!is.finite(distances$DIST)) || any(distances$DIST < 0) ||
      !identical(x$min_distance, min(distances$DIST))) {
    fail("has invalid retained distances")
  }
  diagnostics <- x$diagnostics
  if (!is.list(diagnostics) ||
      !identical(diagnostics$distance_method, parameters$dist_method) ||
      !identical(diagnostics$max_iterations_per_threshold, parameters$stop_iter) ||
      !identical(diagnostics$retained_min_distance, x$min_distance)) {
    fail("has inconsistent optimization diagnostics")
  }
  invisible(x)
}
