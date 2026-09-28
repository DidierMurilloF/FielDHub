#' Check pair-swap labels before their integer representation is used
#' @noRd
validate_swap_matrix <- function(X) {
  if (!is.matrix(X)) fieldhub_abort("Input must be a matrix")
  if (!is.numeric(X) || is.complex(X)) fieldhub_abort("Matrix elements must be numeric")
  active <- X[!is.na(X)]
  if (length(active) == 0L) fieldhub_abort("X must contain at least one active cell")
  if (any(!is.finite(active)) || any(active %% 1 != 0) ||
      any(abs(active) > .Machine$integer.max)) {
    fieldhub_abort("Active matrix entries must be finite whole numbers within R's integer range.")
  }
  invisible(NULL)
}

#' Select a validated coordinate distance function
#' @noRd
swap_distance_function <- function(dist_method) {
  if (!is.character(dist_method) || length(dist_method) != 1L || is.na(dist_method) ||
      !dist_method %in% c("euclidean", "manhattan")) {
    fieldhub_abort("Invalid dist_method. Use 'euclidean' or 'manhattan'.")
  }
  if (dist_method == "euclidean") .vec_dist_euclidean else .vec_dist_manhattan
}

#' Check finite search controls before evaluating a pair-swap distance range
#' @noRd
validate_swap_controls <- function(starting_dist, stop_iter, lambda,
                                    dist_method, candidate_sample_size) {
  controls <- list(starting_dist = starting_dist, stop_iter = stop_iter,
                   lambda = lambda, candidate_sample_size = candidate_sample_size)
  for (name in names(controls)) {
    value <- controls[[name]]
    minimum <- if (name == "candidate_sample_size") 1 else 0
    if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
        length(value) != 1L || !is.finite(value) || value < minimum) {
      fieldhub_abort("`", name, "` must be one finite number at least ", minimum, ".")
    }
    if (name %in% c("stop_iter", "candidate_sample_size")) {
      validate_iteration_budget(value, name, minimum)
    }
  }
  swap_distance_function(dist_method)
  invisible(NULL)
}
