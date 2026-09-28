#' @title Swap pairs in a matrix of integers
#'
#' @description Attempts to separate repeated entries by swapping cells of
#' different entries. Distance thresholds increase from \code{starting_dist}
#' in steps of one. Candidate sampling, mean pairwise distance, and a centrality
#' penalty guide the search. The last successful threshold layout is returned;
#' if none succeeds, the original matrix is returned. A requested minimum
#' distance is not guaranteed.
#'
#' @param X A numeric matrix of whole-number entry identifiers within R's
#' integer range. Missing cells are inactive positions and never move.
#' @param starting_dist First distance threshold; a finite nonnegative number.
#' Default is 3.
#' @param stop_iter Maximum complete swap sweeps per threshold, as a
#' nonnegative whole number within R's integer range. Default is 10.
#' @param lambda Finite nonnegative weight for the centrality penalty.
#' Default is 0.5.
#' @param dist_method Coordinate distance used for candidate filtering,
#' scoring, stopping, and reported distances: "euclidean" (default) or "manhattan".
#' @param candidate_sample_size Maximum candidates evaluated per swap, as a
#' positive whole number within R's integer range. Default is 4.
#' @param seed Optional randomization seed. When omitted, one integer is
#' drawn from the current random-number stream and recorded; the swap's own
#' randomization does not change the caller's stream.
#'
#' @details The finite threshold range is bounded by the field geometry. For
#' Euclidean distance without missing cells, the historical bound is
#' \code{sqrt(nrow(X)^2 + ncol(X)^2)}; with missing cells it is the maximum
#' distance between active positions. For Manhattan distance it is the maximum
#' Manhattan distance between active positions. There are at most
#' \code{floor(bound - starting_dist) + 1} thresholds when the bound is at least
#' \code{starting_dist}, and none otherwise. Each threshold permits at most
#' \code{stop_iter} complete sweeps. Diagnostics distinguish an exhausted
#' threshold budget from completing or skipping the distance range.
#'
#' Standalone calls use the shared seed contract. To reproduce a previous
#' \code{set.seed(s); swap_pairs(X)} call, with \code{X} already constructed,
#' pass \code{seed = s}. Calls without a seed now select an automatic seed,
#' so their layouts can differ from previous versions. Optimization performed
#' inside field-design engines retains its existing draw sequence.
#' Use \code{reproduce_design()} to replay a recorded optimization result.
#'
#' @return A list containing:
#' \item{optim_design}{The modified matrix.}
#' \item{designs}{A list of all intermediate designs, starting from the input matrix.}
#' \item{distances}{A list of all pair distances for each intermediate design.}
#' \item{min_distance}{The minimum distance between pairs of occurrences of the same integer in the final design.}
#' \item{pairwise_distance}{A data frame with the pairwise distances for the final design.}
#' \item{rows_incidence}{Row-repetition counts for retained threshold steps,
#' or for the original matrix when no step succeeds.}
#' \item{diagnostics}{The distance metric, stop reason, total completed sweeps
#' (\code{iterations}), per-threshold budget, number of attempted thresholds,
#' last threshold, last attempted minimum distance, and retained minimum distance.
#' Stop reasons are \code{iteration_limit}, \code{distance_range_complete},
#' and \code{no_distance_thresholds}. A failed attempt is not retained in
#' \code{optim_design}.}
#' \item{metadata}{The design identifier, schema and package versions, RNG
#' settings, resolved seed and complete input parameters, including \code{X}.
#' The result inherits from \code{fieldhub_optimization}.}
#'
#' @examples
#' set.seed(123)
#' X <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
#' B <- swap_pairs(
#'   X,
#'   starting_dist = 3,
#'   stop_iter = 50,
#'   lambda = 0.5,
#'   dist_method = "euclidean",
#'   candidate_sample_size = 3,
#'   seed = 123
#' )
#' B$optim_design
#'
#' @export
swap_pairs <- function(X,
                       starting_dist = 3,
                       stop_iter = 10,
                       lambda = 0.5,
                       dist_method = "euclidean",
                       candidate_sample_size = 4,
                       seed = NULL) {
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  parameters <- list(X = X, starting_dist = starting_dist, stop_iter = stop_iter,
                      lambda = lambda, dist_method = dist_method,
                      candidate_sample_size = candidate_sample_size, seed = seed)
  result <- swap_pairs_core(X, starting_dist, stop_iter, lambda, dist_method, candidate_sample_size)
  new_fieldhub_optimization(result, parameters)
}

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
