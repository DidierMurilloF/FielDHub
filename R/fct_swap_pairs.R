#' @title Swap pairs in a matrix of integers
#'
#' @description Attempts to separate repeated entries by swapping cells of
#' different entries. Distance thresholds increase from \code{starting_dist}
#' in steps of one. Candidate sampling, mean pairwise distance, and a centrality
#' penalty guide the search. The last successful threshold layout is returned
#' (with the row/column rule on, the successful layout with the fewest pairs of
#' copies sharing a row or column; see \code{rowcol_penalty}); if none succeeds,
#' the original matrix is returned. A requested minimum distance is not
#' guaranteed.
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
#' @param rowcol_penalty Weight of the row/column rule: one number at least 0,
#' or \code{Inf}. The default 0 turns the rule off and leaves every layout
#' unchanged. When positive, the copies of a covered entry that share a row or a
#' column are treated as a problem to fix even when they are far apart, and a
#' candidate swap scores its distance score minus \code{rowcol_penalty} times the
#' change in the number of shared pairs. \code{Inf} is strict: a swap that leaves
#' fewer shared pairs always wins, and distance decides among equals (rows and
#' columns first). With the rule on, the result keeps the successful threshold
#' step with the fewest shared pairs (the largest threshold breaks ties), the
#' search steps after it are dropped from \code{designs}, \code{distances} and
#' \code{rows_incidence}, and \code{diagnostics} adds \code{rowcol_penalty},
#' \code{rowcol_scope}, \code{retained_threshold}, \code{shared_pairs_input},
#' \code{shared_pairs_final} and \code{entries_sharing_final}.
#' @param rowcol_scope Entries the row/column rule covers: \code{"pairs"}
#' (default), the entries with exactly two copies, or \code{"all"}, every entry
#' with two or more copies, checks included.
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
#' \item{designs}{A list of the retained threshold layouts, starting from the input matrix;
#' with the row/column rule on, it ends at the kept layout.}
#' \item{distances}{A list of the pair distances of each layout in \code{designs}.}
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
#' @references
#' Williams, E., Piepho, H.-P., & Whitaker, D. (2011). Augmented p-rep designs.
#' Biometrical Journal, 53(1), 19-27. https://doi.org/10.1002/bimj.201000102
#'
#' Cullis, B. R., Smith, A. B., & Coombes, N. E. (2006). On the design of early generation variety trials
#' with correlated data. Journal of Agricultural, Biological, and Environmental Statistics, 11,
#' 381-393. https://doi.org/10.1198/108571106X154443
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
                       seed = NULL,
                       rowcol_penalty = 0,
                       rowcol_scope = "pairs") {
  # Checked before resolve_seed(), which may draw from the caller's stream.
  validate_rowcol_controls(rowcol_penalty, rowcol_scope)
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  parameters <- list(X = X, starting_dist = starting_dist, stop_iter = stop_iter,
                      lambda = lambda, dist_method = dist_method,
                      candidate_sample_size = candidate_sample_size, seed = seed,
                      rowcol_penalty = rowcol_penalty, rowcol_scope = rowcol_scope)
  result <- swap_pairs_core(X, starting_dist, stop_iter, lambda, dist_method, candidate_sample_size,
                            rowcol_penalty, rowcol_scope)
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
  # Results recorded before the row/column rule (spec-m15 5.4) lack its two
  # parameters; they replay with the defaults, which reproduce them.
  current <- names(formals(swap_pairs))
  legacy <- setdiff(current, c("rowcol_penalty", "rowcol_scope"))
  if (!is.list(parameters) ||
      !(identical(names(parameters), current) || identical(names(parameters), legacy))) {
    fail("has incomplete input parameters")
  }
  rowcol_penalty <- if (is.null(parameters$rowcol_penalty)) 0 else parameters$rowcol_penalty
  rowcol_scope <- if (is.null(parameters$rowcol_scope)) "pairs" else parameters$rowcol_scope
  input <- parameters$X
  output <- x$optim_design
  tryCatch({
    validate_swap_matrix(input)
    validate_swap_matrix(output)
    validate_swap_controls(parameters$starting_dist, parameters$stop_iter, parameters$lambda,
                            parameters$dist_method, parameters$candidate_sample_size)
    validate_rowcol_controls(rowcol_penalty, rowcol_scope)
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
  rule_fields <- c("rowcol_penalty", "rowcol_scope", "retained_threshold", "shared_pairs_input",
                   "shared_pairs_final", "entries_sharing_final")
  if (rowcol_penalty > 0) {
    if (!all(rule_fields %in% names(diagnostics)) ||
        !identical(diagnostics$rowcol_penalty, rowcol_penalty) ||
        !identical(diagnostics$rowcol_scope, rowcol_scope)) {
      fail("has inconsistent row/column diagnostics")
    }
  } else if (any(rule_fields %in% names(diagnostics))) {
    fail("has row/column diagnostics without the row/column rule")
  }
  invisible(x)
}
