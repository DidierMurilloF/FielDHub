#' Single-level block design engine (internal)
#'
#' Clean-room D-optimal search for one block factor, following Edmondson (2020),
#' JABES 25:500-522, sections 2.2-2.3, with the block-space swap updates in the
#' research derivation (gate G0). Draws come from R's generator; the caller owns
#' the seed. Output has the shape of `blocksdesign::blocks()` for a single
#' integer `blocks`.
#' @return A list with `Design` (`Level_1`, `plots`, `treatments` factors),
#'   `Blocks_model` (one row: `Level`, `Blocks`, `D-Efficiency`, `A-Efficiency`,
#'   `A-Bound`) and `search` (evaluations, budget, stop reason, refresh counts, the
#'   number of even-up repair swaps `even_repairs` (spec-m5 section 6.1) and,
#'   when `FIELDHUB_ENGINE_DEBUG=1`, the largest relative error of the running
#'   log determinant against a fresh computation after each accepted swap).
#' @noRd
blocks_engine <- function(treatments, replicates, blocks, searches = 20L,
                          jumps_per_escape = NULL, max_stale_escapes = 5L,
                          budget = NULL, time_limit = 60) {
  validate_iteration_budget(treatments, "treatments", minimum = 2)
  validate_iteration_budget(replicates, "replicates", minimum = 1)
  validate_iteration_budget(blocks, "blocks", minimum = 2)
  validate_iteration_budget(searches, "searches", minimum = 1)
  n <- as.double(treatments) * replicates
  if (is.null(jumps_per_escape)) jumps_per_escape <- blocks_engine_default_jumps(n)
  validate_iteration_budget(jumps_per_escape, "jumps_per_escape", minimum = 1)
  validate_iteration_budget(max_stale_escapes, "max_stale_escapes", minimum = 1)
  if (n > .Machine$integer.max) {
    fieldhub_abort("The design has too many plots for the block engine.",
                   data = list(plots = n))
  }
  if (blocks > 1000 || as.double(treatments) * blocks > 1e8) {
    fieldhub_abort("The block engine supports at most 1000 blocks and 1e8 treatment-block cells.",
                   data = list(blocks = blocks, treatments = treatments))
  }
  if (n < 2 * blocks || treatments + blocks - 1 > n) {
    fieldhub_abort("There are too few plots for ", blocks, " connected blocks.",
                   data = list(plots = n, blocks = blocks))
  }
  if (is.null(budget)) budget <- blocks_engine_default_budget(n, searches)
  validate_iteration_budget(budget, "budget", minimum = 1)
  if (!is.numeric(time_limit) || length(time_limit) != 1L || !is.finite(time_limit) ||
      time_limit <= 0) {
    fieldhub_abort("`time_limit` must be one positive number of seconds.",
                   data = list(argument = "time_limit"))
  }
  out <- blocks_engine_cpp(as.integer(treatments), as.integer(replicates), as.integer(blocks),
                           as.integer(searches), as.integer(jumps_per_escape),
                           as.integer(max_stale_escapes), as.double(budget),
                           as.double(time_limit), identical(Sys.getenv("FIELDHUB_ENGINE_DEBUG"), "1"))
  stop_reason <- c("budget", "stale", "time_limit", "no_start")[out$stop_reason]
  if (anyNA(out$treatments)) {
    if (stop_reason %in% c("budget", "time_limit")) {
      fieldhub_abort("The search ", if (stop_reason == "budget") "budget" else "time limit",
                     " was reached before a connected starting design was built.",
                     data = list(budget = budget, time_limit = time_limit))
    }
    fieldhub_abort("No connected starting design was found within 10 repaired random starts.",
                   data = list(treatments = treatments, replicates = replicates, blocks = blocks))
  }
  if (is.na(out$d_efficiency) || is.na(out$a_efficiency)) {
    fieldhub_abort("The final design failed its numerical check; no design is returned.",
                   data = list(treatments = treatments, replicates = replicates, blocks = blocks))
  }
  if (identical(stop_reason, "time_limit")) {
    fieldhub_warn("The block design search stopped at its time limit of ", time_limit,
                  " seconds; the result may not be reproducible from the seed.",
                  class = "fieldhub_design_warning", call = NULL)
  }
  sizes <- tabulate(out$blocks, nbins = blocks)
  design <- data.frame(
    Level_1 = factor(paste0("B", out$blocks), levels = paste0("B", seq_len(blocks))),
    plots = factor(seq_len(n)),
    treatments = factor(out$treatments, levels = seq_len(treatments))
  )
  model <- data.frame(Level = 1L, Blocks = as.integer(blocks),
                      `D-Efficiency` = out$d_efficiency, `A-Efficiency` = out$a_efficiency,
                      `A-Bound` = block_a_bound(treatments, replicates, sizes),
                      check.names = FALSE)
  list(Design = design, Blocks_model = model,
       search = list(evaluations = out$evaluations, budget = budget, stop_reason = stop_reason,
                     searches_done = out$searches_done, refreshes = out$refreshes,
                     drift_events = out$drift_events, repairs = out$repairs, even_repairs = out$even_repairs,
                     debug_max_error = out$debug_max_error))
}

#' Default evaluation budget per search (spec-m5 6.4): 25 full passes over the
#' plot pairs when a pass has at most 2000 pairs, max(10 passes, 50 n) up to 2e5
#' pairs (the engine's full-pass threshold), otherwise 50 n sampled evaluations;
#' at least 5000 per search and at most `.Machine$integer.max` in total. A
#' descent from a random start takes about three to four passes and each failed
#' escape about one more, so ten passes per search cover roughly one search's
#' descent and its `max_stale_escapes` = 5 escapes. The budget is one pool for
#' all searches, so harder shapes still stop on it.
#' @noRd
blocks_engine_default_budget <- function(n, searches) {
  pairs <- n * (n - 1) / 2
  per_search <- if (pairs <= 2000) 25 * pairs else if (pairs <= 2e5) max(10 * pairs, 50 * n) else 50 * n
  min(as.double(searches) * max(per_search, 5000), .Machine$integer.max)
}

#' Default escape size: 3 random swaps in full-pass mode (at most 2e5 plot
#' pairs), where a search stands at a proven local optimum and returns to its
#' best one after a failed escape; 10 in sampled mode, as before.
#' @noRd
blocks_engine_default_jumps <- function(n) {
  if (n * (n - 1) / 2 <= 2e5) 3L else 10L
}

#' Convexity upper bound for the A-efficiency of an equireplicate one-way design
#'
#' With efficiencies of C/r on the contrast space, at least max(0, v - b) equal 1;
#' the remaining m = min(v, b) - 1 sum to m - T with T at its minimum when every
#' block's counts are as even as possible; the harmonic mean is then at most
#' (v - 1) / (max(0, v - b) + m^2 / (m - T)).
#' @noRd
block_a_bound <- function(v, r, k) {
  lo <- k %/% v
  hi <- k %% v
  t_min <- sum((hi * (lo + 1)^2 + (v - hi) * lo^2) / (r * k)) - 1
  m <- min(v, length(k)) - 1
  (v - 1) / (max(0, v - length(k)) + m^2 / (m - t_min))
}
