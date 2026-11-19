#' Block design engine (internal)
#'
#' Clean-room D-optimal search for one block factor, following Edmondson (2020),
#' JABES 25:500-522, sections 2.2-2.3, with the block-space swap updates in the
#' research derivation (gate G0). Draws come from R's generator; the caller owns
#' the seed.
#'
#' With `resolvable = TRUE` (nested-engine spec sections 5 and 6.2) the blocks
#' are nested in `replicates` complete replicates and `blocks` is the number of
#' blocks per replicate, which must divide `treatments`. Starts, swaps and full
#' passes stay inside a replicate; the block-space score of the r b blocks is
#' the resolvable design's Level 2 score unchanged, because each replicate is
#' the union of its blocks. Runs are bounded by a work cap rather than the
#' clock, so they reproduce from the seed on the same platform and build, and
#' the total number of blocks is limited to `resolvable_block_limit`. Two
#' replicates of blocks of two are built in closed form (one 2v-cycle, which
#' every connected such design is), so the block limit does not apply to them. `start`, if given, holds v r 1-based
#' treatments in plot order (replicate m in plots m v + 1 .. m v + v, each a
#' permutation of 1..v) and is the start of a single search.
#' @return A list with `Design`, `Blocks_model` and `search` (evaluations,
#'   budget, stop reason, refresh counts, the number of even-up repair swaps
#'   `even_repairs` (spec-m5 section 6.1) and, when `FIELDHUB_ENGINE_DEBUG=1`,
#'   the largest relative error of the running log determinant against a fresh
#'   computation after each accepted swap). Single-level: `Design` has the
#'   factors `Level_1`, `plots`, `treatments` and `Blocks_model` one row
#'   (`Level`, `Blocks`, `D-Efficiency`, `A-Efficiency`, `A-Bound`), the shape
#'   of `blocksdesign::blocks()` for a single integer `blocks`. Resolvable:
#'   `Design` has `Level_1` (replicates `B1..Br`), `Level_2` (blocks
#'   `B<m>.B<j>`), `plots` and `treatments`, with the plots shuffled within
#'   every block, and `Blocks_model` two rows (replicates, then blocks).
#' @noRd
blocks_engine <- function(treatments, replicates, blocks, searches = 20L,
                          jumps_per_escape = NULL, max_stale_escapes = 5L,
                          budget = NULL, time_limit = 60, resolvable = FALSE,
                          start = NULL) {
  validate_iteration_budget(treatments, "treatments", minimum = 2)
  validate_iteration_budget(replicates, "replicates", minimum = 1)
  validate_iteration_budget(blocks, "blocks", minimum = 2)
  validate_iteration_budget(searches, "searches", minimum = 1)
  if (!is.logical(resolvable) || length(resolvable) != 1L || is.na(resolvable)) {
    fieldhub_abort("`resolvable` must be TRUE or FALSE.", data = list(argument = "resolvable"))
  }
  v <- as.double(treatments)
  n <- v * replicates
  total_blocks <- if (resolvable) as.double(replicates) * blocks else as.double(blocks)
  cycle <- FALSE
  unoptimised <- FALSE
  if (n > .Machine$integer.max) {
    fieldhub_abort("The design has too many plots for the block engine.",
                   data = list(plots = n))
  }
  if (resolvable) {
    if (treatments %% blocks != 0) {
      fieldhub_abort("A resolvable design needs the blocks per replicate (", blocks,
                     ") to divide the number of treatments (", treatments, ").",
                     data = list(treatments = treatments, blocks = blocks))
    }
    # Two replicates of blocks of two need no search (closed form below).
    cycle <- replicates == 2 && treatments == 2 * blocks && is.null(start)
    # Above the block limit the design is built without a search (owner
    # decision, spec section 10).
    unoptimised <- total_blocks > resolvable_block_limit && !cycle
  } else if (blocks > 1000) {
    fieldhub_abort("The block engine supports at most 1000 blocks and 1e8 treatment-block cells.",
                   data = list(blocks = blocks, treatments = treatments))
  }
  if (v * total_blocks > 1e8 && !unoptimised) {
    fieldhub_abort("The block engine supports at most ",
                   if (resolvable) paste0(resolvable_block_limit, " blocks") else "1000 blocks",
                   " and 1e8 treatment-block cells.",
                   data = list(blocks = total_blocks, treatments = treatments))
  }
  if (n < 2 * total_blocks || v + total_blocks - 1 > n) {
    fieldhub_abort("There are too few plots for ", total_blocks, " connected blocks.",
                   class = c("fieldhub_unconnected_error", "fieldhub_input_error"),
                   data = list(plots = n, blocks = total_blocks))
  }
  if (!is.null(start)) {
    segments_ok <- is.numeric(start) && length(start) == n && !anyNA(start) &&
      all(start == round(start)) &&
      all(vapply(seq_len(replicates), function(m) {
        identical(sort(as.integer(start[(m - 1) * v + seq_len(v)])), seq_len(v))
      }, logical(1)))
    if (!resolvable || !segments_ok) {
      fieldhub_abort("`start` must hold, for each replicate in turn, a permutation of the treatments.",
                     data = list(argument = "start"))
    }
    if (!resolvable_connected(as.integer(start), rep(seq_len(total_blocks), each = v %/% blocks), v)) {
      fieldhub_abort("`start` is a disconnected design; a connected start is needed.",
                     data = list(argument = "start"))
    }
  }
  if (!is.numeric(time_limit) || length(time_limit) != 1L || !is.finite(time_limit) ||
      time_limit <= 0) {
    fieldhub_abort("`time_limit` must be one positive number of seconds.",
                   data = list(argument = "time_limit"))
  }
  if (resolvable && cycle) {
    return(cycle_resolvable_design(treatments, blocks))
  }
  if (unoptimised) {
    fieldhub_warn("The design has ", total_blocks, " incomplete blocks, more than the ",
                  resolvable_block_limit, " the block search supports, so it was not optimized: ",
                  "it is a random connected resolvable design. A larger block size gives fewer ",
                  "blocks and an optimized design.",
                  class = "fieldhub_design_warning", call = NULL)
    return(random_resolvable_design(treatments, replicates, blocks))
  }
  pairs <- if (resolvable) replicates * v * (v - 1) / 2 else n * (n - 1) / 2
  if (resolvable && !is.null(start)) {
    searches <- 1L  # a given start is the start of one search
  } else if (resolvable && missing(searches)) {
    searches <- resolvable_searches(total_blocks)
  }
  if (is.null(jumps_per_escape)) {
    jumps_per_escape <- if (resolvable) 3L else blocks_engine_default_jumps(n, pairs)
  }
  validate_iteration_budget(jumps_per_escape, "jumps_per_escape", minimum = 1)
  validate_iteration_budget(max_stale_escapes, "max_stale_escapes", minimum = 1)
  if (is.null(budget)) {
    budget <- blocks_engine_default_budget(
      n, searches, pairs, multiplier = if (resolvable) resolvable_budget_multiplier else 1,
      full_passes = resolvable
    )
  }
  validate_iteration_budget(budget, "budget", minimum = 1)
  work_cap <- if (resolvable) resolvable_work_cap(v, replicates, total_blocks, budget, pairs) else Inf
  refresh_every <- 256L
  out <- blocks_engine_cpp(as.integer(treatments), as.integer(replicates), as.integer(total_blocks),
                           as.integer(searches), as.integer(jumps_per_escape),
                           as.integer(max_stale_escapes), as.double(budget),
                           as.double(time_limit), identical(Sys.getenv("FIELDHUB_ENGINE_DEBUG"), "1"),
                           resolvable, as.integer(if (is.null(start)) integer() else start),
                           as.double(work_cap), as.integer(refresh_every))
  stop_reason <- c("budget", "stale", "time_limit", "no_start")[out$stop_reason]
  if (anyNA(out$treatments)) {
    if (stop_reason %in% c("budget", "time_limit")) {
      fieldhub_abort("The search ", if (stop_reason == "budget") "budget" else "time limit",
                     " was reached before a connected starting design was built.",
                     data = list(budget = budget, time_limit = time_limit))
    }
    if (!is.null(start)) {
      # `start` was checked connected above, so this is a numerical failure,
      # not a lack of replication: callers must not translate it.
      fieldhub_abort("The given start could not be scored; no design is returned.",
                     class = "fieldhub_numerical_error",
                     data = list(treatments = treatments, replicates = replicates, blocks = blocks))
    }
    fieldhub_abort("No connected starting design was found within 10 repaired random starts.",
                   class = c("fieldhub_unconnected_error", "fieldhub_input_error"),
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
  search <- list(evaluations = out$evaluations, budget = budget, stop_reason = stop_reason,
                 searches_done = out$searches_done, refreshes = out$refreshes,
                 drift_events = out$drift_events, repairs = out$repairs, even_repairs = out$even_repairs,
                 debug_max_error = out$debug_max_error)
  if (resolvable) {
    result <- resolvable_engine_output(out$treatments, treatments, replicates, blocks)
    result$Blocks_model[2L, c("D-Efficiency", "A-Efficiency")] <-
      c(out$d_efficiency, out$a_efficiency)
    result$search <- c(search, list(work = out$work, work_cap = work_cap, refresh_every = refresh_every))
    return(result)
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
  list(Design = design, Blocks_model = model, search = search)
}

#' Two-level output of a resolvable design (nested-engine spec section 6.2)
#'
#' `treat` holds the treatments by plot, replicate by replicate and block by
#' block (`blocks` blocks of `v / blocks` plots per replicate). The plots are
#' shuffled within every block with R's generator, so a plot's position inside
#' its block carries no pattern from the construction. The Level 2 row has the
#' one-way bound for the r b blocks; its efficiencies are filled by the caller.
#' @noRd
resolvable_engine_output <- function(treat, v, r, blocks) {
  k <- v %/% blocks
  total <- r * blocks
  for (j in seq_len(total)) {
    idx <- (j - 1) * k + seq_len(k)
    treat[idx] <- treat[idx][sample.int(k)]
  }
  block <- rep(seq_len(blocks), each = k, times = r)
  rep_of <- rep(seq_len(r), each = v)
  design <- data.frame(
    Level_1 = factor(paste0("B", rep_of), levels = paste0("B", seq_len(r))),
    Level_2 = factor(paste0("B", rep_of, ".B", block),
                     levels = paste0("B", rep(seq_len(r), each = blocks), ".B",
                                     rep(seq_len(blocks), times = r))),
    plots = factor(seq_len(v * r)),
    treatments = factor(treat, levels = seq_len(v))
  )
  model <- data.frame(Level = 1:2, Blocks = as.integer(c(r, total)),
                      `D-Efficiency` = c(1, NA_real_), `A-Efficiency` = c(1, NA_real_),
                      `A-Bound` = c(1, block_a_bound(v, r, rep(k, total))),
                      check.names = FALSE)
  list(Design = design, Blocks_model = model)
}

#' Two replicates of blocks of two (nested-engine spec section 6.2)
#'
#' Every connected design of this shape is one cycle through all v treatments
#' and all such cycles are isomorphic (efficiency factors (1 - cos(2 pi j / v)) / 2,
#' j = 1..v-1; A = 3 / (v + 1)), so no search is needed: for a random order p,
#' replicate 1 holds {p1, p2}, {p3, p4}, ... and replicate 2 {p2, p3}, ...,
#' {pv, p1}; the block order within each replicate is then shuffled, and the
#' plots within blocks by `resolvable_engine_output()`.
#' @noRd
cycle_resolvable_design <- function(v, blocks) {
  p <- sample.int(v)
  first <- matrix(p, nrow = 2L)
  second <- matrix(c(p[-1L], p[1L]), nrow = 2L)
  treat <- c(first[, sample.int(blocks)], second[, sample.int(blocks)])
  result <- resolvable_engine_output(treat, v, 2L, blocks)
  efficiency <- BlockEfficiencies(result$Design[, c("Level_2", "plots", "treatments")])
  result$Blocks_model[2L, c("D-Efficiency", "A-Efficiency")] <-
    c(efficiency[["D-Efficiency"]], efficiency[["A-Efficiency"]])
  result$search <- list(evaluations = 0, budget = 0, stop_reason = "closed_form",
                        searches_done = 0L, refreshes = 0, drift_events = 0, repairs = 0,
                        even_repairs = 0L, debug_max_error = NA_real_)
  result
}

#' Unoptimised resolvable design above the block limit (spec section 10)
#'
#' Each replicate is a random permutation of the treatments cut into blocks.
#' The treatment-block graph is checked for connectivity by label propagation
#' (O(n) per sweep, no dense algebra); a disconnected layout is reshuffled, up
#' to 10 layouts. Level 2 efficiencies are left NA: the callers report them
#' from the field book.
#' @noRd
random_resolvable_design <- function(v, r, blocks) {
  k <- v %/% blocks
  block <- rep(seq_len(r * blocks), each = k)
  for (attempt in seq_len(10L)) {
    treat <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
    if (resolvable_connected(treat, block, v)) {
      result <- resolvable_engine_output(treat, v, r, blocks)
      result$search <- list(evaluations = 0, budget = 0, stop_reason = "unoptimised",
                            searches_done = 0L, refreshes = 0, drift_events = 0, repairs = 0,
                            even_repairs = 0L, debug_max_error = NA_real_)
      return(result)
    }
  }
  fieldhub_abort("No connected starting design was found within 10 random resolvable layouts.",
                 class = c("fieldhub_unconnected_error", "fieldhub_input_error"),
                 data = list(treatments = v, replicates = r, blocks = blocks))
}

#' Connectivity of a treatment-block layout: every treatment and block takes the
#' smallest label among its neighbours until no label changes.
#' @noRd
resolvable_connected <- function(treat, block, v) {
  label <- seq_len(v)
  repeat {
    block_label <- tapply(label[treat], block, min)
    next_label <- pmin(label, tapply(block_label[block], treat, min)[as.character(seq_len(v))])
    if (identical(as.integer(next_label), as.integer(label))) break
    label <- as.integer(next_label)
  }
  all(label == 1L)
}

#' Resolvable-mode limits (nested-engine spec sections 6.2 and 6.2b). B is the
#' total number of blocks r b. Every run is bounded by its evaluation budget
#' and by a work cap on the dense algebra (refreshes, rank-2 updates, drift
#' checks), both fixed by the inputs, so a run ends at a point fixed by the
#' seed rather than on the wall clock. The cap is derived from the budget.
#' Measured on an installed build (2026-11-18), a search makes about 1.5-2 B
#' accepted swaps and a few refreshes, and a search runs about 30-70 full passes
#' before it goes stale, so the cap allows 3 (one refresh + 2 B rank-2 updates)
#' for each search the budget can start, min(budget / per-search budget,
#' passes / 30), and at least 1e10 units. Runs used 0.1-0.4 of it from B = 375
#' to 3000; the floor covers smaller shapes. 20 searches up to 1000 blocks and
#' 20 (1000 / B)^3 above. The block limit, 2000, is the largest B measured to
#' be far faster than blocksdesign (B = 2000: 5-14 s against 222 s and more
#' than 600 s on the reference machine) and to end well inside the 60 s time
#' limit even under load; B = 3000 took 38 s idle and hit the time limit
#' under load.
#' @noRd
resolvable_block_limit <- 2000

#' @noRd
resolvable_budget_multiplier <- 1

#' @noRd
resolvable_searches <- function(B) {
  as.integer(if (B <= 1000) 20 else max(1, floor(20 * (1000 / B)^3)))
}

#' @noRd
resolvable_work_cap <- function(v, r, B, budget, pairs) {
  n <- v * r
  refresh <- B^3 + n * r + n * B
  swap <- 4 * B^2 + 2 * B * (if (B * v <= 2.5e7) v else r)  # with the G cache up to 2.5e7 cells
  passes <- budget / pairs
  searches <- min(budget / max(10 * pairs, 50 * n), ceiling(passes / 30))
  max(1e10, searches * 3 * (refresh + 2 * B * swap))
}

#' Default evaluation budget per search (spec-m5 6.4): 25 full passes over the
#' candidate pairs when a pass has at most 2000 pairs, max(10 passes, 50 n) up to
#' 2e5 pairs (the engine's full-pass threshold), otherwise 50 n sampled
#' evaluations; at least 5000 per search and at most `.Machine$integer.max` in
#' total. A descent from a random start takes about three to four passes and
#' each failed escape about one more, so ten passes per search cover roughly one
#' search's descent and its `max_stale_escapes` = 5 escapes. The budget is one
#' pool for all searches, so harder shapes still stop on it. The candidate pairs
#' are all plot pairs, n (n - 1) / 2, or in resolvable mode the pairs inside a
#' replicate, r v (v - 1) / 2; `multiplier` (resolvable mode only) scales the
#' per-search budget. Resolvable mode always runs full passes (`full_passes`,
#' nested-engine spec 6.2b), so it keeps max(10 passes, 50 n) above 2e5 pairs.
#' @noRd
blocks_engine_default_budget <- function(n, searches, pairs = n * (n - 1) / 2, multiplier = 1,
                                         full_passes = FALSE) {
  per_search <- if (pairs <= 2000) 25 * pairs else if (pairs <= 2e5 || full_passes) max(10 * pairs, 50 * n) else 50 * n
  min(as.double(searches) * max(multiplier * per_search, 5000), .Machine$integer.max)
}

#' Default escape size: 3 random swaps in full-pass mode (at most 2e5 candidate
#' pairs), where a search stands at a proven local optimum and returns to its
#' best one after a failed escape; 10 in sampled mode, as before.
#' @noRd
blocks_engine_default_jumps <- function(n, pairs = n * (n - 1) / 2) {
  if (pairs <= 2e5) 3L else 10L
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
