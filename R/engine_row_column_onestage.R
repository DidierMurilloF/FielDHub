# Staged onestage row-column search (phase 2 spec 6.1 and 6.2). Canonical plot
# order of a row-column design: replicate m, then row i, then column j, plot
# p = (m - 1) v + (i - 1) ncols + j. The row block is (m - 1) nrows + i when
# rows are nested in the replicates and i when they are shared; the column
# block is (m - 1) ncols + j nested and j shared.

#' Work cap of one onestage build (phase 2 spec 6.1, "Work cap and
#' admission"): every stage 1, repair and joint search of the build together.
#' Each engine call gets the work left; once it is spent no further search
#' starts. A shape is admitted when `onestage_predicted_work()` is at most 0.8
#' of it (`onestage_admitted()`). PROVISIONAL (Task 8): kept at the resolvable
#' cap, 2e11, until the controller's measurement at t = 300-2000 sets it so
#' that the largest admitted shape stops on the cap well inside the 60 s time
#' limit at a quarter of the reference machine's speed (phase 1 rule). The
#' measurements so far are with `onestage_predicted_work()`: at t <= 200 the
#' largest search used 8.4e9 units, at 4-16e9 units per second on a loaded
#' machine. Those measurements predate the joint stage's present effort
#' (`onestage_joint_passes`); with it (2026-11-22, one search, seed 1) a
#' nested search uses 4.7e10 units at 200/100/4, 3.2e10 at 200/2/4 and 8.2e10
#' at 500/10/4, so at `iterations` = 200 the builds of the first two stop on
#' this cap after about 4 and 6 of their 10 searches.
#' @noRd
onestage_work_limit <- 2e11

#' Blocks that span segments (block engine mode 4, phase 2 spec 6.1)
#'
#' A one-way block design search in which the `blocks` blocks, of n / blocks
#' plots each, may span the segments, and a swap exchanges two plots of one
#' segment, so every segment keeps the treatments `start` gives it. A block may
#' hold a treatment more than once. Each of the `searches` searches starts from
#' `start` with every segment shuffled by R's generator; the engine runs full
#' passes, first improvement and escapes as in resolvable mode. A disconnected
#' start is repaired with in-segment swaps charged to the work cap only (phase
#' 1 fix I-1). The evaluation budget is the default full-pass budget over the
#' pairs inside the segments; every evaluation is charged to `work_cap` as in
#' resolvable mode.
#'
#' @param start n = treatments * replicates 1-based treatments by plot, each
#'   treatment `replicates` times.
#' @param block_of,segment_of The 1-based block and segment of every plot;
#'   every block holds n / blocks plots and all segments the same number.
#' @return A list with `treatments` (plot order as `start`), `d_efficiency` and
#'   `a_efficiency` of the blocks (NA, with `treatments` = `start`, when no
#'   search found a connected design) and `search` (`evaluations`, `work`,
#'   `stop_reason`, `work_capped`: TRUE when the run stopped on `work_cap`).
#' @noRd
segmented_blocks <- function(start, block_of, segment_of, treatments, replicates, blocks,
                             searches = 20L, work_cap = onestage_work_limit) {
  v <- as.double(treatments)
  r <- as.double(replicates)
  n <- v * r
  if (length(start) != n || length(block_of) != n || length(segment_of) != n ||
      anyNA(start) || anyNA(block_of) || anyNA(segment_of)) {
    fieldhub_abort("`start`, `block_of` and `segment_of` must give one value per plot.",
                   data = list(plots = n))
  }
  if (!is.numeric(work_cap) || length(work_cap) != 1L || is.na(work_cap) || work_cap <= 0) {
    fieldhub_abort("`work_cap` must be one positive number.", data = list(argument = "work_cap"))
  }
  segments <- max(segment_of)
  seg_size <- n / segments
  pairs <- segments * seg_size * (seg_size - 1) / 2
  budget <- blocks_engine_default_budget(n, searches, pairs, full_passes = TRUE)
  evaluation_work <- resolvable_evaluation_work(v, r, blocks)
  time_limit <- 60
  out <- blocks_engine_cpp(as.integer(v), as.integer(r), as.integer(blocks), as.integer(searches),
                           3L, 5L, as.double(budget), time_limit,
                           identical(Sys.getenv("FIELDHUB_ENGINE_DEBUG"), "1"),
                           as.integer(start), as.double(work_cap), 256L, as.double(evaluation_work),
                           4L, integer(), as.integer(block_of) - 1L, as.integer(segment_of) - 1L)
  stop_reason <- c("budget", "stale", "time_limit", "no_start")[out$stop_reason]
  if (identical(stop_reason, "time_limit")) {
    fieldhub_warn("The block search stopped at its time limit of ", time_limit,
                  " seconds; the result may not be reproducible from the seed.",
                  class = "fieldhub_design_warning", call = NULL)
  }
  found <- !anyNA(out$treatments) && !is.na(out$a_efficiency)
  list(treatments = if (found) out$treatments else as.integer(start),
       d_efficiency = if (found) out$d_efficiency else NA_real_,
       a_efficiency = if (found) out$a_efficiency else NA_real_,
       search = list(evaluations = out$evaluations, work = out$work, stop_reason = stop_reason,
                     work_capped = out$work_capped))
}

#' Stage 1 of the onestage search: rows first (phase 2 spec 6.1)
#'
#' An unconstrained one-way search of the rows, swaps within a replicate, then
#' the columns within the rows, swaps only between two plots of the same row of
#' the same replicate, so the row design is untouched. The column step only
#' builds a start for the joint stage.
#' - Nested (`latinize = FALSE`): the rows are resolvable blocks of size ncols
#'   (`blocks_engine(resolvable = TRUE)`); the columns are then the twostage
#'   row step on the transposed grid (`rows_within_columns()`, its rows the
#'   ncols columns of each replicate, its columns our rows, `effort = 1`).
#' - Shared (`latinize = TRUE`): `segmented_blocks()` for the rows (blocks the
#'   nrows shared rows, segments the replicates, start one `sample.int(nt)`
#'   per replicate), then for the columns (blocks the ncols shared columns,
#'   segments the replicate x row cells, start the row design).
#'
#' The work cap applies to the whole stage: each step gets what the earlier
#' ones left. Nested, the row step gets `work_cap` less the column step's
#' admission need (`onestage_column_reserve()`), so a row step that ends below
#' its cap always leaves the column step admitted. Each step is checked
#' against its own admission before it runs. A step that its admission would
#' refuse, a step that stops on its work cap and a callee's own large-design
#' refusal (class `fieldhub_search_refused_warning`) all abort with class
#' `onestage_not_admitted` (also `onestage_infeasible`): the stage did not
#' finish inside the work cap it was given, so the shape (or, for a later
#' search of a build, the work left) is too small for it. These stops depend
#' on the work counts only, so they are reproducible from the seed.
#'
#' Any other warning of a step is passed on unchanged. A step that stops at
#' its 60 s time limit therefore warns once (`fieldhub_design_warning`, "may
#' not be reproducible from the seed"), as `crossed_search()` does, and the
#' stage goes on with that step's design.
#'
#' A row step that finds no connected design aborts with class
#' `onestage_infeasible` (no full-rank design can follow), so `row_column()`
#' falls back to twostage. Draws from R's generator; the caller owns the seed.
#'
#' @return A list with `treatments` (1-based, canonical plot order, every
#'   replicate holding each treatment once), `work` (the work units used) and
#'   `step_work` (the row step's and the column step's work).
#' @noRd
onestage_stage1 <- function(nt, nrows, ncols, reps, latinize, work_cap = onestage_work_limit) {
  v <- as.integer(nt)
  r <- as.integer(reps)
  nrows <- as.integer(nrows)
  ncols <- as.integer(ncols)
  if (nrows * ncols != v) {
    fieldhub_abort("nrows * ncols must equal the number of treatments.",
                   data = list(nrows = nrows, ncols = ncols, treatments = nt))
  }
  where <- paste0(" (t = ", nt, ", nrows = ", nrows, ", r = ", reps, ")")
  refuse <- function(what, parent = NULL) {
    fieldhub_abort("The onestage stage 1 ", what, where, ".",
                   class = c("onestage_not_admitted", "onestage_infeasible",
                             "fieldhub_optimization_error"),
                   data = list(parent = parent), call = NULL)
  }
  unconnected <- function(parent = NULL) {
    fieldhub_abort("The onestage stage 1 row search found no connected design", where, ".",
                   class = c("onestage_infeasible", "fieldhub_optimization_error"),
                   data = list(treatments = nt, nrows = nrows, replicates = reps, parent = parent),
                   call = NULL)
  }
  # Only a callee's large-design refusal becomes a refusal of the stage; a
  # time-limit warning, or any other, reaches the caller as it is.
  step <- function(name, expr) {
    withCallingHandlers(expr, fieldhub_search_refused_warning = function(w) {
      refuse(paste0(name, " step was refused by its search: ", conditionMessage(w)), w)
    })
  }
  capped <- function(name, search) {
    if (isTRUE(search$work_capped)) refuse(paste0(name, " step reached its work cap"))
  }
  used <- function(search) if (is.null(search$work)) 0 else search$work
  rep_of <- rep(seq_len(r), each = v)
  row_in <- rep(rep(seq_len(nrows), each = ncols), r)
  col_in <- rep(seq_len(ncols), nrows * r)
  if (!latinize) {
    reserve <- onestage_column_reserve(v, r, nrows, ncols)
    row_cap <- work_cap - reserve
    if (!onestage_row_step_admitted(v, r, nrows, row_cap)) {
      refuse("row step is not admitted inside the work cap")
    }
    rows <- tryCatch(
      step("row", blocks_engine(v, r, nrows, resolvable = TRUE, work_cap = row_cap)),
      fieldhub_unconnected_error = function(e) unconnected(e)
    )
    capped("row", rows$search)
    # Plot order of the engine output: replicate, row (block), plot in row,
    # which is the canonical order with the plot's position as its column.
    treat <- as.integer(rows$Design$treatments)
    row_work <- used(rows$search)
    # Transposed grid: replicate, column, row. The row step ended below
    # row_cap, so at least `reserve` is left and the column step is admitted.
    transposed <- order(rep_of, col_in, row_in)
    cols <- step("column", rows_within_columns(treat[transposed], row_in[transposed], v, r, ncols,
                                               effort = 1, work_cap = work_cap - row_work))
    capped("column", cols$search)
    treat[transposed] <- cols$treatments
  } else {
    start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
    rows <- step("row", segmented_blocks(start, row_in, rep_of, v, r, nrows, work_cap = work_cap))
    capped("row", rows$search)
    if (is.na(rows$a_efficiency)) unconnected()
    row_work <- used(rows$search)
    if (work_cap - row_work <= 0) refuse("row step used the whole work cap")
    cols <- step("column", segmented_blocks(rows$treatments, col_in, (rep_of - 1L) * nrows + row_in,
                                            v, r, ncols, work_cap = work_cap - row_work))
    capped("column", cols$search)
    treat <- cols$treatments
  }
  step_work <- c(rows = row_work, columns = used(cols$search))
  list(treatments = as.integer(treat), work = sum(step_work), step_work = step_work)
}

#' Effort of one joint-stage search (phase 2 spec 6.1): its evaluation budget
#' in full passes over the r v (v - 1) / 2 pairs inside the replicates, and
#' the number of failed escapes in a row that ends it.
#'
#' Measured 2026-11-22 (root cause of the nested joint D shortfall at t >= 50).
#' The first values were resolvable mode's, ten passes and five escapes. On
#' nested shapes with t >= 50 every search then stopped on the budget, before
#' or just after its first local optimum (100/5/4: 0.7455 after 10 passes,
#' 0.7458 at the first stale stop after 39), and five escapes of three
#' row-preserving jumps rarely leave an optimum of that size: one search
#' reached 0.7462 with 20 escapes and 0.7465 with 40, against the 1.7.0 mean
#' of 0.74619 (SD 8.8e-5). More searches did not help, because each one ended
#' as short as the first. Mean joint D less the 1.7.0 mean, in 1.7.0 SDs
#' (rule: at least -0.5), 8 seeds, `iterations` = 200:
#'
#' | Nested shape | 10 passes, 5 | 200, 20 | 400, 40 | 800, 80 |
#' |---|---|---|---|---|
#' | 50/5/2 | -0.83 | +0.73 | +0.80 | +0.81 |
#' | 100/5/2 | -1.62 | +1.51 | +1.92 | +3.06 |
#' | 50/5/4 | -2.01 | +2.60 | +3.45 | +4.18 |
#' | 100/5/4 | -5.71 | +2.04 | +3.20 | +4.96 |
#'
#' 400 passes and 40 escapes cost 3.1 s per call at 100/5/4, where 1.7.0 took
#' 46 s. Small shapes end stale long before the budget, as before.
#' @noRd
onestage_joint_passes <- 400
#' @noRd
onestage_joint_stale_escapes <- 40L

#' Evaluation budget of one joint-stage search (phase 2 spec 6.1):
#' `onestage_joint_passes` full passes over the r v (v - 1) / 2 pairs inside
#' the replicates, at least 5000 evaluations and at most
#' `.Machine$integer.max`. Besides one evaluation per ratio and per accepted
#' escape jump, every drift check charges n + B evaluations to it (as in
#' resolvable mode). A search ends on it or after
#' `onestage_joint_stale_escapes` failed escapes in a row, whichever comes
#' first. The work model of mode 3 (evaluation work, swap and refresh costs)
#' is resolvable mode's, copied rather than measured; the work cap and the
#' admission fit above rest on measured work, not on that model.
#' @noRd
crossed_default_budget <- function(v, r) {
  pairs <- as.double(r) * v * (v - 1) / 2
  min(max(onestage_joint_passes * pairs, 5000), .Machine$integer.max)
}

#' Joint stage of the onestage search (block engine mode 3, phase 2 spec 6.1)
#'
#' One search from `start` (a complete row-column design in canonical plot
#' order: replicate, row, column): first-improvement full passes on the joint
#' log det of rows and columns, under the Row D and Row A floors (running
#' values against the fixed values of `start`), with escapes from
#' row-preserving pairs. Rows and columns are nested in the replicates, or
#' `shared` across them. The search ends on `budget` or after
#' `max_stale_escapes` failed escapes in a row (`onestage_joint_passes`,
#' `onestage_joint_stale_escapes`). Draws from R's generator.
#'
#' A start that fails the full-rank test of spec 5.1b is first repaired
#' (spec 6.1 stage 2): first improvement on log det(C / r + eps P), eps =
#' 1e-6, over the same swaps and under the same Row floors, until the test
#' passes. The repair has its own allowance (30 full passes), charged to
#' `work_cap` and never to `budget`: `evaluations` excludes it and
#' `repair_evaluations` counts it. If it fails, its last design is returned
#' with `full_rank = FALSE` and no joint stage runs.
#'
#' Row D and Row A of the result are never below those of `start`, compared
#' exactly as the unrounded values returned here.
#'
#' @return A list with `treatments` (1-based, plot order as `start`),
#'   `full_rank`, unrounded `row_d`, `row_a`, `joint_d`, `joint_a`,
#'   `joint_log_det` of the result (joint values NA when not full rank), the
#'   Row values of `start` (`stage1_row_d`, `stage1_row_a`) and `search`:
#'   `evaluations`, `work`, `stop_reason`, `repair_swaps`,
#'   `repair_evaluations`, `repair_log_det` (the repair criterion in block
#'   space, log det((1 + eps) I - N'N / r + y y' / n), before and after the
#'   repair; NA without one), `repair_score_error` (the largest error of the
#'   rank-2 score of an accepted repair swap), `floor_fallback` (1 if the
#'   result was replaced to keep the Row floors exactly, 2 if the repair's
#'   output was rejected for them, else 0), `refreshes`, `drift_events`,
#'   `work_capped`, `debug_max_error`.
#' @noRd
crossed_search <- function(start, v, r, nrows, ncols, shared, budget = crossed_default_budget(v, r),
                           work_cap, jumps_per_escape = 3L,
                           max_stale_escapes = onestage_joint_stale_escapes) {
  v <- as.integer(v)
  r <- as.integer(r)
  nrows <- as.integer(nrows)
  ncols <- as.integer(ncols)
  if (nrows * ncols != v || length(start) != v * r || anyNA(start)) {
    fieldhub_abort("`start` must give one treatment per plot of a complete nrows x ncols grid.",
                   data = list(treatments = v, nrows = nrows, ncols = ncols, replicates = r))
  }
  if (!is.numeric(work_cap) || length(work_cap) != 1L || is.na(work_cap) || work_cap <= 0) {
    fieldhub_abort("`work_cap` must be one positive number.", data = list(argument = "work_cap"))
  }
  rep0 <- rep(seq_len(r) - 1L, each = v)
  row_in <- rep(rep(seq_len(nrows) - 1L, each = ncols), r)
  col_in <- rep(seq_len(ncols) - 1L, nrows * r)
  row_block <- if (shared) row_in else rep0 * nrows + row_in
  col_block <- if (shared) col_in else rep0 * ncols + col_in
  b_rows <- if (shared) nrows else r * nrows
  B <- b_rows + (if (shared) ncols else r * ncols)
  time_limit <- 60
  # Check hook (tools/check-onestage-engine.R): 2L skips the exact Row
  # confirmation of each swap, so the end-of-search comparison can be reached.
  hook <- if (identical(Sys.getenv("FIELDHUB_ENGINE_DEBUG"), "slack_floors")) 2L else 1L
  out <- blocks_engine_cpp(v, r, as.integer(b_rows), hook, as.integer(jumps_per_escape),
                           as.integer(max_stale_escapes), as.double(budget), time_limit,
                           identical(Sys.getenv("FIELDHUB_ENGINE_DEBUG"), "1"),
                           as.integer(start), as.double(work_cap), 256L,
                           as.double(resolvable_evaluation_work(v, r, B)), 3L,
                           as.integer(col_block), as.integer(row_block), integer())
  stop_reason <- c("budget", "stale", "time_limit", "no_start")[out$stop_reason]
  if (identical(stop_reason, "time_limit")) {
    fieldhub_warn("The joint row-column search stopped at its time limit of ", time_limit,
                  " seconds; the result may not be reproducible from the seed.",
                  class = "fieldhub_design_warning", call = NULL)
  }
  list(treatments = out$treatments, full_rank = out$full_rank,
       row_d = out$row_d, row_a = out$row_a, joint_d = out$joint_d, joint_a = out$joint_a,
       joint_log_det = out$joint_log_det,
       stage1_row_d = out$stage1_row_d, stage1_row_a = out$stage1_row_a,
       search = list(evaluations = out$evaluations, work = out$work, stop_reason = stop_reason,
                     repair_swaps = out$repair_swaps, repair_evaluations = out$repair_evaluations,
                     repair_log_det = c(out$repair_log_det_start, out$repair_log_det),
                     repair_score_error = out$repair_score_error,
                     floor_fallback = out$floor_fallback,
                     refreshes = out$refreshes, drift_events = out$drift_events,
                     work_capped = out$work_capped, debug_max_error = out$debug_max_error))
}

#' Whether the joint row-column model can estimate every treatment contrast
#' (phase 2 spec 5.1b): the residual degrees of freedom of the joint additive
#' model, n - 1 - (r - 1) - row df - column df, are at least v - 1. Nested:
#' r (nrows - 1)(ncols - 1) >= v - 1; shared: v r - r - nrows - ncols + 2 >=
#' v - 1. Necessary for a full-rank design, not sufficient for a given one.
#' @noRd
onestage_feasible <- function(v, r, nrows, ncols, latinize) {
  if (latinize) v * r - r - nrows - ncols + 2 >= v - 1 else r * (nrows - 1) * (ncols - 1) >= v - 1
}

#' Joint searches at `iterations = 200` (phase 2 spec 6.2); each search has its
#' own stage 1 design, repair and joint stage.
#' @noRd
onestage_searches_at_200 <- 10

#' Number of joint searches of an onestage build: max(1, round(10 iterations /
#' 200)) (phase 2 spec 6.2). `iterations` sets the joint stage's effort only.
#' @noRd
onestage_joint_searches <- function(iterations) {
  max(1L, as.integer(round(onestage_searches_at_200 * as.double(iterations) / 200)))
}

#' Admission need of the nested column step of stage 1: the work cap for which
#' `rows_within_columns()` (its rows the ncols columns of each replicate)
#' admits the shape, i.e. its fixed cost (one refresh and ten passes) over 0.8.
#' @noRd
onestage_column_reserve <- function(v, r, nrows, ncols) {
  B <- as.double(r) * ncols
  n <- as.double(v) * r
  pairs <- as.double(r) * nrows * ncols * (ncols - 1) / 2
  fixed <- B^3 + n * r + n * B + 10 * pairs * resolvable_evaluation_work(v, r, B)
  fixed / 0.8 * (1 + 1e-9)
}

#' Whether the nested row step of stage 1 (`blocks_engine(resolvable = TRUE)`
#' with nrows blocks per replicate) searches the shape inside `work_cap`
#' without its own large-design warning: the closed-form two-plot cycle, or
#' `resolvable_searchable()` for a random start (which implies it for a
#' lattice start), and the engine's 1e8 cell limit.
#' @noRd
onestage_row_step_admitted <- function(v, r, nrows, work_cap) {
  B <- as.double(r) * nrows
  if (work_cap <= 0 || v * B > 1e8) return(FALSE)
  (r == 2 && v == 2 * nrows) || resolvable_searchable(v, r, B, FALSE, work_cap)
}

#' Predicted work of one onestage search: stage 1, the repair and one joint
#' search (phase 2 spec 6.1, "Work cap and admission"; Task 8 Step 5).
#'
#' PROVISIONAL (2026-11-20, Task 8 implementer): fitted on t <= 200 only; the
#' controller extends the measurement to t = 300-2000 and finalises this fit
#' and `onestage_work_limit`.
#'
#' Model: 1.5 (E + S). E is the evaluation work the budgets allow: 250 work
#' units (`resolvable_evaluation_work()`) per evaluation of the row step's
#' budget (`resolvable_searches()` searches over the r v (v - 1) / 2 pairs
#' inside the replicates), the column step's (one search nested, 20 shared,
#' over the r nrows ncols (ncols - 1) / 2 pairs inside the rows) and one joint
#' search's (`crossed_default_budget()`). S stands for the accepted swaps: n
#' swaps per search of each step, each costing 4 B^2 + 2 B v for the step's B
#' blocks (rows Br = r nrows nested or nrows shared, columns Bc likewise, and
#' the joint stage both its Bj = Br + Bc blocks and the Row state's Br).
#'
#' Measurement (tools/measure-onestage-work.R, installed build, work_cap =
#' Inf, seeds 1-2): every `os` shape with t <= 200, both codings, the 219
#' shape-codings that pass `onestage_feasible()` (438 runs). Measured work
#' over the model's E + S: median 0.33, 90th percentile 0.93, largest 1.27
#' (30/15/4 nested; ratios above 1 occur only below 4e8 work units), so the
#' factor 1.5 bounds every run. Stage 1 is most of the work (median share
#' 0.94). The largest run used 8.4e9 units (200/100/4 nested) in 1.24 s on a
#' loaded machine; throughput was 4-16e9 units per second. Every measured
#' shape is predicted below 3e10, far below 0.8 of the cap. Not measured:
#' t >= 300, where the budget term E overstates the work (searches end stale
#' long before the budget: median 0.36 of the row step's E at t <= 200).
#'
#' STALE FOR THE JOINT TERM (2026-11-22): the measurement above was taken with
#' a joint budget of 10 passes and 5 escapes. With `onestage_joint_passes` =
#' 400 the joint E is 40 times larger, and a long joint search accepts far
#' more than n swaps, which S does not count: one nested search (seed 1)
#' measured 1.57 of this prediction at 200/2/4, 1.24 at 200/2/3, 1.23 at
#' 100/50/4, 1.19 at 200/100/4, and 0.51-0.72 at 200/8/4, 500/10/4 and the
#' latinized 200/100/4 and 500/10/4. Every `os` shape is still admitted
#' (largest prediction 0.60 of the cap, 500/10/4 latinized). Task 8 Step 5
#' refits the joint term with tools/measure-onestage-work.R.
#' @noRd
onestage_predicted_work <- function(v, r, nrows, ncols, latinize) {
  v <- as.double(v)
  r <- as.double(r)
  n <- v * r
  Br <- if (latinize) nrows else r * nrows
  Bc <- if (latinize) ncols else r * ncols
  Bj <- Br + Bc
  swap <- function(B) 4 * B^2 + 2 * B * v
  row_searches <- if (latinize) 20 else resolvable_searches(Br)
  col_searches <- if (latinize) 20 else 1
  budget <- function(searches, pairs) blocks_engine_default_budget(n, searches, pairs, full_passes = TRUE)
  E <- resolvable_evaluation_work(v, r, Br) * budget(row_searches, r * v * (v - 1) / 2) +
    resolvable_evaluation_work(v, r, Bc) * budget(col_searches, r * nrows * ncols * (ncols - 1) / 2) +
    resolvable_evaluation_work(v, r, Bj) * crossed_default_budget(v, r)
  S <- n * (row_searches * swap(Br) + col_searches * swap(Bc) + swap(Bj) + swap(Br))
  1.5 * (E + S)
}

#' Whether the onestage build admits a shape (phase 2 spec 6.1): the
#' predicted work of stage 1, the repair and one joint search
#' (`onestage_predicted_work()`) is at most 0.8 of `work_cap`, and the stage 1
#' steps' own limits hold at that cap, so no step of an admitted build gives
#' its large-design warning: nested, the row step is admitted with the column
#' step's need reserved (`onestage_row_step_admitted()`,
#' `onestage_column_reserve()`) and the column step's blocks fit the block
#' engine (at most `resolvable_block_limit` blocks and 1e8 cells); shared, the
#' row and column blocks are at most nrows + ncols and the joint stage's B v
#' at most 1e8 cells.
#' @noRd
onestage_admitted <- function(v, r, nrows, ncols, latinize, work_cap = onestage_work_limit) {
  v <- as.double(v)
  r <- as.double(r)
  B_joint <- if (latinize) nrows + ncols else r * (nrows + ncols)
  if (v * B_joint > 1e8) return(FALSE)
  if (!latinize) {
    if (r * ncols > resolvable_block_limit || v * r * ncols > 1e8) return(FALSE)
    if (!onestage_row_step_admitted(v, r, nrows,
                                    work_cap - onestage_column_reserve(v, r, nrows, ncols))) {
      return(FALSE)
    }
  }
  onestage_predicted_work(v, r, nrows, ncols, latinize) <= 0.8 * work_cap
}
