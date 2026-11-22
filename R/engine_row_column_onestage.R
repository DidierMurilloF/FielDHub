# Staged onestage row-column search (phase 2 spec 6.1 and 6.2). Canonical plot
# order of a row-column design: replicate m, then row i, then column j, plot
# p = (m - 1) v + (i - 1) ncols + j. The row block is (m - 1) nrows + i when
# rows are nested in the replicates and i when they are shared; the column
# block is (m - 1) ncols + j nested and j shared.

#' Block columns above which mode 3 builds its dense state from the treatment
#' space (phase 2 spec 5.1c, 6.1 W3), when there are fewer treatments than
#' block columns. Every `os` shape has at most 408 block columns, so their
#' arithmetic is the block-space form's, unchanged.
#' @noRd
onestage_form_blocks <- 500

#' Limits of one onestage build (phase 2 spec 6.1, "Work cap; no size
#' admission", rules W1-W8). No shape is refused for its size: the work cap
#' cuts the search short instead.
#'
#' `onestage_work_limit` (W) is one cap per build, in weighted work: every
#' stage 1, repair and joint search of the build together. Work is the
#' engine's own count (about one unit per multiply-add of the dense algebra,
#' plus a fixed charge per evaluation), times `onestage_work_weight()` of the
#' call. A call that reaches the limit it was given is cut, not refused: it
#' returns the design it holds. So where a build is cut depends on the seed
#' and the inputs only, never on the clock. It stays at the resolvable cap,
#' 2e11, which modes 1 and 2 reach in 8.1-13.3 s on the reference machine;
#' it is not lowered to meet the time rule, because that would cut `os`
#' shapes. With the joint stage's present effort (`onestage_joint_passes`),
#' measured 2026-11-22 (one search, seed 1), a nested search uses 4.7e10
#' units at 200/100/4, 3.2e10 at 200/2/4 and 8.2e10 at 500/10/4, so at
#' `iterations` = 200 the builds of the first two are cut after about 4 and 6
#' of their 10 searches.
#'
#' `onestage_startup_limit` and `onestage_memory_limit` bound what cannot be
#' cut: the start and the finish of an engine call (`onestage_call_work()`)
#' and its dense state, 8 (B^2 + B v) bytes (`onestage_startable()`).
#' `onestage_row_share` and `onestage_column_share` are the parts of W the
#' two steps of stage 1 may use in each search, so the repair and the joint
#' stage of search 1 always have at least half the cap.
#'
#' START VALUES (Task 8 Step 3b): the start-up and memory limits, the shares
#' and the weight are set from measurements in Task 8 Step 5.
#' @noRd
onestage_work_limit <- 2e11
#' @noRd
onestage_startup_limit <- 1e11
#' @noRd
onestage_memory_limit <- 1e9
#' @noRd
onestage_row_share <- 0.3
#' @noRd
onestage_column_share <- 0.2

#' The limits above as one list, so the checks can lower them
#' (`build_row_column_onestage(limits = )`).
#' @noRd
onestage_limits <- function() {
  list(work = onestage_work_limit, startup = onestage_startup_limit,
       memory = onestage_memory_limit, row_share = onestage_row_share,
       column_share = onestage_column_share, form_blocks = onestage_form_blocks)
}

#' Weight of the work of one engine call in the work cap (phase 2 spec 6.1
#' W1): a unit of work takes longer when the matrices are large, so the work
#' of a call with B block columns counts as w times the engine's count. It is
#' 1 up to 500 block columns (every `os` shape) and non-decreasing above, and
#' it is the larger of the search's ratio and that of the start plus the
#' finish. START VALUE: 1 everywhere; Task 8 Step 5 calibrates it.
#' @noRd
onestage_work_weight <- function(mode, B) 1

#' Work of one refresh and of one factorisation of an engine state with B
#' block columns in `factors` block factors (1, or 2 for the joint state),
#' as the engine charges them (src/crossed_space.cpp, src/blocks_engine.cpp).
#' `sizes2` is the sum of the squared block sizes. Block-space form:
#' B^3 + n F^2 r + n F B for both. Treatment-space form (`form`): v^3 +
#' sizes2 + F n (v + 2 B) + 2 B^2 for a refresh and v^3 / 3 + sizes2 + v^2
#' for a factorisation.
#' @noRd
onestage_dense_work <- function(v, r, B, factors, sizes2, form) {
  n <- v * r
  if (!form) {
    work <- B^3 + n * factors^2 * r + n * factors * B
    return(c(refresh = work, factor = work))
  }
  c(refresh = v^3 + sizes2 + factors * n * (v + 2 * B) + 2 * B^2,
    factor = v^3 / 3 + sizes2 + v^2)
}

#' Start and finish work of one engine call of the onestage search, in the
#' engine's units (phase 2 spec 6.1 W3): what the call spends before it can
#' return anything, and what it spends to score its result after its search
#' has ended. Neither can be cut. `B` is the call's block columns and `B_row`
#' (mode 3) the row blocks among them. Upper bounds of what the engine does,
#' checked by `tools/check-onestage-engine.R truncation`.
#'
#' Mode 3 (repair and joint stage, `crossed_search()`), with R and R_row a
#' refresh of the joint and of the Row state and T a factorisation of the
#' joint state (`onestage_dense_work()`):
#' - block-space form: start 2 R + R_row (the joint state, its full-rank test
#'   and the Row state); finish 5 R + 3 R_row (the current design, the result
#'   and its full-rank test, both once more if the result has to be replaced
#'   by the design the joint stage started from, and three Row values);
#' - treatment-space form (v < B and B above `onestage_form_blocks`): start
#'   R + T + R_row; finish R + T + R_row plus one drift check of the running
#'   state, B (3 B + 6 n). An alarm of that check adds one refresh, which is
#'   not reserved (spec W7: one dense step), and so does a result that fails
#'   the full-rank test.
#' The engine charges this finish itself and ends its search while it still
#' fits (`finish_charged` of the call equals it).
#'
#' Modes 1 and 4 (row step; shared column step) and mode 2 (nested column
#' step), with R a refresh of the step's state: the engine scores a start
#' with n + B evaluations and one refresh (modes 1 and 4), or one refresh
#' after `rows_within_columns()` has scored the given design once (mode 2);
#' the start is twice that. The finish is what R reserves for the work the
#' engine does not charge: the final refresh of the call, one drift check,
#' B (3 B + 3 n), and one rank-2 update, 4 B^2 + 2 B v, by which the last
#' accepted swap may pass the limit. Such a call gets its limit less the
#' finish. Not counted and not reserved (src/blocks_engine.cpp,
#' `run_search()`): every search of the call ends with one drift check that
#' is not charged, so a call of s searches does s - 1 more of them than the
#' one reserved (s is 20 in mode 4; in mode 1 it is `resolvable_searches()`,
#' 20 up to 1000 blocks and 1 from 2155; 1 in mode 2), and a check that
#' alarms adds one refresh of that search's design. The checks are small
#' beside the cap: the 19 of a shared row step with 1000 rows and n = 8000
#' are 5.1e8 units, 0.26% of `onestage_work_limit`, and the largest nested
#' row step with 20 searches (1000 blocks, n = 4000) has 2.9e8. The refresh
#' after an alarm is the one dense step of spec 6.1 W7.
#' @noRd
onestage_call_work <- function(mode, v, r, B, B_row = NULL) {
  v <- as.double(v)
  r <- as.double(r)
  B <- as.double(B)
  n <- v * r
  if (mode == 3) {
    form <- v < B && B > onestage_form_blocks
    joint <- onestage_dense_work(v, r, B, 2, n^2 / B_row + n^2 / (B - B_row), form)
    row <- onestage_dense_work(v, r, B_row, 1, n^2 / B_row, form && v < B_row)
    if (!form) {
      return(c(start = 2 * joint[["refresh"]] + row[["refresh"]],
               finish = 5 * joint[["refresh"]] + 3 * row[["refresh"]]))
    }
    scoring <- joint[["refresh"]] + joint[["factor"]] + row[["refresh"]]
    return(c(start = scoring, finish = scoring + B * (3 * B + 6 * n)))
  }
  refresh <- onestage_dense_work(v, r, B, 1, n^2 / B, v < B && B > resolvable_block_limit)[["refresh"]]
  start <- if (mode == 2) 2 * refresh else {
    2 * ((n + B) * resolvable_evaluation_work(v, r, B) + refresh)
  }
  c(start = start, finish = refresh + B * (3 * B + 3 * n) + 4 * B^2 + 2 * B * v)
}

#' Whether an engine call of the onestage search can start and finish (phase
#' 2 spec 6.1 W3): with w its weight, w (start + finish) is at most the
#' start-up limit, the room the stage is sure of in search 1 (the row share,
#' the column share, or what the two leave for the joint call) and `room`;
#' and its dense state, 8 (B^2 + B v) bytes, fits the memory limit. With
#' `room = Inf` the test reads the shape only. `stage` is "row", "column" or
#' "joint"; by default the stage of modes 1, 2 and 3 (mode 4 runs both steps
#' of a shared stage 1, so it must be named).
#' @noRd
onestage_startable <- function(mode, v, r, B, room = Inf, limits = onestage_limits(),
                               stage = c("row", "column", "joint")[mode], B_row = NULL) {
  work <- onestage_call_work(mode, v, r, B, B_row)
  sure <- limits$work * switch(stage, row = limits$row_share, column = limits$column_share,
                               joint = 1 - limits$row_share - limits$column_share)
  onestage_work_weight(mode, B) * sum(work) <= min(limits$startup, sure, room) &&
    8 * (as.double(B)^2 + as.double(B) * v) <= limits$memory
}

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
#' resolvable mode. With fewer treatments than blocks and more blocks than
#' `resolvable_block_limit` the dense state is built from the treatment space
#' (phase 2 spec 5.1c). The work cap is strict: a step that would pass it is
#' not made, so `search$work` never exceeds `work_cap`. What the engine does
#' after its search (one drift check and the final refresh) is not in that
#' count; the onestage search reserves it (`onestage_call_work()`).
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
                           3L, 5L, as.double(budget), time_limit, engine_debug("1"),
                           as.integer(start), as.double(work_cap), 256L, as.double(evaluation_work),
                           4L, integer(), as.integer(block_of) - 1L, as.integer(segment_of) - 1L,
                           engine_form_option(TRUE, v, blocks, resolvable_block_limit) + 2L)
  stop_reason <- c("budget", "stale", "time_limit", "no_start")[out$stop_reason]
  if (identical(stop_reason, "time_limit")) {
    fieldhub_warn("The block search stopped at its time limit of ", time_limit,
                  " seconds; the result may not be reproducible from the seed.",
                  class = "fieldhub_design_warning", call = NULL)
  }
  found <- !anyNA(out$treatments) && !is.na(out$a_efficiency)
  search <- list(evaluations = out$evaluations, work = out$work, stop_reason = stop_reason,
                 work_capped = out$work_capped)
  # For the checks: the largest error of an accepted ratio, and whether the
  # dense state was built from the treatment space.
  if (engine_debug("1")) search$debug_max_error <- out$debug_max_error
  if (engine_debug("treatment_form")) search$treatment_form <- out$treatment_form
  list(treatments = if (found) out$treatments else as.integer(start),
       d_efficiency = if (found) out$d_efficiency else NA_real_,
       a_efficiency = if (found) out$a_efficiency else NA_real_,
       search = search)
}

#' Stage 1 of the onestage search: rows first (phase 2 spec 6.1)
#'
#' An unconstrained one-way search of the rows, swaps within a replicate, then
#' the columns within the rows, swaps only between two plots of the same row of
#' the same replicate, so the row design is untouched. The column step only
#' builds a start for the joint stage.
#' - Nested (`latinize = FALSE`): the rows are resolvable blocks of size ncols
#'   (`blocks_engine(resolvable = TRUE)`, mode 1); the columns are then the
#'   twostage row step on the transposed grid (`rows_within_columns()`, mode
#'   2: its rows the ncols columns of each replicate, its columns our rows,
#'   `effort = 1`). Both run with `admission = FALSE`.
#' - Shared (`latinize = TRUE`): `segmented_blocks()` (mode 4) for the rows
#'   (blocks the nrows shared rows, segments the replicates, start one
#'   `sample.int(nt)` per replicate), then for the columns (blocks the ncols
#'   shared columns, segments the replicate x row cells, start the row design).
#'
#' Work (spec 6.1 W3, W4, W6). No step is refused for its size, and none
#' gives a size warning.
#' - The row step may use `limits$row_share * limits$work` and the column
#'   step `limits$column_share * limits$work`, and never more than is left of
#'   `work_left`. A step with room L is called with the engine limit L / w
#'   less its finish (`onestage_work_weight()`, `onestage_call_work()`), and
#'   w times the engine's count plus the finish is what it has used. A step
#'   that reaches its limit is cut: it returns its best design so far, and
#'   that design is the stage 1 design.
#' - A step that is not startable by its shape (`onestage_startable()`, the
#'   backstop beyond the measured universe) is not called. The row step then
#'   draws random replicates until the rows are connected (up to 10 draws; if
#'   none is, it aborts with class `onestage_infeasible`), and the column
#'   step keeps the row design. Its status is "not_started".
#' - A step that has no room to start and finish inside `work_left`, or that
#'   is called and returns no scored design for any reason (its work limit,
#'   an engine stop without a start, any FielDHub error of the callee, a
#'   callee's `fieldhub_search_refused_warning`, which is muffled): with
#'   `first = TRUE` (search 1 of a build) it is treated as not started, as
#'   above; with `first = FALSE` the function returns `NULL`, and the build
#'   drops that search. It is never an error.
#' - A callee's time-limit warning is muffled and counted (`time_limited`);
#'   the build gives one warning for all its calls.
#'
#' Draws from R's generator; the caller owns the seed.
#'
#' @return `NULL` (see above), or a list with `treatments` (1-based, canonical
#'   plot order, every replicate holding each treatment once), `work` (the
#'   weighted work used), `step_work` and `steps` (both named `row` and
#'   `column`; a step is "complete", "cut" or "not_started") and
#'   `time_limited` (the number of calls that stopped at their time limit).
#' @noRd
onestage_stage1 <- function(nt, nrows, ncols, reps, latinize, work_left, first,
                            limits = onestage_limits()) {
  v <- as.integer(nt)
  r <- as.integer(reps)
  nrows <- as.integer(nrows)
  ncols <- as.integer(ncols)
  if (nrows * ncols != v) {
    fieldhub_abort("nrows * ncols must equal the number of treatments.",
                   data = list(nrows = nrows, ncols = ncols, treatments = nt))
  }
  rep_of <- rep(seq_len(r), each = v)
  row_in <- rep(rep(seq_len(nrows), each = ncols), r)
  col_in <- rep(seq_len(ncols), nrows * r)
  row_block <- if (latinize) row_in else (rep_of - 1L) * nrows + row_in
  time_limited <- 0L
  # One engine call: NULL when the callee refuses or fails; a time-limit
  # warning is counted and muffled.
  quietly <- function(expr) {
    refused <- FALSE
    out <- tryCatch(
      withCallingHandlers(
        expr,
        fieldhub_search_refused_warning = function(w) {
          refused <<- TRUE
          invokeRestart("muffleWarning")
        },
        fieldhub_design_warning = function(w) {
          if (grepl("time limit", conditionMessage(w), fixed = TRUE)) {
            time_limited <<- time_limited + 1L
            invokeRestart("muffleWarning")
          }
        }
      ),
      fieldhub_error = function(e) NULL
    )
    if (refused) NULL else out
  }
  # One step. `run(limit)` makes the engine call and returns NULL or a list
  # with `treatments`, `scored` and `search` (NULL when no engine ran).
  # Returns `status` ("complete", "cut", "not_started": not startable by
  # shape, or "none": no room, or no scored design), `treatments` and `used`.
  step <- function(stage, mode, B, room, run) {
    if (!onestage_startable(mode, v, r, B, limits = limits, stage = stage)) {
      return(list(status = "not_started", used = 0))
    }
    weight <- onestage_work_weight(mode, B)
    work <- onestage_call_work(mode, v, r, B)
    if (weight * sum(work) > room) return(list(status = "none", used = 0))
    out <- quietly(run(room / weight - work[["finish"]]))
    # A call that failed is counted at its whole room (its work is not known).
    if (is.null(out)) return(list(status = "none", used = room))
    ran <- !is.null(out$search$work)
    used <- if (ran) weight * (out$search$work + work[["finish"]]) else 0
    if (!out$scored) return(list(status = "none", used = used))
    cut <- ran && (isTRUE(out$search$work_capped) || identical(out$search$stop_reason, "time_limit"))
    list(status = if (cut) "cut" else "complete", treatments = as.integer(out$treatments), used = used)
  }

  # Row step
  B_row <- if (latinize) nrows else r * nrows
  start <- if (latinize) as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
  rows <- step("row", if (latinize) 4L else 1L, B_row, min(limits$row_share * limits$work, work_left),
               function(limit) {
                 if (latinize) {
                   out <- segmented_blocks(start, row_in, rep_of, v, r, nrows, work_cap = limit)
                   list(treatments = out$treatments, scored = !is.na(out$a_efficiency), search = out$search)
                 } else {
                   # Plot order of the engine output: replicate, row (block),
                   # plot in row: the canonical order, the plot's position
                   # in its row as its column.
                   out <- blocks_engine(v, r, nrows, resolvable = TRUE, work_cap = limit, admission = FALSE)
                   list(treatments = out$Design$treatments, scored = TRUE, search = out$search)
                 }
               })
  if (rows$status == "none" && !first) return(NULL)
  if (rows$status %in% c("none", "not_started")) {
    rows$status <- "not_started"
    rows$treatments <- NULL
    for (attempt in seq_len(10L)) {
      draw <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
      if (resolvable_connected(draw, row_block, v)) {
        rows$treatments <- draw
        break
      }
    }
    if (is.null(rows$treatments)) {
      fieldhub_abort("The onestage stage 1 found no design with connected rows (t = ", nt,
                     ", nrows = ", nrows, ", r = ", reps, ").",
                     class = c("onestage_infeasible", "fieldhub_optimization_error"),
                     data = list(treatments = nt, nrows = nrows, replicates = reps), call = NULL)
    }
  }
  treat <- rows$treatments

  # Column step: the columns within the rows
  B_col <- if (latinize) ncols else r * ncols
  transposed <- order(rep_of, col_in, row_in)  # replicate, column, row
  cols <- step("column", if (latinize) 4L else 2L, B_col,
               min(limits$column_share * limits$work, work_left - rows$used),
               function(limit) {
                 if (latinize) {
                   out <- segmented_blocks(treat, col_in, (rep_of - 1L) * nrows + row_in, v, r, ncols,
                                           work_cap = limit)
                   list(treatments = out$treatments, scored = !is.na(out$a_efficiency), search = out$search)
                 } else {
                   out <- rows_within_columns(treat[transposed], row_in[transposed], v, r, ncols,
                                              effort = 1, work_cap = limit, admission = FALSE)
                   back <- treat
                   back[transposed] <- out$treatments
                   # A start that is already optimal is returned unsearched.
                   list(treatments = back,
                        scored = !is.na(out$a_efficiency) || identical(out$search$stop_reason, "no_swaps"),
                        search = out$search)
                 }
               })
  if (cols$status == "none" && !first) return(NULL)
  if (cols$status %in% c("none", "not_started")) {
    cols$status <- "not_started"
  } else {
    treat <- cols$treatments
  }
  step_work <- c(row = rows$used, column = cols$used)
  list(treatments = as.integer(treat), work = sum(step_work), step_work = step_work,
       steps = c(row = rows$status, column = cols$status), time_limited = time_limited)
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
#' as short as the first. The single-search figures above are from
#' `~/fieldhub-libs/scratch/rc1/d2.R` and `d3.R` (seed 1 of R's default
#' generator, Mersenne-Twister; logs `rc1-d2-100-5-4.log` and `rc1-d3.log` in
#' `~/fieldhub-libs/runs/phase2/`). Mean joint D less the 1.7.0 mean, in 1.7.0 SDs
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
#' 46 s. Small shapes end stale long before the budget, as before. 400 passes
#' is not a plateau: at 100/5/4 and at t = 200 a search still ends on this
#' budget, not stale.
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
#' is resolvable mode's, copied rather than measured.
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
#' exactly as the unrounded values returned here. In the block-space form
#' every swap between two rows is confirmed with Row values from scratch. In
#' the treatment-space form (below) such a value costs min(v, row blocks)^3,
#' so a swap is confirmed only when a running Row value is near its floor,
#' the current design is checked from scratch at checkpoints, and a design
#' found below a floor is replaced by the best one known to meet both
#' (controller ruling R9, 2026-11-22; src/crossed_engine.cpp).
#'
#' Work limit (phase 2 spec 6.1 W2, W3). The start (scoring `start`) is always
#' done. The finish (scoring the result) is charged as a fixed amount,
#' `search$finish_charged`, the mode 3 finish of `onestage_call_work()`, and
#' the repair and the search stop while it still fits: a step that would take
#' the work past `work_cap` less that charge is not made. So `search$work`,
#' which includes the charge, is at most `work_cap` whenever `work_cap` is at
#' least the start plus the finish, and the call then returns a design that
#' is complete in each replicate, scored, not below the Row floors, and full
#' rank if `start` is. `search$repair` is "none" (the start was full rank),
#' "complete", "cut" (the work or time limit ended the repair) or "failed"
#' (it ended on its own without a full-rank design).
#'
#' With fewer treatments than block columns and more than
#' `onestage_form_blocks` block columns the engine builds its dense states
#' from the treatment space and scores the start and the result with fewer
#' factorisations (phase 2 spec 5.1c, 6.1 W3; src/crossed_engine.cpp). The
#' check hook FIELDHUB_ENGINE_DEBUG=treatment_form forces that at any size.
#'
#' @return A list with `treatments` (1-based, plot order as `start`),
#'   `full_rank`, unrounded `row_d`, `row_a`, `joint_d`, `joint_a`,
#'   `joint_log_det` of the result (joint values NA when not full rank), the
#'   Row values of `start` (`stage1_row_d`, `stage1_row_a`), its joint D
#'   (`stage1_joint_d`, 0 when `start` is not full rank) and `search`:
#'   `evaluations`, `work`, `stop_reason`, `joint_swaps` (the swaps the joint
#'   stage applied, escape jumps included), `repair`, `start_work` (the work
#'   of the start), `finish_charged`, `finish_work` (the work of the finish
#'   steps actually done), `repair_swaps`,
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
  # Check hooks (tools/check-onestage-engine.R; src/crossed_engine.cpp): 2L
  # skips the exact Row confirmation of each swap, so the end-of-search
  # comparison can be reached; 3L confirms every swap between rows also in
  # the treatment-space form; 4L confirms none there and checks the Row
  # values after every such swap, so the rollback can be reached.
  hook <- if (engine_debug("slack_floors")) 2L else if (engine_debug("confirm_floors")) 3L else
    if (engine_debug("late_floors")) 4L else 1L
  out <- blocks_engine_cpp(v, r, as.integer(b_rows), hook, as.integer(jumps_per_escape),
                           as.integer(max_stale_escapes), as.double(budget), time_limit,
                           engine_debug("1"),
                           as.integer(start), as.double(work_cap), 256L,
                           as.double(resolvable_evaluation_work(v, r, B)), 3L,
                           as.integer(col_block), as.integer(row_block), integer(),
                           engine_form_option(TRUE, v, B, onestage_form_blocks) + 2L)
  stop_reason <- c("budget", "stale", "time_limit", "no_start")[out$stop_reason]
  if (identical(stop_reason, "time_limit")) {
    fieldhub_warn("The joint row-column search stopped at its time limit of ", time_limit,
                  " seconds; the result may not be reproducible from the seed.",
                  class = "fieldhub_design_warning", call = NULL)
  }
  result <- list(
    treatments = out$treatments, full_rank = out$full_rank,
    row_d = out$row_d, row_a = out$row_a, joint_d = out$joint_d, joint_a = out$joint_a,
    joint_log_det = out$joint_log_det,
    stage1_row_d = out$stage1_row_d, stage1_row_a = out$stage1_row_a,
    stage1_joint_d = out$stage1_joint_d,
    search = list(evaluations = out$evaluations, work = out$work, stop_reason = stop_reason,
                  joint_swaps = out$accepted,
                  repair = c("none", "complete", "cut", "failed")[out$repair_status + 1L],
                  start_work = out$start_work, finish_charged = out$finish_charged,
                  finish_work = out$finish_work,
                  repair_swaps = out$repair_swaps, repair_evaluations = out$repair_evaluations,
                  repair_log_det = c(out$repair_log_det_start, out$repair_log_det),
                  repair_score_error = out$repair_score_error,
                  floor_fallback = out$floor_fallback,
                  refreshes = out$refreshes, drift_events = out$drift_events,
                  work_capped = out$work_capped, debug_max_error = out$debug_max_error)
  )
  # For the checks (tools/check-onestage-engine.R), with any check hook set:
  # whether the treatment-space form was used, the engine's classification of
  # the start (and tr(M) of its joint state, NA when it could not be built),
  # the swaps the joint stage applied, the refreshes that scored the start,
  # the repaired design and the result (joint state, Row state), whether
  # the end-of-search drift check alarmed, and the Row confirmation of ruling
  # R9 (`row_floors`: the swaps between rows the joint stage applied, the Row
  # values from scratch that confirmed a swap, the checkpoints, the rollbacks
  # after a breach, and the largest distance of a running Row value from the
  # value from scratch: log det, absolute, and A denominator, relative).
  if (nzchar(Sys.getenv("FIELDHUB_ENGINE_DEBUG"))) {
    result$search$check <- list(treatment_form = out$treatment_form,
                                start_full_rank = out$start_full_rank,
                                start_trace = out$start_trace, accepted = out$accepted,
                                scoring_refreshes = c(joint = out$scoring_joint,
                                                      row = out$scoring_row),
                                end_alarm = out$end_alarm,
                                row_floors = c(swaps = out$row_swaps,
                                               confirmations = out$row_confirmations,
                                               checkpoints = out$row_checkpoints,
                                               rollbacks = out$row_rollbacks,
                                               drift_ld = out$row_drift_ld,
                                               drift_sum = out$row_drift_sum,
                                               diag = out$row_diag))
  }
  # For tools/measure-onestage-work.R: the seconds of the start and of the
  # finish of the call.
  if (engine_debug("seconds")) {
    result$search$seconds <- c(start = out$start_seconds, finish = out$finish_seconds)
  }
  result
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
