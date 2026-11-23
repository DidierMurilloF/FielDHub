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
#' plus a fixed charge per evaluation), weighted by `onestage_work_weight()`.
#' A call that reaches the limit it was given is cut, not refused: it
#' returns the design it holds. So where a build is cut depends on the seed
#' and the inputs only, never on the clock. It stays at the resolvable cap,
#' 2e11; it is not lowered to meet the time rule, because that would cut
#' `os` shapes. With the joint stage's present effort
#' (`onestage_joint_passes`), measured 2026-11-22 (one search, seed 1), a
#' nested search uses 4.7e10 units at 200/100/4, 3.2e10 at 200/2/4 and
#' 8.2e10 at 500/10/4, so at `iterations` = 200 the builds of the first two
#' are cut after about 4 and 6 of their 10 searches.
#'
#' `onestage_startup_limit` and `onestage_memory_limit` bound what cannot be
#' cut: the start and the finish of an engine call (`onestage_call_work()`,
#' weighted) and its memory (`onestage_startable()`).
#' `onestage_row_share` and `onestage_column_share` are the parts of W the
#' two steps of stage 1 may use in each search, so the repair and the joint
#' stage of search 1 always have at least half the cap.
#'
#' Measured 2026-11-22 (Task 8 Step 5; reference machine, idle, one run at a
#' time, installed build of d552f67; `tools/measure-onestage-work.R`, runs
#' and logs in `~/fieldhub-libs/runs/phase2/t8s5/`), with a weight of 1
#' everywhere and these shares.
#'
#' Time rule (spec 6.1 W7), engine time of whole builds at `iterations` =
#' 200, first engine call to last. Builds whose calls all have at most 500
#' block columns, so a weight of 1: the three `os` shapes the cap cuts,
#' 200/100/4, 200/2/4 and 200/100/3 nested, 14.2-14.5 s, 14.1 s and 12.8-12.9
#' s; nested 2000/20/4 13.0-13.1 s; every other one at most 12.3 s (t = 500,
#' 1000, 1500 and 2000; search 1 of nested 2000/100/4, which no build run
#' held, took 14.3 s in `what=stages`). So W stays 2e11. Three builds with larger calls broke the
#' rule at weight 1: nested 1000/500/4 15.5 s, nested 1500/750/3 15.2 s and
#' latinized 2000/1000/3 15.5 and 14.7 s (two seeds). The weights below are
#' for them. Timed again with the weights and these limits (commit d7d544c,
#' same machine and runs, `~/fieldhub-libs/runs/phase2/t8s5/confirm/`): the
#' longest of 102 builds takes 14.16 s (`os` 200/2/4 nested; 200/100/4
#' nested 14.1 s), and the three builds above take 13.7 s, 12.1 s and 10.9
#' to 11.0 s. No `os` step of stage 1 and no repair is cut in search 1 there
#' either.
#'
#' Start and finish (`what=startup`): the slowest are the joint call of
#' nested 2000/1000/4, 2.23 s for 3.57e10 units (4008 block columns), and
#' the row and column steps with 4000 blocks (nested 2000/1000/4 and
#' 2000/2/4), 1.67 s for 2.45e10 units. `onestage_startup_limit` is the
#' smallest value of two digits that holds the largest weighted start plus
#' finish of the universe, 5.58e10 (that joint call, 3.82e10 by the formula,
#' times 1.46).
#'
#' Shares: no `os` shape has a step of stage 1 cut in search 1 (438 builds,
#' two seeds; the largest row step uses 3.8% of W, the largest column step
#' 1.2%), and no universe shape has a repair cut in search 1 (570 builds).
#' With the weights the largest need of a stage, as a part of the room it is
#' sure of: row step 0.46 (2.73e10 of 6e10), column step 0.61 (2.46e10 of
#' 4e10, nested 2000/2/4), joint call 0.56 (5.58e10 of 1e11).
#'
#' Memory: peak resident memory of one R process (`/usr/bin/time -l`), which
#' holds about 100 MB before any engine call. S is the dense state of the
#' joint call, 8 (B^2 + B v) bytes.
#'
#' | Shape | S, MB | Engine calls alone | Whole build | Whole call |
#' |---|---|---|---|---|
#' | 2000/1000/4 nested | 193 | 1310 | 1378 | 1646 |
#' | 2000/2/4 nested | 193 | 1052 | 1114 | 1153 |
#' | 2000/1000/3 nested | 120 | 885 | | 1067 |
#' | 1500/750/4 nested | 108 | 777 | | 941 |
#' | 1000/500/4 nested | 48 | 388 | 452 | 570 |
#' | 500/250/4 nested | 12 | 160 | | 260 |
#' | 2000/1000/4 latinized | 24 | 226 | 420 | 581 |
#'
#' The engine calls take 4.9 to 6.5 times S above the 100 MB (the engine
#' holds several matrices of the order of the state). So
#' `onestage_memory_factor` is 6.5, the test of `onestage_startable()` is
#' 6.5 S <= `onestage_memory_limit`, and the limit is the smallest value of
#' two digits that holds the universe: 6.5 x 1.926e8 = 1.25e9 bytes. A whole
#' `row_column()` call at that size peaked at 1.65e9 bytes.
#'
#' Whole `row_column(iterations = 200)` calls (`what=call`), seconds, at
#' weight 1 and then in the confirm run: latinized 2000/20/4 14.3 and 14.3,
#' nested 1000/2/4 14.6 and 12.7, nested 2000/20/4 17.2 and 17.0, latinized
#' 2000/2/4 18.9 and 15.8, nested 2000/2/4 23.1 and 15.7, nested 2000/1000/4
#' 33.7 and 18.7. The twelve calls of the confirm run (t = 200 to 2000) take
#' 12.1 to 18.7 s. Not all are inside 15 s (60 s at a quarter of the speed),
#' so the documentation says that the work of the search is capped, not
#' that the call ends in a bounded time. Of the 33.7 s, 14.2 s were the
#' singular value decomposition of the Row report, which is now read from a
#' Gram matrix (`block_incidence_gram_values()`); in the confirm run that
#' call spends 11.0 s in the engine, 1.5 s on the joint efficiency, 3.2 s on
#' the report and 3.0 s on the concurrence tables.
#' @noRd
onestage_work_limit <- 2e11
#' @noRd
onestage_startup_limit <- 5.6e10
#' @noRd
onestage_memory_limit <- 1.3e9
#' @noRd
onestage_memory_factor <- 6.5
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

#' Weights of the work of one engine call in the work cap (phase 2 spec 6.1
#' W1; controller ruling R10, 2026-11-22): a unit of work takes longer when
#' the matrices are large, and by a different factor in the search of a call
#' than in its start and finish. So the call's work counts in two parts:
#' `fixed` times its start and finish (`onestage_call_work()`) and `search`
#' times the rest of the engine's count (`onestage_weighted_work()`). Both
#' are 1 up to 500 block columns (every `os` shape) and non-decreasing
#' above, steps at 750, 1000, 2000, 3000 and 4000 block columns of the call.
#'
#' Each weight is the seconds per unit of that part, the slowest call of the
#' size class, over the seconds per unit of the slowest call of the same
#' mode and part with at most 500 block columns (ruling R10.3), rounded up
#' to two decimals and never below the class before. For the start and
#' finish of mode 3 the unit depends on the form of the dense state
#' (`onestage_dense_work()`), so the treatment-space form (v < B) and the
#' block-space form have their own row.
#'
#' Measured 2026-11-22 (Task 8 Step 5; `tools/measure-onestage-work.R`,
#' `what=rate` for the search and `what=startup`, median of 5 seeds, for the
#' start and finish; 88 and 96 shapes and codings at t = 200 to 2000;
#' `~/fieldhub-libs/runs/phase2/t8s5/`). Nanoseconds per unit, fastest to
#' slowest call:
#'
#' | Mode, part | B <= 500 | 501-750 | 751-1000 | 1001-2000 | 2001-3000 | 3001-4000 | 4008 |
#' |---|---|---|---|---|---|---|---|
#' | 1 search | .019-.048 | .045-.050 | .048 | .050-.057 | .060-.070 | .071 | |
#' | 2 search | .036-.067 | .052-.054 | .055 | .056-.060 | .063-.070 | .070 | |
#' | 4 search | .018-.038 | .035-.043 | .041-.054 | | | | |
#' | 3 search | .055-.081 | .066-.078 | .066-.084 | .062-.089 | .075-.084 | .070-.092 | .070-.077 |
#' | 1 start, finish | .061 | .060-.061 | .061 | .061-.062 | .065-.067 | .068 | |
#' | 2 start, finish | .061-.079 | .060 | .061 | .061-.062 | .065-.067 | .068 | |
#' | 4 start, finish | .062-.072 | .065-.068 | .066-.069 | | | | |
#' | 3 start, finish, block form | .044-.052 | .045-.048 | .050-.055 | .051-.055 | | | |
#' | 3 start, finish, treatment form | | .075 | .061-.062 | .058-.069 | .058-.065 | .057-.064 | .060-.063 |
#'
#' - The baseline of mode 4 leaves out the column steps of a design with two
#'   columns (2 blocks), .085-.162: they use at most 1.06e9 units of a
#'   build (latinized 2000/1000/4, 0.13 s; confirm run).
#' - Steps with less than 5 ms of work are left out (the clock of the steps
#'   has 1 ms).
#' - The search of a call depends on r and on the coding as much as on its
#'   size: at 1002 block columns mode 3 takes .062 (latinized 2000/1000/2)
#'   to .089 (latinized 2000/2/4).
#' - Check (ruling R10.2): for every measured call above 500 block columns
#'   and each part, the seconds per weighted unit are at most the baseline
#'   and at least the baseline over 1.5. The lowest is the baseline over
#'   1.45 (mode 3 search, latinized 2000/1000/2), then over 1.33 (mode 2
#'   start and finish) and 1.32.
#' - No call is measured above 4008 block columns (modes 1 and 2: 4000;
#'   mode 4: 1000; mode 3 block form: 1002): the last weight is kept there.
#' @noRd
onestage_weight_breaks <- c(500, 750, 1000, 2000, 3000, 4000)
#' @noRd
onestage_weights <- list(
  search = list(c(1, 1.05, 1.05, 1.20, 1.48, 1.49, 1.49),   # mode 1
                c(1, 1.00, 1.00, 1.00, 1.06, 1.06, 1.06),   # mode 2
                c(1, 1.00, 1.05, 1.11, 1.11, 1.14, 1.14),   # mode 3
                c(1, 1.15, 1.42, 1.42, 1.42, 1.42, 1.42)),  # mode 4
  fixed = list(c(1, 1.00, 1.00, 1.02, 1.10, 1.11, 1.11),
               c(1, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00),
               c(1, 1.00, 1.06, 1.08, 1.08, 1.08, 1.08),    # mode 3, block-space form
               c(1, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00)),
  fixed_treatment_form = c(1, 1.46, 1.46, 1.46, 1.46, 1.46, 1.46)  # mode 3, v < B
)

#' @return `c(search = , fixed = )` for a call of `mode` with `v` treatments
#'   and `B` block columns.
#' @noRd
onestage_work_weight <- function(mode, v, B) {
  class <- findInterval(B, onestage_weight_breaks, left.open = TRUE) + 1L
  fixed <- if (mode == 3 && v < B) onestage_weights$fixed_treatment_form else onestage_weights$fixed[[mode]]
  c(search = onestage_weights$search[[mode]][class], fixed = fixed[class])
}

#' The engine's work limit for a call with the weighted room `room` (phase 2
#' spec 6.1 W1, W3; ruling R10.2), and the weighted work of a call from the
#' engine's count.
#'
#' With S and F the start and the finish of `onestage_call_work()`: the
#' first S units of the engine's count are its start, which with F counts
#' `fixed` times; the rest is the search and counts `search` times. So a
#' call with room L may count S + (L - fixed (S + F)) / search units before
#' its finish, and never uses more than L. `count` is the engine's
#' `search$work`: for mode 3 it includes the finish, which the engine
#' charges itself; modes 1, 2 and 4 do not count their finish, so their
#' limit leaves it out and it is added to what they used. Where the two
#' weights are equal (always up to 500 block columns) the limit is L / w
#' (less F) and the work w times the count (plus F), as before the ruling.
#' `room` must hold the weighted start and finish.
#' @noRd
onestage_engine_limit <- function(mode, v, r, B, room, B_row = NULL) {
  weight <- onestage_work_weight(mode, v, B)
  work <- onestage_call_work(mode, v, r, B, B_row)
  counted_finish <- if (mode == 3) 0 else work[["finish"]]
  if (weight[["search"]] == weight[["fixed"]]) return(room / weight[["search"]] - counted_finish)
  work[["start"]] + (room - weight[["fixed"]] * sum(work)) / weight[["search"]] +
    work[["finish"]] - counted_finish
}
#' @noRd
onestage_weighted_work <- function(mode, v, r, B, count, B_row = NULL) {
  weight <- onestage_work_weight(mode, v, B)
  work <- onestage_call_work(mode, v, r, B, B_row)
  if (weight[["search"]] == weight[["fixed"]]) {
    return(weight[["search"]] * (if (mode == 3) count else count + work[["finish"]]))
  }
  before_finish <- if (mode == 3) max(0, count - work[["finish"]]) else count
  weight[["fixed"]] * (min(before_finish, work[["start"]]) + work[["finish"]]) +
    weight[["search"]] * max(0, before_finish - work[["start"]])
}

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
#' 2 spec 6.1 W3): its weighted start plus finish (`onestage_call_work()`,
#' the `fixed` weight of `onestage_work_weight()`) is at most the start-up
#' limit, the room the stage is sure of in search 1 (the row share, the
#' column share, or what the two leave for the joint call) and `room`; and
#' its memory, `onestage_memory_factor` times the dense state of 8 (B^2 +
#' B v) bytes, fits the memory limit. With `room = Inf` the test reads the
#' shape only. `stage` is "row", "column" or "joint"; by default the stage
#' of modes 1, 2 and 3 (mode 4 runs both steps of a shared stage 1, so it
#' must be named).
#' @noRd
onestage_startable <- function(mode, v, r, B, room = Inf, limits = onestage_limits(),
                               stage = c("row", "column", "joint")[mode], B_row = NULL) {
  work <- onestage_call_work(mode, v, r, B, B_row)
  sure <- limits$work * switch(stage, row = limits$row_share, column = limits$column_share,
                               joint = 1 - limits$row_share - limits$column_share)
  onestage_work_weight(mode, v, B)[["fixed"]] * sum(work) <= min(limits$startup, sure, room) &&
    onestage_memory_factor * 8 * (as.double(B)^2 + as.double(B) * v) <= limits$memory
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

#' Failed escapes in a row that end one search of the nested row step of
#' stage 1 (`blocks_engine(resolvable = TRUE)`, mode 1; phase 2 spec 6.1).
#' The evaluation budget of the step is unchanged: resolvable mode's pool of
#' 20 searches of ten full passes.
#'
#' Work. Where the step already ended on its budget (21 nested `os` shapes,
#' among them every t = 200 shape with 25 or 100 rows that is searched,
#' 300/10/4 and 500/10/4) its work and time are the same as with five
#' escapes. Elsewhere they are not: with five escapes the step
#' ended stale below its budget on 86 of the 107 nested `os` shapes it
#' searches (at 0.03 to 0.97 of the budget), and with 40 it uses the whole
#' budget on 67 of them, a median of 3.5 times the work (up to 6.7 times on
#' small shapes, where the step takes milliseconds). The Row values of those
#' shapes do not change. The largest increases in the share of
#' `onestage_work_limit` one row step uses: 500/10/2 3.8% to 6.3%, 500/10/3
#' 7.7% to 9.5%, 200/2/4 0.8% to 2.0%; the largest row step is still 12.9%
#' (500/10/4), and no step is cut by its share. A build that the work cap
#' cuts short can therefore lose a joint search: nested 500/10/2 at
#' `iterations` = 200, seed 1, completes 5 searches where it completed 6,
#' and 1000/10/2 starts 1 where it started 2 (review of 7160823;
#' `~/fieldhub-libs/scratch/rv-rc3/rowstep.R`, 2 or 3 seeds a shape).
#'
#' Measured 2026-11-22 (root cause of the nested Row shortfall at t = 200 and
#' `iterations` = 200; scratch `~/fieldhub-libs/scratch/rc3/d2.R`, logs
#' `rc3-d2-*.log` in `~/fieldhub-libs/runs/phase2/`). With resolvable mode's
#' five escapes the step ended on its budget after 2 to 7 of its 20 searches
#' on every nested shape with t = 200, each search as shallow as the one
#' before, and `iterations` does not raise the effort of stage 1 (spec 6.2).
#' So at `iterations` = 200 the Row values of a build were the best of ten
#' such steps, while the 1.7.0 Row values rise with `iterations`: the mean
#' Row D and Row A ended 0.1 to 0.4 of a 1.7.0 SD below the 1.7.0 mean at
#' 200/25/3, 200/100/3, 200/25/4 and 200/100/4. With the same budget and
#' work, one deeper search does better than several shallow ones. Mean Row D
#' of the step alone, 12 seeds, less the value with 5 escapes, in units of
#' 1e-6 (Row A in brackets):
#'
#' | Nested shape | 10 | 20 | 40 | 80 |
#' |---|---|---|---|---|
#' | 200/25/3 | 2.2 (9) | 2.4 (10) | 4.3 (16) | 4.3 (16) |
#' | 200/100/3 | 1.1 (9) | 1.3 (11) | 2.2 (19) | 2.1 (19) |
#' | 200/25/4 | 3.0 (10) | 4.7 (16) | 4.7 (16) | 4.7 (16) |
#' | 200/100/4 | 1.1 (9) | 1.6 (12) | 1.9 (13) | 1.9 (13) |
#'
#' From 40 on the first search uses the whole budget there, so a larger
#' value changes nothing. Through `row_column()`, `iterations` = 200, 50
#' seeds, mean Row D less the 1.7.0 mean in 1.7.0 SDs, before and after:
#' 200/25/3 -0.36 and +0.67; 200/100/3 -0.23 and +0.16; 200/25/4 -0.10 and
#' +0.52; 200/100/4 -0.10 and +0.12. The shared row step (mode 4) keeps five
#' escapes: its Row values pass the Row rules on every `os` shape, none
#' near the margin.
#'
#' Not adopted: one jump per escape instead of three in this step
#' (`jumps_per_escape`). Measured 2026-11-23 it raises Row further at less
#' work (Row A at 200/100/3, `iterations` = 200: bootstrap margin -0.37 to
#' +6.96) and creates no failure outside the accepted-loss rule on 189
#' nested shape rows, but three checks of `tools/check-onestage-engine.R`
#' then fail on their fixed seeds, so it waits for a ruling (RC3 report).
#' @noRd
onestage_row_stale_escapes <- 40L

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
#'   `work_left`. A step with room L is called with the engine limit of
#'   `onestage_engine_limit()` (L / w less its finish where one weight w
#'   holds for the call), and `onestage_weighted_work()` of the engine's
#'   count is what it has used. A step that reaches its limit is cut: it
#'   returns its best design so far, and that design is the stage 1 design.
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
    work <- onestage_call_work(mode, v, r, B)
    if (onestage_work_weight(mode, v, B)[["fixed"]] * sum(work) > room) {
      return(list(status = "none", used = 0))
    }
    out <- quietly(run(onestage_engine_limit(mode, v, r, B, room)))
    # A call that failed is counted at its whole room (its work is not known).
    if (is.null(out)) return(list(status = "none", used = room))
    ran <- !is.null(out$search$work)
    used <- if (ran) onestage_weighted_work(mode, v, r, B, out$search$work) else 0
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
                   out <- blocks_engine(v, r, nrows, resolvable = TRUE, work_cap = limit, admission = FALSE,
                                        max_stale_escapes = onestage_row_stale_escapes)
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
