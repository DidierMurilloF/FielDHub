# Staged onestage row-column search (phase 2 spec 6.1 and 6.2). Canonical plot
# order of a row-column design: replicate m, then row i, then column j, plot
# p = (m - 1) v + (i - 1) ncols + j. The row block is (m - 1) nrows + i when
# rows are nested in the replicates and i when they are shared; the column
# block is (m - 1) ncols + j nested and j shared.

#' Work cap of one onestage build (phase 2 spec 6.1, "Work cap and
#' admission"): stage 1, the repair and the joint stage together. Set from
#' measurement in phase 2 Task 8.
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
#'   `stop_reason`).
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
       search = list(evaluations = out$evaluations, work = out$work, stop_reason = stop_reason))
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
#' The work cap applies to the whole build: each step gets what the earlier
#' ones left. Stage 1 never warns: a warning from a step (a shape refused by
#' its admission, or a time limit) is an admission defect and becomes an
#' error. Draws from R's generator; the caller owns the seed.
#'
#' @return A list with `treatments` (1-based, canonical plot order, every
#'   replicate holding each treatment once) and `work` (the work units used).
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
  step <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      fieldhub_abort("The onestage stage 1 search was not admitted for t = ", nt, ", nrows = ",
                     nrows, ", r = ", reps, ": ", conditionMessage(w),
                     class = c("fieldhub_optimization_error"),
                     data = list(parent = w), call = NULL)
    })
  }
  used <- function(search) if (is.null(search$work)) 0 else search$work
  rep_of <- rep(seq_len(r), each = v)
  row_in <- rep(rep(seq_len(nrows), each = ncols), r)
  col_in <- rep(seq_len(ncols), nrows * r)
  if (!latinize) {
    rows <- step(blocks_engine(v, r, nrows, resolvable = TRUE, work_cap = work_cap))
    # Plot order of the engine output: replicate, row (block), plot in row,
    # which is the canonical order with the plot's position as its column.
    treat <- as.integer(rows$Design$treatments)
    work <- used(rows$search)
    # Transposed grid: replicate, column, row.
    transposed <- order(rep_of, col_in, row_in)
    cols <- step(rows_within_columns(treat[transposed], row_in[transposed], v, r, ncols,
                                     effort = 1, work_cap = max(work_cap - work, 0)))
    treat[transposed] <- cols$treatments
    work <- work + used(cols$search)
  } else {
    start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
    rows <- step(segmented_blocks(start, row_in, rep_of, v, r, nrows, work_cap = work_cap))
    if (is.na(rows$a_efficiency)) {
      fieldhub_abort("The onestage stage 1 row search found no connected design.",
                     class = "fieldhub_numerical_error",
                     data = list(treatments = nt, nrows = nrows, replicates = reps), call = NULL)
    }
    work <- used(rows$search)
    cols <- step(segmented_blocks(rows$treatments, col_in, (rep_of - 1L) * nrows + row_in, v, r,
                                  ncols, work_cap = max(work_cap - work, 1)))
    treat <- cols$treatments
    work <- work + used(cols$search)
  }
  list(treatments = as.integer(treat), work = work)
}
