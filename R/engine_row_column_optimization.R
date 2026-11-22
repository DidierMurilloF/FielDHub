# Twostage row step (nested-engine spec section 6.8): improves the rows of a
# twostage row-column design (Level_1 = replicate, Level_2 = column, Level_3 =
# row nested in its replicate) and keeps every column's treatments, with the
# block engine's rows-within-columns search (rows_within_columns()). The
# generator is seeded with `seed`. `iterations` is the search effort: the
# engine's default budget is scaled by iterations / 1000, so the default 1000
# is the full search and 1 the smallest. A design that is not a complete
# row-column grid in every replicate, or has one row per replicate, has no
# column-preserving swap to make and is returned unchanged. Returns the
# improved design, the row-level BlockEfficiencies() report (replicates and
# rows) and the row A-efficiency.
#' @noRd
improve_efficiency <- function(design, iterations, seed) {
  validate_iteration_budget(iterations)
  set.seed(seed)
  best_design <- design
  rep_of <- as.integer(factor(design$Level_1, levels = unique(design$Level_1)))
  row_of <- as.integer(factor(design$Level_3, levels = unique(design$Level_3)))
  column <- paste(design$Level_1, design$Level_2, sep = "\r")
  col_of <- stats::ave(seq_along(column), rep_of,
                       FUN = function(i) match(column[i], unique(column[i])))
  code <- as.integer(factor(design$treatments))
  v <- max(code)
  r <- max(rep_of)
  rows <- max(row_of) %/% r
  cols <- v %/% max(rows, 1L)
  # Engine plot order: replicate by replicate, row by row
  ord <- order(rep_of, row_of, col_of)
  grid_ok <- rows * r == max(row_of) && rows * cols == v && length(code) == v * r &&
    all(tabulate(row_of, max(row_of)) == cols) &&
    all(table(factor(code, levels = seq_len(v)), rep_of) == 1L) &&
    all(rep_of[ord] == rep(seq_len(r), each = v)) &&
    all(row_of[ord] == rep(seq_len(r * rows), each = cols)) &&
    !anyDuplicated(paste(row_of, col_of)) && max(col_of) == cols
  known <- NULL
  if (grid_ok) {
    step <- rows_within_columns(code[ord], col_of[ord], v, r, rows,
                                effort = iterations / 1000)
    value <- design$treatments[match(seq_len(v), code)]  # one label per code
    best_design$treatments[ord] <- value[step$treatments]
    if (!is.na(step$a_efficiency)) {
      known <- data.frame(`D-Efficiency` = c(1, step$d_efficiency),
                          `A-Efficiency` = c(1, step$a_efficiency), check.names = FALSE)
    }
  }
  row_blocks <- best_design[c("Level_1", "Level_3", "plots", "treatments")]
  best_efficiencies <- BlockEfficiencies(row_blocks, known = known,
                                         nested_in_replicates = TRUE, replicates = "Level_1")
  best_a_efficiency <- best_efficiencies$`A-Efficiency`[best_efficiencies$Level == 2]
  return(
    list(
      best_design = best_design, 
      best_efficiencies = best_efficiencies, 
      best_a_efficiency = best_a_efficiency
    )
  )
}

# Builds a resolvable row-column layout with the staged onestage search of
# FielDHub's block engine (phase 2 spec 6.1 and 6.2): per joint search, a
# rows-first stage 1 design (onestage_stage1()), then the joint-rank repair and
# the joint stage (crossed_search()). `searches` is the row_column()
# `iterations`: it sets the number of joint searches asked for
# (onestage_joint_searches()).
#
# - A shape that fails the degrees-of-freedom rule (onestage_feasible())
#   aborts with class onestage_infeasible, which row_column() turns into the
#   twostage fallback. No shape is refused for its size (spec 6.1, "Work cap;
#   no size admission").
# - Otherwise the generator is seeded once with `seed` and the searches run in
#   turn from that stream, each building its own stage 1 design. One work
#   cap, limits$work, covers the whole build in weighted work
#   (onestage_work_weight()): each call gets the work left as its room, and
#   the weighted work it used is taken off.
#   - Search 1 always runs. Where the joint call is not startable by its
#     shape (onestage_startable(), the backstop beyond the measured universe)
#     it is the only search and ends with its stage 1 design.
#   - Search s >= 2 starts only while work is left. It is dropped, and the
#     searches end, when its stage 1 has no room or returns no design
#     (onestage_stage1() gives NULL), or when its joint call has no room to
#     start and finish or returns no scored design. That is never an error.
#   - A search that the cap cut is kept and enters the return rule like any
#     other.
#   - A build that the cap cuts short gives no warning. The engine calls that
#     stopped at their 60 s time limit are counted, and the build gives one
#     fieldhub_design_warning for all of them at its end.
# - Return rule (spec 6.2): of the searches the engine ends full rank, keep
#   those within 1.5e-7 of the best Row D, then of the best Row A (the
#   engine's unrounded values), then take the highest joint D, the earliest
#   on a tie (onestage_best_search()). This is applied to the searches that
#   also pass the report's full-rank test (onestage_joint_efficiency() gives
#   a joint A-efficiency), exactly as if they were filtered first, but that
#   test is a v x v eigen problem, so it is run (once per search, cached)
#   only on the searches the outcome rests on: the one with
#   the best Row D, the one with the best Row A among those kept for Row D,
#   and the winner. If one of them fails it is dropped and the rest are
#   ranked again. If no search is left it aborts with onestage_infeasible.
#
# Returns a data frame with the structure of the pre-change builder
# (tools/reference-onestage.R): Level_1 = Rep, Level_2 = Column, Level_3 = Row,
# plots, treatments, all factors, in plot order replicate, row, column; rows
# and columns are labelled "<rep>_<i>" when nested and "<i>" when shared. Its
# attribute "joint_efficiency" is the onestage_joint_efficiency() of the
# design, for report_efficiency(). Its attribute "onestage_search", which
# row_column() removes, describes the build: `requested` (the searches
# `searches` asks for), `started` (the searches that entered the return
# rule), `complete` (those whose every engine call ended on its own stop, not
# on a work or time limit), `row_step`, `column_step`, `joint_stage` (search
# 1: "complete", "cut" or "not_started"), `repair` (search 1: "none",
# "complete", "cut", or "failed" when it ended on its own without a full-rank
# design), `winner` (the search returned) and `winner_cut` (whether any of
# its calls was cut), `blocks` (the joint stage's block columns), `work`
# (the weighted work used), and, with one value per search that entered the
# return rule, `joint_swaps` (the swaps its joint stage applied, escape jumps
# included) and `joint_gain` (the engine's joint D of its result less that of
# its stage 1 design, which counts as 0 when it is not full rank; NA when the
# result is not full rank). Both are 0 where the joint stage was not started.
# They tell a joint stage that the cap cut after a few swaps from one that
# searched. An onestage_infeasible abort raised after the
# searches carries the same list as its `search` field (`winner` NA).
#' @noRd
build_row_column_onestage <- function(nt, nrows, ncols, reps, latinize, searches, seed,
                                      limits = onestage_limits()) {
  validate_iteration_budget(searches, "searches")
  where <- paste0("t = ", nt, ", nrows = ", nrows, ", r = ", reps)
  if (!onestage_feasible(nt, reps, nrows, ncols, latinize)) {
    fieldhub_abort("The joint row-and-column model has too few residual degrees of freedom (",
                   where, ").",
                   class = c("onestage_infeasible", "fieldhub_optimization_error"), call = NULL)
  }
  set.seed(seed)
  left <- limits$work
  requested <- onestage_joint_searches(searches)
  B_row <- if (latinize) nrows else reps * nrows
  B <- B_row + (if (latinize) ncols else reps * ncols)
  weight <- onestage_work_weight(3L, B)
  need <- weight * sum(onestage_call_work(3L, nt, reps, B, B_row))
  joint_startable <- onestage_startable(3L, nt, reps, B, limits = limits, B_row = B_row)
  time_limited <- 0L
  time_warning <- function() {
    if (time_limited > 0L) {
      fieldhub_warn(time_limited, " engine call(s) of the onestage search stopped at the time ",
                    "limit of 60 seconds; the result may not be reproducible from the seed.",
                    class = "fieldhub_design_warning", call = NULL)
    }
  }
  results <- list()
  cut <- logical()
  first <- list(row_step = "not_started", column_step = "not_started",
                joint_stage = "not_started", repair = "none")
  for (s in seq_len(requested)) {
    if (s > 1L && left <= 0) break
    # An onestage_infeasible abort of search 1 (no design with connected
    # rows) goes on to the caller, after the warning for the calls so far.
    stage1 <- withCallingHandlers(
      onestage_stage1(nt, nrows, ncols, reps, latinize, work_left = left, first = s == 1L,
                      limits = limits),
      onestage_infeasible = function(cnd) time_warning()
    )
    if (is.null(stage1)) break
    time_limited <- time_limited + stage1$time_limited
    left <- left - stage1$work
    if (s == 1L) {
      first$row_step <- stage1$steps[["row"]]
      first$column_step <- stage1$steps[["column"]]
    }
    if (!joint_startable || need > left) {
      # No joint call: a later search is dropped; search 1 ends with its
      # stage 1 design, which the return rule tests for full rank.
      if (s == 1L) {
        results <- list(list(treatments = stage1$treatments, full_rank = TRUE,
                             row_d = 0, row_a = 0, joint_d = 0, stage1_joint_d = 0,
                             search = list(joint_swaps = 0)))
        cut <- TRUE
      }
      break
    }
    joint <- withCallingHandlers(
      crossed_search(stage1$treatments, nt, reps, nrows, ncols, shared = latinize,
                     work_cap = left / weight),
      fieldhub_design_warning = function(w) {
        if (grepl("time limit", conditionMessage(w), fixed = TRUE)) {
          time_limited <<- time_limited + 1L
          invokeRestart("muffleWarning")
        }
      }
    )
    left <- left - weight * joint$search$work
    # A later search whose joint call could not score its start is dropped.
    if (s > 1L && is.na(joint$stage1_row_d)) break
    joint_cut <- isTRUE(joint$search$work_capped) || identical(joint$search$stop_reason, "time_limit")
    results[[length(results) + 1L]] <- joint
    cut[length(results)] <- joint_cut || any(stage1$steps != "complete")
    if (s == 1L) {
      first$joint_stage <- if (joint_cut) "cut" else "complete"
      first$repair <- joint$search$repair
    }
  }
  info <- function(winner) {
    c(list(requested = requested, started = length(results), complete = sum(!cut)), first,
      list(winner = winner, winner_cut = if (is.na(winner)) NA else cut[winner], blocks = B,
           work = limits$work - left,
           joint_swaps = vapply(results, function(x) as.double(x$search$joint_swaps), numeric(1)),
           joint_gain = vapply(results, function(x) as.double(x$joint_d - x$stage1_joint_d),
                               numeric(1))))
  }
  value <- function(name) vapply(results, function(x) as.double(x[[name]]), numeric(1))
  row_d <- value("row_d")
  row_a <- value("row_a")
  joint_d <- value("joint_d")
  candidates <- which(vapply(results, function(x) isTRUE(x$full_rank), logical(1)))
  designs <- vector("list", length(results))
  joint_efficiency <- vector("list", length(results))
  confirmed <- function(i) {
    if (is.null(joint_efficiency[[i]])) {
      designs[[i]] <<- onestage_design_frame(results[[i]]$treatments, nt, nrows, ncols, reps,
                                             latinize)
      joint_efficiency[[i]] <<- onestage_joint_efficiency(designs[[i]])
    }
    !is.na(joint_efficiency[[i]]$Aeffic)
  }
  while (length(candidates) > 0L) {
    # The best Row D search, the best Row A search and the winner, in this order
    best <- candidates[onestage_best_search(row_d[candidates], row_a[candidates],
                                            joint_d[candidates])]
    failed <- unique(best)[!vapply(unique(best), confirmed, logical(1))]
    if (length(failed) == 0L) {
      pick <- best[3L]
      design <- designs[[pick]]
      attr(design, "joint_efficiency") <- joint_efficiency[[pick]]
      attr(design, "onestage_search") <- info(unname(pick))
      time_warning()
      return(design)
    }
    candidates <- setdiff(candidates, failed)
  }
  time_warning()
  fieldhub_abort("No onestage search found a full-rank design (", where, ").",
                 class = c("onestage_infeasible", "fieldhub_optimization_error"),
                 data = list(search = info(NA_integer_)), call = NULL)
}

# The onestage return rule (spec 6.2) on the given searches: keep those
# within 1.5e-7 of the highest Row D, then those within 1.5e-7 of the highest
# Row A among them, then take the highest joint D, the earliest on a tie.
# Returns the positions of the searches the outcome rests on: `row_d`, a
# search with the highest Row D; `row_a`, one with the highest Row A among
# those kept for Row D; and the `winner`. If all three belong to a subset of
# the searches, the winner is also the rule's winner on that subset.
#' @noRd
onestage_best_search <- function(row_d, row_a, joint_d) {
  kept <- seq_along(row_d)
  best_d <- kept[which.max(row_d[kept])]
  kept <- kept[row_d[kept] >= row_d[best_d] - 1.5e-7]
  best_a <- kept[which.max(row_a[kept])]
  kept <- kept[row_a[kept] >= row_a[best_a] - 1.5e-7]
  c(row_d = best_d, row_a = best_a, winner = kept[which.max(joint_d[kept])])
}

# The onestage output frame (structure of tools/reference-onestage.R) of a
# design given as 1-based treatments in plot order replicate, row, column.
#' @noRd
onestage_design_frame <- function(treatments, nt, nrows, ncols, reps, latinize) {
  rep_of <- rep(seq_len(reps), each = nt)
  row_in <- rep(rep(seq_len(nrows), each = ncols), reps)
  col_in <- rep(seq_len(ncols), nrows * reps)
  rows <- if (latinize) as.character(row_in) else paste(rep_of, row_in, sep = "_")
  cols <- if (latinize) as.character(col_in) else paste(rep_of, col_in, sep = "_")
  data.frame(
    Level_1    = factor(rep_of, levels = seq_len(reps)),
    Level_2    = factor(cols, levels = unique(cols)),
    Level_3    = factor(rows, levels = unique(rows)),
    plots      = factor(seq_len(nt * reps)),
    treatments = factor(treatments, levels = seq_len(nt))
  )
}

# Function to calculate the joint (combined) row-by-column A- and D-Efficiency
# of the treatment contrasts, adjusting for the row and column blocking factors
# simultaneously (unlike the per-factor efficiencies from BlockEfficiencies()).
# It uses the design's actual row and column factors, Level_3 and Level_2, which
# are nested within replicates for the two-stage and nested one-stage designs
# and crossed (shared across replicates) for latinized designs, matching how the
# marginal row and column efficiencies are reported. The treatment information
# matrix is built after eliminating those block effects, C = T'(I - P_B)T, via
# qr.resid() (rank-revealing LINPACK QR, so a rank-deficient additive block model
# introduces no spurious dimensions), and the canonical efficiency factors of C
# are returned. The replicate main effect need not be included: for the nested
# coding the replicate contrasts already lie in the row space (Level_3 is
# replicate-specific), and for the crossed coding a complete balanced grid in
# every replicate leaves the replicates orthogonal to the rows, columns and
# treatments jointly; either way adding Reps does not change the treatment
# information matrix. (This relies on each replicate being a complete balanced
# array; it would not hold for unbalanced or partially replicated layouts.) The
# resulting A-Efficiency is that of the full ~Reps + Rows + Cols model of the
# corresponding (nested or crossed) coding.
# Returns NA when the joint model is not estimable (e.g. a design too small to
# have residual degrees of freedom for every treatment contrast).
#' @noRd
row_column_joint_efficiency <- function(design) {
  blocks <- data.frame(Rows = factor(design$Level_3), Cols = factor(design$Level_2))
  TF <- factor(design$treatments)
  v <- nlevels(TF)
  r <- mean(table(TF))
  Tind <- stats::model.matrix(~ TF - 1)             # full treatment indicators
  Bind <- stats::model.matrix(~ Rows + Cols, data = blocks) # row + column block model
  resid <- qr.resid(qr(Bind), Tind)                 # (I - P_B) T, rank-aware
  C <- crossprod(resid)                             # treatment info eliminating blocks
  ev <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values,
             decreasing = TRUE)
  eff <- ev[seq_len(v - 1)] / r                     # canonical efficiency factors
  if (anyNA(eff) || min(eff) < 1e-7) {
    return(list(Deffic = NA_real_, Aeffic = NA_real_))
  }
  list(
    Deffic = round(exp(mean(log(eff))), 7),
    Aeffic = round((v - 1) / sum(1 / eff), 7)
  )
}

# sum_j n_j n_j' / k for one block factor with blocks of one size k: the
# v x v matrix N N' / k of the treatment-by-block counts N, accumulated block
# by block (phase 2 spec 6.1 W7). A block adds 1 for every ordered pair of its
# plots, at the pair's treatments. Blocks of at most v plots are taken several
# at a time as their k^2 plot pairs (about 4e6 pairs per pass); a larger
# block as the outer product of its count vector. The cost is of order
# n min(k, v^2 / k) + v^2 per pass, never the v^2 b of a dense product.
#' @noRd
onestage_block_products <- function(treatments, block, v) {
  b <- max(block)
  k <- length(treatments) %/% b
  plots <- matrix(treatments[order(block)], nrow = k)  # k x b: the plots of each block
  total <- numeric(v * v)
  if (k > v) {
    for (j in seq_len(b)) {
      counts <- tabulate(plots[, j], v)
      total <- total + as.vector(tcrossprod(counts))
    }
  } else {
    first <- rep(seq_len(k), times = k)
    second <- rep(seq_len(k), each = k)
    per_pass <- max(1L, as.integer(4e6 %/% (k * k)))
    for (from in seq.int(1L, b, by = per_pass)) {
      part <- plots[, from:min(b, from + per_pass - 1L), drop = FALSE]
      total <- total + tabulate((part[first, ] - 1L) * v + part[second, ], v * v)
    }
  }
  matrix(total, v, v) / k
}

# Joint row-by-column D- and A-efficiency of an onestage design, as
# row_column_joint_efficiency() reports it, from the v x v form of the
# treatment information (phase 2 spec 5.1 and 6.1 W7):
#   C = r I - N_row N_row' / k_row - N_col N_col' / k_col + (r / v) J,
# with N_row and N_col the treatment-by-row and treatment-by-column counts
# (rows and columns as the design codes them, nested in the replicates or
# shared). It is the C of the QR form because every replicate is a complete
# nrows x ncols grid holding each treatment once, which every onestage design
# is and which this function requires; then X' P_rep X = (r / v) J and the
# rows and columns are orthogonal given the replicates. C 1 = 0, so its
# v - 1 largest eigenvalues over r are the canonical efficiency factors. NA
# when the smallest is below 1e-7, the same test; the same rounding to
# `digits` decimals (NULL: unrounded, for the checks). One symmetric eigen
# problem of order v replaces the QR of the n x (B - 1) block model, which
# took 158 s at 2000/2/4 nested.
#' @noRd
onestage_joint_efficiency <- function(design, digits = 7) {
  treatments <- as.integer(factor(design$treatments))
  v <- max(treatments)
  r <- length(treatments) / v
  rows <- as.integer(factor(design$Level_3))
  cols <- as.integer(factor(design$Level_2))
  C <- -onestage_block_products(treatments, rows, v) - onestage_block_products(treatments, cols, v) + r / v
  diag(C) <- diag(C) + r
  ev <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  eff <- ev[seq_len(v - 1)] / r
  if (anyNA(eff) || min(eff) < 1e-7) {
    return(list(Deffic = NA_real_, Aeffic = NA_real_))
  }
  D <- exp(mean(log(eff)))
  A <- (v - 1) / sum(1 / eff)
  if (is.null(digits)) list(Deffic = D, Aeffic = A) else list(Deffic = round(D, digits), Aeffic = round(A, digits))
}

# Function to calculate and return combined BlockEfficiencies. `nested` says
# whether the reported design's rows and columns are nested in its replicates
# (Level_1): they are then claimed nested and their class verified
# (BlockEfficiencies()); otherwise they are shared across replicates and use
# the general class (spec 6.2). The replicate factor is always claimed. `joint`
# is the joint efficiency of `design` when the caller already has it (the
# onestage builder computes it, with onestage_joint_efficiency(), for its
# return rule); NULL computes it with row_column_joint_efficiency().
#' @noRd
report_efficiency <- function(design, nested = TRUE, joint = NULL) {
  # Calculate row block efficiencies
  row_blocks <- design |> 
    dplyr::select(Level_1, Level_3, plots, treatments)
  row_efficiencies <- BlockEfficiencies(row_blocks, nested_in_replicates = c(TRUE, nested),
                                        replicates = "Level_1")
  row_efficiencies <- row_efficiencies |> 
    dplyr::filter(Level == 2) |> 
    dplyr::mutate(Level = "Row")
  
  # Calculate column block efficiencies
  col_blocks <- design |> 
    dplyr::select(Level_1, Level_2, plots, treatments)
  col_efficiencies <- BlockEfficiencies(col_blocks, nested_in_replicates = c(TRUE, nested),
                                        replicates = "Level_1")
  col_efficiencies <- col_efficiencies |> 
    dplyr::filter(Level == 2) |> 
    dplyr::mutate(Level = "Column")
  
  # Get replication efficiencies
  rep_efficiencies <- BlockEfficiencies(row_blocks, nested_in_replicates = TRUE,
                                        replicates = "Level_1") |> 
    dplyr::filter(Level == 1) |> 
    dplyr::mutate(Level = "Rep")
  
  # Joint (combined) row-by-column efficiency of the standard resolvable
  # row-column analysis model.
  if (is.null(joint)) joint <- row_column_joint_efficiency(design)
  joint_efficiency <- data.frame(
    Level = "Row-by-Column",
    Blocks = NA_integer_,
    `D-Efficiency` = joint$Deffic,
    `A-Efficiency` = joint$Aeffic,
    `A-Bound` = NA_real_,
    check.names = FALSE
  )

  # Combine the results
  combined_efficiencies <- dplyr::bind_rows(
    rep_efficiencies, row_efficiencies, col_efficiencies, joint_efficiency
  )

  return(combined_efficiencies)
}
