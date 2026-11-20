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
# `iterations`: it sets the number of joint searches only
# (onestage_joint_searches()); stage 1 and the repair always run in full.
#
# - A shape that onestage_admitted() refuses aborts with class
#   onestage_not_admitted (also onestage_infeasible); a shape that fails the
#   degrees-of-freedom rule (onestage_feasible()) aborts with
#   onestage_infeasible. row_column() turns both into the twostage fallback.
# - Otherwise the generator is seeded once with `seed` and the searches run in
#   turn from that stream, each building its own stage 1 design. One work cap,
#   onestage_work_limit, covers the whole build: each engine call gets the
#   work left, and once it is spent no further search starts. A later search
#   whose stage 1 would not finish inside the work left is not run; the first
#   search's is an admission defect and aborts as not admitted.
# - Return rule (spec 6.2): of the searches whose design has a joint
#   A-efficiency (row_column_joint_efficiency(), the report's full-rank test),
#   keep those within 1.5e-7 of the best Row D, then of the best Row A (the
#   engine's unrounded values), then return the highest joint D, the earliest
#   on a tie. If none is full rank it aborts with onestage_infeasible
#   (onestage_not_admitted when the work cap cut a search short).
#
# Returns a data frame with the structure of the pre-change builder
# (tools/reference-onestage.R): Level_1 = Rep, Level_2 = Column, Level_3 = Row,
# plots, treatments, all factors, in plot order replicate, row, column; rows
# and columns are labelled "<rep>_<i>" when nested and "<i>" when shared.
#' @noRd
build_row_column_onestage <- function(nt, nrows, ncols, reps, latinize, searches, seed) {
  validate_iteration_budget(searches, "searches")
  where <- paste0("t = ", nt, ", nrows = ", nrows, ", r = ", reps)
  if (!onestage_admitted(nt, reps, nrows, ncols, latinize)) {
    fieldhub_abort("The onestage search does not admit this design (", where, ").",
                   class = c("onestage_not_admitted", "onestage_infeasible",
                             "fieldhub_optimization_error"), call = NULL)
  }
  if (!onestage_feasible(nt, reps, nrows, ncols, latinize)) {
    fieldhub_abort("The joint row-and-column model has too few residual degrees of freedom (",
                   where, ").",
                   class = c("onestage_infeasible", "fieldhub_optimization_error"), call = NULL)
  }
  set.seed(seed)
  left <- onestage_work_limit
  results <- list()
  cut_short <- FALSE
  for (s in seq_len(onestage_joint_searches(searches))) {
    if (left <= 0) break
    stage1 <- tryCatch(onestage_stage1(nt, nrows, ncols, reps, latinize, work_cap = left),
                       onestage_not_admitted = function(cnd) cnd)
    if (inherits(stage1, "onestage_not_admitted")) {
      if (s == 1L) {
        fieldhub_abort(conditionMessage(stage1),
                       class = c("onestage_not_admitted", "onestage_infeasible",
                                 "fieldhub_optimization_error"),
                       data = list(parent = stage1), call = NULL)
      }
      break
    }
    left <- left - stage1$work
    if (left <= 0) {
      if (s == 1L) {
        fieldhub_abort("The onestage stage 1 used the whole work cap (", where, ").",
                       class = c("onestage_not_admitted", "onestage_infeasible",
                                 "fieldhub_optimization_error"), call = NULL)
      }
      break
    }
    joint <- crossed_search(stage1$treatments, nt, reps, nrows, ncols, shared = latinize,
                            work_cap = left)
    left <- left - joint$search$work
    results[[s]] <- joint
    if (isTRUE(joint$search$work_capped)) {
      cut_short <- TRUE
      break
    }
  }
  designs <- lapply(results, function(x) onestage_design_frame(x$treatments, nt, nrows, ncols,
                                                               reps, latinize))
  kept <- which(vapply(seq_along(results), function(i) {
    isTRUE(results[[i]]$full_rank) &&
      !is.na(row_column_joint_efficiency(designs[[i]])$Aeffic)
  }, logical(1)))
  if (length(kept) == 0L) {
    fieldhub_abort("No onestage search found a full-rank design (", where, ").",
                   class = c(if (cut_short) "onestage_not_admitted", "onestage_infeasible",
                             "fieldhub_optimization_error"), call = NULL)
  }
  value <- function(name) vapply(results[kept], function(x) x[[name]], numeric(1))
  row_d <- value("row_d")
  kept <- kept[row_d >= max(row_d) - 1.5e-7]
  row_a <- value("row_a")
  kept <- kept[row_a >= max(row_a) - 1.5e-7]
  joint_d <- value("joint_d")
  designs[[kept[which.max(joint_d)]]]
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
# resulting A-Efficiency equals the A-Efficiency blocksdesign reports for the
# full ~Reps + Rows + Cols model of the corresponding (nested or crossed) coding.
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

# Function to calculate and return combined BlockEfficiencies. `nested` says
# whether the reported design's rows and columns are nested in its replicates
# (Level_1): they are then claimed nested and their class verified
# (BlockEfficiencies()); otherwise they are shared across replicates and use
# the general class (spec 6.2). The replicate factor is always claimed.
#' @noRd
report_efficiency <- function(design, nested = TRUE) {
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
  joint <- row_column_joint_efficiency(design)
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
