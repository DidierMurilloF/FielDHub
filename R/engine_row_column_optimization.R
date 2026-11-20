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
  best_efficiencies <- BlockEfficiencies(row_blocks, known = known)
  best_a_efficiency <- best_efficiencies$`A-Efficiency`[best_efficiencies$Level == 2]
  return(
    list(
      best_design = best_design, 
      best_efficiencies = best_efficiencies, 
      best_a_efficiency = best_a_efficiency
    )
  )
}

# Function to build a resolvable row-column layout in a single optimization
# stage using blocksdesign::design() (Genstat "onestage" method), instead of
# the two-stage method (columns first, then the block engine's row search).
# Rows and columns are optimized jointly.
# Returns a data frame with the same structure as the two-stage best_design
# (Level_1 = Rep, Level_2 = Column, Level_3 = Row, plots, treatments) so the
# downstream field book construction is unchanged.
#' @noRd
build_row_column_onestage <- function(nt, nrows, ncols, reps, latinize, searches, seed) {
  validate_iteration_budget(searches, "searches")
  N <- nt * reps
  trt  <- gl(nt, reps, N)      # each treatment replicated reps times
  Reps <- gl(reps, nt, N)      # replicate blocking factor
  RowW <- gl(nrows, ncols, N)  # row within replicate (1..nrows)
  ColW <- gl(ncols, 1, N)      # column within replicate (1..ncols)
  if (latinize) {
    # Crossed (latinized) row/column effects shared across replicates
    Rows <- factor(RowW)
    Cols <- factor(ColW)
  } else {
    # Nested row/column effects within each replicate (standard resolvable RC)
    Rows <- factor(paste(Reps, RowW, sep = "_"))
    Cols <- factor(paste(Reps, ColW, sep = "_"))
  }
  optd <- tryCatch(
    blocksdesign::design(
      treatments = data.frame(treatments = trt),
      blocks     = data.frame(Reps = Reps, Rows = Rows, Cols = Cols),
      searches   = searches,
      seed       = seed
    ),
    error = function(e) e
  )
  if (inherits(optd, "error")) {
    # The joint row-and-column model is (most often) over-parameterized for a
    # design this small. Signal a recoverable, classed condition that carries the
    # underlying blocksdesign message, so the caller can fall back to the
    # two-stage method while still surfacing (not masking) an unrelated failure.
    fieldhub_abort(
      conditionMessage(optd),
      class = c("onestage_infeasible", "fieldhub_optimization_error"),
      data = list(parent = optd),
      call = NULL
    )
  }
  des <- optd$Design
  # Map to the two-stage layout structure (Level_2 = columns, Level_3 = rows),
  # keeping the design() ordering (Reps, Rows, Cols).
  best_design <- data.frame(
    Level_1    = factor(des$Reps, levels = unique(des$Reps)),
    Level_2    = factor(des$Cols, levels = unique(des$Cols)),
    Level_3    = factor(des$Rows, levels = unique(des$Rows)),
    plots      = des$plots,
    treatments = des$treatments
  )
  return(best_design)
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

# Function to calculate and return combined BlockEfficiencies
#' @noRd
report_efficiency <- function(design) {
  # Calculate row block efficiencies
  row_blocks <- design |> 
    dplyr::select(Level_1, Level_3, plots, treatments)
  row_efficiencies <- BlockEfficiencies(row_blocks)
  row_efficiencies <- row_efficiencies |> 
    dplyr::filter(Level == 2) |> 
    dplyr::mutate(Level = "Row")
  
  # Calculate column block efficiencies
  col_blocks <- design |> 
    dplyr::select(Level_1, Level_2, plots, treatments)
  col_efficiencies <- BlockEfficiencies(col_blocks)
  col_efficiencies <- col_efficiencies |> 
    dplyr::filter(Level == 2) |> 
    dplyr::mutate(Level = "Column")
  
  # Get replication efficiencies
  rep_efficiencies <- BlockEfficiencies(row_blocks) |> 
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
