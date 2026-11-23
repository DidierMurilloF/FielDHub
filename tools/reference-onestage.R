# The pre-change onestage builder (phase 1 code), kept verbatim for paired
# timing against the new engine. It calls blocksdesign::design(), so it needs
# blocksdesign installed; do not use it in the package. Source it after
# library(FielDHub) so it can reach FielDHub's internals (validate_iteration_budget(),
# fieldhub_abort()), e.g. environment(build_row_column_onestage) <- asNamespace("FielDHub").

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
