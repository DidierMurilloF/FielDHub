# Function to randomly swap a pair of treatments within a random 
# level of Level_2 for all levels of Level_1
#' @noRd
swap_treatments <- function(df) {
  # Split the dataframe by Level_1
  df_split <- split(df, df$Level_1)
  
  # Initialize an empty dataframe to store the results
  result <- data.frame()
  
  # Loop through each level of Level_1
  for (level1 in names(df_split)) {
    df_level1 <- df_split[[level1]]
    
    # Get the unique values of Level_2
    unique_levels2 <- unique(df_level1$Level_2)
    
    # Randomly select one level of Level_2
    random_level2 <- sample(unique_levels2, 1)
    df_level2 <- df_level1[df_level1$Level_2 == random_level2, ]
    
    if (nrow(df_level2) >= 2) {
      # Randomly select two different rows to swap treatments
      rows_to_swap <- sample(1:nrow(df_level2), 2)
      temp <- df_level2$treatments[rows_to_swap[1]]
      df_level2$treatments[rows_to_swap[1]] <- df_level2$treatments[rows_to_swap[2]]
      df_level2$treatments[rows_to_swap[2]] <- temp
    }
    
    # Combine the modified dataframe with the rest
    df_level1[df_level1$Level_2 == random_level2, ] <- df_level2
    result <- rbind(result, df_level1)
  }
  
  return(result)
}

# Function to improve A-Efficiency for Level 2
#' @noRd
improve_efficiency <- function(design, iterations, seed) {
  set.seed(seed)
  # Initial design
  best_design <- design
  
  # Calculate initial efficiencies
  row_blocks <- best_design |> 
    dplyr::select(Level_1, Level_3, plots, treatments)
  efficiencies <- BlockEfficiencies(row_blocks)
  best_a_efficiency <- efficiencies$`A-Efficiency`[efficiencies$Level == 2]
  best_efficiencies <- efficiencies
  
  # Run iterations to improve A-Efficiency
  for (i in 1:iterations) {
    # Generate a new design by swapping treatments
    new_design <- swap_treatments(best_design)
    
    # Calculate efficiencies for the new design
    new_row_blocks <- new_design |> 
      dplyr::select(Level_1, Level_3, plots, treatments)
    new_efficiencies <- BlockEfficiencies(new_row_blocks)
    new_a_efficiency <- new_efficiencies$`A-Efficiency`[new_efficiencies$Level == 2]
    
    # Update the best design if the new A-Efficiency is higher
    if (new_a_efficiency > best_a_efficiency) {
      best_design <- new_design
      best_a_efficiency <- new_a_efficiency
      best_efficiencies <- new_efficiencies
    }
  }
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
# the default two-stage greedy search. Rows and columns are optimized jointly.
# Returns a data frame with the same structure as the two-stage best_design
# (Level_1 = Rep, Level_2 = Column, Level_3 = Row, plots, treatments) so the
# downstream field book construction is unchanged.
#' @noRd
build_row_column_onestage <- function(nt, nrows, ncols, reps, latinize, searches, seed) {
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
    stop(structure(
      class = c("onestage_infeasible", "error", "condition"),
      list(message = conditionMessage(optd), call = NULL)
    ))
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
  Rows <- factor(design$Level_3)                    # actual row block factor
  Cols <- factor(design$Level_2)                    # actual column block factor
  TF <- factor(design$treatments)
  v <- nlevels(TF)
  r <- mean(table(TF))
  Tind <- stats::model.matrix(~ TF - 1)             # full treatment indicators
  Bind <- stats::model.matrix(~ Rows + Cols)        # row + column block model
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
