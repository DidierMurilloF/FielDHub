# Reference implementation of the twostage row step as it was before the block
# engine took it over (FielDHub at b110e4f, R/engine_row_column_optimization.R:
# swap_treatments() and improve_efficiency(), copied verbatim below). It is used
# only by tools/time-row-step-pairs.R for the paired timing of spec 6.8, sourced
# into an environment whose parent is the FielDHub namespace. Not part of the package.

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
      rows_to_swap <- sample(seq_len(nrow(df_level2)), 2)
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
  validate_iteration_budget(iterations)
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
  for (i in seq_len(iterations)) {
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

