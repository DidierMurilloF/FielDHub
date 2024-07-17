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
  
  # Combine the results
  combined_efficiencies <- dplyr::bind_rows(rep_efficiencies, row_efficiencies, col_efficiencies)
  
  return(combined_efficiencies)
}
