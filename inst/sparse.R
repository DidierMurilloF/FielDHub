#######################
library(blocksdesign)
library(dplyr)
library(FielDHub)
# More Realiostic Example
# 250 treatments each with 12 plants, to be distributed in 8 locations
trt <- 350  # genotypes/treatments
loc <- 6  # number of locations
plant_reps <- 5


# You can change: searches & jumps
mydes <- blocks(
  treatments = trt, 
  replicates= plant_reps, 
  blocks = loc, 
  searches = 17, 
  seed = 1
)

ls(mydes)
mydes$Blocks_model # Efficiency measures
mydes$Design      # fieldbook
head(mydes$Replication)  # Replication per treatment


allocation <- table(mydes$Design$treatments, mydes$Design$Level_1)
allocation
allocation_df <- as.data.frame.matrix(allocation)
colSums(allocation_df)
colnames(allocation_df) <- paste0("LOC", 1:loc)
wide_allocation_df <- allocation_df %>%
  mutate(
    copies = rowSums(.),
    avg = copies / loc
  )

head(wide_allocation_df, 10)

long_allocation <- as.data.frame(allocation) %>%
  rename_with(~c("ENTRY", "LOCATION", "REPS"), everything()) %>%  # rename columns
  mutate(
    LOCATION = gsub("B", "LOC", LOCATION),
    NAME = paste0("G-", ENTRY)
  ) %>%  
  select(LOCATION, ENTRY, NAME, REPS)

head(long_allocation, 10)
## Create the p-reps randomization for each location using the FielDHub package
# Create a list of data frames
    unrep <- do_optim(
        design = "sparse",
        lines = 350, 
        loc = 6, 
        plant_reps = 5, 
        checks = 4, 
        seed = 1
    )
    # Create a space in memory for the unrep randomizations
    unrep_designs <- setNames(
        object = vector(mode = "list", length = 6), 
        nm = names(unrep$list_locs)
    )
    for (site in names(unrep$list_locs)) {
        df_loc <- unrep$list_locs[[site]] %>% 
            dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>% 
            dplyr::select(ENTRY, NAME)
        unrep_designs[[site]] <- FielDHub::diagonal_arrangement(nrows = 15, ncols = 21, checks = 4, data = df_loc, sparse = TRUE)
    }
}


unrep_designs$LOC1



#' @title  average prep allocation
#' @examples
#' prep_average <- average_allocation(
#'   lines = 324, 
#'   nrows = 19, 
#'   ncols = 24, 
#'   l = 6, 
#'   plant_reps = 8, 
#'   checks = 3, 
#'   rep_checks = c(8,8,8),
#'   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
#'   seed = 1234
#' )
average_allocation <- function(
    lines, 
    nrows, 
    ncols, 
    l, 
    planter, 
    plotNumber, 
    desired_avg, 
    plant_reps, 
    checks,
    rep_checks,
    exptName, 
    locationNames, 
    seed) {
    if (missing(plant_reps)) {
        plant_reps <- ceiling(l * desired_avg)
    }
    preps <- do_optim(
        design = "prep",
        lines = lines, 
        loc = l, 
        plant_reps = plant_reps, 
        checks = checks, 
        rep_checks = rep_checks,
        seed = seed
    )
    # Create a space in memory for the preps randomizations
    preps_designs <- setNames(
        object = vector(mode = "list", length = l), 
        nm = names(preps$list_locs)
    )
    for (site in names(preps$list_locs)) {
        df_loc <- preps$list_locs[[site]] %>% 
        dplyr::mutate(
            ENTRY = as.numeric(ENTRY),
            REPS = as.numeric(REPS)
        ) %>% 
        dplyr::select(ENTRY, NAME, REPS)
        preps_designs[[site]] <- partially_replicated(
            nrows = nrows, 
            ncols = ncols, 
            data = df_loc
        )
    }
    return(
        list(
            designs = preps_designs, 
            list_locs = preps$list_locs, 
            allocation = preps$allocation, 
            size_locations = preps$size_locations
        )
    )
}
