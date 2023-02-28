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
