#######################
library(blocksdesign)
library(dplyr)
library(FielDHub)
# More Realiostic Example
# 350 treatments each with 12 plants, to be distributed in 8 locations
trt <- 350  # genotypes/treatments
loc <- 8   # number of locations
desired_average <- 1.5 # desired average number of plants per location
plant_reps <- ceiling(loc * desired_average)  # number of plant copies to be distributed
plant_reps

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
preps_locs <- setNames(
    object = vector(mode = "list", length = loc), 
    nm = unique(long_allocation$LOCATION)
)
for (site in unique(long_allocation$LOCATION)) {
  df_loc <- long_allocation %>% 
    dplyr::filter(LOCATION == site) %>% 
    dplyr::mutate(ENTRY = as.numeric(ENTRY), REPS = as.numeric(REPS)) %>% 
    dplyr::select(ENTRY, NAME, REPS)
  preps_locs[[site]] <- partially_replicated(nrows = 25, ncols = 21, data = df_loc)
}
preps_locs$LOC1

preps_locs$LOC5$fieldBook

plot(preps_locs$LOC5)














## Unreplicated design example
#######################
library(blocksdesign)
library(dplyr)
library(FielDHub)


# More Realiostic Example
# 420 treatments each with 12 plants, to be distributed in 8 locations
trt <- 620  # genotypes/treatments
loc <- 8   # number of locations
plant_reps <- 6# number of plant copies to be distributed

# You can change: searches & jumps
mydes <- blocks(treatments = trt, replicates= plant_reps, blocks = loc, searches = 17, seed = 1)
ls(mydes)
mydes$Blocks_model # Efficiency measures
mydes$Design      # fieldbook
head(mydes$Replication)  # Replication per treatment


allocation <- table(mydes$Design$treatments, mydes$Design$Level_1)
allocation
allocation_df <- as.data.frame.matrix(allocation)
colnames(allocation_df) <- paste0("LOC", 1:loc)
colSums(allocation_df)
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

## Create the Un-replicated randomization for each location using the FielDHub package
checks <- 5 # Checks are mandatory for the FielDHub package
# Create a data frame for the checks
max_entry <- trt # Checks start at the last entry + 1 in the data frame
df_checks <- data.frame(
    ENTRY = (max_entry + 1):((max_entry + checks)), 
    NAME = paste0("Check-", (max_entry + 1):((max_entry + checks)))
)
df_checks
# Create a space in memory for the locations data entry list
list_locs <- setNames(
    object = vector(mode = "list", length = loc), 
    nm = unique(long_allocation$LOCATION)
)
# Create a list of unreplicated design objects of length loc
unrep_designs <- setNames(
    object = vector(mode = "list", length = loc), 
    nm = unique(long_allocation$LOCATION)
)

for (site in unique(long_allocation$LOCATION)) {
  df_loc <- long_allocation %>% 
    dplyr::filter(LOCATION == site, REPS > 0) %>% 
    dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>% 
    dplyr::select(ENTRY, NAME) %>%
    dplyr::bind_rows(df_checks) %>%
    dplyr::arrange(desc(ENTRY))

  list_locs[[site]] <- df_loc
  unrep_designs[[site]] <- FielDHub::diagonal_arrangement(nrows = 25, ncols = 20, checks = 5, data = df_loc)
}
unrep_designs$LOC1

unrep_designs$LOC2






# Sparse allocation function
do_optim <- function(design = "prep", lines, loc, plant_reps, checks, rep_checks, data, seed) {
    # You can change: searches & jumps
    optim_blocks <- blocksdesign::blocks(
        treatments = lines, 
        replicates = plant_reps, 
        blocks = loc, 
        searches = 10, 
        seed = seed
    )
    allocation <- table(optim_blocks$Design$treatments, optim_blocks$Design$Level_1)
    allocation_df <- as.data.frame.matrix(allocation)
    colnames(allocation_df) <- paste0("LOC", 1:loc)
    col_sum <- colSums(allocation_df)
    # Create a wide data frame with number of copies and average per plant
    wide_allocation <- allocation_df %>%
        mutate(
            copies = rowSums(.),
            avg = copies / loc
        )
    # Create a long data frame with the allocations per location
    long_allocation <- as.data.frame(allocation) %>%
        rename_with(~c("ENTRY", "LOCATION", "REPS"), everything()) %>%  # rename columns
        mutate(
            LOCATION = gsub("B", "LOC", LOCATION),
            NAME = paste0("G-", ENTRY)
        ) %>%  
        select(LOCATION, ENTRY, NAME, REPS)
    # Create a data frame for the checks
    max_entry <- lines # Checks start at the last entry + 1 in the data frame
    if (design != "prep") {
        df_checks <- data.frame(
            ENTRY = (max_entry + 1):((max_entry + checks)), 
            NAME = paste0("CH-", (max_entry + 1):((max_entry + checks)))
        )
    } else {
        df_checks <- data.frame(
            ENTRY = (max_entry + 1):((max_entry + checks)), 
            NAME = paste0("CH-", (max_entry + 1):((max_entry + checks))),
            REPS = rep_checks
        )
    }
    # Create a space in memory for the locations data entry list
    list_locs <- setNames(
        object = vector(mode = "list", length = loc), 
        nm = unique(long_allocation$LOCATION)
    )
    # Generate the lists of entries for each location
    for (site in unique(long_allocation$LOCATION)) {
        df_loc <- long_allocation %>% 
        dplyr::filter(LOCATION == site, REPS > 0) %>% 
        dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>% 
        dplyr::select(ENTRY, NAME, REPS) %>%
        dplyr::bind_rows(df_checks) %>%
        dplyr::arrange(desc(ENTRY))
        
        list_locs[[site]] <- df_loc
        #unrep_designs[[site]] <- FielDHub::diagonal_arrangement(nrows = nrows, ncols = ncols, checks = checks, data = df_loc)
    }
    out <- list(list_locs = list_locs, size_locations = col_sum)
    class(out) <- "SparseUnrep"
    return(out)
}


#### Prep example #############
avg_prep <- do_optim(design = "sparse", lines = 320, loc = 6, plant_reps = 8, checks = 3, rep_checks = c(8,8,8), seed = 5025)
prep_example <- FielDHub::partially_replicated(nrows = 11, ncols = 41, data = avg_prep$list_locs$LOC1)
prep_example
#### Un replicated example ####
sparse_unrep <- do_optim(design = "sparse", lines = 380, loc = 6, plant_reps = 5, checks = 4, seed = 502)
sparse_example <- FielDHub::diagonal_arrangement(nrows = nrows, ncols = ncols, data = sparse_unrep$list_locs$LOC1)


sparse_unrep$size_locations


lines <- 785
t1 <- floor(lines + lines * 0.10)
t2 <- ceiling(lines + lines * 0.20)
t <- t1:t2
n <- t[-numbers::isPrime(t)]
choices_list <- list()
i <- 1
for (n in t) {
    choices_list[[i]] <- FielDHub:::factor_subsets(520, diagonal = TRUE)$labels
    i <- i + 1
}


sparse_allocation <- function(lines, nrows, ncols, l, planter, plotNumber,  plant_reps, checks, exptName, locationNames, seed) {
    # Check if the reps per plant are mising
    if (missing(plant_reps)) {
        stop("You must specify the number of reps per plant")
    }
    unrep <- do_optim(
        design = "sparse",
        lines = lines, 
        loc = l, 
        plant_reps = plant_reps, 
        checks = checks, 
        seed = seed
    )
    # Create a space in memory for the unrep randomizations
    unrep_designs <- setNames(
        object = vector(mode = "list", length = loc), 
        nm = names(unrep$list_locs)
    )
    for (site in names(unrep$list_locs)) {
        df_loc <- unrep$list_locs[[site]] %>% 
            dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>% 
            dplyr::select(ENTRY, NAME)
        unrep_designs[[site]] <- FielDHub::diagonal_arrangement(nrows = nrows, ncols = ncols, checks = checks, data = df_loc)
    }
    return(unrep_designs)
}

sparse <- sparse_allocation(
    lines = 380, 
    nrows = 15, 
    ncols = 20, 
    l = 6, 
    plant_reps = 4, 
    checks = 5, 
    locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
    seed = 1234
)

average_allocation <- function(lines, 
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
    unrep <- do_optim(
        desig = "prep",
        lines = lines, 
        loc = l, 
        plant_reps = plant_reps, 
        checks = checks, 
        rep_checks = rep_checks,
        seed = seed
    )
    # Create a space in memory for the unrep randomizations
    unrep_designs <- setNames(
        object = vector(mode = "list", length = loc), 
        nm = names(unrep$list_locs)
    )
    for (site in names(unrep$list_locs)) {
        df_loc <- unrep$list_locs[[site]] %>% 
            dplyr::mutate(
                ENTRY = as.numeric(ENTRY),
                REPS = as.numeric(REPS)
                ) %>% 
            dplyr::select(ENTRY, NAME, REPS)
        unrep_designs[[site]] <- FielDHub::partially_replicated(nrows = nrows, ncols = ncols, data = df_loc)
    }
    return(unrep_designs)
}

x <- do_optim(lines = 320, loc = 6, plant_reps = 8, checks = 3, rep_checks = c(8,8,8), seed = 5025)
x
prep_average <- average_allocation(
    lines = 324, 
    nrows = 19, 
    ncols = 24, 
    l = 6, 
    plant_reps = 8, 
    checks = 3, 
    rep_checks = c(8,8,8),
    locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
    seed = 1234
)

prep_average$LOC1$fieldBook
plot(prep_average$LOC1)

unrep_sparse$LOC1$fieldBook

plot(unrep_sparse$LOC1)
plot(unrep_sparse$LOC2)
plot(unrep_sparse$LOC3)


(324 * 8) %% 6 == 0




