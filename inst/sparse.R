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
#' @noRd 
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
    optim_list, 
    seed) {

    if (missing(plant_reps)) {
        plant_reps <- ceiling(l * desired_avg)
    }

    if (!missing(optim_list)) {
        if (!inherits(optim_list, "Sparse")) {
            stop("sparse_list must be an object of class 'Sparse'")
        }
        preps <- optim_list
    } else {
        preps <- do_optim(
            design = "prep",
            lines = lines, 
            l = l, 
            plant_reps = plant_reps, 
            checks = checks, 
            rep_checks = rep_checks,
            seed = seed
        )
    }
    print(preps$size_locations)
    if (missing(nrows) || missing(ncols)) {
        lines_within_loc <- as.numeric(preps$size_locations[1])
        n <- lines_within_loc + sum(rep_checks)
        choices <- FielDHub:::factor_subsets(n)$labels
        dif <- vector(mode = "numeric", length = length(choices))
        for (option in 1:length(choices)) {
            dims <- unlist(strsplit(choices[[option]], " x "))
            dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
        }
        df_choices <- data.frame(choices = unlist(choices), diff_dim = dif)
        df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
        dimensions <- unlist(strsplit(df_choices[1,1], " x "))
        nrows <- as.numeric(dimensions[1])
        ncols <- as.numeric(dimensions[2])
        print(c(nrows, ncols))
    }
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


library(FielDHub)

optim_list <- do_optim(
    design = "prep",
    lines = 360, 
    l = 6, 
    plant_reps = 8, 
    checks = 3, 
    rep_checks = c(8,8,8),
    seed = 1234
)

optim_list

# View the data frame
geno_df <- optim_list$allocation

# Calculate the allele frequencies for each locus
allele_freq <- colMeans(geno_df)

# View the allele frequencies
allele_freq

# Calculate the genetic distance between each pair of genotypes
# geno_dist <- dist(t(geno_df), method = "binary")
# Calculate the genetic distance between each pair of genotypes
geno_dist <- dist(t(geno_df), method = "manhattan")


# View the genetic distance matrix
geno_dist
test = as.matrix(geno_dist)
diag(test) = NA

agriutilities::covcor_heat(test,   corr = FALSE, size = 10)


prep <- average_allocation( 
    lines = 360, 
    l = 6, 
    plant_reps = 8, 
    checks = 3, 
    rep_checks = c(8,8,8),
    locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
    optim_list = optim_list,
    seed = 1234
)
