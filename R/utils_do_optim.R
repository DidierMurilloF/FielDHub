#' @title  Optimization for sparse allocation 
#' @noRd
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
    col_sum <- base::colSums(allocation_df)
    # Create a wide data frame with number of copies and average per plant
    wide_allocation <- allocation_df %>%
        dplyr::mutate(
        copies = rowSums(.),
        avg = copies / loc
        )
    # Create a long data frame with the allocations per location
    long_allocation <- as.data.frame(allocation) %>%
        dplyr::rename_with(~c("ENTRY", "LOCATION", "REPS"), dplyr::everything()) %>%  # rename columns
        dplyr::mutate(
        LOCATION = gsub("B", "LOC", LOCATION),
        NAME = paste0("G-", ENTRY)
        ) %>%  
        dplyr::select(LOCATION, ENTRY, NAME, REPS)
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
        dplyr::arrange(dplyr::desc(ENTRY))
        
        list_locs[[site]] <- df_loc
    }
    out <- list(
        list_locs = list_locs,
        allocation = allocation_df, 
        size_locations = col_sum
    )
    class(out) <- "Sparse"
    return(out)
}

#' @title  Sparse allocation 
#' @example 
#' sparse <- sparse_allocation(
#'   lines = 380, 
#'   nrows = 16, 
#'   ncols = 22, 
#'   l = 6, 
#'   plant_reps = 5, 
#'   checks = 4, 
#'   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
#'   seed = 1234
#' )
#' @export 
sparse_allocation <- function(
    lines, 
    nrows, 
    ncols, 
    l, 
    planter, 
    plotNumber,  
    plant_reps, 
    checks, 
    exptName, 
    locationNames, 
    seed) {
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
        object = vector(mode = "list", length = l), 
        nm = names(unrep$list_locs)
    )
    for (site in names(unrep$list_locs)) {
        df_loc <- unrep$list_locs[[site]] %>% 
        dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>% 
        dplyr::select(ENTRY, NAME)
        unrep_designs[[site]] <- diagonal_arrangement(
        nrows = nrows, 
        ncols = ncols, 
        checks = checks, 
        data = df_loc
        )
    }
    return(
        list(
            designs = unrep_designs, 
            list_locs = unrep$list_locs, 
            allocation = unrep$allocation, 
            size_locations = unrep$size_locations
        )
    )
}

#' @title  average prep allocation
#' @example 
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
#' @export
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
    return(preps_designs)
}