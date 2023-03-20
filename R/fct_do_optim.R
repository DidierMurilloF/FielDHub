#' @title  Generate the sparse allocation.
#' @param design Type of experimental design. It can be \code{prep} or \code{sparse}
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param plant_reps Number of copies per plant. 
#' When design is \code{sparse} then \code{plant_reps} should be less than \code{l}
#' @param add_checks Option to add checks. Optional if \code{design = "prep"}
#' @param checks Number of genotypes checks. 
#' @param rep_checks Replication for each check.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' 
#' @author Didier Murillo, Salvador Gezan
#' @examples 
#' sparse_example <- do_optim(
#'    design = "sparse",
#'    lines = 240, 
#'    l = 4, 
#'    plant_reps = 3, 
#'    add_checks = TRUE, 
#'    checks = 4,
#'    seed = 15
#' )
#' @export 
do_optim <- function(
    design = "sparse", 
    lines, 
    l, 
    plant_reps, 
    add_checks = FALSE, 
    checks, 
    rep_checks, 
    seed,
    data) {
    # set a random seed if it is missing
    if (missing(seed)) seed <- base::sample.int(10000, size = 1) 
    if (design == "prep" & plant_reps <= l) { 
        stop("p-reps option requires that plant_reps be greater than the number of locations")
    }
    if (!missing(data)) {
        data_input <- stats::na.omit(data[,1:2])
        df_checks <- data_input[1:checks,]
        df_data <- data_input[(checks + 1):nrow(data_input),]
        colnames(df_data) <- c("ENTRY", "NAME")
        ENTRY <- as.vector(df_data$ENTRY)
        NAME <- as.vector(df_data$NAME)
        if (nrow(df_data) != lines) {
            stop("The number of treatments/lines in the data does not match the input value")
        }
    }
    # Generate the optim IBs
    optim_blocks <- blocksdesign::blocks(
        treatments = lines, 
        replicates = plant_reps, 
        blocks = l, 
        searches = 20, 
        seed = seed
    )
    # Create allocation table
    allocation <- table(optim_blocks$Design$treatments, optim_blocks$Design$Level_1)
    key_value <- 0
    add_value <- 1
    if (design == "prep") {
        key_value <- 1
        add_value <- 2
    }
    # Check if there are unbalanced locations and force them to be balanced
    size_locs <- as.vector(base::colSums(allocation))
    max_size_locs <- max(size_locs)
    if (!all(size_locs == max_size_locs)) {
        unbalanced_locs <- which(size_locs != max_size_locs)
        max_swaps <- length(unbalanced_locs)
        k <- nrow(allocation)
        init <- 1
        while (init <= max_swaps) {
            # Add an additional gen copy to the unbalanced locations
            add_gen <- as.vector(allocation[k, unbalanced_locs])
            if (length(which(add_gen == key_value)) > 0) {
                one_index <- which(add_gen == key_value)[1] #sample(which(add_gen == key_value), size = 1)
                add_gen[one_index] <- add_value
                allocation[k, unbalanced_locs] <- add_gen
                unbalanced_locs <- unbalanced_locs[-one_index]
                init <- init + 1
            }
            k <- k - 1
        }
    }
    allocation_df <- as.data.frame.matrix(allocation)
    colnames(allocation_df) <- paste0("LOC", 1:l)
    # Create a wide data frame with number of copies and average per plant
    col_sum <- base::colSums(allocation_df)
    wide_allocation <- allocation_df %>%
        dplyr::mutate(
            copies = rowSums(.),
            avg = copies / l
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
        if (!add_checks) stop("Un-replicated designs need checks")
        if (!missing(checks) & checks > 0) {
            df_checks <- data.frame(
                ENTRY = (max_entry + 1):((max_entry + checks)), 
                NAME = paste0("CH-", (max_entry + 1):((max_entry + checks)))
            )
        }
    } else {
        if (add_checks == TRUE & !missing(checks) & !missing(rep_checks)) {
            if (length(rep_checks) != checks) {
                stop("Length of rep_checks does not match with number of checks")
            } 
            df_checks <- data.frame(
                ENTRY = (max_entry + 1):((max_entry + checks)), 
                NAME = paste0("CH-", (max_entry + 1):((max_entry + checks))),
                REPS = rep_checks
            )
        } else df_checks <- NULL
    }
    # Create a space in memory for the locations data entry list
    list_locs <- setNames(
        object = vector(mode = "list", length = l), 
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

        if (design == "prep") {
            df_loc <- df_loc %>% 
                dplyr::arrange(dplyr::desc(REPS))
        }

        if (design != "prep") {
            df_loc <- df_loc %>% 
                dplyr::select(ENTRY, NAME)
        }

        list_locs[[site]] <- df_loc
    }
    # out object with the allocation and the list of entries per location
    out <- list(
        list_locs = list_locs,
        allocation = allocation_df, 
        size_locations = col_sum
    )
    # Create the class "SparsePrep" for the object out
    design_class <- "MultiPrep"
    if (design != "prep") design_class <- "Sparse"
    class(out) <- design_class
    #return the object out
    return(out)
}

#' @title  Sparse allocation 
#' @param checks_allocation Way to allocate the checks in the field. It can be
#' \code{diagonal} or \code{randomly}
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param checks Number of genotypes checks. 
#' @param planter Option for \code{serpentine} or \code{cartesian} plot arrangement. 
#' By default  \code{planter = 'serpentine'}.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. 
#' By default \code{plotNumber = 101}.
#' @param plant_reps Number of copies per plant. 
#' When design is \code{sparse} then \code{plant_reps} < \code{l}
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Names each location.
#' @param nrows Number of rows in the field. 
#' @param ncols Number of columns in the field.
#' @param sparse_list (optional) A class "Sparse" object generated by \code{do_optim()} function.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' @examples
#' sparse <- sparse_allocation(
#' checks_allocation = "diagonal",
#'   lines = 260, 
#'   l = 5, 
#'   plant_reps = 5, 
#'   checks = 4, 
#'   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
#'   seed = 1234
#' )
#' @export 
sparse_allocation <- function(
    checks_allocation,
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
    sparse_list, 
    seed) {
    # set a random seed if it is missing
    if (missing(seed)) seed <- base::sample.int(10000, size = 1)   
    # Check if the reps per plant are mising
    if (missing(plant_reps)) {
        stop("You must specify the number of reps per plant")
    }

    if (!missing(sparse_list)) {
        if (!inherits(sparse_list, "Sparse")) {
            stop("sparse_list must be an object of class 'Sparse'")
        }
        unrep <- sparse_list
    } else {
        unrep <- do_optim(
            design = "sparse",
            lines = lines, 
            l = l, 
            plant_reps = plant_reps,
            add_checks = TRUE, 
            checks = checks, 
            seed = seed
        )
    }

    if (missing(nrows) || missing(ncols)) {
        lines_within_loc <- as.numeric(unrep$size_locations[1])
        print(lines_within_loc)
        t1 <- floor(lines_within_loc + lines_within_loc * 0.11)
        t2 <- ceiling(lines_within_loc + lines_within_loc * 0.20)
        t <- t1:t2
        non_primes <- t[-numbers::isPrime(t)]
        choices_list <- list()
        i <- 1
        for (n in non_primes) {
            choices_list[[i]] <- factor_subsets(n, diagonal = TRUE)$labels
            i <- i + 1
        }
        choices <- unlist(choices_list[!sapply(choices_list, is.null)])
        dif <- vector(mode = "numeric", length = length(choices))
        for (option in 1:length(choices)) {
            dims <- unlist(strsplit(choices[[option]], " x "))
            dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
        }
        df_choices <- data.frame(choices = choices, diff_dim = dif)
        df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
        dimensions <- unlist(strsplit(df_choices[1,1], " x "))
        nrows <- as.numeric(dimensions[1])
        ncols <- as.numeric(dimensions[2])
    }
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

#' @title  Optimized multi-location partially replicated design
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param plant_reps Number of total copies per treatment.
#' @param desired_avg (optional) Desired average of treatments across locations.
#' @param checks Number of checks.
#' @param rep_checks Number of replications per check.
#' @param planter Option for \code{serpentine} or \code{cartesian} movement. By default  \code{planter = 'serpentine'}. 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Name for each location.
#' @param nrows Number of rows field.
#' @param ncols Number of columns field.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param optim_list (optional) A class "SparsePrep" object generated by \code{do_optim()} function.
#' 
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' @return A list with five elements.
#' \itemize{
#'   \item \code{designs} is a list with each location p-rep randomization.
#'   \item \code{list_locs} is a list with each location list of entries.
#'   \item \code{allocation} is a matrix with the allocation of treatments.
#'   \item \code{size_locations} is a data frame with columns as number of locations
#' and one row with the size of the location.
#' }
#'
#' @references
#' Edmondson, R.N. Multi-level Block Designs for Comparative Experiments. JABES 25, 
#' 500–522 (2020). https://doi.org/10.1007/s13253-020-00416-0
#'
#'
#' @examples
#' # Example 1: Generates a spatial optimized multi-location p-rep design with 240 
#' # genotypes. The number of copies per plant available for this experiment are 9. 
#' # The plant is carry out this experiment in 6 locations.
#' # In this case we also add 3 controls (checks) with 6 reps each.
#' # With this set up, the experiment will have 240 + 3 = 243 entries and the
#' # number of entries per location will be 360. Also, the overal average 
#' # genotype allocation will be 1.5 plants copies per location. 
#' optim_multi_prep <- multi_location_prep(
#'   lines = 180,  
#'   l = 5, 
#'   plant_reps = 8, 
#'   checks = 3, 
#'   rep_checks = c(6,6,6),
#'   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
#'   seed = 1234
#' )
#' designs <- optim_multi_prep$designs
#' field_book_loc_1 <- designs$LOC1$fieldBook
#' head(field_book_loc_1, 10)
#' @export 
multi_location_prep <- function(
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
    # set a random seed if it is missing
    if (missing(seed)) seed <- base::sample.int(10000, size = 1)  
    if (missing(plant_reps) & missing(desired_avg)) {
        stop("multi_location_prep() requires either plant_reps or desired_avg.")
    } 
    if (missing(plant_reps)) {
        plant_reps <- ceiling(l * desired_avg)
    }
    add_checks <- FALSE
    if (!missing(checks) & !missing(rep_checks)) add_checks <- TRUE
    if (!missing(optim_list)) {
        if (!inherits(optim_list, "MultiPrep")) {
            stop("sparse_list must be an object of class 'SparsePrep'")
        }
        preps <- optim_list
    } else {
        preps <- do_optim(
            design = "prep",
            lines = lines, 
            l = l, 
            plant_reps = plant_reps,
            add_checks = add_checks, 
            checks = checks, 
            rep_checks = rep_checks,
            seed = seed
        )
    }
    # Set the number of rows and columns
    if (missing(nrows) || missing(ncols)) {
        lines_within_loc <- as.numeric(preps$size_locations[1])
        n <- lines_within_loc + sum(rep_checks)
        choices <- factor_subsets(n)$labels
        if (is.null(choices)) {
            stop("The size of locations ends with a prime number. Please specify a different number of locations or treatments.")
        }
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