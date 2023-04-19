#' @title  Generate the sparse or p-rep allocation to multiple locations.
#' @param design Type of experimental design. It can be \code{prep} or \code{sparse}
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param copies_per_entry Number of copies per plant. 
#' When design is \code{sparse} then \code{copies_per_entry} should be less than \code{l}
#' @param add_checks Option to add checks. Optional if \code{design = "prep"}
#' @param checks Number of genotypes checks. 
#' @param rep_checks Replication for each check.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME }. ENTRY must be numeric.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb]
#' 
#' @return A list with three elements.
#' \itemize{
#'   \item \code{list_locs} is a list with each location list of entries.
#'   \item \code{allocation} is a matrix with the allocation of treatments.
#'   \item \code{size_locations} is a data frame with one column for each 
#'  location and one row with the size of the location.
#' }
#' 
#' @references
#' Edmondson, R.N. Multi-level Block Designs for Comparative Experiments. JABES 25, 
#' 500–522 (2020). https://doi.org/10.1007/s13253-020-00416-0
#' 
#' @examples 
#' sparse_example <- do_optim(
#'    design = "sparse",
#'    lines = 120, 
#'    l = 4, 
#'    copies_per_entry = 3, 
#'    add_checks = TRUE, 
#'    checks = 4,
#'    seed = 15
#' )
#' @export 
do_optim <- function(
    design = "sparse", 
    lines, 
    l, 
    copies_per_entry, 
    add_checks = FALSE, 
    checks = NULL, 
    rep_checks = NULL, 
    seed,
    data = NULL) {
    # set a random seed if it is missing
    if (missing(seed)) seed <- base::sample.int(10000, size = 1) 
    if (missing(lines)) stop("Please, define the number of lines/treatments for this design.")
    if (missing(l)) stop("Please, define the number of locations for this design.")
    if (missing(design) || is.null(design)) stop("Paramenter design is missing.")
    if (all(c("prep", "sparse") != design)) {
        stop("Input design is unknown. Please, choose one: 'sparse' or 'prep'.")
    } 
    if (design == "prep" & copies_per_entry <= l) { 
        stop("p-reps option requires that copies_per_entry be greater than the number of locations")
    }
    if (design == "sparse") {
        if (is.null(checks)) stop("Please, specify the number of checks!")
    }
    if (design == "prep") {
        if (add_checks == TRUE & is.null(checks) & is.null(rep_checks)) {
            stop("Please, specify the number of checks!")
        }
    }
    max_entry <- lines 
    if (!is.null(data)) {
        if (design == "sparse") {
            data_input <- data[, 1:2]
            data_input <- stats::na.omit(data_input)
            colnames(data_input) <- c("ENTRY", "NAME")
            if (length(data_input$ENTRY) != length(unique(data_input$ENTRY))) {
              stop("Please ensure all ENTRIES in data are distinct.")
            }
            if (length(data_input$NAME) != length(unique(data_input$NAME))) {
              stop("Please ensure all NAMES in data are distinct.")
            }
            df_data_checks <- data_input[1:checks, ]
            df_data_lines <- data_input[(checks + 1):nrow(data_input), ]
            ENTRY <- as.vector(df_data_lines$ENTRY)
            if (!is.numeric(ENTRY)) stop("ENTRY column should have integer numbers!")
            # max_entry <- max(ENTRY)
            max_entry <- lines
            if (nrow(df_data_lines) != lines) {
                stop("The number of treatments/lines in the data does not match the input value")
            }
        } else {
            if (add_checks == TRUE) {
                data_input <- data[, 1:2]
                data_input <- stats::na.omit(data_input)
                colnames(data_input) <- c("ENTRY", "NAME")
                if (length(data_input$ENTRY) != length(unique(data_input$ENTRY))) {
                  stop("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(data_input$NAME) != length(unique(data_input$NAME))) {
                  stop("Please ensure all NAMES in data are distinct.")
                }
                df_data_checks <- data_input[1:checks, ]
                df_data_lines <- data_input[(checks + 1):nrow(data_input), ]
                ENTRY <- as.vector(df_data_lines$ENTRY)
                if (!is.numeric(ENTRY)) stop("ENTRY column should have integer numbers!")
                # max_entry <- max(ENTRY)
                max_entry <- lines
                if (nrow(df_data_lines) != lines) {
                  stop("The number of treatments/lines in the data does not match the input value")
                }
            } else {
                data_input <- data[, 1:2]
                data_input <- stats::na.omit(data_input)
                df_data_lines <- data_input
                colnames(df_data_lines) <- c("ENTRY", "NAME")
                if (length(df_data_lines$ENTRY) != length(unique(df_data_lines$ENTRY))) {
                    stop("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(df_data_lines$NAME) != length(unique(df_data_lines$NAME))) {
                    stop("Please ensure all NAMES in data are distinct.")
                }
                ENTRY <- as.vector(df_data_lines$ENTRY)
                if (!is.numeric(ENTRY)) stop("ENTRY column should have integer numbers!")
                #max_entry <- max(ENTRY)
                max_entry <- lines
                if (nrow(df_data_lines) != lines) {
                    stop("The number of treatments/lines in the data does not match the input value")
                }
            }
        }
    }
    # Generate the optim IBs
    optim_blocks <- blocksdesign::blocks(
        treatments = lines, 
        replicates = copies_per_entry, 
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
                one_index <- which(add_gen == key_value)[1] 
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
    if (design != "prep") {
        if (!add_checks) stop("Un-replicated designs need checks")
        if (!is.null(checks) & checks > 0) {
            df_checks <- data.frame(
                ENTRY = (max_entry + 1):((max_entry + checks)), 
                NAME = paste0("CH-", (max_entry + 1):((max_entry + checks)))
            )
        }
    } else {
        if (add_checks == TRUE & !is.null(checks) & !is.null(rep_checks)) {
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
    if (design == "prep") {
        # Combine the data frames into a single data frame with a new column for the list element name
        multi_location_data <- dplyr::bind_rows(lapply(names(list_locs), function(name) {
            dplyr::mutate(list_locs[[name]], LOCATION = name)
        })) %>% 
            dplyr::select(LOCATION, ENTRY, NAME, REPS)
    } else {
         # Combine the data frames into a single data frame with a new column for the list element name
        multi_location_data <- dplyr::bind_rows(lapply(names(list_locs), function(name) {
            dplyr::mutate(list_locs[[name]], LOCATION = name)
        })) %>% 
            dplyr::select(LOCATION, ENTRY, NAME)
    }
    # out object with the allocation and the list of entries per location
    out <- list(
        multi_location_data = multi_location_data,
        list_locs = list_locs,
        allocation = allocation_df, 
        size_locations = col_sum
    )
    # Create the class "MultiPrep" for the object out
    design_class <- "MultiPrep"
    if (design != "prep") design_class <- "Sparse"
    class(out) <- design_class
    #return the object out
    return(out)
}

#' @title  Unreplicated designs using the sparse allocation approach
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param checks Number of genotypes checks. 
#' @param planter Option for \code{serpentine} or \code{cartesian} plot arrangement. 
#' By default  \code{planter = 'serpentine'}.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. 
#' By default \code{plotNumber = 101}.
#' @param copies_per_entry Number of copies per plant. 
#' When design is \code{sparse} then \code{copies_per_entry} < \code{l}
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Names each location.
#' @param nrows Number of rows in the field. 
#' @param ncols Number of columns in the field.
#' @param sparse_list (optional) A class "Sparse" object generated by \code{do_optim()} function.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME }. ENTRY must be numeric.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb]
#' 
#' @return A list with four elements.
#' \itemize{
#'   \item \code{designs} is a list with each location unreplicated randomization.
#'   \item \code{list_locs} is a list with each location list of entries.
#'   \item \code{allocation} is a matrix with the allocation of treatments.
#'   \item \code{size_locations} is a data frame with one column for each 
#'  location and one row with the size of the location.
#' }
#' 
#' @references
#' Edmondson, R.N. Multi-level Block Designs for Comparative Experiments. JABES 25, 
#' 500–522 (2020). https://doi.org/10.1007/s13253-020-00416-0
#' 
#' @examples
#' sparse <- sparse_allocation(
#'   lines = 120, 
#'   l = 4, 
#'   copies_per_entry = 3, 
#'   checks = 4, 
#'   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
#'   seed = 1234
#' )
#' @export 
sparse_allocation <- function(
    lines, 
    nrows, 
    ncols, 
    l, 
    planter = "serpentine", 
    plotNumber,  
    copies_per_entry, 
    checks = NULL, 
    exptName = NULL, 
    locationNames,
    sparse_list, 
    seed,
    data = NULL) {
    # set a random seed if it is missing
    if (missing(seed)) seed <- base::sample.int(10000, size = 1) 
    if (missing(l)) stop("Please, define the number of locations for this design.")
    if (missing(locationNames) || length(locationNames) != l)  {
        locationNames <- paste0("LOC", 1:l)
    }
    if (missing(plotNumber) || length(plotNumber) != l) {
        plotNumber <- seq(1, 1000 * l, by = 1000)[1:l]
    }
    if (missing(exptName)) exptName <- "SparseExpt"
    if (missing(planter) || is.null(planter)) planter <- "serpentine"
    if (all(c("serpentine", "cartesian") != planter)) {
        stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
    }
    # Check if the reps per plant are mising
    if (missing(copies_per_entry)) {
        stop("You must specify the number of reps per plant")
    }
    if (copies_per_entry >= l) stop("Please, enter copies_per_entry < l")
    if (is.null(checks)) stop("Please, define the number of checks for this design.")
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
            copies_per_entry = copies_per_entry,
            add_checks = TRUE, 
            checks = checks, 
            seed = seed
        )
    }
    # Merge user input data
    if (!is.null(data)) {
        unrep <- merge_user_data(
            optim_out = unrep, 
            data = data, 
            lines = lines, 
            add_checks = TRUE, 
            checks = checks
        )
    } 
    # Define field dimensions (rows and columns)
    if (missing(nrows) || missing(ncols)) {
        lines_within_loc <- as.numeric(unrep$size_locations[1])
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
    unrep_designs <- diagonal_arrangement(
        nrows = nrows, 
        ncols = ncols, 
        checks = checks,
        planter = planter, 
        plotNumber = plotNumber,
        l = l,
        locationNames = locationNames,
        exptName = exptName,
        seed = seed,
        multiLocationData= TRUE,
        data = unrep
    )
    unrep_designs$infoDesign$id_design <- "Sparse"
    output <- list(
        infoDesign = unrep_designs$infoDesign,
        layoutRandom = unrep_designs$layoutRandom, 
        plotsNumber = unrep_designs$plotsNumber,
        data_entry = unrep_designs$data_entry, 
        fieldBook = unrep_designs$fieldBook,
        list_locs = unrep$list_locs, 
        allocation = unrep$allocation, 
        size_locations = unrep$size_locations
    )
    class(output) <- "FielDHub"
    return(invisible(output))
}
#' @title Optimized multi-location partially replicated design
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param copies_per_entry Number of total copies per treatment.
#' @param desired_avg (optional) Desired average of treatments across locations.
#' @param checks Number of checks.
#' @param rep_checks Number of replications per check.
#' @param planter Option for \code{serpentine} or \code{cartesian} movement. By default  \code{planter = 'serpentine'}. 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Name for each location.
#' @param nrows Numeric vector with the number of rows field at each location.
#' @param ncols Numeric vector with the number of columns field at each location.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param optim_list (optional) A list object of class "MultiPrep"generated by \code{do_optim()} function.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME }. ENTRY must be numeric.
#' 
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Jean-Marc Montpetit [ctb],
#'         Ana Heilman [ctb]
#' 
#' @return A list of class \code{FielDHub} with several elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{layoutRandom} is a matrix with the randomization layout.
#'   \item \code{plotNumber} is a matrix with the layout plot number.
#'   \item \code{binaryField} is a matrix with the binary field.
#'   \item \code{dataEntry} is a data frame with the data input.
#'   \item \code{genEntries} is a list with the entries for replicated and non-replicated parts.
#'   \item \code{fieldBook} is a data frame with field book design. This includes the index (Row, Column).
#'   \item \code{min_pairswise_distance} is a data frame with the minimum pairwise distance between 
#'              each pair of locations.
#'   \item \code{reps_info} is a data frame with information on the number of replicated and 
#'              non-replicated treatments at each location.
#'   \item \code{pairsDistance} is a data frame with the pairwise distances between each pair of 
#'              treatments.
#'   \item \code{treatments_with_reps} is a list with the entries for the replicated part of the design.
#'   \item \code{treatments_with_no_reps} is a list with the entries for the non-replicated part of the design.
#'   \item \code{list_locs} is a list with each location list of entries.
#'   \item \code{allocation} is a matrix with the allocation of treatments.
#'   \item \code{size_locations} is a data frame with one column for each 
#'              location and one row with the size of the location.
#' }
#'
#' @references
#' Edmondson, R.N. Multi-level Block Designs for Comparative Experiments. JABES 25, 
#' 500–522 (2020). https://doi.org/10.1007/s13253-020-00416-0
#'
#'
#' @examples
#' # Example 1: Generates a spatially optimized multi-location p-rep design with 142 
#' # genotypes. The number of copies per plant available for this experiment is 9. 
#' # This experiment is carried out in 5 locations, and there are seven seeds available 
#' # for each plant to make replications.
#' # In this case, we add three controls (checks) with six reps each.
#' # With this setup, the experiment will have 142 treatments + 3 checks = 145 
#' # entries and the number of plots per location after the allocation process 
#' # will be 196. 
#' # The average genotype allocation will be 1.5 copies per location.
#'\dontrun{
#' optim_multi_prep <- multi_location_prep(
#'   lines = 150,  
#'   l = 5, 
#'   copies_per_entry = 7, 
#'   checks = 3, 
#'   rep_checks = c(6,6,6),
#'   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
#'   seed = 1234
#' )
#' designs <- optim_multi_prep$designs
#' field_book_loc_1 <- designs$LOC1$fieldBook
#' head(field_book_loc_1, 10)
#' }
#' @export 
multi_location_prep <- function(
    lines, 
    nrows, 
    ncols, 
    l, 
    planter = "serpentine", 
    plotNumber, 
    desired_avg, 
    copies_per_entry, 
    checks = NULL,
    rep_checks = NULL,
    exptName, 
    locationNames,
    optim_list, 
    seed,
    data = NULL) {
    # set a random seed if it is missing
    if (missing(seed)) seed <- base::sample.int(10000, size = 1)
    if (missing(l)) stop("Please, define the number of locations for this design.")
    if (missing(locationNames) || length(locationNames) != l) {
        locationNames <- paste0("LOC", 1:l)
    } 
    if (missing(plotNumber) || length(plotNumber) != l) {
        plotNumber <- seq(1, 1000 * l, by = 1000)[1:l]
    }
    if (missing(exptName)) exptName <- "PrepExpt"
    if (missing(planter) || is.null(planter)) planter <- "serpentine"
    if (all(c("serpentine", "cartesian") != planter)) {
        stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
    }  
    if (missing(copies_per_entry) & missing(desired_avg)) {
        stop("multi_location_prep() requires either the argument copies_per_entry or desired_avg.")
    } 
    if (missing(copies_per_entry)) {
        copies_per_entry <- ceiling(l * desired_avg)
    }
    add_checks <- FALSE
    if (!is.null(checks) & !is.null(rep_checks)) add_checks <- TRUE
    if (!missing(optim_list)) {
        if (!inherits(optim_list, "MultiPrep")) {
            stop("sparse_list must be an object of class 'MultiPrep'")
        }
        preps <- optim_list
    } else {
        preps <- do_optim(
            design = "prep",
            lines = lines, 
            l = l, 
            copies_per_entry = copies_per_entry,
            add_checks = add_checks, 
            checks = checks, 
            rep_checks = rep_checks,
            seed = seed,
            data = data
        )
    }
    # Merge user input data
    if (!is.null(data)) {
        preps <- merge_user_data(
            optim_out = preps, 
            data = data, 
            lines = lines, 
            add_checks = add_checks, 
            checks = checks, 
            rep_checks = rep_checks
        )
    } 
    # Set the number of rows and columns
    if (missing(nrows) || missing(ncols)) {
        lines_within_loc <- as.numeric(preps$size_locations[1])
        n <- lines_within_loc + sum(rep_checks)
        choices <- factor_subsets(n)$labels
        if (is.null(choices)) {
            stop("The size of locations ends up with a prime number. Please specify a different number of locations or treatments.")
        }
        dif <- vector(mode = "numeric", length = length(choices))
        for (option in 1:length(choices)) {
            dims <- unlist(strsplit(choices[[option]], " x "))
            dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
        }
        df_choices <- data.frame(choices = unlist(choices), diff_dim = dif)
        df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
        dimensions <- unlist(strsplit(df_choices[1,1], " x "))
        nrows <- rep(as.numeric(dimensions[1]), times = l)
        ncols <- rep(as.numeric(dimensions[2]), times = l)
    }
    # Generate the p-rep randomization
    design_randomization <- partially_replicated(
        nrows = nrows, 
        ncols = ncols,
        l = l,
        planter = planter,
        plotNumber = plotNumber,
        locationNames = locationNames,
        exptName = exptName,
        seed = seed,
        multiLocationData = TRUE,
        data = preps$list_locs
    )
    design_randomization$infoDesign$id_design <- "MultiPrep"
    output <- list(
        infoDesign = design_randomization$infoDesign,
        min_pairswise_distance = design_randomization$min_pairswise_distance,
        reps_info = design_randomization$reps_info,
        layoutRandom = design_randomization$layoutRandom, 
        pairsDistance = design_randomization$pairsDistance,
        plotNumber = design_randomization$plotNumber,
        binaryField = design_randomization$binaryField,
        dataEntry = design_randomization$dataEntry,
        genEntries = design_randomization$genEntries,
        treatments_with_reps = design_randomization$treatments_with_reps,
        treatments_with_no_reps = design_randomization$treatments_with_no_reps,
        fieldBook = design_randomization$fieldBook,
        multi_location_data = preps$multi_location_data, 
        list_locs = preps$list_locs, 
        allocation = preps$allocation, 
        size_locations = preps$size_locations
    )
    class(output) <- "FielDHub"
    return(invisible(output))
}

#' Merge user data with optimization output
#'
#' This function merges user data with optimization output to prepare input
#' data for randomization. It accepts the output from the optimization function
#' `do_optim()` and user data with entries and corresponding line names.
#' It returns a modified `optim_out` object containing the merged data.
#'
#' @param optim_out Output object from the optimization function `do_optim()`.
#' @param data A data frame containing entries and corresponding names.
#' @param lines Number of entries.
#' @param add_checks A boolean indicating whether to add checks to the input data.
#' @param checks An integer containing the number of checks.
#' @param rep_checks A numeric vector containing replicates for each checks.
#'
#' @return The modified `optim_out` object containing merged data.
#' @noRd
merge_user_data <- function(
    optim_out,
    data, 
    lines,
    add_checks = FALSE, 
    checks, 
    rep_checks = NULL) {
    if (!is.null(data)) {
        data_entry <- data[, 1:2]
        data_entry <- na.omit(data_entry) 
        colnames(data_entry) <- c("ENTRY", "NAME")
        if (length(data_entry$ENTRY) != length(unique(data_entry$ENTRY))) {
            stop("Please ensure all ENTRIES in data are distinct.")
        }
        if (length(data_entry$NAME) != length(unique(data_entry$NAME))) {
                stop("Please ensure all NAMES in data are distinct.")
        }
        if (add_checks) input_checks <- checks else input_checks <- 0
        if (!is.null(rep_checks)) {
            if (length(rep_checks) != input_checks) {
                stop("Length of checks does not match replications!")
            }
        }
        df_data_lines <- data_entry[(input_checks + 1):nrow(data_entry), ]
        entries_in_file <- nrow(df_data_lines)
        if (entries_in_file != lines) {
            stop("Input lines does not match number of lines in input data!")
        }
        if (add_checks) {
            max_entry <- lines
            vlookup_entry <- c((max_entry + 1):((max_entry + input_checks)), 1:lines)
        } else vlookup_entry <- 1:lines
        user_data_input <- data_entry
        locs <- length(optim_out$list_locs)
        size_location <- vector(mode = "numeric", length = locs)
        merged_list_locs <- setNames(
            vector("list", length = locs), 
            nm = paste0("LOC", 1:locs)
        )
        locs_range <- 1:locs
        # Merge each optimized location into the user data input
        for (LOC in locs_range) {
            iter_loc <- optim_out$list_locs[[LOC]]
            data_input_mutated <- user_data_input %>%
                dplyr::mutate(
                    USER_ENTRY = ENTRY,
                    ENTRY = vlookup_entry
                ) %>%
                dplyr::select(USER_ENTRY, ENTRY, NAME) %>%
                dplyr::left_join(y = iter_loc, by = "ENTRY") %>% 
                {
                    if (inherits(optim_out, "MultiPrep")) {
                        dplyr::select(.data = ., USER_ENTRY, NAME.x, REPS) %>%
                        dplyr::arrange(dplyr::desc(REPS)) %>%
                        dplyr::rename(ENTRY = USER_ENTRY, NAME = NAME.x)
                    } else if (inherits(optim_out, "Sparse")) {
                        dplyr::filter(.data = ., !is.na(NAME.y)) %>% 
                        dplyr::select(USER_ENTRY, NAME.x) %>%
                        dplyr::rename(ENTRY = USER_ENTRY, NAME = NAME.x)
                    } 
                }
            # Store the number of plots (It does not include checks)
            df_to_check <- data_input_mutated[(input_checks + 1):nrow(data_input_mutated), ]
            if (inherits(optim_out, "MultiPrep")) {
                size_location[LOC] <- sum(df_to_check$REPS)
            } else {
                size_location[LOC] <- nrow(df_to_check)
            }
            # Store the merged data
            merged_list_locs[[LOC]] <- data_input_mutated
        }
        # Check if the number of plots are the same after the data merge
        if (!all(size_location == as.numeric(optim_out$size_locations))) {
            stop("After data merge, size of locations does not match!")
        }
        optim_out$list_locs <- merged_list_locs
        return(optim_out)
    }
}