#' Generates a Spatial Partially Replicated Arrangement Design
#'
#' @description
#' Randomly generates a spatial partially replicated (p-rep) design for single or multiple locations. 
#' 
#' @details
#' This function generates and optimizes a partially replicated (p-rep) experimental design for a 
#' given set of treatments and replication levels. The design is represented by a matrix and optimized 
#' using a pairwise distance metric. The function outputs various information about the optimized design 
#' including the field layout, replicated and unreplicated treatments, and pairwise distances between 
#' treatments. Note that the design generation needs the dimension of the field (number of rows and columns).
#'
#' @param nrows Numeric vector with the number of rows field at each location.
#' @param ncols Numeric vector with the number of columns field at each location.
#' @param repGens Numeric vector with the amount genotypes to replicate.
#' @param repUnits Numeric vector with the number of reps of each genotype.
#' @param planter Option for \code{serpentine} or \code{cartesian} movement. By default  \code{planter = 'serpentine'}. 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Name for each location.
#' @param multiLocationData (optional) Option to pass an entry list for multiple locations. 
#' By default \code{multiLocationData = FALSE}.
#' @param data (optional) Data frame with 3 columns: \code{ENTRY | NAME | REPS}. If  
#' \code{multiLocationData = TRUE} then the \code{data} must have
#' 4 columns: \code{LOCATION | ENTRY | NAME | REPS}
#' 
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb],
#'         Jean-Marc Montpetit [ctb],
#'         Richard Horsley [ctb]
#' 
#' 
#' 
#' @return A list with several elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{layoutRandom} is a matrix with the randomization layout.
#'   \item \code{plotNumber} is a matrix with the layout plot number.
#'   \item \code{binaryField} is a matrix with the binary field.
#'   \item \code{dataEntry} is a data frame with the data input.
#'   \item \code{genEntries} is a list with the entries for replicated and non-replicated parts.
#'   \item \code{fieldBook} is a data frame with field book design. This includes the index (Row, Column).
#'   \item \code{min_pairwise_distance} is a data frame with the minimum pairwise distance between 
#'           each pair of locations.
#'   \item \code{reps_info} is a data frame with information on the number of replicated and 
#'           non-replicated treatments at each location.
#'   \item \code{pairsDistance} is a data frame with the pairwise distances between each pair of 
#'           treatments.
#'   \item \code{treatments_with_reps} is a list with the entries for the replicated part of the design.
#'   \item \code{treatments_with_no_reps} is a list with the entries for the non-replicated part of the design.
#' }
#'
#' @references
#' Cullis, S., B. R., & Coombes, N. E. (2006). On the design of early generation variety trials
#' with correlated data. Journal of Agricultural, Biological, and Environmental Statistics, 11,
#' 381–393. https://doi.org/10.1198/108571106X154443
#'
#'
#' @examples
#' # Example 1: Generates a spatial optimized partially replicated arrangement design in one 
#' # location with 335 genotypes for a field with dimensions 15 rows x 28 cols. 
#' # Note that there are 250 genotypes unreplicated (only one time), 85 genotypes replicated 
#' # two times, and three checks 8 times each.
#'\dontrun{
#' prep_deseign1 <- partially_replicated(
#'  nrows = 12, 
#'  ncols = 37,  
#'  repGens = c(250, 85, 3),
#'  repUnits = c(1, 2, 8),
#'  planter = "cartesian", 
#'  plotNumber = 101,
#'  seed = 77
#' )
#' prep_deseign1$infoDesign
#' prep_deseign1$layoutRandom
#' prep_deseign1$plotNumber
#' head(prep_deseign1$fieldBook, 12)
#' }
#' 
#' # Example 2: Generates a spatial optimized partially replicated arrangement design with 492 
#' # genotypes in a field with dimensions 30 rows x 20 cols. Note that there 384 genotypes 
#' # unreplicated (only one time), 108 genotypes replicated two times. 
#' # In this case we don't have check plots.
#' # As example, we set up the data option with the entries list.
#'\dontrun{
#' NAME <- paste("G", 1:492, sep = "")
#' repGens = c(108, 384);repUnits = c(2,1)
#' REPS <- rep(repUnits, repGens)
#' treatment_list <- data.frame(list(ENTRY = 1:492, NAME = NAME, REPS = REPS))
#' head(treatment_list, 12) 
#' tail(treatment_list, 12)
#' prep_deseign2 <- partially_replicated(
#'   nrows = 30, 
#'   ncols = 20, 
#'   planter = "serpentine", 
#'   plotNumber = 101,
#'   seed = 41,
#'   data = treatment_list
#' )
#' prep_deseign2$infoDesign
#' prep_deseign2$layoutRandom
#' prep_deseign2$plotNumber
#' head(prep_deseign2$fieldBook, 10)
#' }
#' 
#' @export
partially_replicated <- function(
    nrows = NULL, 
    ncols = NULL, 
    repGens = NULL, 
    repUnits = NULL, 
    planter = "serpentine", 
    l = 1, 
    plotNumber = 101, 
    seed = NULL, 
    exptName = NULL, 
    locationNames = NULL,
    multiLocationData = FALSE,  
    data = NULL) {
    
    if (all(c("serpentine", "cartesian") != planter)) {
        base::stop('Input "planter" is unknown. Please, choose one: "serpentine" or "cartesian"')
    }
  
    if (is.null(nrows) || is.null(ncols) || !is.numeric(nrows) || !is.numeric(ncols)) {
        base::stop('Basic design parameters missing (nrows, ncols) or is not numeric.')
    }
    if (length(nrows) != l) {
        if (length(nrows) < l) {
            warning("Number of nrows values not matching number of locations", call. = FALSE)
            # Filling missing nrows values with last provided value
            nrows <- c(nrows, rep(nrows[length(nrows)], l - length(nrows)))
        } else {
            warning("Number of nrows values not matching number of locations", call. = FALSE)
            # Filling missing nrows values with last provided value
            nrows <- nrows[1:l]
        }
    }
    if (length(ncols) != l) {
        if (length(ncols) < l) {
            warning("Number of ncols values not matching number of locations", call. = FALSE)
            # Filling missing nrows values with last provided value
            ncols <- c(ncols, rep(ncols[length(ncols)], l - length(ncols)))
        } else  {
            warning("Number of ncols values not matching number of locations", call. = FALSE)
            # Filling missing nrows values with last provided value
            ncols <- ncols[1:l]
        }
    }
    
    if (is.null(data)) {
        if (is.null(repGens) || is.null(repUnits)) {
            base::stop("Input repGens and repUnits are missing.")
        } 
        if (length(repGens) != length(repUnits)) {
            base::stop("Input repGens and repUnits should have the same length.")
        }
    }
    
    if (!is.numeric(plotNumber) && !is.integer(plotNumber)) {
        stop("plotNumber should be an integer or a numeric vector.")
    }
    
    if (any(plotNumber %% 1 != 0)) {
        stop("plotNumber should be integers.")
    }
    
    if (!is.null(l)) {
        if (is.null(plotNumber) || length(plotNumber) != l) {
            if (l > 1) {
                plotNumber <- seq(1001, 1000*(l+1), 1000)
            } else {
                plotNumber <- 1001
            } 
        }
    } else stop("Number of locations/sites is missing")
    if (!is.null(data)) {
        if (multiLocationData) {
            if (is.data.frame(data)) {
                gen_list <- data
                gen_list <- as.data.frame(gen_list)
                gen_list <- na.omit(gen_list[, 1:4])
                if (ncol(gen_list) < 4) {
                    base::stop("Input data should have 4 columns: LOCATION | ENTRY | NAME | REPS")
                }
                colnames(gen_list) <- c("LOCATION", "ENTRY", "NAME", "REPS")
                if (any(gen_list$ENTRY < 1) || any(gen_list$REPS < 1)) {
                    base::stop("Please ensure all ENTRIES and REPS in data are positive integers.")
                }
                locs_in_data <- length(unique(gen_list$LOCATION))
                if (locs_in_data != l) {
                  stop("Number of locations in data do not match with the input value l")
                }
                # Create a space in memory for the locations data entry list
                list_locs <- setNames(
                    object = vector(mode = "list", length = l), 
                    nm = unique(gen_list$LOCATION)
                )
                # Generate the lists of entries for each location
                for (site in unique(gen_list$LOCATION)) {
                    df_loc <- gen_list |> 
                        dplyr::filter(LOCATION == site) |> 
                        dplyr::mutate(ENTRY = as.numeric(ENTRY)) |> 
                        dplyr::select(ENTRY, NAME, REPS) |>
                        dplyr::arrange(dplyr::desc(REPS))

                    if (length(df_loc$ENTRY) != length(unique(df_loc$ENTRY))) {
                      stop("Please ensure all ENTRIES in data are distinct.")
                    }
                    if (length(df_loc$NAME) != length(unique(df_loc$NAME))) {
                      stop("Please ensure all NAMES in data are distinct.")
                    }
                    
                    list_locs[[site]] <- df_loc
                }
            } else if (is.list(data)){
                list_locs <- data
            }
        } else {
            if (!is.data.frame(data)) base::stop("Data must be a data frame!")
            if (ncol(data) < 3) {
                base::stop("Input data should have 3 columns: ENTRY | NAME | REPS")
            }
            gen_list <- data[, 1:3]
            gen_list <- na.omit(gen_list)
            colnames(gen_list) <- c("ENTRY", "NAME", "REPS")
            if (length(gen_list$ENTRY) != length(unique(gen_list$ENTRY))) {
                stop("Please ensure all ENTRIES in data are distinct.")
            }
            if (length(gen_list$NAME) != length(unique(gen_list$NAME))) {
                stop("Please ensure all NAMES in data are distinct.")
            }
            if (any(gen_list$ENTRY < 1) || any(gen_list$REPS < 1)) {
                base::stop("Please ensure all ENTRIES and REPS in data are positive integers.")
            } 
            gen_list_order <- gen_list[order(gen_list$REPS, decreasing = TRUE), ]
            GENOS <- subset(gen_list_order, REPS == 1)
            reps_data <- subset(gen_list_order, REPS > 1)
            reps_data <- reps_data[order(reps_data$REPS, decreasing = TRUE),]
            RepChecks <- as.vector(reps_data[,3])
            checksEntries <- as.vector(reps_data[,1])
            checks <- length(checksEntries)
            lines <- sum(GENOS$REPS)
            t_plots <- sum(as.numeric(gen_list$REPS))
            if (numbers::isPrime(t_plots)) {
                stop("No options when the total number of plots is a prime number.", call. = FALSE)
            }
            list_locs <- vector(mode = "list", length = l)
            for (data_list in 1:l) {
                list_locs[[data_list]] <- gen_list
            }
        }
    } else if (is.null(data)) {
        if (length(repGens) != length(repUnits)) {
            stop("Input repGens and repUnits need to be of the same length.")
        } 
        t_plots <- sum(repGens * repUnits)
        if (numbers::isPrime(t_plots)) {
            stop("No options when the total number of plots is a prime number.", call. = FALSE)
        }
        ENTRY <- 1:sum(repGens)
        NAME <- paste(rep("G", sum(repGens)), 1:sum(repGens), sep = "")
        # REPS <- as.numeric(sort(rep(repUnits, times = repGens), decreasing = TRUE))
        REPS <- as.numeric(rep(repUnits, times = repGens))
        data <- data.frame(list(ENTRY = ENTRY,
                                NAME = NAME,
                                REPS = REPS))
        colnames(data) <- c("ENTRY", "NAME", "REPS")
        list_locs <- vector(mode = "list", length = l)
        for (data_list in 1:l) {
            list_locs[[data_list]] <- data
        }
    }
    if (is.null(seed)) seed <- base::sample.int(10000, size = 1)
    set.seed(seed)
    field_book_sites <- vector(mode = "list", length = l)
    layout_random_sites <- vector(mode = "list", length = l)
    plot_numbers_sites <- vector(mode = "list", length = l)
    col_checks_sites <- vector(mode = "list", length = l)
    pairwise_distance_sites <- vector(mode = "list", length = l)
    min_distance_sites <- vector(mode = "numeric", length = l)
    treatments_with_reps = vector(mode = "list", length = l)
    treatments_with_no_reps = vector(mode = "list", length = l)
    rows_incidence <- vector(mode = "numeric", length = l)
    for (sites in 1:l) {
        prep <- pREP(
            nrows = nrows[sites], 
            ncols = ncols[sites], 
            Fillers = 0,
            seed = seed, 
            optim = TRUE, 
            niter = 1000, 
            data = list_locs[[sites]]
        )
        rows_incidence[sites] <- prep$rows_incidence[length(prep$rows_incidence)]
        min_distance_sites[sites] <- prep$min_distance
        dataInput <- prep$gen.list
        BINAY_CHECKS <- prep$binary.field
        random_entries_map <- as.matrix(prep$field.map)
        genEntries <- prep$gen.entries

        EntryChecks <- prep$entryChecks
        Checks <- length(EntryChecks)

        if (sum(genEntries[[2]]) == 0) {
            rep_treatments <- 0
        } else rep_treatments <- length(genEntries[[2]])
        
        if (!is.null(exptName)) {
            Name_expt <- exptName[1]
        }else Name_expt <- "Expt1"
        
        split_name_spat <- function(){
            split_names <- base::matrix(
                data = Name_expt, 
                nrow = nrows, 
                ncol = ncols, 
                byrow = TRUE
            )
            return(list(my_names = split_names))
        }
        
        plot_number_spat <- function() {
            datos_name <- split_name_spat()$my_names
            plot_n_start <- plotNumber[sites]
            plot_number(
                planter = planter,
                plot_number_start = plot_n_start,
                layout_names = datos_name,
                expe_names = Name_expt,
                fillers = 0
            )
        }
        
        if (is.null(locationNames) || length(locationNames) != l) {
            locationNames <- paste0("LOC", 1:l)
        } 
        plot_num <- plot_number_spat()$w_map_letters1
        plot_number_L <- apply(plot_num, c(1,2), as.numeric)
        export_spat <- function() {
            loc <- locationNames
            random_entries_map <- as.matrix(prep$field.map)
            plot_number_L <- as.matrix(plot_number_L)
            Col_checks <- as.matrix(BINAY_CHECKS)
            my_names <- as.matrix(split_name_spat()$my_names)
            year <- format(Sys.Date(), "%Y")
            my_data_VLOOKUP <- prep$gen.list
            results_to_export <- list(
                random_entries_map, 
                plot_number_L, 
                Col_checks, 
                my_names
            )
            final_expt_export <- export_design(
                G = results_to_export, 
                movement_planter =  planter,
                location = loc[sites], 
                Year = year, 
                data_file = my_data_VLOOKUP,
                reps = FALSE
            )
            return(list(final_expt = final_expt_export))
        }
        
        fieldBook <- as.data.frame(export_spat()$final_expt)
        fieldBook <- fieldBook[,-11]
        ID <- 1:nrow(fieldBook)
        fieldBook <- fieldBook[, c(6,7,9,4,2,3,5,1,10)]
        fieldBook <- cbind(ID, fieldBook)
        colnames(fieldBook)[10] <- "TREATMENT"
        layoutR = prep$field.map
        rownames(layoutR) <- paste("Row", nrow(layoutR):1, sep = "")
        colnames(layoutR) <- paste("Col", 1:ncol(layoutR), sep = "")
        rownames(plot_num) <- paste("Row", nrow(plot_num):1, sep = "")
        colnames(plot_num) <- paste("Col", 1:ncol(plot_num), sep = "")
        
        field_book_sites[[sites]] <- fieldBook
        layout_random_sites[[sites]] <- layoutR
        plot_numbers_sites[[sites]] <- plot_number_L
        col_checks_sites[[sites]] <- as.matrix(BINAY_CHECKS)
        pairwise_distance_sites[[sites]] <- prep$pairwise_distance
        treatments_with_reps[[sites]] = prep$replicated_treatments
        treatments_with_no_reps[[sites]] = prep$unreplicated_treatments
    }
    
    field_book <- dplyr::bind_rows(field_book_sites)
    RepChecks <- prep$reps.checks
    reps_info <- data.frame(
        LOCATION = locationNames,
        Replicated = lengths(treatments_with_reps),
        Unreplicated = lengths(treatments_with_no_reps)
    )
    min_dist_df <- data.frame(LOCATION = locationNames, DISTANCE = min_distance_sites)
    infoDesign <- list(
        rows = nrows,
        columns = ncols,
        min_distance = min_distance_sites,
        incidence_in_rows = rows_incidence,
        locations = l,
        planter = planter,
        seed = seed,
        id_design = 13)
    output <- list(
        infoDesign = infoDesign,
        min_pairwise_distance = min_dist_df,
        reps_info = reps_info,
        layoutRandom = layout_random_sites, 
        pairsDistance = pairwise_distance_sites,
        plotNumber = plot_numbers_sites,
        binaryField = col_checks_sites,
        dataEntry = dataInput,
        genEntries = genEntries,
        treatments_with_reps = treatments_with_reps,
        treatments_with_no_reps = treatments_with_no_reps,
        fieldBook = field_book
    )
    class(output) <- "FielDHub"
    return(invisible(output))
}
