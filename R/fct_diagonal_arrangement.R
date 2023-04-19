#' Spatial Un-replicated Diagonal Arrangement Design
#' 
#' Randomly generates an spatial un-replicated diagonal arrangement design.
#'
#' @param nrows Number of rows in the field.
#' @param ncols Number of columns in the field.
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param checks Number of genotypes checks. 
#' @param kindExpt Type of diagonal design, with single options: Single Un-replicated Diagonal Checks
#' \code{'SUDC'} and Decision Blocks Un-replicated Design with Diagonal Checks \code{'DBUDC'} 
#' for multiple experiments. By default \code{kindExpt = 'SUDC'}.
#' @param planter Option for \code{serpentine} or \code{cartesian} plot arrangement. 
#' By default  \code{planter = 'serpentine'}.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. 
#' By default \code{plotNumber = 101}.
#' @param splitBy Option to split the field when \code{kindExpt = 'DBUDC'} is selected. 
#' By default \code{splitBy = 'row'}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param blocks Number of experiments or blocks to generate an \code{DBUDC} design. 
#' If \code{kindExpt = 'DBUDC'} and data is null, \code{blocks} are mandatory.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Names each location.
#' @param multiLocationData (optional) Option to pass an entry list for multiple locations. 
#' By default \code{multiLocationData = FALSE}.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME }.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' 
#' @return A list with five elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{layoutRandom} is a matrix with the randomization layout.
#'   \item \code{plotsNumber} is a matrix with the layout plot number.
#'   \item \code{data_entry} is a data frame with the data input.
#'   \item \code{fieldBook} is a data frame with field book design. This includes the index (Row, Column).
#' }
#'
#' @references
#' Clarke, G. P. Y., & Stefanova, K. T. (2011). Optimal design for early-generation plant
#' breeding trials with unreplicated or partially replicated test lines. Australian & New
#' Zealand Journal of Statistics, 53(4), 461–480.
#'
#'
#' @examples
#' 
#' # Example 1: Generates a spatial single diagonal arrangement design in one location
#' # with 270 treatments and 30 check plots for a field with dimensions 15 rows x 20 cols
#' # in a serpentine arrangement.
#' spatd <- diagonal_arrangement(
#'   nrows = 15, 
#'   ncols = 20, 
#'   lines = 270, 
#'   checks = 4, 
#'   plotNumber = 101, 
#'   kindExpt = "SUDC", 
#'   planter = "serpentine", 
#'   seed = 1987,
#'   exptName = "20WRY1", 
#'   locationNames = "MINOT"
#' )
#' spatd$infoDesign
#' spatd$layoutRandom
#' spatd$plotsNumber
#' head(spatd$fieldBook, 12)
#' 
#' # Example 2: Generates a spatial decision block diagonal arrangement design in one location
#' # with 720 treatments allocated in 5 experiments or blocks for a field with dimensions
#' # 30 rows x 26 cols in a serpentine arrangement. In this case, we show how to set up the data 
#' # option with the entries list.
#' checks <- 5;expts <- 5
#' list_checks <- paste("CH", 1:checks, sep = "")
#' treatments <- paste("G", 6:725, sep = "")
#' treatment_list <- data.frame(list(ENTRY = 1:725, NAME = c(list_checks, treatments)))
#' head(treatment_list, 12) 
#' tail(treatment_list, 12)
#' spatDB <- diagonal_arrangement(
#'   nrows = 30, 
#'   ncols = 26,
#'   checks = 5, 
#'   plotNumber = 1, 
#'   kindExpt = "DBUDC", 
#'   planter = "serpentine", 
#'   splitBy = "row", 
#'   blocks = c(150,155,95,200,120),
#'   data = treatment_list
#' )
#' spatDB$infoDesign
#' spatDB$layoutRandom
#' spatDB$plotsNumber
#' head(spatDB$fieldBook,12)
#'
#' # Example 3: Generates a spatial decision block diagonal arrangement design in one location
#' # with 270 treatments allocated in 3 experiments or blocks for a field with dimensions
#' # 20 rows x 15 cols in a serpentine arrangement. Which in turn is an augmented block (3 blocks).
#' spatAB <- diagonal_arrangement(
#'   nrows = 20, 
#'   ncols = 15, 
#'   lines = 270, 
#'   checks = 4, 
#'   plotNumber = c(1,1001,2001), 
#'   kindExpt = "DBUDC", 
#'   planter = "serpentine",
#'   exptName = c("20WRA", "20WRB", "20WRC"), 
#'   blocks = c(90, 90, 90),
#'   splitBy = "column"
#' )
#' spatAB$infoDesign
#' spatAB$layoutRandom
#' spatAB$plotsNumber
#' head(spatAB$fieldBook,12)
#' 
#' @export
diagonal_arrangement <- function(
    nrows = NULL, 
    ncols = NULL, 
    lines = NULL, 
    checks = NULL, 
    planter = "serpentine", 
    l = 1,
    plotNumber = 101, 
    kindExpt = "SUDC", 
    splitBy = "row",
    seed = NULL, 
    blocks = NULL,
    exptName = NULL, 
    locationNames = NULL, 
    multiLocationData = FALSE,
    data = NULL) {
  
    if (all(c("serpentine", "cartesian") != planter)) {
        base::stop('Input for planter is unknown. Please, choose one: "serpentine" or "cartesian"')
    }
    if (all(c("SUDC", "DBUDC") != kindExpt)) {
        base::stop('Input for kindExpt is unknown. Please, choose one: "SUDC" or "DBUDC"')
    }
    if (all(c("column", "row") != splitBy)) {
        base::stop('Input for splitBy is unknown. Please, choose one: "column" or "row"')
    }
    
    if (!inherits(plotNumber,"list")) { 
        if (!is.null(plotNumber) && is.numeric(plotNumber)) {
          if(any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
              base::stop('diagonal_arrangement() requires plotNumber to be positive and sorted integers.')
          }
        }
        if(!is.numeric(plotNumber) && !is.integer(plotNumber)) {
            base::stop("plotNumber should be an integer or a numeric vector.")
        }
        if (any(plotNumber %% 1 != 0)) {
            base::stop("plotNumber should be integers.")
        }
    }
    
    if (kindExpt == "SUDC") {
        if (!is.null(l)) {
        if (is.null(plotNumber) || length(plotNumber) != l) {
            if (l > 1){
            plotNumber <- as.list(seq(1001, 1000*(l+1), 1000))
            } else plotNumber <- list(1001)
        }
        } else stop("Number of locations/sites is missing")
    }
    
    if (kindExpt != "SUDC") {
        num_expts <- length(blocks)
        if (!is.null(l)) {
        if (!is.null(plotNumber)) {
            if (inherits(plotNumber,"list")) {
            if (all(lengths(plotNumber) == num_expts) &
                length(plotNumber) == l) {
                plotNumber <- plotNumber
            } else plotNumber <- as.list(seq(1001, 1000*(l+1), 1000))
            } else {
            if (l == 1) {
                if (length(plotNumber) == num_expts) {
                plotNumber <- list(plotNumber)
                } else if (length(plotNumber) == 1) {
                plotNumber <- as.list(plotNumber)
                }
            } else {
                if (length(plotNumber) == l) {
                plotNumber <- as.list(plotNumber)
                } else plotNumber <- as.list(seq(1001, 1000*(l+1), 1000))
            }
            } 
        }
        }
    }
    # 
    if (!is.null(data)) {
        arg1 <- list(nrows, ncols, l);arg2 <- c(nrows, ncols, l)
        if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
            base::stop("'diagonal_arrangement()' requires input nrows, ncols, and l to be numeric and distint of NULL")
        }
    } else {
        arg1 <- list(nrows, ncols, lines, l);arg2 <- c(nrows, ncols, lines, l)
        if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
            base::stop("'diagonal_arrangement()' requires input nrows, ncols, and l to be numeric and distint of NULL")
        }
    } 
    # Set Option_NCD to TRUE
    Option_NCD <- TRUE
    if (splitBy == "column") {
        stacked <- "By Column"
    } else {
        stacked <- "By Row"
    }
    # Read or generate the data based on the inputs
    getData <- unrep_data_parameters(
        lines = lines,
        nrows = nrows, 
        ncols = ncols,
        checks = checks, 
        kindExpt = kindExpt, 
        planter = planter, 
        blocks = blocks,
        stacked = stacked,
        l = l,
        Option_NCD = Option_NCD,
        multiLocationData = multiLocationData,
        data = data
    )
    
    if (is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
    ## Start for loop
    field_book_sites <- vector(mode = "list", length = l)
    layout_random_sites <- vector(mode = "list", length = l)
    plot_numbers_sites <- vector(mode = "list", length = l)
    col_checks_sites <- vector(mode = "list", length = l)
    RepChecks_list <- vector(mode = "list", length = l)
    percentChecks_vector <- vector(mode = "numeric", length = l)
    if (is.null(seed)) seed = sample.int(100000, 1)
    set.seed(seed)
    for (sites in 1:l) {
        checks_percentages <- available_percent(
            n_rows = nrows, 
            n_cols = ncols,
            checks = getData$checksEntries[[sites]],
            Option_NCD = TRUE, 
            kindExpt = kindExpt,
            stacked = stacked,
            planter_mov1 = planter, 
            data = getData$data_entry[[sites]],
            dim_data = getData$dim_data_entry[[sites]],
            dim_data_1 = getData$dim_data_1[[sites]],
            Block_Fillers = NULL
        )
        if (is.null(checks_percentages) & !is.null(getData$data_entry[[sites]])) {
            checks <- as.numeric(checks)
            total_entries <- as.numeric(getData$dim_data_entry[[sites]])
            lines <- total_entries - checks
            t1 <- floor(lines + lines * 0.10)
            t2 <- ceiling(lines + lines * 0.20)
            t <- t1:t2
            n <- t[-numbers::isPrime(t)]
            choices_list <- list()
            i <- 1
            for (n in t) {
                choices_list[[i]] <- factor_subsets(n, diagonal = TRUE)$labels
                i <- i + 1
            }
            choices <- unlist(choices_list[!sapply(choices_list, is.null)])
            if (is.null(choices)) {
                stop("Field dimensions do not fit with the data entered. Try another amount of treatments!", 
                    call. = FALSE)
            } 
            if (!is.null(choices)) {
                message(cat("\n", "Error in diagonal_arrangement(): ", "\n", "\n",
                    "Field dimensions do not fit with the data entered!", "\n",
                    "Try one of the following options: ", "\n"))
                return(for (i in 1:length(choices)) {print(choices[[i]])})
            } else {
                stop("Field dimensions do not fit with the data entered. Try another amount of treatments!", 
                    call. = FALSE)
            }
        }
        new_lines <- nrow(getData$data_entry[[sites]]) - checks
        infoP <- as.data.frame(checks_percentages$P)
        infoP$V7 <- nrows*ncols - infoP[,6]
        minLines <- min(infoP$V7)
        maxLines <- max(infoP$V7)
        exptlines <- new_lines
        if (exptlines >= minLines & exptlines <= maxLines) {
            if (exptlines %in% as.vector(infoP[,7])) {
                Option_NCD <- TRUE # FALSE
                Exptlines <- exptlines
            } else if (all(as.vector(infoP[,7]) != exptlines)) {
                Option_NCD <- TRUE
                Exptlines <- exptlines
                if (kindExpt == "DBUDC") {
                    Block_Fillers <- as.numeric(getData$Blocks[[sites]])
                    checks_percentages <- available_percent(
                        n_rows = nrows,
                        n_cols = ncols,
                        checks = getData$checksEntries[[sites]], 
                        Option_NCD = Option_NCD,
                        kindExpt = kindExpt,
                        stacked = stacked,
                        planter_mov1 = planter,
                        data = getData$data_entry[[sites]],
                        dim_data = getData$dim_data_entry[[sites]],
                        dim_data_1 = getData$dim_data_1[[sites]],
                        Block_Fillers = Block_Fillers
                    )
                    infoP <- as.data.frame(checks_percentages$P)
                    infoP$V7 <- Exptlines
                } else {
                    Block_Fillers <- NULL
                    checks_percentages <- available_percent(
                        n_rows = nrows, 
                        n_cols = ncols,
                        checks = getData$checksEntries[[sites]],
                        Option_NCD = Option_NCD,
                        Visual_ch = NULL,
                        visualCheck = FALSE,
                        kindExpt = kindExpt,
                        stacked = stacked,
                        planter_mov1 = planter, 
                        data = getData$data_entry[[sites]],
                        dim_data = getData$dim_data_entry[[sites]],
                        dim_data_1 = getData$dim_data_1[[sites]],
                        Block_Fillers = Block_Fillers
                    )
                    infoP <- as.data.frame(checks_percentages$P)
                    infoP$V7 <- Exptlines
                }  
            }
        } else stop("Field dimensions do not fit with the data entered!")
        percent_table <- checks_percentages$dt
        percent_col <- percent_table[,2]
        len <- length(percent_col)
        selected_percent <- as.numeric(percent_col[len])
        rand_checks <- random_checks(
            dt = checks_percentages$dt, 
            d_checks = checks_percentages$d_checks, 
            p = infoP, 
            percent = selected_percent,
            exptlines = NULL,
            kindExpt = kindExpt, 
            planter_mov = planter, 
            Checks = getData$checksEntries[[sites]], 
            stacked = stacked,
            data = getData$data_entry[[sites]], 
            data_dim_each_block = checks_percentages$data_dim_each_block,
            n_reps = NULL, 
            Option_NCD = Option_NCD,
            seed = NULL
        )
        data_entry <- getData$data_entry[[sites]]
        w_map <- rand_checks$map_checks
        n_rows = nrows; n_cols = ncols
        my_split_r <- rand_checks$map_checks
        multi <- kindExpt == "RDC" || kindExpt == "DBUDC"
        if (multi == TRUE) {
            map_checks <- rand_checks$map_checks
            data_entry <- getData$data_entry[[sites]]
            if (kindExpt == "DBUDC" && stacked == "By Row") {
                data_dim_each_block <- checks_percentages$data_dim_each_block
                my_row_sets <- automatically_cuts(
                    data = map_checks, planter_mov = planter,
                    stacked = "By Row", 
                    dim_data = data_dim_each_block
                )[[1]]
                if (is.null(my_row_sets)) return(NULL)
                n_blocks <- length(my_row_sets)
            } else if (kindExpt == "DBUDC" && stacked == "By Column") {
                data_dim_each_block <- checks_percentages$data_dim_each_block
                cuts_by_c <- automatically_cuts(data = map_checks, planter_mov = planter, stacked = "By Column",
                                                dim_data = data_dim_each_block)
                if (is.null(cuts_by_c)) return(NULL)
                n_blocks <- length(cuts_by_c)
                m = diff(cuts_by_c)
                my_col_sets = c(cuts_by_c[1], m)
            }
            if (stacked == "By Column") {
                data_random <- get_random_stacked(
                    stacked = "By Column", 
                    n_rows = n_rows,
                    n_cols = n_cols,
                    matrix_checks = my_split_r,
                    Fillers = FALSE,
                    checks = getData$checksEntries[[sites]],
                    data = data_entry,
                    data_dim_each_block = data_dim_each_block
                )
            } else {
                if (Option_NCD == FALSE) {
                    data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
                    data_random <- get_DBrandom(
                        binaryMap = w_map, 
                        data_dim_each_block = data_dim_each_block, 
                        data_entries = data_entry1,
                        planter = planter
                    )
                } else {
                    Block_Fillers <- as.numeric(getData$Blocks[[sites]])
                    data_random <- get_random(
                        n_rows = nrows, 
                        n_cols = ncols, 
                        d_checks = my_split_r,
                        Fillers = FALSE, 
                        row_sets = my_row_sets,
                        checks = getData$checksEntries[[sites]], 
                        data = data_entry, 
                        planter_mov  = planter,
                        Multi.Fillers = TRUE, 
                        which.blocks = Block_Fillers
                    )
                }
            }
        } else {
            n_blocks <- 1
            data_random <- get_single_random(
                n_rows = n_rows, 
                n_cols = n_cols, 
                matrix_checks = my_split_r, 
                checks = getData$checksEntries[[sites]], 
                data = data_entry
            ) 
        }
        if (kindExpt != "DBUDC") {
            n_blocks <- 1
        } else {
            n_blocks <- length(data_dim_each_block)
        }
        if (!missing(exptName) & !is.null(exptName)) {
        if (length(exptName) == n_blocks) { 
            expe_names <- exptName 
        } else { 
            expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
        }
        } else {
            expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
        }
        map_letters <- data_random$w_map_letters
        if (kindExpt == "DBUDC") {
            if (stacked == "By Row" ) {
                split_name_diagonal1 <- names_layout(
                    w_map = w_map, 
                    stacked = "By Row",
                    kindExpt = "DBUDC", 
                    w_map_letters = map_letters,
                    data_dim_each_block = data_dim_each_block, 
                    expt_name = expe_names, 
                    Checks = getData$checksEntries[[sites]]
                )
            } else if (stacked == "By Column") {
                split_name_diagonal1 <- names_layout(
                    w_map = w_map, 
                    stacked = "By Column",
                    kindExpt = "DBUDC",
                    data_dim_each_block = data_dim_each_block,
                    w_map_letters = map_letters,
                    expt_name = expe_names,
                    Checks = getData$checksEntries[[sites]]
                )
            }
        } else {
            w_map_letters1 <- data_random$w_map_letters
            split_name_diagonal1 <- names_layout(
                w_map = w_map, 
                stacked = "By Row",
                kindExpt = "SUDC",
                expt_name = expe_names 
            )
        }
        plot_n_start <- as.numeric(plotNumber[[sites]])
        datos_name <- split_name_diagonal1$my_names
        fillers <- sum(datos_name == "Filler")
        if (kindExpt == "DBUDC") { 
            plot_nuber_layout <- plot_number(
                planter = planter,
                plot_number_start = plot_n_start,
                layout_names = datos_name,
                expe_names = expe_names,
                fillers = fillers
            )
        } else {
            fillers <- sum(datos_name == "Filler")
            plot_nuber_layout <- plot_number(
                planter = planter,
                plot_number_start = plot_n_start,
                layout_names = datos_name,
                expe_names = expe_names,
                fillers = fillers
            )
        }
        my_export_design <- function(){

            year <- format(Sys.Date(), "%Y")
            
            if (is.null(data_random$rand)) base::stop("Random matrix is missing.")
            if (is.null(rand_checks$col_checks)) base::stop("checks matrix is missing.")
            if (is.null(plot_nuber_layout$w_map_letters1)) base::stop("Plot numbers matrix is missing.")
            if (is.null(split_name_diagonal1$my_names)) base::stop("Names matrix is missing.")
            
            movement_planter <- planter
            random_entries_map <- data_random$rand
            random_entries_map[random_entries_map == "Filler"] <- 0
            random_entries_map <- apply(random_entries_map, 2, as.numeric)
            my_data_VLOOKUP <- getData$data_entry[[sites]]
            COLNAMES_DATA <- colnames(my_data_VLOOKUP)
            if (Option_NCD == TRUE) {
                if (kindExpt != "DBUDC") {
                Entry_Fillers <- data.frame(list(0, "Filler"))
                } else {
                Entry_Fillers <- data.frame(list(0, "Filler", "NA"))
                }
                colnames(Entry_Fillers) <- COLNAMES_DATA
                my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
            }
            Col_checks <- as.matrix(rand_checks$col_checks)
            plot_number <- as.matrix(plot_nuber_layout$w_map_letters1)
            plot_number <- apply(plot_number, 2 ,as.numeric)
            my_names <- split_name_diagonal1$my_names
            if (multi == FALSE && Option_NCD == TRUE) {
                my_names <- split_name_diagonal1$my_names
            }
            results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
            final_expt_export <- export_design(
                G = results_to_export, 
                movement_planter = movement_planter,
                location = locationNames[sites], 
                Year = year,
                data_file = my_data_VLOOKUP, 
                reps = FALSE
            )
            if (Option_NCD == TRUE) {
                final_expt_export$CHECKS <- ifelse(
                    final_expt_export$NAME == "Filler",
                    0,
                    final_expt_export$CHECKS
                )
            }
            return(list(final_expt = final_expt_export))
        }
        
        fieldBook <- as.data.frame(my_export_design()$final_expt)
        if (is.null(fieldBook) || !is.data.frame(fieldBook)) {
            base::stop("fieldBook is NULL or != data frame.")
        }
        if (dim(fieldBook)[1]*dim(fieldBook)[2] == 0) {
            base::stop("fieldBook is NULL or != data frame or length 0.")
        }
        fieldBook <- fieldBook[,-11]
        
        ID <- 1:nrow(fieldBook)
        fieldBook <- fieldBook[, c(6,7,9,4,2,3,5,1,10)]
        fieldBook <- cbind(ID, fieldBook)
        colnames(fieldBook)[10] <- "TREATMENT"
        rownames(fieldBook) <- 1:nrow(fieldBook)
        
        linesexpt <- data_random$Lines
        plot_num <- as.matrix(plot_nuber_layout$w_map_letters1)
        layoutR <- data_random$rand
        rownames(layoutR) <- paste("Row", nrow(layoutR):1, sep = "")
        colnames(layoutR) <- paste("Col", 1:ncol(layoutR), sep = "")
        rownames(plot_num) <- paste("Row", nrow(plot_num):1, sep = "")
        colnames(plot_num) <- paste("Col", 1:ncol(plot_num), sep = "")
        dataEntry <- getData$data_entry[[sites]]
        infoD <- as.data.frame(checks_percentages$dt)
        exptNames <- split_name_diagonal1$my_names
        Col_checks <- as.matrix(rand_checks$col_checks)
        Fillers <- 0
        RepChecks_site <- numeric()
        z <- 1
        for (i in factor(getData$checksEntries[[sites]])) {
            RepChecks_site[z] <- sum(layoutR == i)
            z <- z + 1
        }
        RepChecks_list[[sites]] <- RepChecks_site
        if (any(as.vector(layoutR) == "Filler")) Fillers <- sum(layoutR == "Filler") 
        percentChecks <- round(sum(RepChecks_site) / (nrows * ncols),3) * 100
        percentChecks <- paste(percentChecks, "%", sep = "")
        if (Fillers == 0) {
            layoutR <- apply(layoutR, c(1,2), as.numeric)
        }
        field_book_sites[[sites]] <- fieldBook
        layout_random_sites[[sites]] <- layoutR
        plot_numbers_sites[[sites]] <- plot_num
        col_checks_sites[[sites]] <- as.matrix(Col_checks)
        percentChecks_vector[[sites]] <- percentChecks
        
    }
    
    field_book <- dplyr::bind_rows(field_book_sites)
    
    infoDesign <- list(
        rows = nrows,
        columns = ncols,
        treatments = linesexpt,
        checks = length(getData$checksEntries[[1]]),
        entry_checks = getData$checksEntries, 
        rep_checks = RepChecks_list,
        locations = l,
        planter = planter,
        percent_checks = percentChecks_vector,
        fillers = Fillers, 
        seed = seed, 
        id_design = 15
    )
    output <- list(
        infoDesign = infoDesign, 
        layoutRandom = layout_random_sites, 
        plotsNumber = plot_numbers_sites,
        data_entry = getData$data_entry, 
        fieldBook = field_book
    )
    
    class(output) <- "FielDHub"
    return(invisible(output))
}

#' Spatial Un-replicated Diagonal Arrangement Design
#' 
#' Randomly generates an spatial un-replicated diagonal arrangement design.
#'
#' @param nrows Number of rows in the field.
#' @param ncols Number of columns in the field.
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param checks Number of genotypes checks. 
#' @param kindExpt Type of diagonal design, with single options: Single Un-replicated Diagonal Checks
#' \code{'SUDC'} and Decision Blocks Un-replicated Design with Diagonal Checks \code{'DBUDC'} 
#' for multiple experiments. By default \code{kindExpt = 'SUDC'}.
#' @param planter Option for \code{serpentine} or \code{cartesian} plot arrangement. 
#' By default  \code{planter = 'serpentine'}.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param Option_NCD Option to pass an entry list for multiple locations.
#' @param blocks Number of blocks. By default \code{blocks = NULL}.
#' @param stacked  Option for \code{"By Row"} or \code{"By Column"} stacked of planting.
#' @param multiLocationData (optional) Option to pass an entry list for multiple locations. 
#' By default \code{multiLocationData = FALSE}.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME }.
#' 
#' @noRd
unrep_data_parameters <- function(
    lines,
    nrows,
    ncols,
    checks, 
    kindExpt = "SUDC", 
    planter = "serpentine",
    stacked = "By Row",
    blocks = NULL, 
    l = 1,
    Option_NCD = TRUE,
    multiLocationData = FALSE,
    data = NULL) {

    if (inherits(data, "Sparse")) {
        data <- data$list_locs
    } 
    # Set Blocks to 1
    Blocks <- 1
    checksEntries_list <- vector(mode = "list", length = l)
    data_entry_list = vector(mode = "list", length = l)
    dim_data_entry_list = vector(mode = "list", length = l)
    dim_data_1_list = vector(mode = "list", length = l)
    blocks_list = vector(mode = "list", length = l)
    locations <- 1:l
    # Iterate through the locations
    for (location in locations) {
        if (!is.null(data)) {
            if (multiLocationData) {
                if (is.data.frame(data)) {
                    gen_list <- data
                    gen_list <- as.data.frame(gen_list)
                    gen_list <- na.omit(gen_list[, 1:3])
                    if (ncol(gen_list) < 3) {
                        base::stop("Input data should have 4 columns: LOCATION, ENTRY, NAME")
                    }
                    colnames(gen_list) <- c("LOCATION", "ENTRY", "NAME")
                    if (length(gen_list$ENTRY) != length(unique(gen_list$ENTRY))) {
                      stop("Please ensure all ENTRIES in data are distinct.")
                    }
                    if (length(gen_list$NAME) != length(unique(gen_list$NAME))) {
                      stop("Please ensure all NAMES in data are distinct.")
                    }
                    # Create a space in memory for the locations data entry list
                    list_locs <- setNames(
                        object = vector(mode = "list", length = l), 
                        nm = unique(gen_list$LOCATION)
                    )
                    # Generate the lists of entries for each location
                    for (site in unique(gen_list$LOCATION)) {
                        df_loc <- gen_list %>% 
                            dplyr::filter(LOCATION == site) %>% 
                            dplyr::mutate(ENTRY = as.numeric(ENTRY)) %>% 
                            dplyr::select(ENTRY, NAME) 

                        list_locs[[site]] <- df_loc
                    }
                    data_entry <- list_locs[[location]]
                    data_entry_UP <- na.omit(data_entry[,1:2]) 
                    colnames(data_entry_UP) <- c("ENTRY", "NAME")
                    if (length(data_entry_UP$ENTRY) != length(unique(data_entry_UP$ENTRY))) {
                      stop("Please ensure all ENTRIES in data are distinct.")
                    }
                    if (length(data_entry_UP$NAME) != length(unique(data_entry_UP$NAME))) {
                      stop("Please ensure all NAMES in data are distinct.")
                    }
                } else if (is.list(data)) {
                    list_locs <- data
                    data_entry <- list_locs[[location]]
                    data_entry_UP <- na.omit(data_entry[ ,1:2]) 
                    colnames(data_entry_UP) <- c("ENTRY", "NAME")
                    if (length(data_entry_UP$ENTRY) != length(unique(data_entry_UP$ENTRY))) {
                      stop("Please ensure all ENTRIES in data are distinct.")
                    }
                    if (length(data_entry_UP$NAME) != length(unique(data_entry_UP$NAME))) {
                      stop("Please ensure all NAMES in data are distinct.")
                    }
                }
            } else {
                data_entry <- data
                data_entry_UP <- na.omit(data_entry[,1:2]) 
                colnames(data_entry_UP) <- c("ENTRY", "NAME")
                if (length(data_entry_UP$ENTRY) != length(unique(data_entry_UP$ENTRY))) {
                  stop("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(data_entry_UP$NAME) != length(unique(data_entry_UP$NAME))) {
                  stop("Please ensure all NAMES in data are distinct.")
                }
            }
            ##############################################################################################
            # Check if the data entry is a data frame
            if (!is.null(checks) && is.numeric(checks) && all(checks %% 1 == 0)) {
                if (!is.null(data_entry)) {
                    if (length(checks) == 1 && checks >= 1) {
                        checksEntries <- sort(as.numeric(data_entry[1:checks,1]))
                        checks <- checks
                    } else if (length(checks) > 1) {
                        checksEntries <- checks
                        checks <- length(checks)
                    } 
                } 
            } else base::stop("'diagonal_arrangement()' requires input checks to be an integer greater than 0.")
            if (any(diff(checksEntries) > 1) || any(diff(checksEntries) < 0)) {
                base::stop(paste("'diagonal_arrangement()' requires input checks to be a continuous range."))
            }
            ###############################################################################################
            if (kindExpt == "DBUDC") {
                data_entry_UP <- na.omit(data_entry[,1:2]) 
                data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
                colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
                if (length(data_entry_UP$ENTRY) != length(unique(data_entry_UP$ENTRY))) {
                  stop("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(data_entry_UP$NAME) != length(unique(data_entry_UP$NAME))) {
                  stop("Please ensure all NAMES in data are distinct.")
                }
                B <- data_entry_UP[(checks + 1):nrow(data_entry_UP),]
                Block_levels <- suppressWarnings(as.numeric(levels(as.factor(B$BLOCK))))
                Block_levels <- na.omit(Block_levels)
                Blocks <- length(Block_levels)
                if (Option_NCD == TRUE) {
                    data_entry1 <- data_entry_UP[(checks + 1):nrow(data_entry_UP), ]
                    Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
                    Block_levels <- na.omit(Block_levels)
                    data_dim_each_block <- numeric()
                    for (i in Block_levels){ 
                        data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
                    }
                    dim_data <- sum(data_dim_each_block)
                }
            }
        } else {
            # Check if the data entry is a data frame
            if (!is.null(checks) && is.numeric(checks) && all(checks %% 1 == 0)) {
                if (length(checks) == 1 && checks >= 1) {
                  checksEntries <- 1:checks
                  checks <- checks
                } else if (length(checks) > 1) {
                  checksEntries <- checks
                  checks <- length(checks)
                }
            } else base::stop("'diagonal_arrangement()' requires input checks to be an integer greater than 0.")
            if (any(diff(checksEntries) > 1) || any(diff(checksEntries) < 0)) {
              base::stop(paste("'diagonal_arrangement()' requires input checks to be a continuous range."))
            }
            if (kindExpt != "DBUDC") {
                NAME <- c(paste0(rep("Check-", checks), 1:checks),
                        paste0(rep("Gen-", lines), (checksEntries[checks] + 1):(checksEntries[1] + lines + checks - 1)))
                data_entry_UP <- data.frame(
                    ENTRY = checksEntries[1]:(checksEntries[1] + lines + checks - 1),	
                    NAME = NAME
                )
                colnames(data_entry_UP) <- c("ENTRY", "NAME")
                if (length(data_entry_UP$ENTRY) != length(unique(data_entry_UP$ENTRY))) {
                  stop("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(data_entry_UP$NAME) != length(unique(data_entry_UP$NAME))) {
                  stop("Please ensure all NAMES in data are distinct.")
                }
                if (nrow(data_entry_UP) != (lines + checks)) base::stop("nrows data != of lines + checks")
            } else if (kindExpt == "DBUDC") {
                if (is.null(blocks)) {
                    stop("'diagonal_arrangement()' requires blocks when kindExpt = 'DBUDC' and data is null.")
                } 
                if (sum(blocks) != lines) {
                    stop("In 'diagonal_arrangement()' number of lines and total lines in 'blocks' do not match.")
                }
                NAME <- c(paste0(rep("Check-", checks), 1:checks),
                        paste0(rep("Gen-", lines), (checksEntries[checks] + 1):(checksEntries[1] + lines + checks - 1)))
                data_entry_UP <- data.frame(
                    ENTRY = checksEntries[1]:(checksEntries[1] + lines + checks - 1), 
                    NAME = NAME
                )
                data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
                colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
                if (length(data_entry_UP$ENTRY) != length(unique(data_entry_UP$ENTRY))) {
                  stop("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(data_entry_UP$NAME) != length(unique(data_entry_UP$NAME))) {
                  stop("Please ensure all NAMES in data are distinct.")
                }
                Blocks <- length(blocks)
                if (Option_NCD == TRUE) {
                    data_entry1 <- data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ] # checksEntries intead of checks
                    Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
                    Block_levels <- na.omit(Block_levels)
                    data_dim_each_block <- numeric()
                    for (i in Block_levels) {
                        data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
                    }
                    dim_data <- sum(data_dim_each_block)
                }
            }
        }
        dim_data_entry <- nrow(data_entry_UP)
        dim_data_1 <- nrow(data_entry_UP[(checks + 1):nrow(data_entry_UP), ])
        checksEntries_list[[location]] <- checksEntries
        data_entry_list[[location]] <- data_entry_UP
        dim_data_entry_list[[location]] <- dim_data_entry
        dim_data_1_list[[location]] <- dim_data_1
        blocks_list[[location]] <- Blocks
    }
    return(
        list(
            data_entry = data_entry_list,
            checksEntries = checksEntries_list, 
            dim_data_entry = dim_data_entry_list,
            dim_data_1 = dim_data_1_list, 
            Blocks = blocks_list
        )
    )
}

#' @noRd 
#' 
#' 
field_dimensions <- function(lines_within_loc) {
    t1 <- floor(lines_within_loc + lines_within_loc * 0.10)
    t2 <- ceiling(lines_within_loc + lines_within_loc * 0.20)
    t <- t1:t2
    non_primes <- t[-numbers::isPrime(t)]
    choices_list <- list()
    i <- 1
    for (n in non_primes) {
        choices_list[[i]] <- factor_subsets(n, diagonal = TRUE)$labels
        i <- i + 1
    }
    return(choices_list)
}
