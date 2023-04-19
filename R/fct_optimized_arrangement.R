#' Generates an Spatial Un-replicated Optimized Arrangement Design
#' 
#'
#' Randomly generates a spatial un-replicated optimized arrangement design, where the distance
#' between checks is maximized in such a way that each row and column have control plots.
#' Note that design generation needs the dimension of the field (number of rows and columns).
#'
#' @param nrows Number of rows in the field.
#' @param ncols Number of columns in the field.
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param checks Number of genotypes as checks.
#' @param amountChecks Integer with the amount total of checks or a numeric vector with the replicates of each check label.
#' @param planter Option for \code{serpentine} or \code{cartesian} arrangement. By default  \code{planter = 'serpentine'}.
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Name for each location.
#' @param data (optional) Data frame with 3 columns: \code{ENTRY | NAME | REPS}.
#' @param optim By default \code{optim = TRUE}.
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
#'   \item \code{plotNumber} is a matrix with the layout plot number.
#'   \item \code{dataEntry} is a data frame with the data input.
#'   \item \code{genEntries} is a list with the entries for replicated and no replicated part.
#'   \item \code{fieldBook} is a data frame with field book design. This includes the index (Row, Column).
#' }
#'
#' @references
#' Clarke, G. P. Y., & Stefanova, K. T. (2011). Optimal design for early-generation plant
#' breeding trials with unreplicated or partially replicated test lines. Australian & New
#' Zealand Journal of Statistics, 53(4), 461–480.
#'
#' @examples
#' # Example 1: Generates a spatial unreplicated optimized arrangement design in one location
#' # with 120 genotypes + 20 check plots (4 checks) for a field with dimension 14 rows x 10 cols.
#' \dontrun{
#' optim_unrep1 <- optimized_arrangement(
#'   nrows = 14, 
#'   ncols = 10, 
#'   lines = 120, 
#'   amountChecks = 20, 
#'   checks = 1:4,
#'   planter = "cartesian", 
#'   plotNumber = 101,
#'   exptName = "20RW1",
#'   locationNames = "CASSELTON",
#'   seed = 14124
#' )
#' optim_unrep1$infoDesign
#' optim_unrep1$layoutRandom
#' optim_unrep1$plotNumber
#' head(optim_unrep1$fieldBook, 12)
#' }
#'                   
#' # Example 2: Generates a spatial unreplicated optimized arrangement design in one location
#' # with 200 genotypes + 20 check plots (4 checks) for a field with dimension 10 rows x 22 cols.
#' # As example, we set up the data option with the entries list.
#' \dontrun{
#' checks <- 4
#' list_checks <- paste("CH", 1:checks, sep = "")
#' treatments <- paste("G", 5:204, sep = "")
#' REPS <- c(5, 5, 5, 5, rep(1, 200))
#' treatment_list <- data.frame(list(ENTRY = 1:204, NAME = c(list_checks, treatments), REPS = REPS))
#' head(treatment_list, 12) 
#' tail(treatment_list, 12)
#' optim_unrep2 <- optimized_arrangement(
#'   nrows = 10, 
#'   ncols = 22, 
#'   planter = "serpentine", 
#'   plotNumber = 101,
#'   seed = 120,
#'   exptName = "20YWA2",
#'   locationNames = "MINOT",
#'   data = treatment_list
#' )
#' optim_unrep2$infoDesign
#' optim_unrep2$layoutRandom
#' optim_unrep2$plotNumber
#' head(optim_unrep2$fieldBook,12)
#' }
#'                   
#' @export
optimized_arrangement <- function(
    nrows = NULL, 
    ncols = NULL, 
    lines = NULL,  
    amountChecks = NULL, 
    checks = NULL,
    planter = "serpentine", 
    l = 1, 
    plotNumber = 101, 
    seed = NULL, 
    exptName = NULL,
    locationNames = NULL, 
    optim = TRUE, 
    data = NULL) {
    
    if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
    if (all(c("serpentine", "cartesian") != planter)) {
        base::stop('Input planter is unknown. Please, choose one: "serpentine" or "cartesian"')
    }
    if (!is.numeric(plotNumber) && !is.integer(plotNumber)) {
        stop("plotNumber should be an integer or a numeric vector.")
    }
    if (any(plotNumber %% 1 != 0)) {
        stop("plotNumber should be integers.")
    }
    if (!is.null(l)) {
        if (is.null(plotNumber) || length(plotNumber) != l) {
            if (l > 1){
                plotNumber <- seq(1001, 1000*(l+1), 1000)
            } else plotNumber <- 1001
            message(cat("Warning message:", "\n", 
            "Since plotNumber was missing, it was set up to default value of: ", plotNumber, "\n",
            "\n"
            ))
        }
    } else stop("Number of locations/sites is missing")
    
    if (!is.null(data)) {
        arg1 <- list(nrows, ncols, l);arg2 <- c(nrows, ncols, l)
        if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
            base::stop('"optimized_arrangement()" requires arguments nrows, ncols, and l to be numeric and distint of NULL')
        }
    } else {
        arg1 <- list(nrows, ncols, lines, l);arg2 <- c(nrows, ncols, lines, l)
        if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
            base::stop('"optimized_arrangement()" requires arguments nrows, ncols, and l to be numeric and distint of NULL')
        }
    } 
    
    if(is.null(data)) {
        if (length(checks) == 1 && checks > 1) {
            checksEntries <- 1:checks
            checks <- checks
        } else if (length(checks) > 1) {
            checksEntries <- sort(checks)
            checks <- length(checks)
        } else if (length(checks) == 1 && checks == 1) {
            checksEntries <- checks
            checks <- length(checks)
        }
    }
    if (is.null(data)) {
        if (!is.null(checks) && is.numeric(checks) && all(checks %% 1 == 0) && 
            !is.null(amountChecks) && is.numeric(amountChecks) && all(amountChecks %% 1 == 0) &&
            all(amountChecks > 0) && all(checks > 0)) {
            if (length(checks) == 1) {
                if (length(amountChecks) == checks) {
                    RepChecks <- amountChecks
                } else if (length(amountChecks) == 1 && amountChecks > checks) {
                    res <- amountChecks %% checks
                    divs <- (amountChecks - res) / checks
                    if (res == 0) {
                        u <- amountChecks / checks
                        RepChecks <- rep(u, checks)
                    } else {
                        RepChecks <- rep(divs, checks - 1)
                        RepChecks <- sample(c(RepChecks, amountChecks - sum(RepChecks)))
                    }
                }
            } else if (length(checks) > 1) {
                if (any(any(checks != sort(checks)) || any(diff(checks) > 1))) {
                    base::stop("Input checks must be in consecutive numbers and sorted.")
                } 
                if(length(unique(checks)) != length(checks)) base::stop("Input checks must be different from each other.")
                if (length(amountChecks) == length(checks)) {
                    RepChecks <- amountChecks
                } else if (length(amountChecks) == 1 && amountChecks > length(checks)) {
                    res <- amountChecks %% length(checks)
                    divs <- (amountChecks - res) / length(checks)
                    if (res == 0) {
                        u <- amountChecks / length(checks)
                        RepChecks <- rep(u, length(checks))
                    } else {
                        RepChecks <- rep(divs, length(checks) - 1)
                        RepChecks <- sample(c(RepChecks, amountChecks - sum(RepChecks)))
                    }
                }
            }
        } else base::stop('"optimized_arrangement()" requires inputs checks and amountChecks to be possitive integers and distinct of NULL.')
        t_plots <- as.numeric(sum(RepChecks) + lines)
        if (numbers::isPrime(t_plots)) {
            stop("No options when the total number of plots is a prime number.", call. = FALSE)
        }
        if (t_plots != (nrows * ncols)) {
            choices <- factor_subsets(t_plots)$labels
            if (!is.null(choices)) {
                message(cat("\n", "Error in optimized_arrangement(): ", "\n", "\n",
                "Field dimensions do not fit with the data entered!", "\n",
                "Try one of the following options: ", "\n"))
                return(for (i in 1:length(choices)) {print(choices[[i]])})
            } else {
                stop("field dimensions do not fit with the data entered", call. = FALSE)
            }
        }
        NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                paste(rep("G", lines), 
                (checksEntries[checks] + 1):(checksEntries[1] + lines + checks - 1), sep = ""))
        reps.checks <- RepChecks
        REPS <- c(reps.checks, rep(1, lines))
        gen_list <- data.frame(
            list(
                ENTRY = checksEntries[1]:(checksEntries[1] + lines + checks - 1),	
                NAME = NAME,	
                REPS = REPS
            )
        )
        colnames(gen_list) <- c("ENTRY", "NAME", "REPS")
    } else {
        if (!is.data.frame(data)) base::stop("Data must be a data frame.")
        gen_list <- data
        gen_list <- gen_list[, 1:3]
        gen_list <- na.omit(gen_list)
        colnames(gen_list) <- c("ENTRY", "NAME", "REPS")
        if (length(gen_list$ENTRY) != length(unique(gen_list$ENTRY))) {
            stop("Please ensure all ENTRIES in data are distinct.")
        }
        if (length(gen_list$NAME) != length(unique(gen_list$NAME))) {
            stop("Please ensure all NAMES in data are distinct.")
        }
        if (any(gen_list$ENTRY < 1) || any(gen_list$REPS < 1)) {
            base::stop("Negatives number are not allowed in the data.")
        } 
        gen_list_ordered <- gen_list[order(gen_list$REPS, decreasing = TRUE), ]
        my_GENS <- subset(gen_list_ordered, gen_list_ordered$REPS == 1)
        my_REPS <- subset(gen_list_ordered, gen_list_ordered$REPS > 1)
        my_REPS <- my_REPS[order(my_REPS$REPS, decreasing = TRUE),]
        RepChecks <- as.vector(my_REPS[,3])
        checksEntries <- as.vector(my_REPS[,1])
        checks <- length(checksEntries)
        lines <- sum(my_GENS$REPS)
        t_plots <- sum(as.numeric(gen_list$REPS))
        if (numbers::isPrime(t_plots)) {
            stop("No options when the total number of plots is a prime number.", call. = FALSE)
        }
        if (t_plots != (nrows * ncols)) {
            choices <- factor_subsets(t_plots)$labels
            if (!is.null(choices)) {
                message(cat("\n", "Error in optimized_arrangement(): ", "\n", "\n",
                "Field dimensions do not fit with the data entered!", "\n",
                "Try one of the following options: ", "\n"))
                return(for (i in 1:length(choices)) {print(choices[[i]])})
            } else {
                stop("Field dimensions do not fit with the data entered. Try another amount of treatments!", call. = FALSE)
            }
        }
    }
    all_gens <- sum(RepChecks) + lines
    if ((all_gens == nrows*ncols)) {
        Fillers <- 0
    }
    field_book_sites <- vector(mode = "list", length = l)
    layout_random_sites <- vector(mode = "list", length = l)
    plot_numbers_sites <- vector(mode = "list", length = l)
    col_checks_sites <- vector(mode = "list", length = l)
    min_distance_sites <- vector(mode = "numeric", length = l)
    set.seed(seed)
    for (sites in 1:l) {
        prep <- pREP(
            nrows = nrows, 
            ncols = ncols, 
            Fillers = 0,
            seed = NULL, 
            optim = TRUE,
            niter = 1000, 
            data = gen_list
        )
        min_distance_sites[sites] <- prep$min_distance
        dataInput <- prep$gen.list
        BINAY_CHECKS <- prep$binary.field
        random_entries_map <- as.matrix(prep$field.map)
        genEntries <- prep$gen.entries
        if (!is.null(exptName)) {
            Name_expt <- exptName 
        } else Name_expt <- "Expt1"
        split_name_spat <- function() {
            split_names <- base::matrix(data = Name_expt, nrow = nrows, ncol = ncols, byrow = TRUE)
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
        if (is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
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
            results_to_export <- list(random_entries_map, plot_number_L, Col_checks, my_names)
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
    }
    field_book <- dplyr::bind_rows(field_book_sites)
    infoDesign <- list(
        rows = nrows,
        columns = ncols,
        min_distance = min_distance_sites,
        treatments = lines, 
        checks = length(RepChecks),
        entry_checks = checksEntries,
        rep_checks = RepChecks, 
        locations = l, 
        planter = planter,
        seed = seed,
        id_design = 16)
    output <- list(
        infoDesign = infoDesign, 
        layoutRandom = layout_random_sites, 
        plotNumber = plot_numbers_sites,
        binaryField = col_checks_sites,
        dataEntry = dataInput,
        genEntries = genEntries,
        fieldBook = field_book
    )
    class(output) <- "FielDHub"
    return(invisible(output))
}
