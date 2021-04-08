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
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' 
#' @return A list with information on the design parameters.
#' @return A matrix with the randomization layout.
#' @return A matrix with the layout plot number.
#' @return A data frame with fieldBook design. This includes the index (Row, Column).
#'
#'
#' @references
#' Clarke, G. P. Y., & Stefanova, K. T. (2011). Optimal design for early-generation plant
#' breeding trials with unreplicated or partially replicated test lines. Australian & New
#' Zealand Journal of Statistics, 53(4), 461–480.
#'
#' @examples
#' # Example 1: Generates a spatial unreplicated optimized arrangement design in one location
#' # with 362 genotypes + 38 check plots (5 checks) for a field with dimension 20 rows x 20 cols.
#' OptimAd1 <- optimized_arrangement(nrows = 20, ncols = 20, lines = 362, 
#'                                   amountChecks = 38, 
#'                                   checks = 1:5,
#'                                   planter = "cartesian", 
#'                                   plotNumber = 101,
#'                                   seed = 14,
#'                                   exptName = "20RW1",
#'                                   locationNames = "CASSELTON")
#' OptimAd1$infoDesign
#' OptimAd1$layoutRandom
#' OptimAd1$plotNumber
#' head(OptimAd1$fieldBook,12)
#'                   
#' # Example 2: Generates a spatial unreplicated optimized arrangement design in one location
#' # with 635 genotypes + 65 check plots (4 checks) for a field with dimension 20 rows x 35 cols.
#' # As example, we set up the data option with the entries list.
#' checks <- 4
#' list_checks <- paste("CH", 1:checks, sep = "")
#' treatments <- paste("G", 5:639, sep = "")
#' REPS <- c(17, 16, 16, 16, rep(1, 635))
#' treatment_list <- data.frame(list(ENTRY = 1:639, NAME = c(list_checks, treatments), REPS = REPS))
#' head(treatment_list, 12) 
#' tail(treatment_list, 12)
#' OptimAd2 <- optimized_arrangement(nrows = 20, ncols = 35, 
#'                                   planter = "serpentine", 
#'                                   plotNumber = 101,
#'                                   seed = 12,
#'                                   exptName = "20YWA2",
#'                                   locationNames = "MINOT",
#'                                   data = treatment_list)
#' OptimAd2$infoDesign
#' OptimAd2$layoutRandom
#' OptimAd2$plotNumber
#' head(OptimAd2$fieldBook,12)
#'                   
#' @export
optimized_arrangement <- function(nrows = NULL, ncols = NULL, lines = NULL,  amountChecks = NULL, checks = NULL,
                                  planter = "serpentine", l = 1, plotNumber = 101, seed = NULL, exptName = NULL,
                                  locationNames = NULL, data = NULL) {
  
  if (all(c("serpentine", "cartesian") != planter)) {
    base::stop('Input planter is unknown. Please, choose one: "serpentine" or "cartesian"')
  }
  
  if (is.null(plotNumber)) {
    plotNumber <- 1001
    warning("Since plotNumber was missing, it was set up to default value of 1001")
  }else if(!is.numeric(plotNumber)) {
    stop("Input plotNumber can be an integer or a numeric vector.")
  }
  
  if (!is.null(data)) {
    arg1 <- list(nrows, ncols, l);arg2 <- c(nrows, ncols, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      base::stop('"diagonal_arrangement()" requires arguments nrows, ncols, and l to be numeric and distint of NULL')
    }
  }else {
    arg1 <- list(nrows, ncols, lines, l);arg2 <- c(nrows, ncols, lines, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      base::stop('"diagonal_arrangement()" requires arguments nrows, ncols, and l to be numeric and distint of NULL')
    }
  } 
  if (is.null(data)) {
    if (!is.null(checks) && is.numeric(checks) && all(checks %% 1 == 0) && 
        !is.null(amountChecks) && is.numeric(amountChecks) && all(amountChecks %% 1 == 0) &&
        all(amountChecks > 0) && all(checks > 0)) {
      if (length(checks) == 1) {
        if (length(amountChecks) == checks) {
          RepChecks <- amountChecks
        }else if (length(amountChecks) == 1 && amountChecks > checks) {
          res <- amountChecks %% checks
          divs <- (amountChecks - res) / checks
          if (res == 0) {
            u <- amountChecks / checks
            RepChecks <- rep(u, checks)
          }else {
            RepChecks <- rep(divs, checks - 1)
            RepChecks <- sample(c(RepChecks, amountChecks - sum(RepChecks)))
          }
        }
      }else if (length(checks) > 1) {
        if(any(any(checks != sort(checks)) || any(diff(checks) > 1))) {
          base::stop("Input checks must be in consecutive numbers and sorted.")
        } 
        if(length(unique(checks)) != length(checks)) base::stop("Input checks must be different from each other.")
        if (length(amountChecks) == length(checks)) {
          RepChecks <- amountChecks
        }else if (length(amountChecks) == 1 && amountChecks > length(checks)) {
          res <- amountChecks %% length(checks)
          divs <- (amountChecks - res) / length(checks)
          if (res == 0) {
            u <- amountChecks / length(checks)
            RepChecks <- rep(u, length(checks))
          }else {
            RepChecks <- rep(divs, length(checks) - 1)
            RepChecks <- sample(c(RepChecks, amountChecks - sum(RepChecks)))
          }
        }
      }
    }else base::stop('"diagonal_arrangement()" requires inputs checks and amountChecks to be possitive integers and distinct of NULL.')
  }else {
    if(!is.data.frame(data)) base::stop("Data must be a data frame.")
    gen.list <- data
    gen.list <- as.data.frame(gen.list)
    gen.list <- na.omit(gen.list[,1:3])
    colnames(gen.list) <- c("ENTRY", "NAME", "REPS")
    if (any(gen.list$ENTRY < 1) || any(gen.list$REPS < 1)) base::stop("Negatives number are not allowed in the data.")
    gen.list.O <- gen.list[order(gen.list$REPS, decreasing = TRUE), ]
    my_GENS <- subset(gen.list.O, gen.list.O$REPS == 1)
    my_REPS <- subset(gen.list.O, gen.list.O$REPS > 1)
    my_REPS <- my_REPS[order(my_REPS$REPS, decreasing = TRUE),]
    RepChecks <- as.vector(my_REPS[,3])
    checksEntries <- as.vector(my_REPS[,1])
    checks <- length(checksEntries)
    lines <- sum(my_GENS$REPS)
  }
  
  if(is.null(data)) {
    if (length(checks) == 1 && checks > 1) {
      checksEntries <- 1:checks
      checks <- checks
    }else if (length(checks) > 1) {
      checksEntries <- checks
      checks <- length(checks)
    } else if (length(checks) == 1 && checks == 1) {
      checksEntries <- checks
      checks <- length(checks)
    }
  }
  
  all_gens <- sum(RepChecks) + lines
  if ((all_gens == nrows*ncols)) {
    Fillers <- 0
  }else if (all_gens > nrows*ncols) {
    shiny::validate("Data input exceeds the field dimentions specified.")
  }else if (all_gens < nrows*ncols) {
    shiny::validate("Data input is not large enough to completely use the field dimensions specified.")
  }
  
  prep <- pREP(nrows = nrows, ncols = ncols, RepChecks = RepChecks, checks = checksEntries, Fillers = Fillers,
               seed = seed, optim = TRUE, niter = 1000, data = data)
  
  dataInput <- prep$gen.list
  BINAY_CHECKS <- prep$binary.field
  
  if (!is.null(exptName)) {
    Name_expt <- exptName 
  }else Name_expt <- "20ExptOptim"
  
  split_name_spat <- function(){
    split_names <- base::matrix(data = Name_expt, nrow = nrows, ncol = ncols, byrow = TRUE)
    return(list(my_names = split_names))
  }
  
  plot_number_spat <- function(){
    datos_name <- split_name_spat()$my_names
    plot_n_start <- plotNumber
    my_split_plot_nub <- plot_number(movement_planter = planter, n_blocks = 1, n_rows = nrows, n_cols = ncols,
                                     plot_n_start = plot_n_start, datos = datos_name, expe_name = Name_expt, ByRow = FALSE,
                                     my_row_sets = NULL, ByCol = TRUE, my_col_sets = ncols)
  }
  
  if (is.null(locationNames)) locationNames <- 1:l
  plot_num <- plot_number_spat()$w_map_letters1
  plot_num <- apply(plot_num, c(1,2), as.numeric)
  export_spat <- function(){
    loc <- locationNames
    random_entries_map <- as.matrix(prep$field.map)
    plot_num <- as.matrix(plot_num)
    Col_checks <- as.matrix(BINAY_CHECKS)
    my_names <- as.matrix(split_name_spat()$my_names)
    year <- format(Sys.Date(), "%Y")
    my_data_VLOOKUP <- prep$gen.list
    results_to_export <- list(random_entries_map, plot_num, Col_checks, my_names)
    final_expt_export <- export_design(G = results_to_export, movement_planter =  planter,
                                       location = loc, Year = year, data_file = my_data_VLOOKUP,
                                       reps = FALSE)
    
    return(list(final_expt = final_expt_export))
    
  }
  
  fieldBook <- as.data.frame(export_spat()$final_expt)
  fieldBook <- fieldBook[,-11]
  ID <- 1:nrow(fieldBook)
  fieldBook <- fieldBook[, c(6,7,9,4,2,3,5,1,10)]
  fieldBook <- cbind(ID, fieldBook)
  colnames(fieldBook)[10] <- "TREATMENT"
  # fieldBook <- fieldBook[, c(2,3,1,4:10)]
  # rownames(fieldBook) <- 1:nrow(fieldBook)
  layoutR = prep$field.map
  rownames(layoutR) <- paste("Row", nrow(layoutR):1, sep = "")
  colnames(layoutR) <- paste("Col", 1:ncol(layoutR), sep = "")
  rownames(plot_num) <- paste("Row", nrow(plot_num):1, sep = "")
  colnames(plot_num) <- paste("Col", 1:ncol(plot_num), sep = "")
  
  infoDesign <- list(Lines = lines, checks = checksEntries, RepChecks = RepChecks)
  
  return(list(infoDesign = infoDesign, dataInput = dataInput, layoutRandom = layoutR, plotNumber = plot_num, fieldBook = fieldBook))
  
}
