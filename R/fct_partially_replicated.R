#' Generates a Spatial Partially Replicated Arrangement Design
#'
#'
#' Randomly generates a spatial partially replicated design, where the distance 
#' between checks is maximized in such a way that each row and column have control plots. 
#' Note that design generation needs the dimension of the field (number of rows and columns).
#'
#' @param nrows Number of rows field.
#' @param ncols Number of columns field.
#' @param repGens Numeric vector with the amount genotypes to replicate.
#' @param repUnits Numeric vector with the number of reps of each genotype.
#' @param planter Option for \code{serpentine} or \code{cartesian} movement. By default  \code{planter = 'serpentine'}. 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Name for each location.
#' @param data (optional) Dataframe with 3 columns: \code{ENTRY | NAME | REPS}.
#' 
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
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
#' Cullis, S., B. R., & Coombes, N. E. (2006). On the design of early generation variety trials
#' with correlated data. Journal of Agricultural, Biological, and Environmental Statistics, 11,
#' 381–393. https://doi.org/10.1198/108571106X154443
#'
#'
#' @examples
#' # Example 1: Generates a spatial optimized partially replicated arrangement design in one 
#' # location with 312 genotypes for a field with dimensions 15 rows x 28 cols. 
#' # Note that there are 250 genotypes unreplicated (only one time), 50 genotypes replicated 
#' # two times, and 10 genotypes replicated three times, and two checks 20 times each one.
#' SpatpREP1 <- partially_replicated(
#'   nrows = 15, 
#'   ncols = 28,  
#'   repGens = c(250,50,10,1,1),
#'   repUnits = c(1,2,3,20,20),
#'   planter = "cartesian", 
#'   plotNumber = 101,
#'   seed = 77
#'  )
#' SpatpREP1$infoDesign
#' SpatpREP1$layoutRandom
#' SpatpREP1$plotNumber
#' head(SpatpREP1$fieldBook,12)
#' 
#' # Example 2: Generates a spatial optimized partially replicated arrangement design with 492 
#' # genotypes in a field with dimensions 30 rows x 20 cols. Note that there 384 genotypes 
#' # unreplicated (only one time), 108 genotypes replicated two times. 
#' # In this case we don't have check plots.
#' # As example, we set up the data option with the entries list.
#' NAME <- paste("G", 1:492, sep = "")
#' repGens = c(108, 384);repUnits = c(2,1)
#' REPS <- rep(repUnits, repGens)
#' treatment_list <- data.frame(list(ENTRY = 1:492, NAME = NAME, REPS = REPS))
#' head(treatment_list, 12) 
#' tail(treatment_list, 12)
#' SpatpREP2 <- partially_replicated(nrows = 30, 
#'                                   ncols = 20, 
#'                                   planter = "serpentine", 
#'                                   plotNumber = 101,
#'                                   seed = 41,
#'                                   data = treatment_list)
#' SpatpREP2$infoDesign
#' SpatpREP2$layoutRandom
#' SpatpREP2$plotNumber
#' head(SpatpREP2$fieldBook,10)
#' 
#' @export
partially_replicated <- function(nrows = NULL, ncols = NULL, repGens = NULL, repUnits = NULL, 
                                 planter = "serpentine", l = 1, plotNumber = 101, seed = NULL, 
                                 exptName = NULL, locationNames = NULL, data = NULL) {
  
  if (all(c("serpentine", "cartesian") != planter)) {
    base::stop('Input "planter" is unknown. Please, choose one: "serpentine" or "cartesian"')
  }
  
  if (is.null(nrows) || is.null(ncols) || !is.numeric(nrows) || !is.numeric(ncols)) {
    base::stop('Basic design parameters missing (nrows, ncols) or is not numeric.')
  }
  
  if (is.null(data)) {
    if (is.null(repGens) || is.null(repUnits)) base::stop("Input repGens and repUnits are missing.")
    if (length(repGens) != length(repUnits)) base::stop("Input repGens and repUnits may have the same length.")
  }
  
  if(!is.numeric(plotNumber) && !is.integer(plotNumber)) {
    stop("plotNumber should be an integer or a numeric vector.")
  }
  
  if (any(plotNumber %% 1 != 0)) {
    stop("plotNumber should be integers.")
  }
  
  if (!is.null(l)) {
    if (is.null(plotNumber) || length(plotNumber) != l) {
      if (l > 1) {
        plotNumber <- seq(1001, 1000*(l+1), 1000)
        message(cat("Warning message:", "\n", 
                "Since plotNumber was missing, it was set up to default value of: ", plotNumber, 
                "\n", "\n"))
      } else {
        plotNumber <- 1001
        message(cat("Warning message:", "\n", 
        "Since plotNumber was missing, it was set up to default value of: ", plotNumber, 
        "\n", "\n"))
      } 
    }
  }else stop("Number of locations/sites is missing")
  
  if (!is.null(data)) {
    arg1 <- list(nrows, ncols, l);arg2 <- c(nrows, ncols, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      base::stop('"partially_replicated()" requires input nrows, ncols, and l to be numeric and distint of NULL.')
    }
  }else {
    arg1 <- list(nrows, ncols, l);arg2 <- c(nrows, ncols, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      base::stop('"partially_replicated()" requires input nrows, ncols, and l to be numeric and distint of NULL.')
    }
  } 
  
  if (!is.null(data)) {
    if(!is.data.frame(data)) base::stop("Data must be a data frame.")
      gen.list <- data
      gen.list <- as.data.frame(gen.list)
      gen.list <- na.omit(gen.list[,1:3])
      colnames(gen.list) <- c("ENTRY", "NAME", "REPS")
      if (any(gen.list$ENTRY < 1) || any(gen.list$REPS < 1)) base::stop("Negatives number are not allowed in the data.")
      gen.list.O <- gen.list[order(gen.list$REPS, decreasing = TRUE), ]
      my_GENS <- subset(gen.list.O, REPS == 1)
      my_REPS <- subset(gen.list.O, REPS > 1)
      my_REPS <- my_REPS[order(my_REPS$REPS, decreasing = TRUE),]
      RepChecks <- as.vector(my_REPS[,3])
      checksEntries <- as.vector(my_REPS[,1])
      checks <- length(checksEntries)
      lines <- sum(my_GENS$REPS)
      t_plots <- sum(as.numeric(gen.list$REPS))
      print(t_plots)
      if (numbers::isPrime(t_plots)) {
        stop("No options when the total number of plots is a prime number.", call. = FALSE)
      }
      if (t_plots != (nrows * ncols)) {
        choices <- factor_subsets(t_plots)$labels
        if (!is.null(choices)) {
          message(cat("\n", "Error in partially_replicated(): ", "\n", "\n",
            "Field dimensions do not fit with the data entered!", "\n",
            "Try one of the following options: ", "\n"))
          return(for (i in 1:length(choices)) {print(choices[[i]])})
        } else {
          stop("Field dimensions do not fit with the data entered. Try another amount of treatments!", call. = FALSE)
        }
      }
  } else if (is.null(data)) {
    if (length(repGens) != length(repUnits)) stop("Input repGens and repUnits need to be of the same length.")
    t_plots <- sum(repGens * repUnits)
    if (numbers::isPrime(t_plots)) {
      stop("No options when the total number of plots is a prime number.", call. = FALSE)
    }
    if (t_plots != (nrows * ncols)) {
      choices <- factor_subsets(t_plots)$labels
      if (!is.null(choices)) {
        message(cat("\n", "Error in partially_replicated(): ", "\n", "\n",
          "Field dimensions do not fit with the data entered!", "\n",
          "Try one of the following options: ", "\n"))
        return(for (i in 1:length(choices)) {print(choices[[i]])})
      } else {
        stop("Field dimensions do not fit with the data entered. Try another amount of treatments!", call. = FALSE)
      }
    }
    ENTRY <- 1:sum(repGens)
    NAME <- paste(rep("G", sum(repGens)), 1:sum(repGens), sep = "")
    REPS <- as.numeric(sort(rep(repUnits, times = repGens), decreasing = TRUE))
    data <- data.frame(list(ENTRY = ENTRY,
                            NAME = NAME,
                            REPS = REPS))
    colnames(data) <- c("ENTRY", "NAME", "REPS")
  }
  if (is.null(seed)) {seed <- runif(1, min = -10000, max = 10000)}
  set.seed(seed)
  field_book_sites <- vector(mode = "list", length = l)
  layout_random_sites <- vector(mode = "list", length = l)
  plot_numbers_sites <- vector(mode = "list", length = l)
  col_checks_sites <- vector(mode = "list", length = l)
  for (sites in 1:l) { 
    prep <- pREP(nrows = nrows, ncols = ncols, RepChecks = NULL, checks = NULL, Fillers = 0,
                 seed = seed, optim = TRUE, niter = 1000, data = data)
    dataInput <- prep$gen.list
    BINAY_CHECKS <- prep$binary.field
    random_entries_map <- as.matrix(prep$field.map)
    genEntries <- prep$gen.entries
    
    if (!is.null(exptName)) {
      Name_expt <- exptName[1]
    }else Name_expt <- "Expt1"
    
    split_name_spat <- function(){
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
      results_to_export <- list(random_entries_map, 
                                plot_number_L, 
                                Col_checks, 
                                my_names)
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
  
  RepChecks <- prep$reps.checks
  EntryChecks <- prep$entryChecks
  Checks <- length(EntryChecks)
  
  if (sum(genEntries[[2]]) == 0) {
    rep_treatments <- 0
  } else rep_treatments <- length(genEntries[[2]])
  
  infoDesign <- list(
    rows = nrows,
    columns = ncols,
    treatments_with_reps = Checks,
    treatments_with_no_reps = rep_treatments,
    locations = l,
    planter = planter,
    seed = seed,
    id_design = 13)
  output <- list(infoDesign = infoDesign, 
                 layoutRandom = layout_random_sites, 
                 plotNumber = plot_numbers_sites,
                 binaryField = col_checks_sites,
                 dataEntry = dataInput,
                 genEntries = genEntries,
                 fieldBook = field_book)

  class(output) <- "FielDHub"
  return(invisible(output))
}
