#' Generates an Alpha Design
#' 
#' 
#' @description  Randomly generates an alpha design like \code{alpha(0,1)} across multiple locations.
#' 
#'
#' @param t Number of  treatments.
#' @param r Number of full blocks (or resolvable replicates) (also number of replicates per treatment).
#' @param k Size of incomplete blocks (number of units per incomplete block). 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param locationNames (optional) String with names for each of the \code{l} locations.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param data (optional) Data frame with label list of treatments.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' 
#' @importFrom stats runif na.omit
#' 
#' 
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the alpha design field book.
#' }
#'
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
#' unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#'
#'
#' @examples
#' # Example 1: Generates an alpha design with 4 full blocks and 15 treatments.
#' # Size of IBlocks k = 3.
#' alphalattice1 <- alpha_lattice(t = 15, 
#'                                k = 3, 
#'                                r = 4, 
#'                                l = 1, 
#'                                plotNumber = 101, 
#'                                locationNames = "GreenHouse", 
#'                                seed = 1247)
#' alphalattice1$infoDesign
#' head(alphalattice1$fieldBook, 10)
#' 
#' # Example 2: Generates an alpha design with 3 full blocks and 25 treatment.
#' # Size of IBlocks k = 5. 
#' # In this case, we show how to use the option data.
#' treatments <- paste("G-", 1:25, sep = "")
#' ENTRY <- 1:25
#' treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
#' head(treatment_list) 
#' alphalattice2 <- alpha_lattice(t = 25,
#'                                k = 5,
#'                                r = 3, 
#'                                l = 1, 
#'                                plotNumber = 1001, 
#'                                locationNames = "A", 
#'                                seed = 1945,
#'                                data = treatment_list)
#' alphalattice2$infoDesign
#' head(alphalattice2$fieldBook, 10)
#' 
#' @export
alpha_lattice <- function(t = NULL, 
                          k = NULL, 
                          r = NULL, 
                          l = 1, 
                          plotNumber = 101, 
                          locationNames = NULL,
                          seed = NULL, 
                          data = NULL) {
  
  if (is.null(seed)) {seed <- runif(1, min=0, max=10000)}
  set.seed(seed)
  lookup <- FALSE
  if(is.null(data)) {
    if (is.null(t) || is.null(k) || is.null(r) || is.null(l)) {
      shiny::validate('Basic design parameters missing (t, k, r or l).')
    }
    arg1 <- list(k, r, l);arg2 <- c(k, r, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      shiny::validate('incomplete_blocks() requires k, r and l to be possitive integers.')
    }
    if (is.numeric(t)) {
      if (length(t) == 1) {
        if (t == 1 || t < 1) {
          shiny::validate('incomplete_blocks() requires more than one treatment.')
        } 
        nt <- t
      }else if ((length(t) > 1)) {
        nt <- length(t)
        TRT <- t
      }
    }else if (is.character(t) || is.factor(t)) {
      if (length(t) == 1) {
        shiny::validate('incomplete_blocks() requires more than one treatment.')
      } 
      nt <- length(t)
    }else if ((length(t) > 1)) {
      nt <- length(t)
    }
    df <- data.frame(list(ENTRY = 1:nt, TREATMENT = paste0("G-", 1:nt)))
    data_alpha <- df
  } else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      shiny::validate('Basic design parameters missing (t, k, r or l).')
    }
    if(!is.data.frame(data)) shiny::validate("Data must be a data frame.")
    if (ncol(data) < 2) base::stop("Data input needs at least two columns with: ENTRY and NAME.")
    data_up <- as.data.frame(data[,c(1,2)])
    data_up <- na.omit(data_up)
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    data_up$TREATMENT <- as.character(data_up$TREATMENT)
    new_t <- length(data_up$TREATMENT)
    if (t != new_t) base::stop("Number of treatments do not match with data input.")
    TRT <- data_up$TREATMENT
    nt <- length(TRT)
    if (nt != t) shiny::validate('Number of treatment do not match with data input')
    data_alpha <- data_up
  }
  if (k >= nt) shiny::validate('incomplete_blocks() requires that k < t.')
  if (!is.null(locationNames)) locationNames <- toupper(locationNames)
  if(is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
  if (numbers::isPrime(t)) shiny::validate('Combinations for this amount of treatments do not exist.')
  s <- nt / k
  dt <- numbers::divisors(t)
  dt <- dt[2:(length(dt) - 1)]
  if (s %% 1 != 0) shiny::validate('Combinations for this amount of treatments do not exist.')
  
  nunits <- k
  matdf <- incomplete_blocks(t = nt, k = nunits, r = r, l = l, plotNumber = plotNumber,
                             seed = seed, locationNames = locationNames,
                             data = data_alpha)
  blocksModel <- matdf$blocksModel
  lambda <- r*(k - 1)/(nt - 1)
  matdf <- matdf$fieldBook
  OutAlpha <- as.data.frame(matdf)
  OutAlpha$LOCATION <- factor(OutAlpha$LOCATION, levels = locationNames)
  rownames(OutAlpha) <- 1:nrow(OutAlpha)
  infoDesign <- list(Reps = r, iBlocks = s, NumberTreatments = nt, NumberLocations = l, 
                     Locations = locationNames, seed = seed, lambda = lambda,
                     id_design = 12)
  output <- list(infoDesign = infoDesign, fieldBook = OutAlpha, blocksModel = blocksModel)
  class(output) <- "FielDHub"
  return(invisible(output))
}