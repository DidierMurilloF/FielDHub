#' Generates an Alpha Design
#' 
#' 
#' @description  Randomly generates an Alpha design like \code{alpha(0,1)} across multiple locations.
#' 
#' @details Alpha lattice designs are a type of replicated designs that divide the replicate into incomplete blocks 
#' that contain a fraction of the total number of entries. Treatments are distributed among the blocks so 
#' that all pairs occur in the same incomplete-block in nearly equal frequency. The design permits removal 
#' of incomplete-block effects from the plot residuals and maximizes the use of comparisons between treatments 
#' in the same incomplete-block. The \code{alpha_lattice} function can only be used to a number of treatments that 
#' are multiply of the size 
#'
#' @param t Number of  treatments.
#' @param r Number of full blocks (or resolvable replicates) (also number of replicates per treatment).
#' @param k Size of incomplete blocks (number of unites per incomplete block). 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param locationNames (optional) String with names for each of the \code{l} locations.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param data (optional) Data frame with label list of treatments.
#' 
#' 
#' @importFrom stats runif na.omit
#' 
#' 
#' @return A list with information on the design parameters. 
#' @return Data frame with the alpha design field book.
#'
#'
#' @references
#' R. N. Edmondson. 2021. blocksdesign: Nested and Crossed Block Designs for Factorial and
#' Unstructured Treatment Sets.
#'
#' @examples
#' # Example 1: Generates an alpha design with 7 full blocks and 15 treatments.
#' # Size of iBlocks k = 3.
#' alphalattice1 <- alpha_lattice(t = 15, k = 3, r = 7, 
#'                                l = 1, 
#'                                plotNumber = 101, 
#'                                locationNames = "GreenHouse", 
#'                                seed = 1247)
#' alphalattice1$infoDesign
#' head(alphalattice1$fieldBook, 10)
#' 
#' # Example 2: Generates an alpha design with 5 full blocks and 50 treatment.
#' # Size of iBlocks k = 10. 
#' alphalattice2 <- alpha_lattice(t = 50, k = 10, r = 5, 
#'                                l = 1, 
#'                                plotNumber = c(101,1001,2001), 
#'                                locationNames = LETTERS[1:3], 
#'                                seed = 1945)
#' alphalattice2$infoDesign
#' head(alphalattice2$fieldBook, 10)
#' 
#' 
#' @export
alpha_lattice <- function(t = NULL, k = NULL, r = NULL, l = 1, plotNumber = 101, locationNames = NULL,
                          seed = NULL, data = NULL) {
  
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
    data_alpha <- NULL
  }else if (!is.null(data)) {
    if (is.null(r) || is.null(k) || is.null(l)) {
      shiny::validate('Basic design parameters missing (t, k, r).')
    }
    if(!is.data.frame(data)) shiny::validate("Data must be a data frame.")
    data <- as.data.frame(na.omit(data[,1]))
    colnames(data) <- "Treatment"
    data$Treatment <- as.character(data$Treatment)
    new_t <- length(data$Treatment)
    if (t != new_t) base::stop("Number of treatments do not match with data input.")
    TRT <- data$Treatment
    nt <- length(TRT)
    if (nt != t) shiny::validate('Number of treatment do not match with data input')
    data_alpha <- data
  }
  if (k >= nt) shiny::validate('incomplete_blocks() requires that k < t.')
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
  lambda <- r*(k - 1)/(nt - 1)
  matdf <- matdf$fieldBook
  OutAlpha <- as.data.frame(matdf)
  OutAlpha$LOCATION <- factor(OutAlpha$LOCATION, levels = locationNames)
  rownames(OutAlpha) <- 1:nrow(OutAlpha)
  infoDesign <- list(Reps = r, iBlocks = s, NumberTreatments = nt, NumberLocations = l, 
                     Locations = locationNames, seed = seed, lambda = lambda)
  
  return(list(infoDesign = infoDesign, fieldBook = OutAlpha))
}