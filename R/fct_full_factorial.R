#' Generates a Full Factorial Design
#'
#' @description It randomly generates a full factorial design across locations.
#'
#' @param setfactors Numeric vector with levels of each factor.
#' @param reps Number of replicates (full blocks).
#' @param l Number of locations. By default \code{l = 1}.
#' @param type Option for CRD or RCBD designs. Values are \code{type =
#'   1} (CRD) or \code{type = 2} (RCBD). By default \code{type = 2}.
#' @param plotNumber Numeric vector with the starting plot number for
#'   each location. By default \code{plotNumber = 101}.
#' @param continuous Logical for plot number continuous or not. By
#'   default \code{continuous = FALSE}.
#' @param planter Option for \code{serpentine} or \code{cartesian} plot
#'   arrangement. By default \code{planter = 'serpentine'}.
#' @param seed (optional) Real number that specifies the starting seed
#'   to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param factorLabels (optional) If \code{TRUE} retain the levels
#'   labels from the original data set otherwise, numeric labels will be
#'   assigned. Default is \code{factorLabels =TRUE}.
#' @param data (optional) Data frame with the labels of factors.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#'
#' @importFrom stats runif na.omit
#'
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the full factorial field book.
#' }
#' 
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#'
#' @examples
#' # Example 1: Generates a full factorial with 3 factors each with 2 levels.
#' # This in an RCBD arrangement with 3 reps.
#' fullFact1 <- full_factorial(setfactors = c(2,2,2), reps = 3, l = 1, type = 2,
#'                             plotNumber = 101,
#'                             continuous = TRUE,
#'                             planter = "serpentine",
#'                             seed = 325,
#'                             locationNames = "FARGO")
#' fullFact1$infoDesign
#' head(fullFact1$fieldBook,10)
#'
#' # Example 2: Generates a full factorial with 3 factors and each with levels: 2,3,
#' # and 2, respectively. In this case, we show how to use the option data
#' FACTORS <- rep(c("A", "B", "C"), c(2,3,2))
#' LEVELS <- c("a0", "a1", "b0", "b1", "b2", "c0", "c1")
#' data_factorial <- data.frame(list(FACTOR = FACTORS, LEVEL = LEVELS))
#' print(data_factorial)
#' # This in an RCBD arrangement with 5 reps in 3 locations.
#' fullFact2 <- full_factorial(setfactors = NULL, reps = 5, l = 3, type = 2,
#'                             plotNumber = c(101,1001,2001),
#'                             continuous = FALSE,
#'                             planter = "serpentine",
#'                             seed = 326,
#'                             locationNames = c("Loc1","Loc2","Loc3"),
#'                             data = data_factorial)
#' fullFact2$infoDesign
#' head(fullFact2$fieldBook,10)
#'
#' @export
full_factorial <- function(setfactors = NULL, reps = NULL, l = 1,
                           type = 2, plotNumber = 101, continuous = FALSE,
                           planter = "serpentine", seed = NULL,
                           locationNames = NULL, factorLabels = TRUE,
                           data = NULL) {
  if (all(c("serpentine", "cartesian") != planter)) {
    stop("Input for planter choice is unknown. Please, choose one: serpentine or cartesian.")
  }
  if (is.null(seed) || is.character(seed) || is.factor(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  if(l < 1 || is.null(l)) stop("Please, check the value for the number of locations.")
  if (!is.null(plotNumber) && length(plotNumber) == l) {
    if (any(!is.numeric(plotNumber)) || any(plotNumber < 1) || any(plotNumber %% 1 != 0) ||
        any(diff(plotNumber) < 0)) {
      shiny::validate("The input plotNumber must be an integer greater than 0 and sorted.")
    }
  }else {
    plotNumber <- seq(1001, 1000*(l+1), 1000)
    warning("'plotNumber' was set up to its default values for each site.")
  }
  if (is.null(data)) {
    if(!is.null(setfactors)) {
      if(is.numeric(setfactors)) {
        if (length(setfactors) < 2) stop("More than one factor needs to be specified.")
        nt <- length(setfactors)
        TRT <- rep(LETTERS[1:nt], each = reps)
        newlevels <- get.levels(k = setfactors)
        allcomb <- expand.grid(newlevels, KEEP.OUT.ATTRS = FALSE,
                               stringsAsFactors = FALSE)
        colnames(allcomb) <- levels(as.factor(TRT))
        data <- data.frame(list(factors = rep(levels(as.factor(TRT)), times = setfactors),
                                levels = unlist(newlevels)))
        levels.by.factor <- as.vector(unlist(newlevels))
        entries_each_factor <- setfactors
      }else stop("In 'full_factorial()' the input setfactors must be a numeric vector.")
    }
  } else {
    if(!is.data.frame(data)) stop("Data must be a data frame.")
    data <- as.data.frame(na.omit(data[,1:2]))
    colnames(data) <- c("factors", "levels")
    data$factors <- factor(data$factors, as.character(unique(data$factors)))
    l.factors <- levels(data$factors)
    levels.by.factor <- list()
    data.by.factor <- list()
    entries_each_factor <- numeric()
    v <- 1
    for(i in l.factors) {
      data.by.factor[[v]] <- subset(data, data$factors == i)
      entries_each_factor[v] <- nrow(subset(data, data$factors == i))
      levels.by.factor[[v]] <- data.by.factor[[v]][,2]
      v <- v + 1
    }
    if (!factorLabels) {
      levels.by.factor <- split_vectors(x = 1:nrow(data), len_cuts = base::lengths(levels.by.factor))
    }
    nt <- length(l.factors)
    allcomb <- base::expand.grid(levels.by.factor, KEEP.OUT.ATTRS = FALSE,
                                 stringsAsFactors = FALSE)
    colnames(allcomb) <- l.factors
    newlevels <- data.by.factor
    TRT <- l.factors
  }
  if (is.null(locationNames)) {
    locationNames <- 1:l
  }else if (!is.null(locationNames)) {
    if (length(locationNames) < l || length(locationNames) > l) locationNames <- 1:l
  }
  nruns <- nrow(allcomb)
  trt <- vector(mode = "character", length = nruns)
  H <- 1:nrow(allcomb)
  for (i in H) {
    trt[i] <- paste(allcomb[i,], collapse = " ")
  }
  design.loc <- list()
  for (locs in 1:l) {
    if (type == 1) {
      m1 <- CRD(t = trt, reps = reps, plotNumber = plotNumber[locs], # seed = seed,
                data = NULL, locationName = locationNames[1])$fieldBook
      m1 <- m1[,-c(1,2)]
      kind <- "CRD"
    }else {
      m1 <- RCBD(t = trt, reps = reps, l = 1, plotNumber = plotNumber[locs], continuous = continuous,
                 planter = planter, locationNames = locationNames[locs])$fieldBook # seed = seed,
      m1 <- m1[,-c(1,2)]
      kind <- "RCBD"
    }
    m1 <- cbind(m1, matrix(data = 0, nrow = nruns, ncol = nt + 1, byrow = TRUE))
    t <- nruns * reps
    z <- 1:t
    for (j in z) {
      m1[j, 4:(4 + nt - 1)] <- unlist(strsplit(as.character(m1[j,3]), " "))
      m1[j, ncol(m1)] <- paste(m1[j, 4:(4 + nt - 1)], collapse = "*")
    }
    design <- m1
    design <- design[,-3]
    design.loc[[locs]] <- design
  }
  design <- paste_by_row(design.loc)
  TRT <- factor(TRT, as.character(unique(TRT)))
  ColFactors <- paste("FACTOR_", levels(TRT), sep = "")
  colnames(design) <- c("PLOT", "REP", ColFactors, "TRT_COMB")
  if (kind == "RCBD") {
    design <- design[order(design$PLOT, design$REP),]
  }
  nruns <- nrow(allcomb)
  design_output <- cbind(ID = 1:nrow(design), LOCATION = rep(locationNames, each = nruns * reps), design)
  levelsByFactor <- levels.by.factor
  #newlevels
  allcomb <- as.data.frame(allcomb)
  fullfactorial <- list(
    factors = levels(TRT), 
    levels = levelsByFactor, 
    runs = nruns, 
    all_treatments = allcomb,
    reps = reps, 
    locations = l, 
    location_names = locationNames, 
    kind = kind, 
    levels_each_factor = entries_each_factor,
    id_design = 4)
  output <- list(infoDesign = fullfactorial, fieldBook = design_output)
  class(output) <- "FielDHub"
  return(invisible(output))
}
