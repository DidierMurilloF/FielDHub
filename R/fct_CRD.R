#' Generates a Completely Randomized Design (CRD)
#'
#' @description It randomly generates a completely randomized design.
#'
#' @param t An integer number with total number of treatments or a vector of dimension t with labels.
#' @param reps Number of replicates of each treatment.
#' @param plotNumber Starting plot number. By default \code{plotNumber = 101}.
#' @param locationName (optional) Name of the location.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param data (optional) Data frame with the 2 columns with labels of each treatments and its number of replicates.
#'
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
#'   \item \code{fieldBook} is a data frame with the CRD field book.
#' }
#'
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#'
#' @examples
#' # Example 1: Generates a CRD design with 10 treatments and 5 reps each.
#' crd1 <- CRD(t = 10,
#'             reps = 5,
#'             plotNumber = 101,
#'             seed = 1987,
#'             locationName = "Fargo")
#' crd1$infoDesign
#' head(crd1$fieldBook,10)
#'
#' # Example 2: Generates a CRD design with 15 treatments and 6 reps each.
#' Gens <- paste("Wheat", 1:15, sep = "")
#' crd2 <- CRD(t = Gens,
#'             reps = 6,
#'             plotNumber = 1001,
#'             seed = 1654,
#'             locationName = "Fargo")
#' crd2$infoDesign
#' head(crd2$fieldBook,10)
#'
#' # Example 3: Generates a CRD design with 12 treatments and 4 reps each.
#' # In this case, we show how to use the option data.
#' treatments <- paste("ND-", 1:12, sep = "")
#' treatment_list <- data.frame(list(TREATMENT = treatments, REP = 4))
#' head(treatment_list)
#' crd3 <- CRD(t = NULL,
#'             reps = NULL,
#'             plotNumber = 2001,
#'             seed = 1655,
#'             locationName = "Cali",
#'             data = treatment_list)
#' crd3$infoDesign
#' head(crd3$fieldBook,10)
#'
#' @export
CRD <- function(t = NULL, reps = NULL, plotNumber = 101, locationName = NULL,
                seed = NULL, data = NULL) {
  
  if (is.null(seed) || is.character(seed) || is.factor(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  if (!is.null(plotNumber)) {
    if (plotNumber < 1 || plotNumber %% 1 != 0) shiny::validate('plotNumber must be an integer greater than 0.')
  }else {
    plotNumber <- 101
    warning("Since plotNumber was NULL, default 'plotNumber = 101' is considered.")
  }
  if (is.null(locationName)) locationName <- 1
  if (is.null(data)) {
    if(!is.null(t) & !is.null(reps)) {
      if(length(t) == 1 & is.numeric(t)) {
        arg2 <- c(t, reps)
        if (base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
          shiny::validate('CRD() requires that t and reps are integers greater than 0.')
        }
        nt <- t
        trts <- paste(rep("T", nt), 1:nt, sep = "")
        TRT <- rep(trts, each = reps)
      }else if(is.character(t) & length(t) > 1) {
        TRT <- t
        nt <- length(t)
      }else if(is.character(t) & length(t) > 1) {
        shiny::validate('"CRD()" requires more than one treatment.')
      }
    } else {
      stop("Inputs t and reps are missing.")
    }
    N <- nt * reps
    REP <- rep(1:reps, times = nt)
  }else {
    if(!is.data.frame(data)) stop("Data must be a data frame.")
    if (ncol(data) < 2) validate("Data input needs at least two columns with the names: Treatment and Reps.")
    data <- as.data.frame(data[,1:2])
    data <- na.omit(data)
    colnames(data) <- c("Treatment", "Reps")
    if(is.character(data[,2]) || is.factor(data[,2])) validate("Reps must be numeric.")
    data$Reps <- as.numeric(data$Reps)
    TRT <- rep(data$Treatment, times = data$Reps)
    N <- sum(data$Reps)
    j <- 1
    REP.l <- list()
    for(i in data$Reps) {
      REP.l[[j]] <- 1:i
      j <- j + 1
    }
    REP <- as.numeric(unlist(REP.l))
    nt <- length(data$Treatment)
    reps <- as.vector(data$Reps)
  }
  
  design <- data.frame(list(LOCATION = rep(locationName, N)),
                       PLOT = sample(plotNumber:(plotNumber + N - 1)),
                       REP = as.factor(REP), TREATMENT = TRT)
  design <- design[order(design$PLOT),]
  id <- 1:nrow(design)
  design <- cbind(id, design)
  colnames(design)[1] <- "ID"
  design <- as.data.frame(design)
  rownames(design) <- 1:N
  TRT <- levels(factor(TRT, as.character(unique(TRT))))
  parameters <- list(numberofTreatments = nt, treatments = TRT, Reps = reps, locationName = locationName,
                     seed = seed, id_design = 1)
  output <- list(infoDesign = parameters, fieldBook = design)
  class(output) <- "FielDHub"
  return(invisible(output))
}