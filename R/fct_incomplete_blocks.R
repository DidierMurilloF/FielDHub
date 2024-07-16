#' Generates a Resolvable Incomplete Block Design
#'
#' @description Randomly generates a resolvable incomplete block design (IBD) of characteristics (t, k, r).
#' The randomization can be done across locations.
#'
#' @param t Number of  treatments.
#' @param r Number of full blocks (or resolvable replicates) (also number of replicates per treatment).
#' @param k Size of incomplete blocks (number of units per incomplete block).
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param data (optional) Data frame with label list of treatments.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#'
#' @importFrom stats runif na.omit aggregate
#'
#'
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the incomplete block design field book.
#' }
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
#' unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#'
#' @examples
#' # Example 1: Generates a resolvable IBD of characteristics (t,k,r) = (12,4,2).
#' # 1-resolvable IBDs
#' ibd1 <- incomplete_blocks(t = 12,
#'                           k = 4,
#'                           r = 2,
#'                           seed = 1984)
#' ibd1$infoDesign
#' head(ibd1$fieldBook)
#'
#' # Example 2: Generates a balanced resolvable IBD of characteristics (t,k,r) = (15,3,7).
#' # In this case, we show how to use the option data.
#' treatments <- paste("TX-", 1:15, sep = "")
#' ENTRY <- 1:15
#' treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
#' head(treatment_list)
#' ibd2 <- incomplete_blocks(t = 15,
#'                           k = 3,
#'                           r = 7,
#'                           seed = 1985,
#'                           data = treatment_list)
#' ibd2$infoDesign
#' head(ibd2$fieldBook)
#'
#' @export
incomplete_blocks <- function(t = NULL, k = NULL, r = NULL, l = 1, plotNumber = 101, 
                              locationNames = NULL, seed = NULL, data = NULL) {

  if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
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
    } else if (is.character(t) || is.factor(t)) {
      if (length(t) == 1) {
        shiny::validate('incomplete_blocks() requires more than one treatment.')
      } 
      nt <- length(t)
    } else if ((length(t) > 1)) {
      nt <- length(t)
    }
    data_up <- data.frame(list(ENTRY = 1:nt, TREATMENT = paste0("G-", 1:nt)))
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    lookup <- TRUE
    df <- data.frame(list(ENTRY = 1:nt, LABEL_TREATMENT = paste0("G-", 1:nt)))
    dataLookUp <- df
  } else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      shiny::validate('Some of the basic design parameters are missing (t, k, r or l)')
    }
    if(!is.data.frame(data)) shiny::validate("Data must be a data frame.")
    if (ncol(data) < 2) base::stop("Data input needs at least two columns with: ENTRY and NAME.")
    data_up <- as.data.frame(data[,c(1,2)])
    data_up <- na.omit(data_up)
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    data_up$TREATMENT <- as.character(data_up$TREATMENT)
    new_t <- length(data_up$TREATMENT)
    if (t != new_t) base::stop("Number of treatments do not match with the data input.")
    TRT <- data_up$TREATMENT
    nt <- length(TRT)
    lookup <- TRUE
    dataLookUp <- data.frame(list(ENTRY = 1:nt, LABEL_TREATMENT = TRT))
  }
  if(any(plotNumber %% 1 != 0) || any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
    shiny::validate("'incomplete_blocks()' requires plotNumber to be possitive integers and sorted.")
  }
  if (is.null(plotNumber) || length(plotNumber) != l) {
    if (length(plotNumber) != l || is.null(plotNumber)) plotNumber <- seq(1001, 1000*(l+1), 1000)
    warning("Since plotNumber was missing, it was set up to default values.")
  }
  if (k >= nt) shiny::validate('incomplete_blocks() requires that k < t.')
  if(is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
  nincblock <- nt*r/k
  N <- nt * r
  if (k * nincblock != N) {
    shiny::validate('Size of experiment defined by number of units per incomplete block (nunits) is inconsistent. Check input parameters.')
  }
  if (nt %% k != 0) {
    shiny::validate('Number of treatments can not be fully distributed over the specified incomplete block specification.')
  }

  ibd_plots <- ibd_plot_numbers(nt = nt, plot.number = plotNumber, r = r, l = l)
  b <- nt/k
  square <- FALSE
  if (sqrt(nt) == round(sqrt(nt))) square <- TRUE
  outIBD_loc <- vector(mode = "list", length = l)
  blocks_model <- list()
  for (i in 1:l) {
    mydes <- blocksdesign::blocks(treatments = nt, replicates = r, blocks = list(r, b), seed = NULL)
    mydes <- rerandomize_ibd(ibd_design = mydes)
    matdf <- base::data.frame(list(LOCATION = rep(locationNames[i], each = N)))
    matdf$PLOT <- as.numeric(unlist(ibd_plots[[i]]))
    matdf$BLOCK <- rep(c(1:r), each = nt)
    matdf$iBLOCK <- rep(c(1:b), each = k)
    matdf$UNIT <- rep(c(1:k), nincblock)
    matdf$TREATMENT <- mydes$Design_new[,4]
    colnames(matdf) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "ENTRY")
    outIBD_loc[[i]] <- matdf
    blocks_model[[i]] <- mydes$Blocks_model_new
  }
  OutIBD <- dplyr::bind_rows(outIBD_loc)
  OutIBD <- as.data.frame(OutIBD)
  OutIBD$ENTRY <- as.numeric(OutIBD$ENTRY)
  OutIBD_test <- OutIBD
  OutIBD_test$ID <- 1:nrow(OutIBD_test)
  if(lookup) {
    OutIBD <- dplyr::inner_join(OutIBD, dataLookUp, by = "ENTRY")
    OutIBD <- OutIBD[,-6]
    colnames(OutIBD) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "TREATMENT")
    OutIBD <- dplyr::inner_join(OutIBD, data_up, by = "TREATMENT")
    OutIBD <- OutIBD[, c(1:5,7,6)]
    colnames(OutIBD) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT")
  }
  ID <- 1:nrow(OutIBD)
  OutIBD_new <- cbind(ID, OutIBD)
  validateTreatments(OutIBD_new)
  lambda <- r*(k - 1)/(nt - 1)
  infoDesign <- list(Reps = r, iBlocks = b, NumberTreatments = nt, NumberLocations = l,
                     Locations = locationNames, seed = seed, lambda = lambda, 
                     id_design = 8)
  output <- list(infoDesign = infoDesign, fieldBook = OutIBD_new, blocksModel = blocks_model[[1]])
  class(output) <- "FielDHub"
  return(invisible(output))
}

#' @noRd 
#' 
#' 
concurrence_matrix <- function(df=NULL, trt=NULL, target=NULL) {
  if (is.null(df)) {
    stop('No input dataset provided.')
  }
  if (is.null(trt)) {
    stop('No input treatment factor provided.')
  }
  if (is.null(target)) {
    stop('No input target design factor provided.')
  }
  df[,target]<-as.factor(df[,target])
  df[,trt]<-as.factor(df[,trt])
  s <- length(levels(df[,target]))
  if (s==0) { stop('No levels found for design factor provided.') }
  inc <- as.matrix(table(df[,target],df[,trt]))
  for (i in 1:s) {
    inc[inc[,i]>0,i] <- 1
  }
  conc.matrix <- t(inc) %*% inc
  summ.rep <- diag(conc.matrix)
  diag(conc.matrix) <- rep(99999999,nrow(conc.matrix))
  summ<-as.data.frame(table(as.vector(conc.matrix)))
  summ$Freq <- summ$Freq/2  # Added
  colnames(summ) <- c('Concurrence',target)
  summ <- summ[-nrow(summ),]
  
  return(summ = summ)
}
