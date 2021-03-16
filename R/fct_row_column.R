#' Generates a Resolvable Row-Column Design (RowColD)
#'
#'
#' @description It randomly generates a resolvable row-column designs (RowColD). 
#' Note that design optimization is only done at the level of rows and not columns; 
#' hence, design is suboptimal. The randomization can be done across locations.
#'
#' @param t Number of  treatments.
#' @param nrows Number of rows of a full resolvable replicate. 
#' @param r Number of blocks (full resolvable replicates).
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param data (optional) Data frame with label list of treatments
#' 
#' 
#' @importFrom stats runif na.omit
#' 
#' @return A list with information on the design parameters.
#' @return A list with the resolvables row columns for the first location.
#' @return Data frame with the RowColD field book.
#'
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
#' unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#'
#' @examples
#' 
#' # Example 1: Generates a row-column design with 3 full blocks and 36 treatments
#' # and 6 rows. This for one location.
#' rowcold1 <- row_column(t = 36, nrows = 6, r = 3, l = 1, 
#'                        plotNumber= 101, 
#'                        locationNames = "Loc1",
#'                        seed = 21)
#' rowcold1$infoDesign
#' rowcold1$resolvableBlocks
#' head(rowcold1$fieldBook,12)
#' 
#' # Example 2: Generates a row-column design with 3 full blocks and 30 treatments
#' # and 5 rows, for two locations.
#' # In this case, we show how to use the option data.
#' treatments <- paste("ND-", 1:30, sep = "")
#' treatment_list <- data.frame(list(TREATMENT = treatments))
#' head(treatment_list)
#' rowcold2 <- row_column(t = 30, nrows = 5, r = 3, l = 2, 
#'                        plotNumber= c(101,1001), 
#'                        locationNames = c("A", "B"),
#'                        seed = 15,
#'                        data = treatment_list)
#' rowcold2$infoDesign
#' rowcold2$resolvableBlocks
#' head(rowcold2$fieldBook,12)
#'   
#' 
#' @export
row_column <- function(t = NULL, nrows = NULL, r = NULL, l = 1, plotNumber= 101, locationNames = NULL,
                       seed = NULL, data = NULL) {
  
  if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  k <- nrows
  lookup <- FALSE
  if(is.null(data)) {
    if (is.null(t) || is.null(k) || is.null(r) || is.null(l)) {
      shiny::validate('Some of the basic design parameters are missing (t, k, r or l).')
    }
    arg1 <- list(k, r, l);arg2 <- c(k, r, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      shiny::validate('row_column() requires k, r and l to be possitive integers.')
    }
    if (is.numeric(t)) {
      if (length(t) == 1) {
        if (t == 1 || t < 1) {
          shiny::validate('row_column() requires more than one treatment.')
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
    data_RowCol <- NULL
  }else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      shiny::validate('Some of the basic design parameters are missing (t, r, k or l).')
    }
    if(!is.data.frame(data)) shiny::validate("Data must be a data frame.")
    data <- as.data.frame(na.omit(data[,1]))
    colnames(data) <- "Treatment"
    data$Treatment <- as.character(data$Treatment)
    new_t <- length(data$Treatment)
    if (t != new_t) base::stop("Number of treatments do not match with data input.")
    TRT <- data$Treatment
    nt <- length(TRT)
    data_RowCol <- data
  }
  if (k >= nt) shiny::validate('incomplete_blocks() requires k < t.')
  if(is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
  nunits <- k
  matdf <- incomplete_blocks(t = nt, k = nunits, r = r, l = l, plotNumber = plotNumber,
                             seed = seed, locationNames = locationNames,
                             data = data_RowCol)
  matdf <- matdf$fieldBook
  matdf <- as.data.frame(matdf)
  colnames(matdf)[5] <- "COLUMN"
  matdf$ROW <- matdf$UNIT
  OutRowCol <- matdf[,-6]
  OutRowCol$LOCATION <- factor(OutRowCol$LOCATION, levels = locationNames)
  OutRowCol <- OutRowCol[order(OutRowCol$LOCATION, OutRowCol$REP, OutRowCol$ROW),]
  RowCol_plots <- ibd_plot_numbers(nt = nt, plot.number = plotNumber, r = r, l = l)
  OutRowCol$PLOT <- as.vector(unlist(RowCol_plots))
  OutRowCol <- OutRowCol[,c(2,3,4,7,5,6)]
  ID <- 1:nrow(OutRowCol)
  OutRowCol <- cbind(ID, OutRowCol)
  rownames(OutRowCol) <- 1:nrow(OutRowCol)
  loc <- levels(OutRowCol$LOCATION)
  ib <- nt/k
  Resolvable_rc_reps <- vector(mode = "list", length = r)
  for (j in 1:r) {
     z <- OutRowCol
     z <- subset(z, z$LOCATION == loc[1] & z$REP == j)
     if (is.null(data)){
       Resolvable_rc_reps[[j]] <- matrix(data = as.vector(z$ENTRY), nrow = nunits, 
                                         ncol = ib, byrow = TRUE)
     }else {
       Resolvable_rc_reps[[j]] <- matrix(data = as.vector(z$TREATMENT), nrow = nunits, 
                                         ncol = ib, byrow = TRUE)
     }
  }
  df <- OutRowCol
  
  if (is.null(data)) trt <- "ENTRY" else trt <- "TREATMENT"
  c1 <- concurrence_matrix(df=df, trt=trt, target='REP')
  c2 <- concurrence_matrix (df=df, trt=trt, target='ROW')
  c3 <- concurrence_matrix (df=df, trt=trt, target='COLUMN')
  summ <- merge(c1, c2, by="Concurrence", all=TRUE)
  new_summ <- merge(summ, c3, by='Concurrence', all=TRUE)

  
  infoDesign <- list(nRows = nrows, nCols = ib, Reps = r, NumberTreatments = nt, NumberLocations = l, 
                     Locations = locationNames, seed = seed)

  return(list(infoDesign = infoDesign, resolvableBlocks = Resolvable_rc_reps, concurrence = new_summ,
              fieldBook = OutRowCol))
}