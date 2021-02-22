#' Strip Plot Design
#' 
#' @description It randomly generates a strip plot design across locations.
#'
#' @param Hplots Number of horizontal factors, as an integer or a vector.
#' @param Vplots Number of vertical factors, as an integer or a vector.
#' @param b Number of blocks (full replicates).
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param planter Option for \code{serpentine} or \code{cartesian} arrangement. By default \code{planter = 'serpentine'}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param data (optional) data frame with the labels of vertical and hirizontal plots.
#' 
#' @importFrom stats runif na.omit
#' 
#' @return A list with information on the design parameters.
#' @return Strip blocks for each location.
#' @return Data frame with the strip plot field book.
#' 
#'
#' @references
#' \emph{Design and Analysis of Experiments, Volume 1, Introduction to Experimental Design. Second Edition}.
#'  Klaus Hinkelmann & Oscar Kempthorne.John Wiley & Sons, Inc., Hoboken, New Jersey.
#' 
#' @examples
#' # Example 1: Generates a strip plot design with 5 vertical strips and 4 horizontal strips,
#' # with 3 reps in one location.
#' H <- paste("H", 1:4, sep = "")
#' V <- paste("V", 1:5, sep = "")
#' strip1 <- strip_plot(Hplots = H, 
#'                      Vplots = V, 
#'                      b = 3, 
#'                      l = 1, 
#'                      plotNumber = 101,
#'                      planter = "serpentine",
#'                      locationNames = "A", 
#'                      seed = 333)
#' strip1$infoDesign                  
#' strip1$stripsBlockLoc
#' strip1$plotLayouts
#' head(strip1$fieldBook,12)                     
#' 
#' # Example 2: Generates a strip plot design with 5 vertical strips and 5 horizontal strips,
#' # with 6 reps across to 3 locations.
#' strip2 <- strip_plot(Hplots = 5, 
#'                      Vplots = 5, 
#'                      b = 6, 
#'                      l = 3, 
#'                      plotNumber = c(101,1001,2001),
#'                      planter = "cartesian",
#'                      locationNames = c("A", "B", "C"), 
#'                      seed = 222)
#' strip2$infoDesign                  
#' strip2$stripsBlockLoc
#' strip2$plotLayouts
#' head(strip2$fieldBook,12)
#'
#' @export
strip_plot <- function(Hplots = NULL, Vplots = NULL, b = 1, l = 1, plotNumber = NULL,
                       planter = "serpentine", locationNames = NULL, seed = NULL, 
                       data = NULL) {
  
  if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  arg0 <- c(Hplots, Vplots)
  arg1 <- list(Hplots, Vplots)
  if (is.null(data)) {
    if(all(!is.null(c(Hplots, Vplots, b, l)))) {
      if (all(base::lengths(arg1) > 1)) {
        if (all(is.character(arg0)) || all(is.numeric(arg0))) {
          nH <- length(Hplots)
          nV <- length(Vplots)
        }
      }else if (all(base::lengths(arg1) == 1)) {
        if (all(is.numeric(arg0))) {
          Hplots <- paste(rep("b", Hplots), 0:(Hplots-1), sep = "")
          Vplots <- paste(rep("a", Vplots), 0:(Vplots-1), sep = "")
          nH <- length(Hplots)
          nV <- length(Vplots)
        }
      }else {
        stop("\n'stripDesign()' requires an 1-dimensional array for input Hplots and Vplots.")
      }
    }else stop("\n 'stripDesign()' requires arguments to be differents than NULL")
  }else {
    if(!is.data.frame(data)) stop("Data must be a data frame.")
    data <- na.omit(data)
    if (ncol(data) < 2) base::stop("Data input needs at least two columns.")
    data <- as.data.frame(data[,1:2])
    colnames(data) <- c("Hplot", "Vplot")
    Hplots <- as.vector(na.omit(data$Hplot))
    Vplots <- as.vector(na.omit(data$Vplot))
    Hplots.f <- factor(Hplots, as.character(unique(Hplots)))
    Vplots.f <- factor(Vplots, as.character(unique(Vplots)))
    nH <- length(levels(Hplots.f))
    nV <- length(levels(Vplots.f))
    Hplots <- as.character(Hplots.f)
    Vplots <- as.character(Vplots.f)
  }
  if(!is.null(l) && is.numeric(l) && length(l) == 1) {
    if (l > 1 && is.null(locationNames)) {
      locationNames <- 1:l
    }else if (l > 1 && !is.null(locationNames)) {
      if (length(locationNames) < l) locationNames <- 1:l
    }
  }else stop("\n'stripDesign()' requires number of locations to be an integer.")
  if (!is.null(plotNumber)) {
    if (any(!is.numeric(plotNumber)) || any(plotNumber < 1) || any(plotNumber %% 1 != 0) ||
        any(diff(plotNumber) < 0)) {
      shiny::validate("Input plotNumber must be an integer greater than 0, and sorted.")
    } 
  }else {
    plotNumber <- seq(1001, 1000*(l+1), 1000)
    warning("Since plotNumber was NULL, it was set up to its default value for each location.")
  }
  plot.numbs <- seriePlot.numbers(plot.number = plotNumber, reps = b, l = l)
  if (!is.null(locationNames) && length(locationNames) == l) {
    locs <- locationNames
  }else locs <- 1:l
  strips.b <- vector(mode = "list", length = b*l)
  stripDesig.b <- vector(mode = "list", length = b*l)
  stripDesig.out.l <- vector(mode = "list", length = l)
  v <- 1:(b * l)
  z <- 1
  x <- seq(1, b * l, b)
  y <- seq(b, b * l, b)
  PLOTS <- vector(mode = "list", length = b*l)
  for (sites in 1:l) {
    for (r in 1:b) {
      D <- plot.numbs[[sites]]
      P <- matrix(data = D[r]:(D[r] + (nH*nV) - 1), nrow = nH, ncol = nV, byrow = TRUE)
      if (planter == "serpentine") P <- serpentinelayout(P, opt = 2)
      PLOTS[[z]] <- P
      Hplots.random <- replicate(1, sample(Hplots))
      Vplots.random <- replicate(1, sample(Vplots))
      strips <- paste(rep(Hplots.random[,1], each = nV), 
                      rep(Vplots.random[,1], times = nH), 
                      sep = "|")
      stripD <- matrix(strips, nrow = nH, byrow = TRUE)
      rownames(stripD) <- Hplots.random
      colnames(stripD) <- Vplots.random
      strips.b[[v[z]]] <- stripD
      stripDesig.b[[v[z]]] <- data.frame(list(LOCATION = locs[sites], 
                                              PLOT = as.vector(t(P)),
                                              REP = r,
                                              HSTRIP = rep(Hplots.random[,1], each = nV),
                                              VSTRIP = rep(Vplots.random[,1], times = nH),
                                              TRT_COMB = strips))
      z <- z + 1
    }
    stripDesig.out.l[[sites]] <- paste_by_row(stripDesig.b[x[sites]:y[sites]])
  }
  stripDesig.output <- paste_by_row(stripDesig.out.l)
  stripDesig.out.loc <- vector(mode = "list", length = l)
  strips.b.loc <- vector(mode = "list", length = l)
  w <- 1
  for (loc in 1:l) {
    stripDesig.out.loc[[loc]] <- paste_by_row(stripDesig.b[x[w]:y[w]])
    strips.b.loc[[loc]] <- strips.b[x[w]:y[w]]
    w <- w + 1
  }
  if (!is.null(locationNames) && length(locationNames) == l) {
    stripDesig.output$LOCATION <- rep(locationNames, each = (nH * nV) * b)
  }
  
  infoDesign <- list(Hplots = nH, Vplots = nV, blocks = b, numberLocations = l,
                     nameLocations = locationNames, seed = seed)
  
  return(list(infoDesign = infoDesign, stripsBlockLoc = strips.b.loc,
              plotLayouts = PLOTS, fieldBook = stripDesig.output))
}