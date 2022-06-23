#' Generates a Randomized Complete Block Design (RCBD)
#' 
#' 
#' @description It randomly generates a randomized complete block design (RCBD) across locations.
#'
#' @param t An integer number with total number of treatments or a vector of dimension t with labels.
#' @param reps Number of replicates (full blocks) of each treatment.
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param continuous Logical value for plot number continuous or not. By default \code{continuous = FALSE}.
#' @param planter Option for \code{serpentine} or \code{cartesian} arrangement. By default \code{planter = 'serpentine'}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param data (optional) Data frame with the labels of treatments.
#' 
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' @importFrom stats runif na.omit setNames
#' 
#' 
#' @return A list with five elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{layoutRandom} is the RCBD layout randomization for each location.
#'   \item \code{plotNumber} is the plot number layout for each location.
#'   \item \code{fieldBook} is a data frame with the RCBD field book design.
#' } 
#' 
#'
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#' 
#' @examples
#' # Example 1: Generates a RCBD design with 3 blocks and 20 treatments across 3 locations.
#' rcbd1 <- RCBD(t = LETTERS[1:20], reps = 5, l = 3, 
#'               plotNumber = c(101,1001, 2001), 
#'               continuous = TRUE,
#'               planter = "serpentine", 
#'               seed = 1020, 
#'               locationNames = c("FARGO", "MINOT", "CASSELTON"))
#' rcbd1$infoDesign                  
#' rcbd1$layoutRandom
#' rcbd1$plotNumber
#' head(rcbd1$fieldBook)
#' 
#' # Example 2: Generates a RCBD design with 6 blocks and 18 treatments in one location.
#' # In this case, we show how to use the option data.
#' treatments <- paste("ND-", 1:18, sep = "")
#' treatment_list <- data.frame(list(TREATMENT = treatments))
#' head(treatment_list)
#' rcbd2 <- RCBD(reps = 6, l = 1, 
#'               plotNumber = 101, 
#'               continuous = FALSE, 
#'               planter = "serpentine", 
#'               seed = 13, 
#'               locationNames = "IBAGUE",
#'               data = treatment_list)
#' rcbd2$infoDesign                  
#' rcbd2$layoutRandom
#' rcbd2$plotNumber
#' head(rcbd2$fieldBook)
#' 
#'
#' @export
RCBD <- function(t = NULL, reps = NULL, l = 1, plotNumber = 101, 
                 continuous = FALSE, planter = "serpentine", 
                 seed = NULL, locationNames = NULL,
                 data = NULL) {
  b <- reps
  if (all(c("serpentine", "cartesian") != planter)) {
    stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  if (is.null(l) || !is.numeric(l) || l %% 1 != 0) {
    shiny::validate("'RCBD()' requires that locations number to be an integer greater than 0.")
  }
  b <- reps
  if (!is.null(plotNumber) && length(plotNumber) == l) {
    if (any(!is.numeric(plotNumber)) || any(plotNumber < 1) || any(plotNumber %% 1 != 0) ||
        any(diff(plotNumber) < 0)) {
      shiny::validate("Input plotNumber must be an integer greater than 0 and sorted.")
    } 
  }else {
    plotNumber <- seq(1001, 1000*(l+1), 1000)
    message(cat("Warning message:", "\n", 
                "Since plotNumber was missing, it was set up to default value of: ",
                plotNumber))
  }
  if (!is.null(locationNames)) {
    locationNames <- toupper(locationNames)
  } else locationName <- 1:l
  if (is.null(data)) {
    if (!is.null(t) & !is.null(b)) {
      if(length(t) == 1 & is.numeric(t)) {
        arg2 <- c(t, b)
        if (base::any(arg2 %% 1 != 0) || base::any(arg2 < 2)) {
          shiny::validate("RCBD() requires input t and b to be integer > 1.")
        }
        nt <- t
        mytreatments <- paste(rep("T", each = nt), 1:nt, sep = "")
        s <- paste(rep("T", each = nt), 1:nt, sep = "")
      }else if(is.character(t) & length(t) > 1) {
        nt <- length(t)
        s <- t
        mytreatments <- t
      }else if(is.character(t) & length(t) > 1) {
        shiny::validate("'RCBD()' requires more than one treatment.")
      }
    }else {
      stop("Input t and b are missing.")
    }
  }else if (!is.null(b) && !is.null(data)) {
    if(!is.data.frame(data)) stop("Data must be a data frame.")
    data <- as.data.frame(na.omit(data[,1]))
    colnames(data) <- "Treatment"
    data$Treatment <- as.character(data$Treatment)
    t <- data$Treatment
    nt <- length(t)
    s <- t
    mytreatments <- data$Treatment
  }
  if (length(locationNames) != l) {
    locationNames <- paste("loc", 1:l, sep = "")
  }
  RCBD <- matrix(data = NA, nrow = b * l, ncol = nt, byrow = TRUE)
  RCBD.layout <- matrix(data = NA, nrow = b, ncol = 2, byrow = TRUE)
  RCBD.layout.loc <- setNames(vector(mode = "list", length = l),
                              paste0("Loc_", locationNames)) # set names
  k <- seq(1, l * b, b)
  m <- seq(b, l * b, b)
  for (i in 1:l) {
    v <- 1
    RCBD.layout <- matrix(data = NA, nrow = b, ncol = 2, byrow = TRUE)
    RCBD.layout[,1] <- 1:b
    colnames(RCBD.layout) <- c("Block","--Treatments--")
    for (j in k[i]:m[i]) {
      RCBD[j,] <- sample(s, size = length(s), replace = FALSE)
      RCBD.layout[v,2] <- paste(RCBD[j,], collapse = " ")
      v <- v + 1
    }
    RCBD.layout.loc[[i]] <- RCBD.layout
  }
  plotNumber <- seriePlot.numbers(plot.number = plotNumber, 
                                  reps = b,
                                  l = l, 
                                  t = nt)
  p.number.loc <- setNames(vector(mode = "list", length = l),
                           paste0("Loc_", locationNames))
  if (!continuous) {
    if (planter == "serpentine") {
      for (i in 1:l) {
        M <- matrix(data = NA, ncol = nt, nrow = b, byrow = TRUE)
        for (k in 1:b) {
          D <- plotNumber[[i]]
          M[k,] <- D[k]:(D[k] + (nt - 1))
        }
        p.number.loc[[i]] <- serpentinelayout(M, opt = 2)
      }
    }else {
      for (i in 1:l) {
        M <- matrix(data = NA, ncol = nt, nrow = b, byrow = TRUE)
        for (k in 1:b) {
          D <- plotNumber[[i]]
          M[k,] <- D[k]:(D[k] + (nt - 1))
        }
        p.number.loc[[i]] <- M
      }
    }
  }else {
    if (planter == "serpentine") {
      for (i in 1:l) {
        D <- plotNumber[[i]]
        M <- matrix(data = D[1]:(D[1] + (nt * b - 1)), ncol = nt,
                                    nrow = b, byrow = TRUE)
        p.number.loc[[i]] <- serpentinelayout(M, opt = 2)
      }
    }else {
      for (i in 1:l) {
        D <- plotNumber[[i]]
        p.number.loc[[i]] <- matrix(data = D[1]:(D[1] + (nt * b - 1)), 
                                    ncol = nt,
                                    nrow = b, 
                                    byrow = TRUE)
      }
    }
  }
  if (l > 1) {
    p.number.loc1 <- paste_by_row(p.number.loc)
  }else {
    p.number.loc1 <- p.number.loc[[1]]
  }
  RCBD.output <- data.frame(list(LOCATION = rep(locationNames, each = nt * b), 
                                 PLOT = as.vector(t(p.number.loc1)),
                                 REP = rep(1:b, each = nt), 
                                 TREATMENT = as.vector(t(RCBD))))
  
  RCBD.output$LOCATION <- factor(RCBD.output$LOCATION, 
                                 levels = as.character(unique(locationNames)))
  RCBD.output <- RCBD.output[order(RCBD.output$LOCATION, RCBD.output$PLOT),]
  
  ID <- 1:nrow(RCBD.output)
  RCBD_output <- cbind(ID, RCBD.output)
  RCBD_output <- as.data.frame(RCBD_output)
  
  RCBD.layout <- as.data.frame(RCBD.layout)
  
  plotNumber <- as.vector(unlist(plotNumber))
  
  parameters = list(blocks = b, 
                    number.of.treatments = nt, 
                    treatments = mytreatments,
                    locations = l, 
                    plotNumber = plotNumber, 
                    locationNames = locationNames,
                    seed = seed, id_design = 2)
  output <- list(infoDesign = parameters, 
                 layoutRandom = RCBD.layout.loc,
                 plotNumber = p.number.loc,
                 fieldBook = RCBD_output)
  class(output) <- "FielDHub"
  return(invisible(output))
}