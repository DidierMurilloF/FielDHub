#' Generates a Latin Square Design
#'
#' Randomly generates a latin square design of up 10 treatments.
#'
#' @param t Number of treatments.
#' @param reps Number of full resolvable squares. By default \code{reps = 1}.
#' @param plotNumber Starting plot number. By default \code{plotNumber = 101}.
#' @param planter Option for \code{serpentine} or \code{cartesian} arrangement. By default \code{planter = 'serpentine'}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Name for the location.
#' @param data (optional) Data frame with label list of treatments.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb],
#'         Thiago de Paula Oliveira[ctb] 
#'         Richard Horsley [ctb]
#' 
#' @importFrom stats runif na.omit setNames
#'
#' @importFrom stats runif na.omit
#'
#' @return A list with information on the design parameters.
#' @return Data frame with the latin square field book.
#' 
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the latin square field book.
#' }
#'
#' @importFrom stats setNames
#'
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#'
#' @examples
#' # Example 1: Generates a latin square design with 4 treatments and 2 reps.
#' latinSq1 <- latin_square(t = 4,
#'                          reps = 2,
#'                          plotNumber = 101,
#'                          planter = "cartesian",
#'                          seed = 1980)
#' print(latinSq1)
#' summary(latinSq1)
#' head(latinSq1$fieldBook)
#'
#' # Example 2: Generates a latin square design with 5 treatments and 3 reps.
#' latin_data <- data.frame(list(ROW = paste("Period", 1:5, sep = ""),
#'                               COLUMN = paste("Cow", 1:5, sep = ""),
#'                               TREATMENT = paste("Diet", 1:5, sep = "")))
#' print(latin_data)
#' latinSq2 <- latin_square(t = NULL,
#'                          reps = 3,
#'                          plotNumber = 101,
#'                          planter = "cartesian",
#'                          seed = 1981,
#'                          data = latin_data)
#' latinSq2$squares
#' latinSq2$plotSquares
#' head(latinSq2$fieldBook)
#'
#' @export
latin_square <- function(t = NULL, reps = 1, plotNumber = 101,  planter = "serpentine",
                         seed = NULL, locationNames = NULL, data = NULL) {

  if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  if (all(c("serpentine", "cartesian") != planter)) {
    base::stop('Input planter is unknown. Please, choose one: "serpentine" or "cartesian"')
  }
  n <- t
  l <- 1
  if (is.null(data)) {
    if (all(!is.null(c(n, reps))) && all(base::lengths(list(n, reps)) == 1)) {
      if (all(is.numeric(c(n, reps))) && all(c(n, reps) %% 1 == 0) & all(c(n, reps) > 0)) {
        if (n > 10) stop("\n'latinsquare()' allows only up to 10 treatments.")
        ls.len <- n
        Name.Rows <- paste(rep("Row", ls.len), 1:ls.len)
        Name.Columns <- paste(rep("Column", ls.len), 1:ls.len)
        Name.Treatments <- paste(rep("T", ls.len), 1:ls.len,  sep = "")
      }else stop("\n'latinsquare()' requires a possitive integer number for input t")
    }else stop("\n'latinsquare()' requires an possitive integer number for input t")
  }else if (!is.null(reps) && !is.null(data)) {
    if(!is.data.frame(data)) stop("Data must be a data frame.")
    data <- as.data.frame(na.omit(data[,1:3]))
    colnames(data) <- c("Row", "Column", "Treatment")
    Row <- as.vector(na.omit(data$Row))
    Column <- as.vector(na.omit(data$Column))
    Treatment <- as.vector(na.omit(data$Treatment))
    Row.f <- factor(Row, as.character(unique(Row)))
    Column.f <- factor(Column, as.character(unique(Column)))
    Treatment.f <- factor(Treatment, as.character(unique(Treatment)))
    n.rows <- length(levels(Row.f))
    n.cols <- length(levels(Column.f))
    n.treatments <- length(levels(Treatment.f))
    if (any(c(n.rows, n.cols, n.treatments) != n.rows)) stop("\n'latinsquare()' requires a balanced data as input!")
    Name.Rows <- as.character(Row.f)
    Name.Columns <- as.character(Column.f)
    Name.Treatments <- as.character(Treatment.f)
    ls.len <- n.treatments
    if (ls.len > 10) stop("\n'latinsquare()' allows only up to 10 treatments.")
  }
  if(!is.null(l) && is.numeric(l) && length(l) == 1) {
    if (l > 1 && is.null(locationNames)) {
      locationNames <- 1:l
    }else if (l > 1 && !is.null(locationNames)) {
      if (length(locationNames) < l) locationNames <- 1:l
    }
    if (length(plotNumber) < l || is.null(plotNumber)) plotNumber <- seq(1001, 1000*(l+1), 1000)
  }else stop("\n'latinsquare()' requires a integer for number of locations!")
  plot.numbs <- seriePlot.numbers(plot.number = plotNumber, reps = reps, l = l, t = ls.len*ls.len)
  if (!is.null(locationNames) && length(locationNames) == l) {
    locs <- locationNames
  }else locs <- 1:l
  step.random <- vector(mode = "list", length = reps)
  lsd.reps <- vector(mode = "list", length = reps)
  out.ls <- vector(mode = "list", length = reps)
  plotSquares <- setNames(vector(mode = "list", length = reps),
                          paste0("rep", seq(1:reps))) # set names
  x <- seq(1, reps * l, reps)
  y <- seq(reps, reps * l, reps)
  for (j in 1:reps) {
    D <- plot.numbs[[l]]
    P <- matrix(data = D[j]:(D[j] + (ls.len*ls.len) - 1), nrow = ls.len, ncol = ls.len,
                byrow = TRUE)
    if(planter == "serpentine") P <- serpentinelayout(P, opt = 2)
    plotSquares[[j]] <- P
    ls.random <- lsq(len = ls.len, reps = 1, seed = NA)
    #get random rows order
    ls.random.r <- ls.random
    row.random <- sample(1:ls.len)
    ls.random.r[,] <- ls.random.r[row.random,]
    rownames(ls.random.r) <- Name.Rows[row.random]
    ls.random.c <- ls.random.r
    #get random columns order
    col.random <- sample(1:ls.len)
    ls.random.c[,] <- ls.random.c[,col.random]
    colnames(ls.random.c) <- Name.Columns[col.random]
    expt.ls <- ls.random.c
    #randomize treatments to the letters
    trt <- Name.Treatments
    trt.r <- sample(trt)
    trt.random <- matrix(c(LETTERS[1:ls.len], trt.r), nrow = 2, ncol = ls.len, byrow = TRUE)
    w <- 1
    for (i in LETTERS[1:ls.len]) {
      expt.ls[expt.ls == i] <- trt.r[w]
      w <- w + 1
    }
    new_expt.ls <- order_ls(S = expt.ls, data = data)
    lsd.reps[[j]] <- new_expt.ls
    step.random[[j]] <- list(ls.random, ls.random.r, ls.random.c)
    Row <- rep(rownames(lsd.reps[[j]]), each = ls.len)
    Column <- rep(colnames(lsd.reps[[j]]), times = ls.len)
    out.ls[[j]] <- data.frame(list(LOCATION = locs[l],
                                   PLOT = as.vector(t(P)),
                                   SQUARE = j,
                                   ROW = Row,
                                   COLUMN = Column,
                                   TREATMENT = as.vector(t(new_expt.ls))))
  }
  expt.ls <- paste_by_row(lsd.reps)
  latinsquare.expt <- paste_by_row(out.ls)
  ls.output <- latinsquare.expt
  lsd.reps <- setNames(lsd.reps, paste0("rep", seq(1:reps))) # set names
  ls.output$ROW <- factor(ls.output$ROW, levels = Name.Rows)
  ls.output$COLUMN <- factor(ls.output$COLUMN, levels = Name.Columns)
  ls.output.order <- ls.output[order(ls.output$PLOT, ls.output$SQUARE, ls.output$ROW), ]
  if (!is.null(locationNames) && length(locationNames) == l) {
    ls.output.order$LOCATION <- rep(locationNames, each = (ls.len * ls.len) * reps)
  }
  rownames(ls.output.order) <- 1:nrow(ls.output.order)
  latin_design <- cbind(ID = 1:nrow(ls.output.order), ls.output.order)
  lsd.reps <- setNames(lsd.reps, paste0("rep", seq(1:reps))) # set names
  parameters <- list(
    numberofTreatments = length(unique(ls.output$ROW)),
    numberofRowns = length(unique(ls.output$ROW)),
    numberofColumns =  length(unique(ls.output$COLUMN)),
    rep = reps,
    locationName =  locationNames,
    seed =  seed
  )
  output <- list(infoDesign =  parameters, squares = lsd.reps,
                 plotSquares = plotSquares, fieldBook = latin_design)
  class(output) <- "FielDHub"
  return(invisible(output))
}

#-----------------------------------------------------------------------
# Print
#-----------------------------------------------------------------------
#' @rdname print.latin_square
#' @method print latin_square
#' @title Print an \code{FielDHub} object
#' @usage \method{print}{latin_square}(x, n, ...)
#' @aliases print.latin_square
#' @description Prints information about latin square design
#' @return an object inheriting from class \code{FielDHub}
#' @param x an object inheriting from class
#' @param n a single integer. If positive or zero, size for the
#'   resulting object: number of elements for a vector (including
#'   lists), rows for a matrix or data frame or lines for a function. If
#'   negative, all but the n last/first number of elements of x.
#'
#' @param ... further arguments passed to \code{\link{head}}.
#' @author Thiago de Paula Oliveira,
#'   \email{thiago.paula.oliveira@@alumni.usp.br}
#' @importFrom utils head str
#' @examples
#' # Example 1: Generates a latin square design with 4 treatments and 2 reps.
#' latinSq1 <- latin_square(t = 4,
#'                          reps = 2,
#'                          plotNumber = 101,
#'                          planter = "cartesian",
#'                          seed = 1980)
#' print(latinSq1)
#'
#' @export

print.latin_square <- function(x, n=10, ...){
  #---------------------------------------------------------------------
  # Parameters
  #---------------------------------------------------------------------
  cat("Latin square design:", "\n\n")
  cat("Information on the design parameters:", "\n")
  str(x$infoDesign)
  #---------------------------------------------------------------------
  # Head
  #---------------------------------------------------------------------
  nr <- nrow(x$fieldBook)
  nhead <- min(n, nr)
  if (n < 0) {
    nhead_print <- nr + n
  }else {
    nhead_print <- nhead
  }
  cat("\n",  nhead_print,
      "First observations of the data frame with the latin_square field book:",
      "\n")
  print(head(x$fieldBook, n=nhead, ...))
}

#-----------------------------------------------------------------------
# Summary
#-----------------------------------------------------------------------
#' @rdname summary.latin_square
#' @method summary latin_square
#' @title Summary an \code{latin_square} object
#' @usage \method{summary}{latin_square}(object, ...)
#' @aliases summary.latin_square
#' @description Summarise information on the design parameters, and data
#'   frame structure
#' @return an object inheriting from class \code{summary.latin_square}
#' @param object an object inheriting from class
#'   \code{FielDHub}
#'
#' @param ... Unused, for extensibility
#' @author Thiago de Paula Oliveira,
#'   \email{thiago.paula.oliveira@@alumni.usp.br}
#'
#' @examples
#' # Example 1: Generates a latin square design with 4 treatments and 2 reps.
#' latinSq1 <- latin_square(t = 4,
#'                          reps = 2,
#'                          plotNumber = 101,
#'                          planter = "cartesian",
#'                          seed = 1980)
#' summary(latinSq1)
#'
#'
#' @export
summary.latin_square <- function(object, ...) {
  structure(object, oClass=class(object),
            class = "summary.latin_square")
}

#-----------------------------------------------------------------------
# Print summary
#-----------------------------------------------------------------------
#' @rdname print.summary.latin_square
#' @method summary latin_square
#' @title Print the summary of an \code{FielDHub} object
#' @usage \method{print}{summary.latin_square}(x, ...)
#' @aliases print.summary.latin_square
#' @description Print summary information on the design parameters, and
#'   data frame structure
#' @return an object inheriting from class \code{FielDHub}
#' @param x an object inheriting from class \code{FielDHub}
#'
#' @param ... Unused, for extensibility
#' @author Thiago de Paula Oliveira,
#'   \email{thiago.paula.oliveira@@alumni.usp.br}
#' @importFrom utils str
#' @importFrom dplyr glimpse
#' @export
print.summary.latin_square <- function(x, ...){
  cat("Latin square design:", "\n\n")
  #---------------------------------------------------------------------
  cat("1. Information on the design parameters:", "\n")
  str(x$infoDesign)
  cat("\n")
  #---------------------------------------------------------------------
  cat("2. Squares:", "\n")
  print(x$squares)
  cat("\n")
  #---------------------------------------------------------------------
  cat("3. Plot squares:", "\n")
  print(x$plotSquares)
  cat("\n")
  #---------------------------------------------------------------------
  cat("4. Strcuture of the data frame with the latin_square field book:", "\n\n")
  dplyr::glimpse(x$fieldBook)
  #---------------------------------------------------------------------
}
