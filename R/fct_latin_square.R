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
                          paste0("rep", seq(1:reps)))
  x <- seq(1, reps * l, reps)
  y <- seq(reps, reps * l, reps)
  for (j in 1:reps) {
    D <- plot.numbs[[l]]
    P <- matrix(data = D[j]:(D[j] + (ls.len*ls.len) - 1), nrow = ls.len, ncol = ls.len,
                byrow = TRUE)
    # plot_matrix <- apply(P, 2, rev)
    plot_matrix <- P
    if(planter == "serpentine") plot_matrix <- serpentinelayout(plot_matrix, opt = 2)
    # print(plot_matrix)
    # print(as.vector(t(plot_matrix)))
    plotSquares[[j]] <- plot_matrix
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
    # print(new_expt.ls)
    # print(as.vector(t(new_expt.ls)))
    lsd.reps[[j]] <- new_expt.ls
    step.random[[j]] <- list(ls.random, ls.random.r, ls.random.c)
    Row <- rep(rownames(lsd.reps[[j]]), each = ls.len)
    Column <- rep(colnames(lsd.reps[[j]]), times = ls.len)
    out.ls[[j]] <- data.frame(list(LOCATION = locs[l],
                                   PLOT = as.vector(t(plot_matrix)),
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
  #ls.output.order <- ls.output[order(ls.output$PLOT, ls.output$SQUARE, ls.output$ROW), ]
  ls.output.order <- ls.output[order(ls.output$SQUARE, ls.output$ROW), ]
  if (!is.null(locationNames) & length(locationNames) == l) {
    ls.output.order$LOCATION <- rep(locationNames, each = (ls.len * ls.len) * reps)
  }
  rownames(ls.output.order) <- 1:nrow(ls.output.order)
  latin_design <- cbind(ID = 1:nrow(ls.output.order), ls.output.order)
  lsd.reps <- setNames(lsd.reps, paste0("rep", seq(1:reps))) # set names
  parameters <- list(
    treatments = length(unique(ls.output$ROW)),
    squares = reps,
    locationName =  locationNames,
    seed =  seed,
    id_design = 3
  )
  output <- list(infoDesign =  parameters, squares = lsd.reps,
                 plotSquares = plotSquares, fieldBook = latin_design)
  class(output) <- "FielDHub"
  return(invisible(output))
}

#' @noRd 
lsq <- function(len, reps = 1, seed = NA) {
  
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }
  allsq <- matrix(nrow = reps*len, ncol = len)
  #if (returnstrings) { squareid <- vector(mode = "character", length = reps) }
  sample1 <- function(x) {
    if (length(x)==1) { return(x) }
    else              { return(sample(x,1)) }
  }
  for (n in 1:reps) {
    sq <- matrix(nrow=len, ncol=len) 
    while (any(is.na(sq))) {
      k <- sample1(which(is.na(sq)))
      i <- (k-1) %% len + 1       
      j <- floor((k-1) / len) + 1 
      sqrow <- sq[i,]
      sqcol <- sq[,j]
      openCell <- rbind(cbind(which(is.na(sqcol)), j),
                        cbind(i, which(is.na(sqrow))))
      openCell <- openCell[sample(nrow(openCell)),]
      openCell <- rbind(c(i,j), openCell)
      openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)
      for (c in 1:nrow(openCell)) {
        ci <- openCell[c,1]
        cj <- openCell[c,2]
        freeNum <- which(!(1:len %in% c(sq[ci,], sq[,cj])))
        if (length(freeNum)>0) { sq[ci,cj] <- sample1(freeNum) }
        else {
          sq <- matrix(nrow=len, ncol=len)
          break;
        }
      }
    }
    allsqrows <- ((n-1)*len) + 1:len
    allsq[allsqrows,] <- sq
  }
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }
  #put LETTERS
  ls4.random <- allsq
  z <- 1
  for (w in 1:len){
    ls4.random[ls4.random == w] <- LETTERS[z]
    z <- z + 1
  }
  colnames(ls4.random) <- paste(rep("Column", len), 1:len)
  rownames(ls4.random) <- paste(rep("Row", len), 1:len)
  
  return(ls4.random)
}