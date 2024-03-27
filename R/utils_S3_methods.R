#-----------------------------------------------------------------------
# Print
#-----------------------------------------------------------------------
#' @rdname print.FielDHub
#' @method print FielDHub
#' @title Print a \code{FielDHub} object
#' @usage \method{print}{FielDHub}(x, n, ...)
#' @aliases print.FielDHub
#' @description Prints information about any \code{FielDHub} function.
#' @return an object inheriting from class \code{FielDHub}
#' @param x an object inheriting from class
#' @param n a single integer. If positive or zero, size for the
#'   resulting object: number of elements for a vector (including
#'   lists), rows for a matrix or data frame or lines for a function. If
#'   negative, all but the n last/first number of elements of x.
#'
#' @param ... further arguments passed to \code{\link{head}}.
#' @author Thiago de Paula Oliveira,
#'   \email{thiago.paula.oliveira@@alumni.usp.br} [aut],
#'   Didier Murillo [aut]
#' @importFrom utils head str
#' @examples
#' # Example 1: Generates a CRD design with 5 treatments and 5 reps each.
#' crd1 <- CRD(t = 5, reps = 5, plotNumber = 101,
#' seed = 1985, locationName = "Fargo")
#' crd1$infoDesign
#' print(crd1)
#'
#' @export
print.FielDHub <- function(x, n=10, ...){
  #---------------------------------------------------------------------
  # Parameters
  #---------------------------------------------------------------------
  if (x$infoDesign$id_design == 1) {
    cat("Completely Randomized Design (CRD)", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the CRD field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 2){
    cat("Randomized Complete Block Design (RCBD):", "\n\n")
    #-----------------------------------------------------------
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the RCBD field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 3){
    cat("Latin Square Design:", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the latin_square field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 4) {
    cat("Full Factorial Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the full_factorial field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 5) {
    cat("Split Plot Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the split_plot field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 6) {
    cat("Split-Split Plot Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the split_split_plot field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 7) {
    cat("Strip Plot Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the strip_plot field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 8) {
    cat("Incomplete Blocks Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the incomplete_blocks field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 9) {
    cat("Row Column Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the row_column field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 10) {
    cat("Square Lattice Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the square_lattice field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 11) {
    cat("Rectangular Lattice Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the rectangular_lattice field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  } else if (x$infoDesign$id_design == 12) {
    cat("Alpha Lattice Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the alpha_lattice field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  } else if (x$infoDesign$id_design == 13) {
    cat("Partially Replicated Design", "\n\n")
    cat("\n", "Replications within location:", "\n")
    print(x$reps_info)
    cat("\n", "Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the partially_replicated field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  } else if (x$infoDesign$id_design == "MultiPrep") {
    cat("Multi-Location Partially Replicated Design", "\n")
    cat("\n", "Replications within location:", "\n")
    print(x$reps_info)
    cat("\n", "Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the partially_replicated field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  } else if (x$infoDesign$id_design == 14) {
    cat("Augmented Randomized Complete Block Design:", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the RCBD_augmented field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 15) {
    cat("Un-replicated Diagonal Arrangement Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the diagonal_arrangement field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  } else if (x$infoDesign$id_design == "Sparse") {
    cat("Sparse Allocation: Un-replicated Diagonal Arrangement Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the diagonal_arrangement field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  } else if (x$infoDesign$id_design == 16) {
    cat("Un-replicated Optimized Arrangement Design", "\n\n")
    cat("Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    # str(x$infoDesign)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$fieldBook)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the optimized_arrangement field book:",
        "\n")
    print(head(x$fieldBook, n=nhead_print, ...))
  }else if (x$infoDesign$id_design == 17) {
    cat("Split families:", "\n\n")
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    cat("\n", "Data frame with the summary of cases by location:",
        "\n")
    print(x$rowsEachlist)
    #---------------------------------------------------------------------
    # Head
    #---------------------------------------------------------------------
    nr <- nrow(x$data_locations)
    nhead_print <- infoPrint(n, nr)
    cat("\n",  nhead_print,
        "First observations of the data frame with the entries for each location:",
        "\n")
    print(head(x$data_locations, n=nhead_print, ...))
  }
}
#-----------------------------------------------------------------------
# Summary
#-----------------------------------------------------------------------
#' @rdname summary.FielDHub
#' @method summary FielDHub
#' @title Summary a \code{FielDHub} object
#' @usage \method{summary}{FielDHub}(object, ...)
#' @aliases summary.FielDHub
#' @description Summarise information on the design parameters, and data
#'   frame structure
#' @return an object inheriting from class \code{summary.FielDHub}
#' @param object an object inheriting from class
#'   \code{FielDHub}
#'
#' @param ... Unused, for extensibility
#' @author Thiago de Paula Oliveira,
#'   \email{thiago.paula.oliveira@@alumni.usp.br}
#'
#' @examples
#' # Example 1: Generates a CRD design with 5 treatments and 5 reps each.
#' crd1 <- CRD(t = 5, reps = 5, plotNumber = 101,
#' seed = 1985, locationName = "Fargo")
#' crd1$infoDesign
#' summary(crd1)
#'
#' @export
summary.FielDHub <- function(object, ...) {
  structure(object, oClass=class(object),
            class = "summary.FielDHub")
}

#-----------------------------------------------------------------------
# Print summary
#-----------------------------------------------------------------------
#' @rdname print.summary.FielDHub
#' @method print summary.FielDHub
#' @title Print the summary of a \code{FielDHub} object
#' @usage \method{print}{summary.FielDHub}(x, ...)
#' @aliases print.summary.FielDHub
#' @description Print summary information on the design parameters, and
#'   data frame structure
#' @return an object inheriting from class \code{FielDHub}
#' @param x an object inheriting from class \code{FielDHub}
#'
#' @param ... Unused, for extensibility
#' @author Thiago de Paula Oliveira,
#'   \email{thiago.paula.oliveira@@alumni.usp.br} [aut],
#'   Didier Murillo [aut]
#' @importFrom utils str
#' @importFrom dplyr glimpse
#' @export
print.summary.FielDHub <- function(x, ...) {
  if (x$infoDesign$id_design == 1) {
    cat("Completely Randomized Design (CRD):", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the CRD field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 2) {
    cat("Randomized Complete Block Design (RCBD):", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Layout randomization for each location:", "\n")
    print(x$layoutRandom)
    cat("\n")
    #---------------------------------------------------------------------
    cat("3. Plot numbers layout:", "\n")
    print(x$plotNumber)
    cat("\n")
    #---------------------------------------------------------------------
    cat("4. Structure of the data frame with the RCBD field book:", "\n\n")
    str(x$fieldBook)
    #---------------------------------------------------------------------
  }else if (x$infoDesign$id_design == 3) {
    cat("Latin Square Design:", "\n\n")
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
    cat("4. Structure of the data frame with the latin_square field book:", "\n\n")
    str(x$fieldBook)
    #---------------------------------------------------------------------
  }else if (x$infoDesign$id_design == 4) {
    cat("Full Factorial Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the full_factorial field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 5) {
    cat("Split Plot Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    
    cat("2. Layout randomization for each location:", "\n")
    print(x$layoutlocations)
    cat("\n")
    #---------------------------------------------------------------------
    cat("3. Structure of the data frame with the split_plot field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 6) {
    cat("Split-Split Plot Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the split_split_plot field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 7) {
    cat("Strip Plot Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #----------------------------------------------------------------------
    cat("2. Layout randomization for each location:", "\n")
    print(x$stripsBlockLoc)
    cat("\n")
    #----------------------------------------------------------------------
    cat("3. Plot number layout for each location:", "\n")
    print(x$plotLayouts)
    cat("\n")
    #---------------------------------------------------------------------
    cat("4. Structure of the data frame with the strip_plot field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 8) {
    cat("Incomplete Blocks Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the incomplete_blocks field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 9) {
    cat("Row Column Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #----------------------------------------------------------------------
    cat("2. Resolvable row column blocks", "\n")
    print(x$resolvableBlocks)
    cat("\n")
    #----------------------------------------------------------------------
    cat("3. Concurrence matrix:", "\n")
    print(x$concurrence)
    cat("\n")
    #---------------------------------------------------------------------
    cat("4. Structure of the data frame with the row_column field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 10) {
    cat("Square Lattice:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the square_lattice field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 11) {
    cat("Rectangular Lattice Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the rectangular_lattice field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 12) {
    cat("Alpha Lattice Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the alpha_lattice field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 13) {
    cat("Partially Replicated Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #----------------------------------------------------------------------
    cat("2. Layout randomization:", "\n")
    print(x$layoutRandom)
    cat("\n")
    #----------------------------------------------------------------------
    cat("3. Plot number layout:", "\n")
    print(x$plotNumber)
    cat("\n")
    #---------------------------------------------------------------------
    cat("4. Structure of the data frame with the data input:", "\n\n")
    str(x$data_entry)
    #---------------------------------------------------------------------
    cat("5. Structure of the data frame with the partially_replicated field book:", "\n\n")
    str(x$fieldBook)
  } else if (x$infoDesign$id_design == 14) {
    cat("Augmented Randomized Complete Block Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #----------------------------------------------------------------------
    cat("2. Layout randomization:", "\n")
    print(x$layoutRandom)
    cat("\n")
    #----------------------------------------------------------------------
    cat("3. Plot number layout:", "\n")
    print(x$plotNumber)
    cat("\n")
    #----------------------------------------------------------------------
    cat("4. Experiments name layout:", "\n")
    print(x$exptNames)
    cat("\n")
    #---------------------------------------------------------------------
    cat("5. Structure of the data frame with the data input:", "\n\n")
    str(x$data_entry)
    #---------------------------------------------------------------------
    cat("6. Structure of the data frame with the RCBD_augmented field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 15) {
    cat("Un-replicated Diagonal Arrangement Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    #----------------------------------------------------------------------
    cat("2. Layout randomization:", "\n")
    print(x$layoutRandom)
    cat("\n")
    #----------------------------------------------------------------------
    cat("3. Plot number layout:", "\n")
    print(x$plotsNumber)
    cat("\n")
    #---------------------------------------------------------------------
    cat("4. Structure of the data frame with the data input:", "\n\n")
    str(x$data_entry)
    #---------------------------------------------------------------------
    cat("5. Structure of the data frame with the diagonal_arrangement field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 16) {
    cat("Un-replicated Optimized Arrangement Design:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Information on the design parameters:", "\n")
    len <- length(x$infoDesign)
    str(x$infoDesign[1:(len-1)])
    cat("\n")
    #----------------------------------------------------------------------
    cat("2. Layout randomization:", "\n")
    print(x$layoutRandom)
    cat("\n")
    #----------------------------------------------------------------------
    cat("3. Plot number layout:", "\n")
    print(x$plotNumber)
    cat("\n")
    #---------------------------------------------------------------------
    cat("4. Structure of the data frame with the data input:", "\n\n")
    str(x$data_entry)
    #---------------------------------------------------------------------
    cat("5. Structure of the data frame with the optimized_arrangement field book:", "\n\n")
    str(x$fieldBook)
  }else if (x$infoDesign$id_design == 17) {
    cat("Split families:", "\n\n")
    #---------------------------------------------------------------------
    cat("1. Structure of the data frame with the summary of entries by location:", "\n\n")
    str(x$rowsEachlist)
    #---------------------------------------------------------------------
    cat("2. Structure of the data frame with the entries for each location:", "\n\n")
    str(x$data_locations)
  }
}
#-----------------------------------------------------------------------
# Print plot
#-----------------------------------------------------------------------
#' @rdname print.fieldLayout
#' @method print fieldLayout
#' @title Print a \code{fieldLayout} plot object
#' @usage \method{print}{fieldLayout}(x, ...)
#' @aliases print.fieldLayout
#' @description Prints a plot object of class \code{fieldLayout}.
#' @return a plot object inheriting from class \code{fieldLayout}.
#' @param x a plot object inheriting from class fieldLayout.
#' @param ... unused, for extensibility.
#' @author Didier Murillo [aut]
#'
#' @export
print.fieldLayout <- function(x, ...) {
  if (!missing(x)) {
    if (is.null(x)) stop("x must be a fieldLayout object!")
    if (!inherits(x,"fieldLayout")) {
      stop("x must be a fieldLayout object!")
    }
    return(print(x$layout))
  } else stop("x is missing!")
}

#-----------------------------------------------------------------------
# Plot
#-----------------------------------------------------------------------
#' @rdname plot.FielDHub
#' @method plot FielDHub
#' @title Plot a \code{FielDHub} object
#' @usage \method{plot}{FielDHub}(x, ...)
#' @aliases plot.FielDHub
#' @description Draw a field layout plot for a \code{FielDHub} object.
#' @return 
#' \itemize{
#'   \item a plot object inheriting from class \code{fieldLayout}
#'   \item \code{field_book} a data frame with the fieldbook that includes the coordinates ROW and COLUMN.
#' } 
#' @param x a object inheriting from class \code{FielDHub}
#' @param ... further arguments passed to utility function \code{plot_layout()}.
#' \itemize{
#'   \item \code{layout} a integer. Options available depend on the 
#'   type of design and its characteristics
#'   \item \code{l} a integer to specify the location to plot.
#'   \item \code{planter} it can be \code{serpentine} or \code{cartesian}.
#'   \item \code{stacked} it can be \code{vertical} or \code{horizontal} stacked layout.
#' } 
#' @author Didier Murillo [aut]
#' @examples
#' \dontrun{
#' # Example 1: Plot a RCBD design with 24 treatments and 3 reps.
#' s <- RCBD(t = 24, reps = 3, plotNumber = 101, seed = 12)
#' plot(s)
#' }
#'
#' @export
plot.FielDHub <- function(x, ...) {
  if (!missing(x)) {
    if (is.null(x)) stop("x must be a FielDHub object!")
    if (!inherits(x,"FielDHub")) {
      stop("x is not a FielDHub class")
    }
    p <- plot_layout(x = x, ...)
    if (is.null(p)) {
      img <- ggplot2::ggplot() + ggplot2::theme_minimal()
      class(img) <- "fieldLayout"
      print(x = img)
    } else {
      out <- list(
        field_book = p$allSitesFieldbook,
        layout = p$out_layout
      )
      class(out) <- "fieldLayout"
      print(x = out)
      return(invisible(list(p = out$layout, field_book = out$field_book)))
    }
  } else stop("x is missing!")
}