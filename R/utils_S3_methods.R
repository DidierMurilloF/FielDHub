#-----------------------------------------------------------------------
# Print
#-----------------------------------------------------------------------
#' @rdname print.FielDHub
#' @method print FielDHub
#' @title Print a \code{FielDHub} object
#' @usage \method{print}{FielDHub}(x, n, ...)
#' @aliases print.FielDHub
#' @description Prints the design parameters and the first rows of the field
#'   book of any \code{FielDHub} design, including results saved by earlier
#'   versions of FielDHub.
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
  # Each design has its own method. Results saved by FielDHub 1.5 or earlier
  # have the class "FielDHub" only, so they get the class of their design.
  design <- with_design_class(x)
  if (identical(class(design), class(x))) return(NextMethod())
  print(design, n = n, ...)
  invisible(x)
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
  object <- with_design_class(object)
  structure(object, oClass=class(object),
            class = unique(c(paste0("summary.", class(object)[1]), "summary.FielDHub")))
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
  # Each design has its own method; this one shows results of other designs
  NextMethod()
  invisible(x)
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
#'   It has no effect on split-plot and split-split-plot designs in
#'   complete blocks (\code{type = 2}), whose whole plots are numbered in a
#'   fixed order.
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
    if (x$infoDesign$id_design == 17) {
      stop("split_families() results have no field layout to plot.", call. = FALSE)
    }
    p <- plot_layout(x = x, ...)
    if (is.null(p)) {
      # plot_layout() warns which layouts or locations are available
      fieldhub_abort("The layout or location requested is not available for this design.")
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