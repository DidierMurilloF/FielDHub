% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_square_lattice.R
\name{square_lattice}
\alias{square_lattice}
\title{Generates a Square Lattice Design.}
\usage{
square_lattice(
  t = NULL,
  k = NULL,
  r = NULL,
  l = 1,
  plotNumber = 101,
  locationNames = NULL,
  seed = NULL,
  data = NULL
)
}
\arguments{
\item{t}{Number of  treatments.}

\item{k}{Size of incomplete blocks (number of units per incomplete block).}

\item{r}{Number of blocks (full resolvable replicates).}

\item{l}{Number of locations. By default \code{l = 1}.}

\item{plotNumber}{Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.}

\item{locationNames}{(optional) Names for each location.}

\item{seed}{(optional) Real number that specifies the starting seed to obtain reproducible designs.}

\item{data}{(optional) Data frame with label list of treatments.}
}
\value{
A list with two elements.
\itemize{
  \item \code{infoDesign} is a list with information on the design parameters.
  \item \code{fieldBook} is a data frame with the square lattice design field book.
}
}
\description{
It randomly generates a square lattice design across locations.
}
\examples{
# Example 1: Generates a square lattice design with 5 full blocks, 8 units per IBlock,
# 8 IBlocks for a square number of treatmens of 64 in two locations.
squareLattice1 <- square_lattice(t = 64, k = 8, r = 5, l = 2, 
                                 plotNumber = c(1001, 2001),
                                 locationNames = c("FARGO", "MINOT"), 
                                 seed = 1986)
squareLattice1$infoDesign
head(squareLattice1$fieldBook,12)

# Example 2: Generates a square lattice design with 3 full blocks, 7 units per IBlock,
# 7 IBlocks for a square number of treatmens of 49 in one location.
# In this case, we show how to use the option data.
treatments <- paste("G", 1:49, sep = "")
ENTRY <- 1:49
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list) 
squareLattice2 <- square_lattice(t = 49, k = 7, r = 3, l = 1, 
                                 plotNumber = 1001,
                                 locationNames = "CASSELTON", 
                                 seed = 1986,
                                 data = treatment_list)
squareLattice2$infoDesign
head(squareLattice2$fieldBook,12)

}
\references{
Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
}
\author{
Didier Murillo [aut],
        Salvador Gezan [aut],
        Ana Heilman [ctb],
        Thomas Walk [ctb], 
        Johan Aparicio [ctb], 
        Richard Horsley [ctb]
}
