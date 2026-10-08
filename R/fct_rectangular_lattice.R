#' Generates a Rectangular Lattice Design.
#
#' @description It randomly generates a rectangular lattice design across locations.
#'
#' @param t Number of treatments, or a character vector with the treatment labels.
#' @param r Deprecated alias for \code{reps}; positional calls remain supported.
#' @param reps Number of full resolvable replicates per location.
#' @param k Size of incomplete blocks (number of units per incomplete block). 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param data (optional) Data frame with label list of treatments.
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
#' 
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the rectangular lattice design field book.
#' }
#' 
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
#' unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#' 
#' @examples
#' # Example 1: Generates a rectangular lattice design with 6 full blocks, 4 units per IBlock (k)
#' # and 20 treatments in one location.
#' rectangularLattice1 <- rectangular_lattice(t = 20, k = 4, reps = 6, l = 1,
#'                                            plotNumber = 101,
#'                                            locationNames = "FARGO", 
#'                                            seed = 126)
#' rectangularLattice1$infoDesign
#' head(rectangularLattice1$fieldBook,12)
#' 
#' # Example 2: Generates a rectangular lattice design with 5 full blocks, 7 units per IBlock (k)
#' # and 56 treatments across 2 locations.
#' # In this case, we show how to use the option data.
#' treatments <- paste("ND-", 1:56, sep = "")
#' ENTRY <- 1:56
#' treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
#' head(treatment_list) 
#' rectangularLattice2 <- rectangular_lattice(t = 56, k = 7, reps = 5, l = 2,
#'                                            plotNumber = c(1001,2001),
#'                                            locationNames = c("Loc1", "Loc2"), 
#'                                            seed = 127,
#'                                            data = treatment_list)
#' rectangularLattice2$infoDesign
#' head(rectangularLattice2$fieldBook,12)
#' 
#' @section Reproducibility:
#' The result records effective inputs and the resolved seed in
#' \code{metadata$parameters}, using \code{reps} for replication. Under the
#' same package versions and RNG settings, rebuild a result \code{x} with
#' \code{do.call(rectangular_lattice, x$metadata$parameters)}.
#'
#' @export
rectangular_lattice <- function(t = NULL, k = NULL, r = NULL, l = 1, plotNumber = 101, locationNames = NULL,
                                seed = NULL, data = NULL, reps = NULL) {
  plotNumber_supplied <- !missing(plotNumber)
  validate_locations(l)
  r <- resolve_argument_alias(
    reps, r, new = "reps", old = "r",
    new_supplied = !missing(reps), old_supplied = !missing(r)
  )
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  treatment_count <- validate_block_design_inputs(t, k, r, l, data)
  lookup <- FALSE
  if(is.null(data)) {
    nt <- treatment_count
    df <- data.frame(list(ENTRY = 1:nt,
                          TREATMENT = treatment_labels(t, nt, "rectangular_lattice")))
    data_alpha <- df
  }else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      fieldhub_abort('Some of the basic design parameters are missing (t, k, r).')
    }
    if(!is.data.frame(data)) fieldhub_abort("Data must be a data frame.")
    data_up <- as.data.frame(data[,c(1,2)])
    data_up <- na.omit(data_up)
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    data_up$TREATMENT <- as.character(data_up$TREATMENT)
    new_t <- length(data_up$TREATMENT)
    if (t != new_t) fieldhub_abort("Number of treatments do not match with data input.")
    TRT <- data_up$TREATMENT
    nt <- length(TRT)
    data_alpha <- data_up
  }
  validate_location_labels(locationNames, l)
  if (!is.null(locationNames)) locationNames <- toupper(locationNames)
  validate_location_labels(locationNames, l)
  s <- nt / k
  if (s %% 1 != 0 || k != (s - 1) || nt != s*(s - 1)) {
    fieldhub_abort('rectangular_lattice() requires t = s*(s-1), where s is the iBlock numbers per replicate.')
  } 
  recorded_locations <- locationNames
  if(is.null(locationNames) || length(locationNames) != l) {
    if (!is.null(locationNames)) warn_default_location_names(locationNames, l, 1:l)
    locationNames <- 1:l
    recorded_locations <- NULL
  }
  nunits <- k
  matdf <- build_incomplete_blocks(t = nt, k = nunits, reps = r, l = l, plotNumber = plotNumber,
                                   seed = seed, locationNames = locationNames,
                                   data = data_alpha, caller = "rectangular_lattice",
                                   plotNumber_supplied = plotNumber_supplied)
  blocksModel <- matdf$blocksModel
  # build_incomplete_blocks() already recorded the effective per-location
  # starts it used (raw plotNumber pre-normalized); record the same value
  # here so reproduce_design() replays silently and identically.
  plotNumber <- matdf$metadata$parameters$plotNumber
  lambda <- r*(k - 1)/(nt - 1)
  matdf <- matdf$fieldBook
  OutRectagular_Lattice <- as.data.frame(matdf)
  OutRectagular_Lattice$LOCATION <- factor(OutRectagular_Lattice$LOCATION, levels = locationNames)
  rownames(OutRectagular_Lattice) <- 1:nrow(OutRectagular_Lattice)

  infoDesign <- list(Reps = r, iBlocks = s, NumberTreatments = nt, NumberLocations = l, 
                     Locations = locationNames, seed = seed, lambda = lambda,
                     id_design = 11)
  output <- list(infoDesign = infoDesign, fieldBook = OutRectagular_Lattice, blocksModel = blocksModel)
  reproduction_parameters <- record_design_parameters(
    environment(), overrides = list(reps = r, locationNames = recorded_locations), exclude = "r"
  )
  output <- new_fieldhub_design(output, "rectangular_lattice", parameters = reproduction_parameters)
  return(invisible(output))
}
