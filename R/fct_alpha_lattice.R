#' Generates an Alpha Design
#' 
#' 
#' @description  Randomly generates an alpha design like \code{alpha(0,1)} across multiple locations.
#' 
#'
#' @param t Number of treatments, or a character vector with the treatment labels.
#' @param r Deprecated alias for \code{reps}; positional calls remain supported.
#' @param reps Number of full resolvable replicates per location.
#' @param k Size of incomplete blocks (number of units per incomplete block). 
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param locationNames (optional) String with names for each of the \code{l} locations.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param data (optional) Data frame with label list of treatments.
#' 
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#' 
#' 
#' @importFrom stats runif na.omit
#' 
#' 
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the alpha design field book.
#' }
#'
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
#' unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#'
#'
#' @examples
#' # Example 1: Generates an alpha design with 4 full blocks and 15 treatments.
#' # Size of IBlocks k = 3.
#' alphalattice1 <- alpha_lattice(t = 15, 
#'                                k = 3, 
#'                                reps = 4,
#'                                l = 1, 
#'                                plotNumber = 101, 
#'                                locationNames = "GreenHouse", 
#'                                seed = 1247)
#' alphalattice1$infoDesign
#' head(alphalattice1$fieldBook, 10)
#' 
#' # Example 2: Generates an alpha design with 3 full blocks and 25 treatment.
#' # Size of IBlocks k = 5. 
#' # In this case, we show how to use the option data.
#' treatments <- paste("G-", 1:25, sep = "")
#' ENTRY <- 1:25
#' treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
#' head(treatment_list) 
#' alphalattice2 <- alpha_lattice(t = 25,
#'                                k = 5,
#'                                reps = 3,
#'                                l = 1, 
#'                                plotNumber = 1001, 
#'                                locationNames = "A", 
#'                                seed = 1945,
#'                                data = treatment_list)
#' alphalattice2$infoDesign
#' head(alphalattice2$fieldBook, 10)
#' 
#' @section Reproducibility:
#' The result records effective inputs and the resolved seed in
#' \code{metadata$parameters}, using \code{reps} for replication. Under the
#' same package versions and RNG settings, rebuild a result \code{x} with
#' \code{do.call(alpha_lattice, x$metadata$parameters)}.
#'
#' @export
alpha_lattice <- function(t = NULL, 
                          k = NULL, 
                          r = NULL, 
                          l = 1, 
                          plotNumber = 101, 
                          locationNames = NULL,
                          seed = NULL, 
                          data = NULL, reps = NULL) {
  validate_locations(l)
  r <- resolve_argument_alias(
    reps, r, new = "reps", old = "r",
    new_supplied = !missing(reps), old_supplied = !missing(r)
  )
  seed <- resolve_seed(seed, default = function() runif(1, min = 0, max = 10000))
  local_design_seed(seed)
  treatment_count <- validate_block_design_inputs(t, k, r, l, data)
  lookup <- FALSE
  if(is.null(data)) {
    nt <- treatment_count
    df <- data.frame(list(ENTRY = 1:nt,
                          TREATMENT = treatment_labels(t, nt, "alpha_lattice")))
    data_alpha <- df
  } else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      fieldhub_abort('Basic design parameters missing (t, k, r or l).')
    }
    if(!is.data.frame(data)) fieldhub_abort("Data must be a data frame.")
    if (ncol(data) < 2) fieldhub_abort("Data input needs at least two columns with: ENTRY and NAME.")
    data_up <- as.data.frame(data[,c(1,2)])
    data_up <- na.omit(data_up)
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    data_up$TREATMENT <- as.character(data_up$TREATMENT)
    new_t <- length(data_up$TREATMENT)
    if (t != new_t) fieldhub_abort("Number of treatments do not match with data input.")
    TRT <- data_up$TREATMENT
    nt <- length(TRT)
    if (nt != t) fieldhub_abort('Number of treatment do not match with data input')
    data_alpha <- data_up
  }
  if (k >= nt) fieldhub_abort('incomplete_blocks() requires that k < t.')
  validate_location_labels(locationNames, l)
  if (!is.null(locationNames)) locationNames <- toupper(locationNames)
  validate_location_labels(locationNames, l)
  recorded_locations <- locationNames
  if(is.null(locationNames) || length(locationNames) != l) {
    if (!is.null(locationNames)) warn_default_location_names(locationNames, l, 1:l)
    locationNames <- 1:l
    recorded_locations <- NULL
  }
  if (is_prime(nt)) fieldhub_abort('Combinations for this amount of treatments do not exist.')
  s <- nt / k
  if (s %% 1 != 0) fieldhub_abort('Combinations for this amount of treatments do not exist.')
  
  nunits <- k
  matdf <- incomplete_blocks(t = nt, k = nunits, reps = r, l = l, plotNumber = plotNumber,
                             seed = seed, locationNames = locationNames,
                             data = data_alpha)
  blocksModel <- matdf$blocksModel
  lambda <- r*(k - 1)/(nt - 1)
  matdf <- matdf$fieldBook
  OutAlpha <- as.data.frame(matdf)
  OutAlpha$LOCATION <- factor(OutAlpha$LOCATION, levels = locationNames)
  rownames(OutAlpha) <- 1:nrow(OutAlpha)
  infoDesign <- list(Reps = r, iBlocks = s, NumberTreatments = nt, NumberLocations = l, 
                     Locations = locationNames, seed = seed, lambda = lambda,
                     id_design = 12)
  output <- list(infoDesign = infoDesign, fieldBook = OutAlpha, blocksModel = blocksModel)
  reproduction_parameters <- record_design_parameters(
    environment(), overrides = list(reps = r, locationNames = recorded_locations), exclude = "r"
  )
  output <- new_fieldhub_design(output, "alpha_lattice", parameters = reproduction_parameters)
  return(invisible(output))
}
