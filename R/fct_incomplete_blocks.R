#' Generates a Resolvable Incomplete Block Design
#'
#' @description Randomly generates a resolvable incomplete block design (IBD) of characteristics (t, k, r).
#' The randomization can be done across locations.
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
#' @param caller Internal. Name of the function to name in error messages,
#'   used so that \code{alpha_lattice()}, \code{square_lattice()} and
#'   \code{rectangular_lattice()} (which build their design through
#'   \code{incomplete_blocks()}) can report failures under their own name
#'   instead of \code{incomplete_blocks()}. Not intended for direct use.
#'
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb],
#'         Thomas Walk [ctb], 
#'         Johan Aparicio [ctb], 
#'         Richard Horsley [ctb]
#'
#' @importFrom stats runif na.omit aggregate
#'
#'
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the incomplete block design field book.
#' }
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for factorial and
#' unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#'
#' @examples
#' # Example 1: Generates a resolvable IBD of characteristics (t,k,r) = (12,4,2).
#' # 1-resolvable IBDs
#' ibd1 <- incomplete_blocks(t = 12,
#'                           k = 4,
#'                           reps = 2,
#'                           seed = 1984)
#' ibd1$infoDesign
#' head(ibd1$fieldBook)
#'
#' # Example 2: Generates a balanced resolvable IBD of characteristics (t,k,r) = (15,3,7).
#' # In this case, we show how to use the option data.
#' treatments <- paste("TX-", 1:15, sep = "")
#' ENTRY <- 1:15
#' treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
#' head(treatment_list)
#' ibd2 <- incomplete_blocks(t = 15,
#'                           k = 3,
#'                           reps = 7,
#'                           seed = 1985,
#'                           data = treatment_list)
#' ibd2$infoDesign
#' head(ibd2$fieldBook)
#'
#' @section Reproducibility:
#' The result records effective inputs and the resolved seed in
#' \code{metadata$parameters}, using \code{reps} for replication. Under the
#' same package versions and RNG settings, rebuild a result \code{x} with
#' \code{do.call(incomplete_blocks, x$metadata$parameters)}.
#'
#' @export
incomplete_blocks <- function(t = NULL, k = NULL, r = NULL, l = 1, plotNumber = 101,
                              locationNames = NULL, seed = NULL, data = NULL,
                              reps = NULL, caller = "incomplete_blocks") {
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
    trt_labels <- treatment_labels(t, nt, caller)
    data_up <- data.frame(list(ENTRY = 1:nt, TREATMENT = trt_labels))
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    lookup <- TRUE
    df <- data.frame(list(ENTRY = 1:nt, LABEL_TREATMENT = trt_labels))
    dataLookUp <- df
  } else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      fieldhub_abort('Some of the basic design parameters are missing (t, k, r or l)')
    }
    if(!is.data.frame(data)) fieldhub_abort("Data must be a data frame.")
    if (ncol(data) < 2) fieldhub_abort("Data input needs at least two columns with: ENTRY and NAME.")
    data_up <- as.data.frame(data[,c(1,2)])
    data_up <- na.omit(data_up)
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    data_up$TREATMENT <- as.character(data_up$TREATMENT)
    new_t <- length(data_up$TREATMENT)
    if (t != new_t) fieldhub_abort("Number of treatments do not match with the data input.")
    TRT <- data_up$TREATMENT
    nt <- length(TRT)
    lookup <- TRUE
    dataLookUp <- data.frame(list(ENTRY = 1:nt, LABEL_TREATMENT = TRT))
  }
  if (!is.null(plotNumber)) validate_plot_starts(plotNumber)
  if(any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
    fieldhub_abort("'", caller, "()' requires plotNumber to be possitive integers and sorted.")
  }
  if (is.null(plotNumber) || length(plotNumber) != l) {
    default_plots <- seq(1001, 1000*(l+1), 1000)
    warn_default_plot_numbers(plotNumber, l, default_plots)
    plotNumber <- default_plots
  }
  if (k >= nt) fieldhub_abort(caller, "() requires that k < t.")
  validate_location_labels(locationNames, l)
  if(is.null(locationNames) || length(locationNames) != l) {
    if (!is.null(locationNames)) warn_default_location_names(locationNames, l, 1:l)
    locationNames <- 1:l
  }
  nincblock <- nt*r/k
  N <- nt * r
  if (k * nincblock != N) {
    fieldhub_abort('Size of experiment defined by number of units per incomplete block (nunits) is inconsistent. Check input parameters.')
  }
  if (nt %% k != 0) {
    fieldhub_abort('Number of treatments can not be fully distributed over the specified incomplete block specification.')
  }

  ibd_plots <- ibd_plot_numbers(nt = nt, plot.number = plotNumber, r = r, l = l)
  b <- nt/k
  square <- FALSE
  if (sqrt(nt) == round(sqrt(nt))) square <- TRUE
  outIBD_loc <- vector(mode = "list", length = l)
  blocks_model <- list()
  local_optimizer_options()
  for (i in 1:l) {
    mydes <- tryCatch(
      blocksdesign::blocks(treatments = nt, replicates = r, blocks = list(r, b), seed = NULL),
      error = function(e) {
        fieldhub_abort(
          caller, "() cannot build a resolvable design for t = ", nt,
          " treatments, k = ", k, ", and reps = ", r, ": not enough replication ",
          "for this block size. Increase reps or use a different block size.",
          call = NULL
        )
      }
    )
    mydes <- rerandomize_ibd(ibd_design = mydes)
    matdf <- base::data.frame(list(LOCATION = rep(locationNames[i], each = N)))
    matdf$PLOT <- as.numeric(unlist(ibd_plots[[i]]))
    matdf$BLOCK <- rep(c(1:r), each = nt)
    matdf$iBLOCK <- rep(c(1:b), each = k)
    matdf$UNIT <- rep(c(1:k), nincblock)
    matdf$TREATMENT <- mydes$Design_new[,4]
    colnames(matdf) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "ENTRY")
    outIBD_loc[[i]] <- matdf
    blocks_model[[i]] <- mydes$Blocks_model_new
  }
  OutIBD <- dplyr::bind_rows(outIBD_loc)
  OutIBD <- as.data.frame(OutIBD)
  OutIBD$ENTRY <- as.numeric(OutIBD$ENTRY)
  OutIBD_test <- OutIBD
  OutIBD_test$ID <- 1:nrow(OutIBD_test)
  if(lookup) {
    OutIBD <- dplyr::inner_join(OutIBD, dataLookUp, by = "ENTRY")
    # ENTRY was only needed to look up LABEL_TREATMENT; drop it by name.
    OutIBD$ENTRY <- NULL
    colnames(OutIBD) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "TREATMENT")
    OutIBD <- dplyr::inner_join(OutIBD, data_up, by = "TREATMENT")
    OutIBD <- OutIBD[, c("LOCATION", "PLOT", "REP", "IBLOCK", "UNIT", "ENTRY",
                         "TREATMENT")]
    colnames(OutIBD) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT")
  }
  ID <- 1:nrow(OutIBD)
  OutIBD_new <- cbind(ID, OutIBD)
  validateTreatments(OutIBD_new)
  lambda <- r*(k - 1)/(nt - 1)
  infoDesign <- list(Reps = r, iBlocks = b, NumberTreatments = nt, NumberLocations = l,
                     Locations = locationNames, seed = seed, lambda = lambda, 
                     id_design = 8)
  output <- list(infoDesign = infoDesign, fieldBook = OutIBD_new, blocksModel = blocks_model[[1]])
  reproduction_parameters <- record_design_parameters(
    environment(), overrides = list(reps = r), exclude = c("r", "caller")
  )
  output <- new_fieldhub_design(output, "incomplete_blocks", parameters = reproduction_parameters)
  return(invisible(output))
}

#' @noRd 
#' 
#' 
concurrence_matrix <- function(df=NULL, trt=NULL, target=NULL) {
  if (is.null(df)) {
    fieldhub_abort('No input dataset provided.')
  }
  if (is.null(trt)) {
    fieldhub_abort('No input treatment factor provided.')
  }
  if (is.null(target)) {
    fieldhub_abort('No input target design factor provided.')
  }
  df[,target]<-as.factor(df[,target])
  df[,trt]<-as.factor(df[,trt])
  s <- length(levels(df[,target]))
  if (s==0) { fieldhub_abort('No levels found for design factor provided.') }
  inc <- as.matrix(table(df[,target],df[,trt]))
  for (i in 1:s) {
    inc[inc[,i]>0,i] <- 1
  }
  conc.matrix <- t(inc) %*% inc
  summ.rep <- diag(conc.matrix)
  diag(conc.matrix) <- rep(99999999,nrow(conc.matrix))
  summ<-as.data.frame(table(as.vector(conc.matrix)))
  summ$Freq <- summ$Freq/2  # Added
  colnames(summ) <- c('Concurrence',target)
  summ <- summ[-nrow(summ),]
  
  return(summ = summ)
}
