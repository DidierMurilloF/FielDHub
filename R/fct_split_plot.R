#' Generates a Split Plot Design
#'
#' @description It randomly generates a split plot design (SPD) across locations.
#'
#' @param wp Number of whole plots, as an integer or a vector.
#' @param sp Number of sub plots per whole plot, as an integer or a vector.
#' @param reps Number of blocks (full replicates).
#' @param type Option for CRD or RCBD designs. Values are \code{type = 1} (CRD) or \code{type = 2} (RCBD). By default \code{type = 2}.
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param factorLabels (optional) If \code{TRUE} retain the levels
#'   labels from the original data set otherwise, numeric labels will be
#'   assigned. Default is \code{factorLabels =TRUE}.
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
#' 
#' @importFrom stats runif
#'
#' 
#' @return A list with two elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{fieldBook} is a data frame with the split plot field book.
#' }
#'
#'
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#'
#' @examples
#' # Example 1: Generates a split plot design SPD with 4 whole plots, 2 sub plots per whole plot,
#' # and 4 reps in an RCBD arrangement. This in for a single location.
#' SPDExample1 <- split_plot(wp = 4, sp = 2, reps = 5, l = 1, 
#'                           plotNumber = 101, 
#'                           seed = 14,
#'                           type = 2, 
#'                           locationNames = "FARGO")
#' SPDExample1$infoDesign
#' SPDExample1$layoutlocations
#' head(SPDExample1$fieldBook,12)
#' 
#' # Example 2: Generates a split plot design SPD with 5 whole plots 
#' # (4 types of fungicide + one control), 10 sub plots per whole plot (10 bean varieties), 
#' # and 6 reps in an RCBD arrangement. This in 3 locations or sites.
#' # In this case, we show how to use the option data.
#' wp <- c("NFung", paste("Fung", 1:4, sep = ""))  # Fungicides (5 Whole plots)
#' sp <- paste("Beans", 1:10, sep = "")            # Beans varieties (10 sub plots)
#' split_plot_Data <- data.frame(list(WHOLPLOT = c(wp, rep(NA, 5)), SUBPLOT = sp))
#' head(split_plot_Data, 12)
#' SPDExample2 <- split_plot(reps = 6, l = 3, 
#'                           plotNumber = c(101, 1001, 2001),
#'                           seed = 23, 
#'                           type = 2, 
#'                           locationNames = c("A", "B", "C"),
#'                           data = split_plot_Data)
#' SPDExample2$infoDesign
#' SPDExample2$layoutlocations
#' head(SPDExample2$fieldBook,12)
#'              
#'                   
#' @section Reproducibility:
#' The result records effective inputs and the resolved seed in
#' \code{metadata$parameters}. Under the same package versions and RNG
#' settings, rebuild a result \code{x} with
#' \code{do.call(split_plot, x$metadata$parameters)}.
#'
#' @export
split_plot <- function(wp = NULL, sp = NULL, reps = NULL, type = 2, l = 1, plotNumber = 101, 
                       seed = NULL, locationNames = NULL, factorLabels = TRUE, 
                       data = NULL) {
  validate_locations(l)
  validate_flag(factorLabels, "factorLabels")
  
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  validate_factorial_type(type)
  args1 <- list(wp, sp, reps, l)
  validate_iteration_budget(reps, "reps")
  if (is.null(data)) {
    resolved <- resolve_design_factors(list(wp = wp, sp = sp), reps, l)
    WholePlots <- resolved$wp$levels
    SubPlots <- resolved$sp$levels
    wp <- resolved$wp$count
    sp <- resolved$sp$count
  } else {
    if (!is.data.frame(data))
      fieldhub_abort("Data must be a data frame.")
    if (ncol(data) < 2L) fieldhub_abort("split_plot() requires whole-plot and sub-plot columns in data.")
    data <- as.data.frame(data[, 1:2])
    colnames(data) <- c("WholePlot", "SubPlot")
    WholePlots <- as.vector(na.omit(data$WholePlot))
    SubPlots <- as.vector(na.omit(data$SubPlot))
    validate_entry_labels(WholePlots, "WholePlot")
    validate_entry_labels(SubPlots, "SubPlot")
    check_unique_labels(WholePlots, "WholePlot")
    check_unique_labels(SubPlots, "SubPlot")
    WholePlots.f <- factor(WholePlots, as.character(unique(WholePlots)))
    SubPlots.f <- factor(SubPlots, as.character(unique(SubPlots)))
    wp <- length(levels(WholePlots.f))
    sp <- length(levels(SubPlots.f))
    WholePlots <- as.character(WholePlots.f)
    SubPlots <- as.character(SubPlots.f)
    if (!factorLabels) {
      WholePlots <- as.character(1:wp)
      SubPlots <- as.character((wp + 1):(wp + sp))
    }
  }
  b <- reps
  validate_design_size(c(wp, sp, reps, l))
  if (!is.null(plotNumber)) {
    validate_plot_starts(plotNumber)
    if (any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
      fieldhub_abort("Input plotNumber must be an integer greater than 0 and sorted.")
    } 
  }else {
    plotNumber <- seq(1001, 1000*(l+1), 1000)
    warning("Since plotNumber was NULL, it was set up to its default value for each location.")
  }
  plot.number <- plotNumber
  if (type == 1) crd <- TRUE else crd <- FALSE
  pred_plots <- plot_number_splits(plot.number = plot.number, reps = b, l = l, t = wp, crd = crd)
  plot.random <- pred_plots$plots
  p.number.loc <- pred_plots$plots_loc
  if (crd) {
    loc.list <- vector(mode = "list", length = l)
    for (v in 1:l) {
      spd.layout <- matrix(data = 0, nrow = wp * b, ncol = 4)
      spd.layout[,1] <- plot.random[,v]
      spd.layout[,2] <- rep(1:b, each = wp)
      spd.layout[,3] <- rep(WholePlots, times = b)
      spd.layout <- spd.layout[order(spd.layout[,1]),]
      rownames(spd.layout) <- 1:(wp * b)
      colnames(spd.layout) <- c("PLOT", "REP", "Whole-plot", "Sub-plot")
      loc.list[[v]] <- spd.layout
    }
    spd.layout <- paste_by_row(loc.list)
    plots.n <- as.numeric(spd.layout[,1])
    wp.reps <- as.numeric(spd.layout[,2])
    wp.random <- as.vector(spd.layout[,3])
    type <- "CRD"
  }else {
    plot.numbers <- as.vector(unlist(p.number.loc))
    wp.random <- replicate(b * l, sample(WholePlots, replace = FALSE))
    spd.layout <- matrix(data = 0, nrow = (b * wp) * l, ncol = 4)
    colnames(spd.layout) <- c("PLOT", "REP", "Whole-plot", "Sub-plot")
    spd.layout[,1] <- plot.numbers
    spd.layout[,2] <- rep(1:b, each = wp)
    spd.layout[,3] <- as.vector(wp.random)
    plots.n <- as.numeric(spd.layout[,1])
    type <- "RCBD"
  }
  sp.random <- replicate((b * wp) * l, sample(SubPlots, replace = FALSE))
  k <- (b * wp) * l
  for(i in 1:k) {
    spd.layout[i,4] <- paste(sp.random[,i], collapse = " ")
  }
  loc.spd.layout <- vector(mode = "list", length = l)
  y <- seq(1, k, b * wp)
  z <- seq(b * wp, k, b * wp)
  i <- 1;j <- 1
  for(sites in 1:l) {
    loc.spd.layout[[sites]] <- spd.layout[y[i]:z[j],]
    i <- i + 1
    j <- j + 1
  }
  spd.layout <- as.data.frame(spd.layout)
  rownames(spd.layout) <- 1:nrow(spd.layout)
  wp.d <- rep(as.vector(wp.random), each = sp)
  sp.d <- as.vector(sp.random)
  validate_location_labels(locationNames, l)
  if (!is.null(locationNames) && length(locationNames) == l) {
    LOCATION <- rep(locationNames, each = (sp * wp) * b)
  }else if (is.null(locationNames) || length(locationNames) != l) {
    LOCATION <- rep(1:l, each = (sp * wp) * b)
  }
  if (crd) {
    PLOT <- rep(plots.n, each = sp)
    REPS <- rep(wp.reps, each = sp)
    spd.output <- data.frame(list(LOCATION = LOCATION, PLOT = PLOT, REP = REPS,
                                  wp = wp.d, sp = sp.d, TREATMENT = NA))
    colnames(spd.output) <- c("LOCATION", "PLOT", "REP", "WHOLE_PLOT", "SUB_PLOT", "TRT_COMB")
  }else {
    PLOT <- rep(plots.n, each = sp)
    Block <- rep(rep(1:b, each = wp * sp), times = l)
    spd.output <- data.frame(list(LOCATION = LOCATION, PLOT = PLOT, BLOCK = Block,
                                  wp = wp.d, sp = sp.d, TREATMENT = NA))
    colnames(spd.output) <- c("LOCATION", "PLOT", "REP", "WHOLE_PLOT", "SUB_PLOT", "TRT_COMB")
  }
  z <- 1:nrow(spd.output)
  for (j in z) {
    spd.output[j, ncol(spd.output)] <- paste(spd.output[j, 4:5], collapse = "|")
  }
  spd_output <- cbind(ID = 1:nrow(spd.output), spd.output)
  info.design = list(WholePlots = WholePlots, SubPlots = SubPlots, locationNumber = l,
                     locationNames = locationNames,
                     plotNumbers = plot.number,
                     typeDesign = type,
                     seed = seed,
                     id_design = 5)
  output <- list(infoDesign = info.design, layoutlocations = loc.spd.layout, 
              fieldBook = spd_output)
  reproduction_parameters <- record_design_parameters(
    environment(), overrides = list(wp = args1[[1]], sp = args1[[2]], type = if (crd) 1 else 2)
  )
  output <- new_fieldhub_design(output, "split_plot", parameters = reproduction_parameters)
  return(invisible(output))
}
