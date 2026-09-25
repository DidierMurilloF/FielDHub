#' Generates a Randomized Complete Block Design (RCBD)
#' 
#' 
#' @description It randomly generates a randomized complete block design (RCBD) across locations.
#'
#' @details
#' When \code{checks} is supplied, one or more checks are repeated multiple times within
#' every block, while every test entry still appears exactly once. In a classical RCBD,
#' the residual is the treatment-by-block interaction; repeating checks inside a block
#' instead supplies a within-block estimate of error and a form of local control.
#'
#' \code{checks} accepts either a single positive integer \code{N} (the first \code{N}
#' entries of \code{data}, or of a character vector \code{t}, are the checks) or a
#' character vector of check labels. When a pool of entries is supplied through \code{data}
#' or a character \code{t}, every label named in \code{checks} must already exist in that
#' pool; an unmatched label is an error that also names the closest case-insensitive match,
#' if any. When \code{t} is a bare count (no pool supplied), the check labels are new and
#' are appended to the auto-generated test entries.
#'
#' \code{rep_checks} sets how many times each check repeats within a block: a single value
#' is recycled across all checks, or one value can be supplied per check.
#'
#' A block (test entries plus repeated checks) larger than 10,000 plots is rejected.
#'
#' With \code{spread_checks = TRUE} (the default), the repeated copies of a check are placed
#' one per contiguous stratum of the block, so they are spread across it rather than
#' clustered together. Two distinct density facts apply here: as soon as checks occupy more
#' than 50 percent of the block, a warning is issued as an early caution, but randomization
#' itself remains effectively unconstrained up to about 67 percent check density; only from
#' about 78 percent and higher does the stratified-placement constraint leave few or no
#' alternative positions, so a repeated check's placement can become nearly or fully
#' deterministic rather than random. This is a property of the stratified-placement geometry,
#' not a bug, and most field trials use far lower check density than either threshold.
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
#' @param checks (optional) Checks to repeat within every block. Either a positive
#'   integer \code{N}, meaning the first \code{N} entries of \code{data} (or of a
#'   character vector \code{t}) are the checks, or a character vector of check
#'   labels. \code{checks} sits after \code{data} in the argument list (rather than
#'   next to \code{t}, where it might otherwise go) precisely so that \code{data}
#'   keeps its original positional slot and existing positional calls to
#'   \code{RCBD()} keep working unchanged. By default \code{checks = NULL}, which
#'   produces an ordinary RCBD.
#' @param rep_checks (optional) Number of times each check is repeated within
#'   every block. A single value is recycled across all checks, or supply one
#'   value per check. By default \code{rep_checks = NULL}, which is treated as 1
#'   for every check.
#' @param spread_checks (optional) Logical. When \code{TRUE} (the default), the
#'   repeated copies of each check are spread across the block by placing one
#'   copy in each of \code{rep_checks} contiguous strata. When \code{FALSE}, the
#'   whole block is randomized without restriction.
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
#'   \item \code{fieldBook} is a data frame with the RCBD field book design. Without
#'   \code{checks} it has columns \code{ID}, \code{LOCATION}, \code{PLOT}, \code{REP} and
#'   \code{TREATMENT}. When \code{checks} is supplied it gains \code{ENTRY} and
#'   \code{CHECKS} columns, ordered \code{ID}, \code{LOCATION}, \code{PLOT}, \code{REP},
#'   \code{ENTRY}, \code{CHECKS}, \code{TREATMENT}.
#' }
#' 
#'
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#'
#' Lin, C.S., & Poushinsky, G. (1985). A modified augmented design (type 2) for
#' rectangular plots. Canadian Journal of Plant Science, 65(3), 743-749.
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
#' # Example 3: RCBD with two checks repeated twice in each of 3 blocks,
#' # alongside 18 test entries. Block size is 18 + 2 + 2 = 22 plots.
#' rcbd3 <- RCBD(t = 18, reps = 3,
#'               checks = c("CK1", "CK2"),
#'               rep_checks = c(2, 2),
#'               plotNumber = 101,
#'               seed = 1234,
#'               locationNames = "FARGO")
#' rcbd3$infoDesign
#' head(rcbd3$fieldBook)
#' # Each check appears twice per block, every test entry exactly once:
#' table(subset(rcbd3$fieldBook, REP == 1)$TREATMENT)
#'
#' @export
RCBD <- function(t = NULL, reps = NULL, l = 1, plotNumber = 101,
                 continuous = FALSE, planter = "serpentine",
                 seed = NULL, locationNames = NULL, data = NULL,
                 checks = NULL, rep_checks = NULL, spread_checks = TRUE) {
  has_checks <- !is.null(checks)
  if (!is.logical(spread_checks) || length(spread_checks) != 1 || is.na(spread_checks)) {
    stop("RCBD() requires 'spread_checks' to be a single TRUE or FALSE.")
  }
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
    default_plots <- seq(1001, 1000*(l+1), 1000)
    warn_default_plot_numbers(plotNumber, l, default_plots)
    plotNumber <- default_plots
  }
  if (!is.null(locationNames)) {
    locationNames <- toupper(locationNames)
  } else locationName <- 1:l
  # 'reps' feeds a matrix nrow(), a plot-number sequence, and (on the checks
  # path) rcbd_resolve_entries()'s block math alike, so it is validated once,
  # here, ahead of every path rather than only inside the numeric-t branch
  # below. For valid input (a whole number >= 2) this changes nothing.
  if (is.null(reps) || !is.numeric(reps) || length(reps) != 1 || is.na(reps) ||
      reps %% 1 != 0 || reps < 2) {
    shiny::validate("RCBD() requires 'reps' to be a single whole number of 2 or more.")
  }
  entries <- NULL
  if (has_checks) {
    entries <- rcbd_resolve_entries(t = t, checks = checks, rep_checks = rep_checks,
                                    data = data, spread_checks = spread_checks)
    n_units       <- sum(entries$reps_per_block)
    n_test        <- sum(entries$CHECKS == 0)
    check_names   <- entries$TREATMENT[entries$CHECKS != 0]
    rep_checks    <- entries$reps_per_block[entries$CHECKS != 0]
    mytreatments  <- entries$TREATMENT[entries$CHECKS == 0]
  } else if (is.null(data)) {
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
        if (anyDuplicated(t) > 0) {
          stop("RCBD() requires unique entry labels; duplicated: ",
               paste(unique(t[duplicated(t)]), collapse = ", "))
        }
        nt <- length(t)
        s <- t
        mytreatments <- t
      }else if(is.character(t) & length(t) == 1) {
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
  if (!has_checks) n_units <- nt
  if (length(locationNames) != l) {
    default_names <- paste("loc", 1:l, sep = "")
    if (!is.null(locationNames)) warn_default_location_names(locationNames, l, default_names)
    locationNames <- default_names
  }
  RCBD <- matrix(data = NA, nrow = b * l, ncol = n_units, byrow = TRUE)
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
      if (has_checks) {
        ids <- rcbd_randomize_block(entries, spread_checks = spread_checks)
        RCBD[j, ] <- entries$TREATMENT[match(ids, entries$ENTRY)]
      } else {
        RCBD[j, ] <- sample(s, size = length(s), replace = FALSE)
      }
      RCBD.layout[v,2] <- paste(RCBD[j,], collapse = " ")
      v <- v + 1
    }
    RCBD.layout.loc[[i]] <- RCBD.layout
  }
  plotNumber <- seriePlot.numbers(plot.number = plotNumber,
                                  reps = b,
                                  l = l,
                                  t = n_units)
  p.number.loc <- setNames(vector(mode = "list", length = l),
                           paste0("Loc_", locationNames))
  if (!continuous) {
    if (planter == "serpentine") {
      for (i in 1:l) {
        M <- matrix(data = NA, ncol = n_units, nrow = b, byrow = TRUE)
        for (k in 1:b) {
          D <- plotNumber[[i]]
          M[k,] <- D[k]:(D[k] + (n_units - 1))
        }
        p.number.loc[[i]] <- serpentinelayout(M, opt = 2)
      }
    }else {
      for (i in 1:l) {
        M <- matrix(data = NA, ncol = n_units, nrow = b, byrow = TRUE)
        for (k in 1:b) {
          D <- plotNumber[[i]]
          M[k,] <- D[k]:(D[k] + (n_units - 1))
        }
        p.number.loc[[i]] <- M
      }
    }
  }else {
    if (planter == "serpentine") {
      for (i in 1:l) {
        D <- plotNumber[[i]]
        M <- matrix(data = D[1]:(D[1] + (n_units * b - 1)), ncol = n_units,
                                    nrow = b, byrow = TRUE)
        p.number.loc[[i]] <- serpentinelayout(M, opt = 2)
      }
    }else {
      for (i in 1:l) {
        D <- plotNumber[[i]]
        p.number.loc[[i]] <- matrix(data = D[1]:(D[1] + (n_units * b - 1)), 
                                    ncol = n_units,
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
  RCBD.output <- data.frame(list(LOCATION = rep(locationNames, each = n_units * b),
                                 PLOT = as.vector(t(p.number.loc1)),
                                 REP = rep(1:b, each = n_units),
                                 TREATMENT = as.vector(t(RCBD))))

  RCBD.output$LOCATION <- factor(RCBD.output$LOCATION,
                                 levels = as.character(unique(locationNames)))
  RCBD.output <- RCBD.output[order(RCBD.output$LOCATION, RCBD.output$PLOT),]

  if (has_checks) {
    idx <- match(RCBD.output$TREATMENT, entries$TREATMENT)
    RCBD.output$ENTRY  <- entries$ENTRY[idx]
    RCBD.output$CHECKS <- entries$CHECKS[idx]
  }

  ID <- 1:nrow(RCBD.output)
  RCBD_output <- cbind(ID, RCBD.output)
  RCBD_output <- as.data.frame(RCBD_output)
  RCBD_output <- RCBD_output[, rcbd_fieldbook_cols(has_checks)]

  RCBD.layout <- as.data.frame(RCBD.layout)
  
  plotNumber <- as.vector(unlist(plotNumber))
  
  parameters <- list(blocks = b,
                     number.of.treatments = if (has_checks) n_test else nt,
                     treatments = mytreatments,
                     locations = l,
                     plotNumber = plotNumber,
                     locationNames = locationNames,
                     seed = seed,
                     id_design = 2)

  if (has_checks) {
    parameters <- append(
      parameters,
      list(checks = length(check_names),
           check_names = check_names,
           rep_checks = rep_checks,
           plots_per_block = n_units,
           spread_checks = spread_checks),
      after = which(names(parameters) == "treatments")
    )
  }
  output <- list(infoDesign = parameters,
                 layoutRandom = RCBD.layout.loc,
                 plotNumber = p.number.loc,
                 fieldBook = RCBD_output)
  class(output) <- "FielDHub"
  return(invisible(output))
}