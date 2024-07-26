#' Generates a Resolvable Row-Column Design (RowColD)
#'
#'
#' @description It randomly generates a resolvable row-column design (RowColD).
#'  The design is optimized in both rows and columns blocking factors. The 
#'  randomization can be done across multiple locations.
#' 
#' @details
#' The Row-Column design in FielDHub is built in two stages. The first step 
#' constructs the blocking factor \code{Columns} using Incomplete Block Units 
#' from an incomplete block design that sets the number of incomplete blocks as 
#' the number of \code{Columns} in the design, each of which has a dimension 
#' equal to the number of \code{Rows}. Once this design is generated, the 
#' \code{Rows} are used as the \code{Row} blocking factor that is optimized for 
#' A-Efficiency, but levels within the original \code{Columns} are fixed.
#' To optimize the \code{Rows} while maintaining the current optimized \code{Columns}, 
#' we use a heuristic algorithm that swaps at random treatment positions within 
#' a given \code{Column (Block)} also selected at random. The algorithm begins 
#' by calculating the A-Efficiency on the initial design, performs a swap iteration, 
#' recalculates the A-Efficiency on the resulting design, and compares it with 
#' the previous one to decide whether to keep or discard the new design. This 
#' iterative process is repeated, by default, 1,000 times.
#'
#' @param t Number of  treatments.
#' @param nrows Number of rows of a full resolvable replicate. 
#' @param r Number of blocks (full resolvable replicates).
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each 
#' location. By default \code{plotNumber = 101}.
#' @param seed (optional) Real number that specifies the starting seed to obtain 
#' reproducible designs.
#' @param locationNames (optional) Names for each location.
#' @param iterations Number of iterations for design optimization. By 
#' default \code{iterations = 1000}.
#' @param data (optional) Data frame with label list of treatments
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
#' @return A list with four elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{resolvableBlocks} a list with the resolvable row columns blocks. 
#'   \item \code{concurrence} is the concurrence matrix.
#'   \item \code{fieldBook} is a data frame with the row-column field book.
#' }
#'
#'
#' @references
#' Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs for 
#' factorial and unstructured treatment sets. https://CRAN.R-project.org/package=blocksdesign
#'
#' @examples
#' 
#' # Example 1: Generates a row-column design with 2 full blocks and 24 treatments
#' # and 6 rows. This for one location. This example uses 100 iterations for the optimization
#' # but 1000 is the default and recomended value.
#' rowcold1 <- row_column(
#'   t = 24, 
#'   nrows = 6, 
#'   r = 2, 
#'   l = 1, 
#'   plotNumber= 101, 
#'   locationNames = "Loc1",
#'   iterations = 100,
#'   seed = 21
#' )
#' rowcold1$infoDesign
#' rowcold1$resolvableBlocks
#' head(rowcold1$fieldBook,12)
#' 
#' # Example 2: Generates a row-column design with 2 full blocks and 30 treatments
#' # and 5 rows, for one location. This example uses 100 iterations for the optimization
#' # but 1000 is the default and recommended value.
#' # In this case, we show how to use the option data.
#' treatments <- paste("ND-", 1:30, sep = "")
#' ENTRY <- 1:30
#' treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
#' head(treatment_list)
#' rowcold2 <- row_column(
#'   t = 30, 
#'   nrows = 5, 
#'   r = 2, 
#'   l = 1, 
#'   plotNumber= 1001, 
#'   locationNames = "A",
#'   seed = 15,
#'   iterations = 100,
#'   data = treatment_list
#' )
#' rowcold2$infoDesign
#' rowcold2$resolvableBlocks
#' head(rowcold2$fieldBook,12)
#'   
#' 
#' @export
row_column <- function(t = NULL, nrows = NULL, r = NULL, l = 1, plotNumber= 101, 
                       locationNames = NULL, seed = NULL, iterations = 1000,
                       data = NULL) {
  
  if (is.null(seed) || !is.numeric(seed)) seed <- runif(1, min = -50000, max = 50000)
  # set.seed(seed)
  k <- nrows
  lookup <- FALSE
  if (is.null(data)) {
    if (is.null(t) || is.null(k) || is.null(r) || is.null(l)) {
      shiny::validate('Some of the basic design parameters are missing (t, k, r or l).')
    }
    arg1 <- list(k, r, l);arg2 <- c(k, r, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      shiny::validate('row_column() requires k, r and l to be possitive integers.')
    }
    if (is.numeric(t)) {
      if (length(t) == 1) {
        if (t == 1 || t < 1) {
          shiny::validate('row_column() requires more than one treatment.')
        } 
        nt <- t
      }else if ((length(t) > 1)) {
        nt <- length(t)
        TRT <- t
      }
    } else if (is.character(t) || is.factor(t)) {
      if (length(t) == 1) {
        shiny::validate('incomplete_blocks() requires more than one treatment.')
      } 
      nt <- length(t)
    } else if ((length(t) > 1)) {
      nt <- length(t)
    }
    data_up <- data.frame(list(ENTRY = 1:nt, TREATMENT = paste0("G-", 1:nt)))
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    lookup <- TRUE
    df <- data.frame(list(ENTRY = 1:nt, LABEL_TREATMENT = paste0("G-", 1:nt)))
    dataLookUp <- df
  } else if (!is.null(data)) {
    if (is.null(t) || is.null(r) || is.null(k) || is.null(l)) {
      shiny::validate('Some of the basic design parameters are missing (t, r, k or l).')
    }
    if(!is.data.frame(data)) shiny::validate("Data must be a data frame.")
    data_up <- as.data.frame(data[,c(1,2)])
    data_up <- na.omit(data_up)
    colnames(data_up) <- c("ENTRY", "TREATMENT")
    data_up$TREATMENT <- as.character(data_up$TREATMENT)
    new_t <- length(data_up$TREATMENT)
    if (t != new_t) base::stop("Number of treatments do not match with data input.")
    TRT <- data_up$TREATMENT
    nt <- length(TRT)
    lookup <- TRUE
    dataLookUp <- data.frame(list(ENTRY = 1:nt, LABEL_TREATMENT = TRT))
  }
  if (k >= nt) shiny::validate('incomplete_blocks() requires k < t.')
  if (nt %% k != 0) {
    shiny::validate('Number of treatments can not be fully distributed over the specified incomplete block specification.')
  }
  if(is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
  nunits <- k
  
  
  ## New code
  N <- nt * r
  out_row_col_loc <- vector(mode = "list", length = l)
  blocks_model <- list()
  for (i in 1:l) {
    reps <- r
    ncols <- nt / nunits
    mydes <- blocksdesign::blocks(
      treatments = nt, 
      replicates = reps, 
      blocks = list(reps, ncols),
      seed = seed + i
    )
    mydes <- rerandomize_ibd(ibd_design = mydes)
    # Create row and column design
    row_col_design <- mydes$Design_new |> 
      dplyr::mutate(Level_3 = rep(rep(paste0("B", 1:nrows), times = ncols), times = reps)) |> 
      dplyr::mutate(Level_3 = paste(Level_1, Level_3, sep = ".")) |> 
      dplyr::mutate(Level_3 = factor(Level_3, levels = unique(Level_3))) |> 
      dplyr::select(Level_1, Level_2, Level_3, plots, treatments)
    
    improved_design <- improve_efficiency(row_col_design, iterations, seed = seed + i)
    field_book_best_design <- improved_design$best_design
    row_column_efficiency <- report_efficiency(improved_design$best_design)
    blocks_model[[i]] <- row_column_efficiency
    row_col_fieldbook <- field_book_best_design |>
      dplyr::rename(
        REP = Level_1, 
        COLUMN = Level_2, 
        ROW = Level_3, 
        PLOT = plots, 
        ENTRY = treatments) |>
      dplyr::mutate(
        REP = as.numeric(factor(REP, levels = unique(REP)))
      ) |> 
      dplyr::group_by(REP) |>
      dplyr::mutate(
        COLUMN = as.numeric(factor(COLUMN, levels = unique(COLUMN))),
        ROW = as.numeric(factor(ROW, levels = unique(ROW)))
      ) |>
      dplyr::select(PLOT, REP, COLUMN, ROW, ENTRY)
    
    locations_df <- data.frame(list(LOCATION = rep(locationNames[i], each = N)))
    row_col_fieldbook <- dplyr::bind_cols(locations_df, row_col_fieldbook)
    out_row_col_loc[[i]] <- row_col_fieldbook
  
  }
  
  out_row_col <- dplyr::bind_rows(out_row_col_loc)
  out_row_col$ENTRY <- as.numeric(out_row_col$ENTRY)
  
  if (lookup) {
    out_row_col <- dplyr::inner_join(out_row_col, dataLookUp, by = "ENTRY")
    out_row_col <- out_row_col |> 
      dplyr::rename(TREATMENT = LABEL_TREATMENT) |> 
      dplyr::select(-ENTRY)
    out_row_col <- dplyr::inner_join(out_row_col, data_up, by = "TREATMENT") |> 
      dplyr::select(LOCATION, PLOT, REP, ROW, COLUMN, ENTRY, TREATMENT)
  }
  
  out_row_col_id <- out_row_col
  
  out_row_col_id <- out_row_col_id[order(out_row_col_id$LOCATION, out_row_col_id$REP, out_row_col_id$ROW),]
  row_col_plots <- ibd_plot_numbers(nt = nt, plot.number = plotNumber, r = r, l = l)
  out_row_col_id$PLOT <- as.vector(unlist(row_col_plots))
  
  ID <- 1:nrow(out_row_col_id)
  out_row_col_fieldbook <- cbind(ID, out_row_col_id)
  
  loc <- levels(out_row_col_fieldbook$LOCATION)
  ib <- nt/k
  Resolvable_rc_reps <- vector(mode = "list", length = r*l)
  w <- 1
  for (sites in 1:l) {
    for (j in 1:r) {
      z <- out_row_col_fieldbook
      z <- subset(z, z$LOCATION == loc[sites] & z$REP == j)
      if (is.null(data)){
        Resolvable_rc_reps[[w]] <- matrix(data = as.vector(z$ENTRY), nrow = nunits,
                                          ncol = ib, byrow = TRUE)
      }else {
        Resolvable_rc_reps[[w]] <- matrix(data = as.vector(z$TREATMENT), nrow = nunits,
                                          ncol = ib, byrow = TRUE)
      }
      w <- w + 1
    }
  }
  NEW_Resolvable <- setNames(vector(mode = "list", length = l),
                             paste0("Loc_", locationNames))
  x <- seq(1, r * l, r)
  y <- seq(r, r * l, r)
  z <- 1
  for (loc in 1:l) {
    NEW_Resolvable[[loc]] <- setNames(Resolvable_rc_reps[x[z]:y[z]],
                                 paste0(rep("rep", r), 1:r))
    z <- z + 1
  }

  df <- out_row_col_fieldbook
  trt <- "ENTRY"
  c1 <- concurrence_matrix(df=df, trt=trt, target='REP')
  c2 <- concurrence_matrix (df=df, trt=trt, target='ROW')
  c3 <- concurrence_matrix (df=df, trt=trt, target='COLUMN')
  summ <- merge(c1, c2, by="Concurrence", all=TRUE)
  new_summ <- merge(summ, c3, by='Concurrence', all=TRUE)
  infoDesign <- list(
    rows = nrows,
    columns = ib,
    reps = r,
    treatments = nt,
    locations = l,
    location_names = locationNames,
    seed = seed,
    id_design = 9
  )
  output <- list(
    infoDesign = infoDesign,
    blocksModel = blocks_model,
    resolvableBlocks = NEW_Resolvable,
    concurrence = new_summ,
    fieldBook = out_row_col_fieldbook
  )
  class(output) <- "FielDHub"
  return(invisible(output))
}