#' Generates an Augmented Randomized Complete Block Design (ARCBD)
#' 
#' @description It randomly generates an augmented randomized complete block design across locations (ARCBD).
#'
#' @param lines Treatments, number of lines for test.
#' @param checks Number of checks per augmented block.
#' @param b Number of augmented blocks.
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param planter Option for \code{serpentine} or \code{cartesian} arrangement. By default \code{planter = 'serpentine'}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param exptName (optional) Name of experiment.
#' @param locationNames (optional) Name for each location.
#' @param repsExpt (optional) Number of reps of experiment. By default \code{repsExpt = 1}.
#' @param random Logical value to randomize treatments or not. By default \code{random = TRUE}.
#' @param data (optional) Data frame with the labels of treatments.
#' @param nrows (optional) Number of rows in the field.
#' @param ncols (optional) Number of columns in the field.
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
#' @return A list with five elements.
#' \itemize{
#'   \item \code{infoDesign} is a list with information on the design parameters.
#'   \item \code{layoutRandom} is the ARCBD layout randomization for the first location.
#'   \item \code{plotNumber} is the plot number layout for the first location.
#'   \item \code{exptNames} is the experiment names layout.
#'   \item \code{data_entry} is a data frame with the data input.
#'   \item \code{fieldBook} is a data frame with the ARCBD field book.
#' }
#' 
#' @references
#' Federer, W. T. (1955). Experimental Design. Theory and Application. New York, USA. The
#' Macmillan Company.
#' 
#' @examples
#' # Example 1: Generates an ARCBD with 6 blocks, 3 checks for each, and 50 treatments 
#' # in two locations.
#' ARCBD1 <- RCBD_augmented(lines = 50, checks = 3, b = 6, l = 2, 
#'                          planter = "cartesian", 
#'                          plotNumber = c(1,1001),
#'                          seed = 23, 
#'                          locationNames = c("FARGO", "MINOT"))
#' ARCBD1$infoDesign
#' ARCBD1$layoutRandom
#' ARCBD1$exptNames
#' ARCBD1$plotNumber
#' head(ARCBD1$fieldBook, 12)
#'                    
#' # Example 2: Generates an ARCBD with 17 blocks, 4 checks for each, and 350 treatments 
#' # in 3 locations.
#' # In this case, we show how to use the option data.
#' checks <- 4;
#' list_checks <- paste("CH", 1:checks, sep = "")
#' treatments <- paste("G", 5:354, sep = "")
#' treatment_list <- data.frame(list(ENTRY = 1:354, NAME = c(list_checks, treatments)))
#' head(treatment_list, 12)
#' ARCBD2 <- RCBD_augmented(lines = 350, checks = 4, b = 17, l = 3, 
#'                          planter = "serpentine", 
#'                          plotNumber = c(101,1001,2001), 
#'                          seed = 24, 
#'                          locationNames = LETTERS[1:3],
#'                          data = treatment_list)
#' ARCBD2$infoDesign
#' ARCBD2$layoutRandom
#' ARCBD2$exptNames
#' ARCBD2$plotNumber
#' head(ARCBD2$fieldBook, 12)
#'                                        
#' @export
RCBD_augmented <- function(lines = NULL, checks = NULL, b = NULL, l = 1, planter = "serpentine", 
                           plotNumber = 101, exptName  = NULL, seed = NULL, locationNames = NULL,
                           repsExpt = 1, random = TRUE, data = NULL, nrows = NULL, ncols = NULL) {
  
  if (all(c("serpentine", "cartesian") != planter)) {
    stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  if (is.null(seed)) seed <- runif(1, min=-50000, max=50000)
  set.seed(seed)
  if(!is.numeric(plotNumber) && !is.integer(plotNumber)) {
    stop("plotNumber should be an integer or a numeric vector.")
  }
  if (any(plotNumber %% 1 != 0)) {
    stop("plotNumber should be integers.")
  }
  if (!is.null(l)) {
    if (is.null(plotNumber) || length(plotNumber) != l) {
      if (l > 1){
        plotNumber <- seq(1001, 1000*(l+1), 1000)
        message(cat("Warning message:", "\n", 
                    "Since plotNumber was missing, it was set up to default value of: ", plotNumber, 
                    "\n", "\n"))
      } else {
        plotNumber <- 1001
        message(cat("Warning message:", "\n", 
                    "Since plotNumber was missing, it was set up to default value of: ", plotNumber, 
                    "\n", "\n"))
      } 
    }
  }else stop("Number of locations/sites is missing")
  if (is.null(lines) || is.null(checks) || is.null(b) || is.null(l)) {
    stop('Some of the basic design parameters are missing (lines, checks, b, l).')
  }
  if(is.null(repsExpt)) repsExpt <- 1
  arg1 <- list(lines, b, l, repsExpt);arg2 <- c(lines, b, l, repsExpt)
  if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
    stop('RCBD_augmented() requires input lines, b and l to be possitive integers.')
  }
  if (!is.null(plotNumber) && is.numeric(plotNumber)) {
    if(any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
      stop("RCBD_augmented() requires input plotNumber to be possitive integers and sorted.")
    }
  }
  if (!is.null(data)) {
    data <- as.data.frame(data)
    if (ncol(data) < 2) base::stop("Data input needs at least two columns with: ENTRY and NAME.")
    data <- data[,1:2]
    data <- na.omit(data)
    colnames(data) <- c("ENTRY", "NAME")
    new_lines <- nrow(data) - checks
    if (lines != new_lines) base::stop("Number of experimental lines do not match with data input provided.")
    lines <- new_lines
  } else {
    NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
              paste(rep("G", lines), (checks + 1):(lines + checks), sep = ""))
    data <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
  }
  all_genotypes <- lines + checks * b
  plots_per_block <- base::ceiling(all_genotypes/b)
  if (!is.null(locationNames)) {
    if (length(locationNames) == l) {
      locationNames <- toupper(locationNames)
    } else {
      locationNames <- 1:l
    }
  } else locationNames <- 1:l
  lines_per_plot <- plots_per_block - checks
  excedent <- plots_per_block * b
  Fillers <- excedent - all_genotypes
  dim_block <- plots_per_block
  #############################################################################################
  if (l < 1 || is.null(l)) base::stop("Check the input for the number of locations.")
  if (length(plotNumber) != l || is.null(plotNumber)) plotNumber <- seq(1001, 1000*(l+1), 1000)
  outputDesign_loc <- vector(mode = "list", length = l)
  if (is.null(exptName) || length(exptName) != repsExpt){
    exptName <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
  } 
  if (any(is.null(c(nrows, ncols)))) {
    nrows_within_block <- 1
    ncols <- plots_per_block
  } else {
    nrows_within_block <- nrows / b
    ncols <- ncols
  }
  ## Start feedback code
  field_rows <- nrows_within_block * b 
  field_cols <- ncols
  lines_arcbd <- lines
  checks_arcbd <- checks
  set_blocks <- set_augmented_blocks(
    lines = lines_arcbd, 
    checks = checks_arcbd
  )
  blocks_arcbd <- set_blocks$b
  if (length(blocks_arcbd) == 0) {
    stop("No options available for that amount of treatments!", call. = FALSE)
  }
  blocks_dims <- set_blocks$blocks_dims
  colnames(blocks_dims) <- c("BLOCKS", "DIMENSIONS")
  feedback <- as.data.frame(blocks_dims)
  set_dims <- paste(field_rows, field_cols, sep = " x ")
  inputs_subset <- subset(feedback, feedback[, 1] == b & feedback[, 2] == set_dims)
  if (nrow(inputs_subset) == 0) {
    message(cat("\n", "Error in RCBD_augmented(): ", "\n", "\n",
      "Field dimensions do not fit with the data entered!", "\n",
      "Try one of the following options: ", "\n"))
    return(print(feedback))
  }
  ## end feedback code
  loc <- 1:l
  expt <- 1:repsExpt
  layout1_loc1 <- vector(mode = "list", length = 1)
  plot_loc1 <- vector(mode = "list", length = 1)
  layout_random_sites <- vector(mode = "list", length = l)
  layout_plots_sites <- vector(mode = "list", length = l)
  for (locations in loc) {
    sky <- length(expt)
    layout1_expt <- vector(mode = "list", length = repsExpt)
    Blocks_info_expt <- vector(mode = "list", length = repsExpt)
    my_names_expt <- vector(mode = "list", length = repsExpt)
    plot_number_expt <- vector(mode = "list", length = repsExpt)
    Col_checks_expt <- vector(mode = "list", length = repsExpt)
    for (expts in expt) {
      if (random) {
        if (Fillers >= (ncols - checks - 1)) {
          stop("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
        } 
        len_cuts <- rep(lines_per_plot, times = b - 1)
        len_cuts <- c(len_cuts, lines - sum(len_cuts))
        entries <- as.vector(data[(checks + 1):nrow(data),1])
        entries <- sample(entries)
        rand_len_cuts <- sample(len_cuts)
        lines_blocks <- split_vectors(x = entries, len_cuts = rand_len_cuts)
        total_rows <- nrows_within_block * b
        datos <- sample(c(rep(0, times = nrows_within_block * ncols - checks),
                          rep(1, checks)))
        randomized_blocks <- setNames(vector(mode = "list", length = b), paste0("Block", 1:b))
        spots_for_checks <- setNames(vector(mode = "list", length = b), paste0("Block", 1:b))
        for (i in 1:b) {
          block <- matrix(data = sample(datos), nrow = nrows_within_block, 
                          ncol = ncols, 
                          byrow = TRUE)
          if (Fillers > 0 && i == 1) {
            if (total_rows %% 2 == 0) { 
              if (planter == "serpentine") {
                block[1,1:Fillers] <- "Filler"
              } else { 
                block[1,((ncol(block) + 1) - Fillers):ncol(block)] <- "Filler" 
              } 
            } else { 
              block[1,((ncol(block) + 1) - Fillers):ncol(block)] <- "Filler" 
            }
            v <- which.min(rand_len_cuts)
            lines_blocks <- lines_blocks[unique(c(v, 1:b))]
            block_fillers <- as.vector(lines_blocks[[1]])
            zeros <- length(block_fillers)
            block[block != "Filler"] <- sample(as.character(c(rep(0, zeros), 1:checks)))
            block <- as.data.frame(block)
            colnames(block) <- paste0("col", 1:ncols)
            spots_for_checks[[i]] <- block
            block_with_checks <- block 
            block_with_checks[block_with_checks == 0] <- sample(as.character(lines_blocks[[i]]))
            block_with_entries_checks <- block_with_checks
            randomized_blocks[[i]] <- block_with_entries_checks
          } else {
            block[block == 1] <- sample(as.character(1:checks))
            block <- as.data.frame(block)
            colnames(block) <- paste0("col", 1:ncols)
            spots_for_checks[[i]] <- block
            block_with_checks <- block
            block_with_checks[block_with_checks == 0] <- sample(as.character(lines_blocks[[i]]))
            block_with_entries_checks <- block_with_checks
            block_with_entries_checks <- as.data.frame(block_with_entries_checks)
            colnames(block_with_entries_checks) <- paste0("col", 1:ncols)
            randomized_blocks[[i]] <- block_with_entries_checks
          }
        }
        layout <- dplyr::bind_rows(randomized_blocks)
        layout <- as.matrix(layout)
        binary_matrix <- dplyr::bind_rows(spots_for_checks)
        binary_matrix <- as.matrix(binary_matrix)
        col_checks <- ifelse(binary_matrix != "0" & binary_matrix != "Filler", 1, 0)
        plotsPerBlock <- rep(ncol(layout) * nrows_within_block, b)
        plotsPerBlock <- c(plotsPerBlock[-length(plotsPerBlock)], ncol(layout) * nrows_within_block - Fillers)
      } else {
        fun <- function(x) {
          matrix(data = sample(c(rep(0, (nrows_within_block * ncols) - checks), 1:checks)),
                 nrow  = nrows_within_block, 
                 byrow = TRUE)
        }
        entries <- as.vector(data[(checks + 1):nrow(data),1])
        if (Fillers > 0) {
          if (Fillers >= (ncols - checks - 1)) {
            stop("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
          }
          blocks_with_checks <- lapply(1:b, fun)
          layout_a <- paste_by_row(blocks_with_checks)
          if ((nrows_within_block * b) %% 2 == 0) { 
            if (planter == "serpentine") {
              layout_a[1,] <- c(rep("Filler", Fillers), 
                                sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)), 
                                       size = ncol(layout_a) - Fillers, replace = FALSE))
            } else {
              layout_a[1,] <- c(sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)), 
                                       size = ncol(layout_a) - Fillers, replace = FALSE),
                                rep("Filler", Fillers))
            } 
          } else {
            layout_a[1,] <- c(sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)), 
                                     size = ncol(layout_a) - Fillers, replace = FALSE),
                              rep("Filler", Fillers))
          }
          col_checks <- ifelse(layout_a != 0, 1,0)
          no_randomData <- no_random_arcbd(checksMap = layout_a, 
                                           data_Entry = entries, 
                                           planter = planter)
          layout <- no_randomData$w_map_letters
          len_cuts <- no_randomData$len_cut 
          plotsPerBlock <- rep(ncol(layout) * nrows_within_block, b)
          plotsPerBlock <- c(plotsPerBlock[-length(plotsPerBlock)], ncol(layout) * nrows_within_block - Fillers)
        } else {
          blocks_with_checks <- lapply(1:b, fun)
          layout_a <- paste_by_row(blocks_with_checks)
          col_checks <- ifelse(layout_a != 0, 1,0)
          no_randomData <- no_random_arcbd(checksMap = layout_a, 
                                           data_Entry = entries, 
                                           planter = planter)
          layout <- no_randomData$w_map_letters
          plotsPerBlock <- rep(ncol(layout) * nrows_within_block, b)
        }
      }
      Blocks_info <- matrix(data = rep(b:1, each = (ncols * nrows_within_block)), 
                            nrow = nrows_within_block * b, 
                            ncol = ncols, 
                            byrow = TRUE)
      new_exptName <- rev(exptName)
      nameEXPT <- ARCBD_name(Fillers = Fillers, 
                             b = nrows_within_block * b, 
                             layout = layout, 
                             name.expt = exptName[expts], 
                             planter = planter)
      plotEXPT <- ARCBD_plot_number(plot.number = plotNumber[locations], 
                                    planter = planter,
                                    b = nrows_within_block * b, 
                                    name.expt = exptName[expts],
                                    Fillers = Fillers, 
                                    nameEXPT = nameEXPT$my_names)
      my_data_VLOOKUP <- data
      COLNAMES_DATA <- colnames(my_data_VLOOKUP)
      layout1 <- layout
      if (Fillers > 0) {
        layout1[layout1 == "Filler"] <- 0
        layout1 <- apply(layout1, 2 , as.numeric)
        Entry_Fillers <- data.frame(list(0,"Filler"))
        colnames(Entry_Fillers) <- COLNAMES_DATA
        my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
      }
      my_names <- nameEXPT$my_names
      plot_number <- apply(plotEXPT$plot_num, 2 ,as.numeric)
      Col_checks <- col_checks
      
      rownames(layout1) <- paste("Row", nrow(layout1):1, sep = "")
      colnames(layout1) <- paste("Col", 1:ncol(layout1), sep = "")
      
      layout1_expt[[sky]] <- as.data.frame(layout1)
      Blocks_info_expt[[sky]] <- as.data.frame(Blocks_info)
      my_names_expt[[sky]] <- as.data.frame(my_names)
      plot_number_expt[[sky]] <- as.data.frame(plot_number)
      Col_checks_expt[[sky]] <- as.data.frame(Col_checks)
      sky <- sky - 1
    }
    
    layout1 <- dplyr::bind_rows(layout1_expt) 
    plot_number <- dplyr::bind_rows(plot_number_expt)  
    Col_checks <- dplyr::bind_rows(Col_checks_expt) 
    my_names <- dplyr::bind_rows(my_names_expt)  
    Blocks_info <- dplyr::bind_rows(Blocks_info_expt)
    
    if (locations == loc[1]) {
      layout1_loc1[[1]] <- layout1
      plot_loc1[[1]] <- plot_number
    } 
    results_to_export <- list(layout1, plot_number, Col_checks, my_names, Blocks_info)
    year <- format(Sys.Date(), "%Y")
    outputDesign <- export_design(G = results_to_export, 
                                  movement_planter = planter, 
                                  location = locationNames[locations],
                                  Year = year, 
                                  data_file = my_data_VLOOKUP, 
                                  reps = TRUE)
    if (Fillers > 0) {
      outputDesign$CHECKS <- ifelse(outputDesign$NAME == "Filler", "NA", outputDesign$CHECKS)
    }
    
    outputDesign_loc[[locations]] <- as.data.frame(outputDesign)
    layout_random_sites[[locations]] <- layout1
    layout_plots_sites[[locations]] <- plot_number
  }
  ##########################################################################################
  fieldbook <- dplyr::bind_rows(outputDesign_loc)
  ID <- 1:nrow(fieldbook)
  fieldbook <- fieldbook[, c(6:9,4,2,3,5,10,1,11)]
  fieldbook <- cbind(ID, fieldbook)
  colnames(fieldbook)[12] <- "TREATMENT"
  rownames(fieldbook) <- 1:nrow(fieldbook)
  
  fieldbook$EXPT <- factor(fieldbook$EXPT, levels = as.character(exptName))
  fieldbook$LOCATION <- factor(fieldbook$LOCATION, levels = as.character(locationNames))
  fieldbook <- fieldbook[order(fieldbook$LOCATION, fieldbook$EXPT),]
  
  fieldbook <- fieldbook[,-4]
  DataChecks <- data[1:checks,]
  
  layout_loc1 <- as.matrix(layout1_loc1[[1]])
  Plot_loc1 <- as.matrix(plot_loc1[[1]])
  checks <- as.numeric(nrow(DataChecks))
  field_dimensions <- c(rows = nrows_within_block * b, cols = ncols)
  blocks_dimensions <-  c(rows = nrows_within_block, cols = ncols)
  infoDesign <- list(
    #field_dimensions = field_dimensions,
    rows = as.numeric(field_dimensions[1]),
    columns = as.numeric(field_dimensions[2]),
    rows_within_blocks = as.numeric(blocks_dimensions[1]),
    columns_within_blocks = as.numeric(blocks_dimensions[2]),
    treatments = lines,
    checks = checks,
    blocks = b, 
    plots_per_block = plotsPerBlock, 
    locations = l, 
    fillers = Fillers, 
    seed = seed, 
    id_design = 14
  )
  output <- list(infoDesign = infoDesign, layoutRandom = layout_loc1, 
                 layout_random_sites = layout_random_sites,
                 layout_plots_sites = layout_plots_sites, 
                 plotNumber = Plot_loc1, exptNames = my_names, 
                 data_entry = data, fieldBook = fieldbook)
  class(output) <- "FielDHub"
  return(invisible(output))
}

#' @noRd 
#' 
#' 
set_augmented_blocks <- function(lines, checks, start = 5) {
  if (lines > 40) div <- 3 else div <- 2
  blocks <- start:ceiling(lines / div)
  b <- vector(mode = "numeric")
  checked_dims <- list()
  blocks_dims <- matrix(ncol = 2, byrow = TRUE)
  n <- 1
  for (i in blocks) {
    all_genotypes <- lines + checks * i
    plots_per_block <- base::ceiling(all_genotypes / i)
    lines_per_plot <- plots_per_block - checks
    excedent <- plots_per_block * i
    Fillers <- excedent - all_genotypes
    dims <- factor_subsets(plots_per_block, augmented = TRUE)$combos
    default_dim <- c(1 * i, plots_per_block)
    options_dims <- list(default_dim)
    if (!is.null(dims)) {
      for (k in 1:length(dims)) {
        options_dims[[k + 1]] <- as.vector(dims[[k]]) * c(i,1)
      }
    }
    for (m in 1:length(options_dims)) {
      if (Fillers < (options_dims[[m]][2] - checks - 1)) {
        dim_option <- as.vector(options_dims[[m]])
        dims_expt <- paste(dim_option[1], "x", dim_option[2], sep = " ")
        checked_dims[[n]] <- dims_expt
        b[n] <- i
        blocks_dims <- rbind(blocks_dims, c(i, dims_expt))
        n <- n + 1
      } 
    }
  }
  blocks_and_dims <- blocks_dims[-1,]
  if (!is.matrix(blocks_and_dims)) {
    blocks_and_dims <- matrix(data = blocks_and_dims, ncol = 2, byrow = TRUE)
  }
  return(list(b = b, 
              option_dims = checked_dims, 
              blocks_dims = blocks_and_dims))
}