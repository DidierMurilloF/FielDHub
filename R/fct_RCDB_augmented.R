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
#'
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
                           repsExpt = 1, random = TRUE, data = NULL) {

  if (all(c("serpentine", "cartesian") != planter)) {
    stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  if (is.null(seed)) seed <- runif(1, min=-50000, max=50000)
  set.seed(seed)
  
  if (is.null(lines) || is.null(checks) || is.null(b) || is.null(l)) {
    shiny::validate('Some of the basic design parameters are missing (lines, checks, b, l).')
  }
  if(is.null(repsExpt)) repsExpt <- 1
  arg1 <- list(lines, b, l, repsExpt);arg2 <- c(lines, b, l, repsExpt)
  if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
    shiny::validate('RCBD_augmented() requires input lines, b and l to be possitive integers.')
  }
  if (!is.null(plotNumber) && is.numeric(plotNumber)) {
    if(any(plotNumber %% 1 != 0) || any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
      shiny::validate("RCBD_augmented() requires input plotNumber to be possitive integers and sorted.")
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
  }else {
    NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
              paste(rep("G", lines), (checks + 1):(lines + checks), sep = ""))
    data <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
  }
  all_genotypes <- lines + checks * b
  plots_per_block <- base::ceiling(all_genotypes/b)
  if(is.null(locationNames) || length(locationNames) != l) locationNames <- 1:l
  lines_per_plot <- plots_per_block - checks
  excedent <- plots_per_block * b
  Fillers <- excedent - all_genotypes
  dim_block <- plots_per_block
  ############################################
  if(l < 1 || is.null(l)) base::stop("Check the input for the number of locations.")
  if (length(plotNumber) != l || is.null(plotNumber)) plotNumber <- seq(1001, 1000*(l+1), 1000)
  outputDesign_loc <- vector(mode = "list", length = l)
  if (is.null(exptName) || length(exptName) != repsExpt) exptName <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
  loc <- 1:l
  expt <- 1:repsExpt
  layout1_loc1 <- vector(mode = "list", length = 1)
  plot_loc1 <- vector(mode = "list", length = 1)
  for (locations in loc) {
    sky <- length(expt)
    layout1_expt <- vector(mode = "list", length = repsExpt)
    Blocks_info_expt <- vector(mode = "list", length = repsExpt)
    my_names_expt <- vector(mode = "list", length = repsExpt)
    plot_number_expt <- vector(mode = "list", length = repsExpt)
    Col_checks_expt <- vector(mode = "list", length = repsExpt)
    for (expts in expt) {
      Blocks <- vector(mode = "list", length = b)
      layout <- base::matrix(data = 0, nrow = b, ncol = plots_per_block, byrow = TRUE)
      if (random) {
        if (Fillers > 0) {
          if (Fillers >= (plots_per_block - checks - 2)) {
            shiny::validate("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
          } 
          len_cuts <- rep(lines_per_plot, times = b - 1)
          len_cuts <- c(len_cuts, lines - sum(len_cuts))
          entries <- as.vector(data[(checks + 1):nrow(data),1])
          entries <- sample(entries)
          rand_len_cuts <- sample(len_cuts)
          lines_blocks <- split_vectors(x = entries, len_cuts = rand_len_cuts)
          if (b %% 2 == 0) { 
            if(planter == "serpentine") {
              layout[1,1:Fillers] <- "Filler"
            }else{ 
              layout[1,((ncol(layout) + 1) - Fillers):ncol(layout)] <- "Filler" 
            } 
          }else { 
            layout[1,((ncol(layout) + 1) - Fillers):ncol(layout)] <- "Filler" 
          }
          z <- b
          w <- 1
          for (i in b:1) {
            datos <- sample(c(rep(0, len_cuts[w]), 1:checks))
            Bi <- sample(datos)
            ci <- as.vector(layout[i,])
            ci[ci == 0] <- Bi
            layout[i,] <- ci
            Blocks[[z]] <- ci
            z <- z - 1
            w <- w + 1
          }
          col_checks <- ifelse(layout != 0, 1, 0)
          v <- which.min(rand_len_cuts)
          Block_Fillers <- lines_blocks[v]
          blocks_data <- 1:b
          blocks_data <- blocks_data[-v]
          random_lines_in_b <- sample(blocks_data)
          BF <- as.vector(layout[1,])
          BF[BF == 0] <- sample(Block_Fillers[[1]])
          layout[1,] <- BF 
          Blocks[[b]] <- BF
          s <- 1
          for (j in 2:b) {
            z <- random_lines_in_b[j-1]
            Bi <- sample(lines_blocks[[z]])
            ci <- as.vector(layout[j,])
            ci[ci == 0] <- Bi
            layout[j,] <- ci
            Blocks[[s]] <- ci
            s <- s + 1
          }
          plotsPerBlock <- rep(ncol(layout), nrow(layout))
          plotsPerBlock <- c(plotsPerBlock[-length(plotsPerBlock)], ncol(layout) - Fillers)
        }else {
          datos <- sample(c(rep(0, lines_per_plot), 1:checks))
          len_cuts <- rep(lines_per_plot, times = b)
          z <- b
          for (i in 1:b) {
            Bi <- sample(datos)
            ci <- as.vector(layout[i,])
            ci[ci == 0] <- Bi
            layout[i,] <- ci
            Blocks[[z]] <- ci
            z <- z - 1
          }
          col_checks <- ifelse(layout != 0, 1,0)
          blocks_data <- 1:b
          random_lines_in_b <- sample(blocks_data)
          entries <- as.vector(data[(checks + 1):nrow(data),1])
          entries <- sample(entries)
          lines_blocks <- split_vectors(x = entries, len_cuts = len_cuts)
          s <- 1
          for (j in 1:b) {
            z <- random_lines_in_b[j]
            Bi <- sample(lines_blocks[[z]])
            cj <- as.vector(layout[j,])
            cj[cj == 0] <- Bi
            layout[j,] <- cj
            Blocks[[s]] <- cj
            s <- s + 1
          }
          plotsPerBlock <- rep(ncol(layout), nrow(layout))
        }
      }else {
        entries <- as.vector(data[(checks + 1):nrow(data),1])
        if (Fillers > 0) {
          if (Fillers >= (plots_per_block - checks - 2)) {
            shiny::validate("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
          } 
          layout_a <- t(replicate(b,  sample(c(rep(0, plots_per_block - checks), 1:checks))))
          if (b %% 2 == 0) { 
            if(planter == "serpentine") {
              layout_a[1,] <- c(rep("Filler", Fillers), 
                                sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)), 
                                       size = ncol(layout_a) - Fillers, replace = FALSE))
            }else{
              layout_a[1,] <- c(sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)), 
                                       size = ncol(layout_a) - Fillers, replace = FALSE),
                                rep("Filler", Fillers))
            } 
          }else {
            layout_a[1,] <- c(sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)), 
                                     size = ncol(layout_a) - Fillers, replace = FALSE),
                              rep("Filler", Fillers))
          }
          col_checks <- ifelse(layout_a != 0, 1,0)
          no_randomData <- NO_Random(checksMap = layout_a, data_Entry = entries, planter = planter)
          layout <- no_randomData$w_map_letters
          len_cuts <- no_randomData$len_cut 
          plotsPerBlock <- c(len_cuts[-length(len_cuts)], ncol(layout_a) - Fillers)
        }else {
          layout_a <- t(replicate(b,  sample(c(rep(0, plots_per_block - checks), 1:checks))))
          col_checks <- ifelse(layout_a != 0, 1,0)
          no_randomData <- NO_Random(checksMap = layout_a, data_Entry = entries, planter = planter)
          layout <- no_randomData$w_map_letters
          plotsPerBlock <- no_randomData$len_cut 
        }
      }
      Blocks_info <- base::matrix(data = rep(b:1, each = plots_per_block), nrow = b, ncol = plots_per_block, byrow = TRUE)
      new_exptName <- rev(exptName)
      nameEXPT <- ARCBD_name(Fillers = Fillers, b = b, layout = layout, name.expt = exptName[expts], planter = planter)
      plotEXPT <- ARCBD_plot_number(plot.number = plotNumber[locations], planter = planter, b = b, name.expt = exptName[expts],
                                    Fillers = Fillers, nameEXPT = nameEXPT$my_names)
      my_data_VLOOKUP <- data
      COLNAMES_DATA <- colnames(my_data_VLOOKUP)
      layout1 <- layout
      if(Fillers > 0) {
        layout1[layout1 == "Filler"] <- 0
        layout1 <- apply(layout1, 2 ,as.numeric)
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
    outputDesign <- export_design(G = results_to_export, movement_planter = planter, location = locationNames[locations],
                                  Year = year, data_file = my_data_VLOOKUP, reps = TRUE)
    if(Fillers > 0) {
      outputDesign$CHECKS <- ifelse(outputDesign$NAME == "Filler", "NA", outputDesign$CHECKS)
    }
    
    outputDesign_loc[[locations]] <- as.data.frame(outputDesign)
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
  
  infoDesign <- list(Blocks = b, plotsPerBlock = plotsPerBlock, Checks = DataChecks, 
                     entries = entries, repsExpt = repsExpt, numberLocations = l, 
                     Fillers = Fillers, seed = seed, idDesign = 14)
  output <- list(infoDesign = infoDesign, layoutRandom = layout_loc1,
                 plotNumber = Plot_loc1, exptNames = my_names, data_entry = data, 
                 fieldBook = fieldbook)
  class(output) <- "FielDHub"
  return(invisible(output))
}