#' @export
new_RCBD_augmented <- function(lines = NULL, checks = NULL, b = NULL, l = 1, planter = "serpentine", 
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
      } else plotNumber <- 1001
      message(cat("Warning message:", "\n", 
      "Since plotNumber was missing, it was set up to default value of: ", plotNumber))
    }
  } else stop("Number of locations/sites is missing")
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
  ############################################
  if (l < 1 || is.null(l)) base::stop("Check the input for the number of locations.")
  if (length(plotNumber) != l || is.null(plotNumber)) plotNumber <- seq(1001, 1000*(l+1), 1000)
  outputDesign_loc <- vector(mode = "list", length = l)
  if (is.null(exptName) || length(exptName) != repsExpt) exptName <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
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
      if (any(is.null(c(nrows, ncols)))) {
        nrows <- 1
        ncols <- plots_per_block
      } else {
        nrows <- nrows / b
        ncols <- ncols
      }
      # Blocks <- vector(mode = "list", length = b)
      # layout <- base::matrix(data = 0, nrow = b, ncol = ncols, byrow = TRUE)
      if (random) {
        if (Fillers >= (ncols - checks - 2)) {
          stop("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
        } 
        len_cuts <- rep(lines_per_plot, times = b - 1)
        len_cuts <- c(len_cuts, lines - sum(len_cuts))
        entries <- as.vector(data[(checks + 1):nrow(data),1])
        entries <- sample(entries)
        rand_len_cuts <- sample(len_cuts)
        lines_blocks <- split_vectors(x = entries, len_cuts = rand_len_cuts)
        print(lines_blocks)
        total_rows <- nrows * b
        datos <- sample(c(rep(0, nrows * ncols - checks),
                          rep(1, checks)))
        randomized_blocks <- setNames(vector(mode = "list", length = b), paste0("Block", 1:b))
        spots_for_checks <- setNames(vector(mode = "list", length = b), paste0("Block", 1:b))
        # randomized_blocks <- vector(mode = "list", length = b)
        # spots_for_checks <- vector(mode = "list", length = b)
        for (i in 1:b) {
          block <- matrix(data = sample(datos), nrow = nrows, 
                          ncol = ncols, 
                          byrow = TRUE)
          # block <- as.data.frame(
          #   matrix(data = sample(datos), nrow = nrows, ncol = ncols, byrow = TRUE)
          # )
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
            spots_for_checks[[i]] <- block #apply(block, c(1,2), as.character)
            block_with_checks <- block #apply(block, c(1,2), as.character)
            block_with_checks[block_with_checks == 0] <- sample(as.character(lines_blocks[[i]]))
            block_with_entries_checks <- block_with_checks
            # block_with_entries_checks <- as.data.frame(block_with_entries_checks)
            # colnames(block_with_entries_checks) <- paste0("col", 1:ncols)
            randomized_blocks[[i]] <- block_with_entries_checks
          } else {
            block[block == 1] <- sample(as.character(1:checks))
            block <- as.data.frame(block)
            colnames(block) <- paste0("col", 1:ncols)
            spots_for_checks[[i]] <- block #apply(block, c(1,2), as.character)
            block_with_checks <- block # apply(block, c(1,2), as.character)
            block_with_checks[block_with_checks == 0] <- sample(as.character(lines_blocks[[i]]))
            block_with_entries_checks <- block_with_checks
            block_with_entries_checks <- as.data.frame(block_with_entries_checks)
            colnames(block_with_entries_checks) <- paste0("col", 1:ncols)
            randomized_blocks[[i]] <- block_with_entries_checks
          }
        }
        layout <- dplyr::bind_rows(randomized_blocks)
        print(layout)
        layout <- as.matrix(layout)
        binary_matrix <- dplyr::bind_rows(spots_for_checks)
        print("binary_matrix")
        print(binary_matrix)
        binary_matrix <- as.matrix(binary_matrix)
        col_checks <- ifelse(binary_matrix != "0" & binary_matrix != "Filler", 1, 0)
        plotsPerBlock <- rep(ncol(layout), nrow(layout))
        plotsPerBlock <- c(plotsPerBlock[-length(plotsPerBlock)], ncol(layout) - Fillers)
      } else {
        entries <- as.vector(data[(checks + 1):nrow(data),1])
        if (Fillers > 0) {
          if (Fillers >= (plots_per_block - checks - 2)) {
            stop("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
          } 
          layout_a <- t(replicate(b,  sample(c(rep(0, plots_per_block - checks), 1:checks))))
          if (b %% 2 == 0) { 
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
          no_randomData <- NO_Random(checksMap = layout_a, data_Entry = entries, planter = planter)
          layout <- no_randomData$w_map_letters
          len_cuts <- no_randomData$len_cut 
          plotsPerBlock <- c(len_cuts[-length(len_cuts)], ncol(layout_a) - Fillers)
        } else {
          layout_a <- t(replicate(b,  sample(c(rep(0, plots_per_block - checks), 1:checks))))
          col_checks <- ifelse(layout_a != 0, 1,0)
          no_randomData <- NO_Random(checksMap = layout_a, data_Entry = entries, planter = planter)
          layout <- no_randomData$w_map_letters
          plotsPerBlock <- no_randomData$len_cut 
        }
      }
      Blocks_info <- matrix(data = rep(b:1, each = plots_per_block), nrow = b, ncol = plots_per_block, byrow = TRUE)
      new_exptName <- rev(exptName)
      nameEXPT <- ARCBD_name(Fillers = Fillers, b = b, layout = layout, name.expt = exptName[expts], planter = planter)
      plotEXPT <- ARCBD_plot_number(plot.number = plotNumber[locations], planter = planter, b = b, name.expt = exptName[expts],
                                    Fillers = Fillers, nameEXPT = nameEXPT$my_names)
      my_data_VLOOKUP <- data
      COLNAMES_DATA <- colnames(my_data_VLOOKUP)
      layout1 <- layout
      if (Fillers > 0) {
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
  
  infoDesign <- list(Blocks = b, plotsPerBlock = plotsPerBlock, Checks = DataChecks, 
                     entries = entries, repsExpt = repsExpt, numberLocations = l, 
                     Fillers = Fillers, seed = seed, id_design = 14)
  output <- list(infoDesign = infoDesign, layoutRandom = layout_loc1, layout_random_sites = layout_random_sites,
                 layout_plots_sites = layout_plots_sites, plotNumber = Plot_loc1, exptNames = my_names, 
                 data_entry = data, fieldBook = fieldbook)
  class(output) <- "FielDHub"
  return(invisible(output))
}
