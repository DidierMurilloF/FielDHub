#' @noRd
stack_reps <- function(x_list, repsStack = c("vertical", "horizontal")) {
  repsStack <- match.arg(repsStack)
  
  # x_list is a list of data.frames (same dims)
  if (length(x_list) == 1) return(x_list[[1]])
  
  mats <- lapply(x_list, function(x) {
    if (is.data.frame(x)) as.matrix(x) else x
  })
  
  out <- if (repsStack == "vertical") {
    do.call(rbind, mats)
  } else {
    do.call(cbind, mats)
  }
  
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Generates an Augmented Randomized Complete Block Design (ARCBD)
#'
#' @description It randomly generates an augmented randomized complete block design across locations (ARCBD).
#'
#' @param lines Treatments, number of lines for test.
#' @param checks Number of checks per augmented block.
#' @param b Number of augmented blocks.
#' @param l Number of locations. By default \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. By default \code{plotNumber = 101}.
#' @param repsStack Option for \code{horizontal} or \code{vertical} layout By default \code{repsStack = 'vertical'}.
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
#' @export
RCBD_augmented <- function(lines = NULL, checks = NULL, b = NULL, l = 1, 
                           planter = "serpentine", plotNumber = 101, 
                           repsStack = c("vertical", "horizontal"),
                           exptName = NULL, seed = NULL, locationNames = NULL, 
                           repsExpt = 1, random = TRUE, data = NULL, 
                           nrows = NULL, ncols = NULL) {
  repsStack <- match.arg(repsStack)
  if (all(c("serpentine", "cartesian") != planter)) {
    stop("Input planter choice is unknown. Please, choose one: 'serpentine' or 'cartesian'.")
  }
  if (is.null(seed)) seed <- runif(1, min = -50000, max = 50000)
  set.seed(seed)
  if (!is.numeric(plotNumber) && !is.integer(plotNumber)) {
    stop("plotNumber should be an integer or a numeric vector.")
  }
  if (any(plotNumber %% 1 != 0)) {
    stop("plotNumber should be integers.")
  }
  
  if (!is.null(l)) {
    if (is.null(plotNumber) || !(length(plotNumber) %in% c(l, repsExpt, l * repsExpt))) {
      if (l > 1) {
        plotNumber <- seq(1001, 1000 * (l + 1), 1000)
        message(cat(
          "Warning message:", "\n",
          "Since plotNumber was missing, it was set up to default value of: ", plotNumber,
          "\n", "\n"
        ))
      } else {
        plotNumber <- 1001
        message(cat(
          "Warning message:", "\n",
          "Since plotNumber was missing, it was set up to default value of: ", plotNumber,
          "\n", "\n"
        ))
      }
    }
  } else {
    stop("Number of locations/sites is missing")
  }
  
  if (is.null(lines) || is.null(checks) || is.null(b) || is.null(l)) {
    stop("Some of the basic design parameters are missing (lines, checks, b, l).")
  }
  if (is.null(repsExpt)) repsExpt <- 1
  
  arg1 <- list(lines, b, l, repsExpt)
  arg2 <- c(lines, b, l, repsExpt)
  if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
    stop("RCBD_augmented() requires input lines, b and l to be possitive integers.")
  }
  if (!is.null(plotNumber) && is.numeric(plotNumber)) {
    if (any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
      stop("RCBD_augmented() requires input plotNumber to be possitive integers and sorted.")
    }
  }
  
  if (!is.null(data)) {
    data <- as.data.frame(data)
    if (ncol(data) < 2) base::stop("Data input needs at least two columns with: ENTRY and NAME.")
    data <- data[, 1:2]
    data <- na.omit(data)
    colnames(data) <- c("ENTRY", "NAME")
    new_lines <- nrow(data) - checks
    if (lines != new_lines) base::stop("Number of experimental lines do not match with data input provided.")
    lines <- new_lines
  } else {
    NAME <- c(
      paste(rep("CH", checks), 1:checks, sep = ""),
      paste(rep("G", lines), (checks + 1):(lines + checks), sep = "")
    )
    data <- data.frame(list(ENTRY = 1:(lines + checks), NAME = NAME))
  }
  
  all_genotypes <- lines + checks * b
  plots_per_block <- base::ceiling(all_genotypes / b)
  excedent <- plots_per_block * b
  Fillers <- excedent - all_genotypes
  
  if (!is.null(locationNames)) {
    if (length(locationNames) == l) {
      locationNames <- toupper(locationNames)
    } else {
      locationNames <- 1:l
    }
  } else {
    locationNames <- 1:l
  }
  
  if (l < 1 || is.null(l)) base::stop("Check the input for the number of locations.")
  if (is.null(plotNumber) || !(length(plotNumber) %in% c(l, repsExpt, l * repsExpt))) {
    plotNumber <- seq(1001, 1000 * (l + 1), 1000)
  }
  
  outputDesign_loc <- vector(mode = "list", length = l)
  if (is.null(exptName) || length(exptName) != repsExpt) {
    exptName <- paste(rep("Expt", repsExpt), 1:repsExpt, sep = "")
  }
  
  # -----------------------------
  # NEW: infer within-block dims + block grid (vertical or side-by-side)
  # -----------------------------
  if (is.null(nrows) || is.null(ncols)) {
    nrows_within_block <- 1
    ncols_within_block <- plots_per_block
    blocks_per_col <- b
    blocks_per_row <- 1
    field_rows <- nrows_within_block * blocks_per_col
    field_cols <- ncols_within_block * blocks_per_row
  } else {
    if (nrows %% 1 != 0 || ncols %% 1 != 0 || nrows < 1 || ncols < 1) {
      stop("nrows and ncols must be positive integers.")
    }
    # We no longer require nrows %% b == 0 because blocks can be side-by-side.
    # We infer a grid (blocks_per_col x blocks_per_row) where blocks_per_col * blocks_per_row = b.
    
    inferred <- infer_arcbd_grid_dims(
      nrows = nrows,
      ncols = ncols,
      b = b,
      plots_per_block = plots_per_block
    )
    
    if (is.null(inferred)) {
      # Feedback (now includes side-by-side options)
      set_blocks <- set_augmented_blocks(lines = lines, checks = checks)
      blocks_dims <- set_blocks$blocks_dims
      colnames(blocks_dims) <- c("BLOCKS", "DIMENSIONS")
      feedback <- as.data.frame(blocks_dims)
      
      message(cat(
        "\n", "Error in RCBD_augmented(): ", "\n", "\n",
        "Field dimensions do not fit with the data entered!", "\n",
        "Try one of the following options: ", "\n"
      ))
      return(print(feedback))
    }
    
    nrows_within_block <- inferred$rows_within_block
    ncols_within_block <- inferred$cols_within_block
    blocks_per_col <- inferred$blocks_per_col
    blocks_per_row <- inferred$blocks_per_row
    field_rows <- nrows
    field_cols <- ncols
  }
  
  # Use within-block columns downstream where the algorithm expects "ncols"
  ncols <- ncols_within_block
  nrows_within_block <- nrows_within_block
  
  # -----------------------------
  # Feedback check (updated to match inferred full field dims)
  # -----------------------------
  # set_blocks <- set_augmented_blocks(lines = lines, checks = checks)
  # blocks_arcbd <- set_blocks$b
  # if (length(blocks_arcbd) == 0) {
  #   stop("No options available for that amount of treatments!", call. = FALSE)
  # }
  # blocks_dims <- set_blocks$blocks_dims
  # colnames(blocks_dims) <- c("BLOCKS", "DIMENSIONS")
  # feedback <- as.data.frame(blocks_dims)
  # set_dims <- paste(field_rows, field_cols, sep = " x ")
  # inputs_subset <- subset(feedback, feedback[, 1] == b & feedback[, 2] == set_dims)
  # # if (nrow(inputs_subset) == 0) {
  # #   message(cat(
  # #     "\n", "Error in RCBD_augmented(): ", "\n", "\n",
  # #     "Field dimensions do not fit with the data entered!", "\n",
  # #     "Try one of the following options: ", "\n"
  # #   ))
  # #   return(print(feedback))
  # # }
  # 
  # if (nrow(inputs_subset) == 0) {
  #   width  <- 55
  #   border <- paste(rep("=", width), collapse = "")
  #   thin   <- paste(rep("-", width), collapse = "")
  #   
  #   cat("\n")
  #   cat(border, "\n")
  #   cat("  ERROR: RCBD_augmented()\n")
  #   cat(thin, "\n")
  #   cat("  Field dimensions do not match the data entered.\n")
  #   cat("  Total plots in data:", lines + checks * b, "\n")
  #   cat("  Field size provided:", field_rows, "x", field_cols, "=", field_rows * field_cols, "plots\n")
  #   cat(thin, "\n")
  #   cat("  Valid dimension options:\n\n")
  #   for (i in seq_len(nrow(feedback))) {
  #     cat(sprintf("   [%2d ]  %s blocks  x  %s\n", i, feedback[i, 1], feedback[i, 2]))
  #   }
  #   cat(border, "\n\n")
  #   return(invisible(NULL))
  # }
  
  
  
  set_blocks <- set_augmented_blocks(lines = lines, checks = checks, start = 3)
  blocks_arcbd <- set_blocks$b
  
  if (length(blocks_arcbd) == 0) {
    stop("No options available for that amount of treatments!", call. = FALSE)
  }
  
  blocks_dims <- set_blocks$blocks_dims
  colnames(blocks_dims) <- c("BLOCKS", "DIMENSIONS")
  feedback <- as.data.frame(blocks_dims)
  
  width  <- 55
  border <- paste(rep("=", width), collapse = "")
  thin   <- paste(rep("-", width), collapse = "")
  
  # Check if b is less than the minimum valid number of blocks
  if (b < min(blocks_arcbd)) {
    cat("\n")
    cat(border, "\n")
    cat("  ERROR: RCBD_augmented()\n")
    cat(thin, "\n")
    cat("  The number of blocks requested is too small.\n")
    cat("  Blocks requested:", b, "\n")
    cat("  Minimum blocks allowed:", min(blocks_arcbd), "\n")
    cat("  Maximum blocks allowed:", max(blocks_arcbd), "\n")
    cat(thin, "\n")
    cat("  Valid dimension options:\n\n")
    for (i in seq_len(nrow(feedback))) {
      cat(sprintf("   [%2d ]  %s blocks :  %s\n", i, feedback[i, 1], feedback[i, 2]))
    }
    cat(border, "\n\n")
    return(invisible(NULL))
  }
  
  set_dims <- paste(field_rows, field_cols, sep = " x ")
  inputs_subset <- subset(feedback, feedback[, 1] == b & feedback[, 2] == set_dims)
  
  if (nrow(inputs_subset) == 0) {
    cat("\n")
    cat(border, "\n")
    cat("  ERROR: RCBD_augmented()\n")
    cat(thin, "\n")
    cat("  Field dimensions do not match the data entered.\n")
    cat("  Total plots in data:", lines + checks * b, "\n")
    cat("  Field size provided:", field_rows, "x", field_cols, "=", field_rows * field_cols, "plots\n")
    cat(thin, "\n")
    cat("  Valid dimension options:\n\n")
    for (i in seq_len(nrow(feedback))) {
      cat(sprintf("   [%2d ]  %s blocks :  %s\n", i, feedback[i, 1], feedback[i, 2]))
    }
    cat(border, "\n\n")
    return(invisible(NULL))
  }
  
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
        # if (Fillers > (ncols - checks - 1)) {
        if (Fillers > 0 && Fillers > (ncols - checks - 1)) {
          stop("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
        }
        
        lines_per_plot <- plots_per_block - checks
        len_cuts <- rep(lines_per_plot, times = b - 1)
        len_cuts <- c(len_cuts, lines - sum(len_cuts))
        entries <- as.vector(data[(checks + 1):nrow(data), 1])
        entries <- sample(entries)
        rand_len_cuts <- sample(len_cuts)
        lines_blocks <- split_vectors(x = entries, len_cuts = rand_len_cuts)
        
        total_rows <- field_rows
        datos <- sample(c(
          rep(0, times = nrows_within_block * ncols - checks),
          rep(1, checks)
        ))
        
        randomized_blocks <- setNames(vector(mode = "list", length = b), paste0("Block", 1:b))
        spots_for_checks <- setNames(vector(mode = "list", length = b), paste0("Block", 1:b))
        
        for (i in 1:b) {
          block <- matrix(
            data = sample(datos),
            nrow = nrows_within_block,
            ncol = ncols,
            byrow = TRUE
          )
          
          if (Fillers > 0 && i == 1) {
            if (total_rows %% 2 == 0) {
              if (planter == "serpentine") {
                block[1, 1:Fillers] <- "Filler"
              } else {
                block[1, ((ncol(block) + 1) - Fillers):ncol(block)] <- "Filler"
              }
            } else {
              block[1, ((ncol(block) + 1) - Fillers):ncol(block)] <- "Filler"
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
            randomized_blocks[[i]] <- block_with_checks
          } else {
            block[block == 1] <- sample(as.character(1:checks))
            block <- as.data.frame(block)
            colnames(block) <- paste0("col", 1:ncols)
            spots_for_checks[[i]] <- block
            
            block_with_checks <- block
            block_with_checks[block_with_checks == 0] <- sample(as.character(lines_blocks[[i]]))
            block_with_entries_checks <- as.data.frame(block_with_checks)
            colnames(block_with_entries_checks) <- paste0("col", 1:ncols)
            randomized_blocks[[i]] <- block_with_entries_checks
          }
        }
        
        layout <- assemble_arcbd_blocks(block_list = randomized_blocks, blocks_per_col = blocks_per_col, blocks_per_row = blocks_per_row)
        binary_matrix <- assemble_arcbd_blocks(block_list = spots_for_checks, blocks_per_col = blocks_per_col, blocks_per_row = blocks_per_row)
        col_checks <- ifelse(binary_matrix != "0" & binary_matrix != "Filler", 1, 0)
        
        plotsPerBlock <- rep(ncols * nrows_within_block, b)
        plotsPerBlock <- c(plotsPerBlock[-length(plotsPerBlock)], ncols * nrows_within_block - Fillers)
      } else {
        # if (Fillers > (ncols - checks - 1)) {
        if (Fillers > 0 && Fillers > (ncols - checks - 1)){
          stop("Number of Filler overcome the amount allowed per block. Please, choose another quantity of blocks.")
        }
        
        fun <- function(x) {
          matrix(
            data = sample(c(rep(0, (nrows_within_block * ncols) - checks), 1:checks)),
            nrow = nrows_within_block,
            byrow = TRUE
          )
        }
        
        entries <- as.vector(data[(checks + 1):nrow(data), 1])
        blocks_with_checks <- lapply(1:b, fun)
        
        layout_a <- assemble_arcbd_blocks(
          block_list = blocks_with_checks,
          blocks_per_col = blocks_per_col,
          blocks_per_row = blocks_per_row
        )
        
        if (Fillers > 0) {
          if (field_rows %% 2 == 0) {
            if (planter == "serpentine") {
              layout_a[1, ] <- c(
                rep("Filler", Fillers),
                sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)),
                       size = ncol(layout_a) - Fillers, replace = FALSE
                )
              )
            } else {
              layout_a[1, ] <- c(
                sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)),
                       size = ncol(layout_a) - Fillers, replace = FALSE
                ),
                rep("Filler", Fillers)
              )
            }
          } else {
            layout_a[1, ] <- c(
              sample(c(1:checks, rep(0, ncol(layout_a) - Fillers - checks)),
                     size = ncol(layout_a) - Fillers, replace = FALSE
              ),
              rep("Filler", Fillers)
            )
          }
        }
        
        col_checks <- ifelse(layout_a != 0, 1, 0)
        
        no_randomData <- no_random_arcbd(
          checksMap = layout_a,
          data_Entry = entries,
          planter = planter
        )
        
        layout <- no_randomData$w_map_letters
        
        plotsPerBlock <- rep(ncols * nrows_within_block, b)
        if (Fillers > 0) {
          plotsPerBlock <- c(plotsPerBlock[-length(plotsPerBlock)], ncols * nrows_within_block - Fillers)
        }
      }
      
      # Blocks info (keep the same "rev" convention as before)
      block_ids <- rev(seq_len(b))
      block_info_list <- lapply(seq_len(b), function(ii) {
        matrix(block_ids[ii], nrow = nrows_within_block, ncol = ncols, byrow = TRUE)
      })
      Blocks_info <- assemble_arcbd_blocks(block_list = block_info_list, blocks_per_col = blocks_per_col, blocks_per_row = blocks_per_row)
      
      nameEXPT <- ARCBD_name(
        Fillers = Fillers,
        b = field_rows,
        layout = layout,
        name.expt = exptName[expts],
        planter = planter
      )
      
      plot_start <- if (length(plotNumber) == l) {
        plotNumber[locations]
      } else if (length(plotNumber) == repsExpt) {
        plotNumber[expts]
      } else {
        plotNumber[(locations - 1) * repsExpt + expts]
      }
      
      plotEXPT <- ARCBD_plot_number(
        plot.number = plot_start,
        planter = planter,
        b = field_rows,
        name.expt = exptName[expts],
        Fillers = Fillers,
        nameEXPT = nameEXPT$my_names
      )
      
      my_data_VLOOKUP <- data
      COLNAMES_DATA <- colnames(my_data_VLOOKUP)
      layout1 <- layout
      
      if (Fillers > 0) {
        layout1[layout1 == "Filler"] <- 0
        layout1 <- apply(layout1, 2, as.numeric)
        Entry_Fillers <- data.frame(list(0, "Filler"))
        colnames(Entry_Fillers) <- COLNAMES_DATA
        my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
      }
      
      my_names <- nameEXPT$my_names
      plot_number <- apply(plotEXPT$plot_num, 2, as.numeric)
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
    
    # ---- MINIMAL FIX: keep EXPT1|EXPT2|EXPT3|EXPT4 when stacking horizontally ----
    if (repsStack == "horizontal") {
      layout1_expt <- rev(layout1_expt)
      plot_number_expt <- rev(plot_number_expt)
      Col_checks_expt <- rev(Col_checks_expt)
      my_names_expt <- rev(my_names_expt)
      Blocks_info_expt <- rev(Blocks_info_expt)
    }
    # -----------------------------------------------------------------------------
    
    layout1 <- stack_reps(layout1_expt, repsStack = repsStack)
    plot_number <- stack_reps(plot_number_expt, repsStack = repsStack)
    Col_checks <- stack_reps(Col_checks_expt, repsStack = repsStack)
    my_names <- stack_reps(my_names_expt, repsStack = repsStack)
    Blocks_info <- stack_reps(Blocks_info_expt, repsStack = repsStack)
    
    if (locations == loc[1]) {
      layout1_loc1[[1]] <- layout1
      plot_loc1[[1]] <- plot_number
    }
    
    results_to_export <- list(layout1, plot_number, Col_checks, my_names, Blocks_info)
    year <- format(Sys.Date(), "%Y")
    outputDesign <- export_design(
      G = results_to_export,
      movement_planter = planter,
      location = locationNames[locations],
      Year = year,
      data_file = my_data_VLOOKUP,
      reps = TRUE
    )
    
    if (Fillers > 0) {
      outputDesign$CHECKS <- ifelse(outputDesign$NAME == "Filler", "NA", outputDesign$CHECKS)
    }
    
    outputDesign_loc[[locations]] <- as.data.frame(outputDesign)
    layout_random_sites[[locations]] <- layout1
    layout_plots_sites[[locations]] <- plot_number
  }
  
  fieldbook <- dplyr::bind_rows(outputDesign_loc)
  ID <- 1:nrow(fieldbook)
  fieldbook <- fieldbook[, c(6:9, 4, 2, 3, 5, 10, 1, 11)]
  fieldbook <- cbind(ID, fieldbook)
  colnames(fieldbook)[12] <- "TREATMENT"
  rownames(fieldbook) <- 1:nrow(fieldbook)
  
  fieldbook$EXPT <- factor(fieldbook$EXPT, levels = as.character(exptName))
  fieldbook$LOCATION <- factor(fieldbook$LOCATION, levels = as.character(locationNames))
  fieldbook <- fieldbook[order(fieldbook$LOCATION, fieldbook$EXPT), ]
  fieldbook <- fieldbook[, -4]
  
  DataChecks <- data[1:checks, ]
  layout_loc1 <- as.matrix(layout1_loc1[[1]])
  Plot_loc1 <- as.matrix(plot_loc1[[1]])
  checks <- as.numeric(nrow(DataChecks))
  
  full_rows <- if (repsStack == "vertical") field_rows * repsExpt else field_rows
  full_cols <- if (repsStack == "vertical") field_cols else field_cols * repsExpt
  
  infoDesign <- list(
    rows = as.numeric(full_rows),
    columns = as.numeric(full_cols),
    rows_within_blocks = as.numeric(nrows_within_block),
    columns_within_blocks = as.numeric(ncols_within_block),
    treatments = lines,
    checks = checks,
    blocks = b,
    plots_per_block = plotsPerBlock,
    locations = l,
    fillers = Fillers,
    seed = seed,
    id_design = 14
  )
  
  output <- list(
    infoDesign = infoDesign,
    layoutRandom = layout_loc1,
    layout_random_sites = layout_random_sites,
    layout_plots_sites = layout_plots_sites,
    plotNumber = Plot_loc1,
    exptNames = my_names,
    data_entry = data,
    fieldBook = fieldbook
  )
  
  class(output) <- "FielDHub"
  return(invisible(output))
}

#' @noRd
set_augmented_blocks <- function(lines, checks, start = 5) {
  if (lines > 40) div <- 3 else div <- 2
  blocks <- start:ceiling(lines / div)
  
  b_out <- vector(mode = "numeric")
  checked_dims <- list()
  blocks_dims <- matrix(ncol = 2, byrow = TRUE)
  n <- 1
  
  for (i in blocks) {
    all_genotypes <- lines + checks * i
    plots_per_block <- base::ceiling(all_genotypes / i)
    excedent <- plots_per_block * i
    Fillers <- excedent - all_genotypes
    
    # Candidate within-block dims (r_block x c_block) with r_block * c_block = plots_per_block
    # --- minimal change: include BOTH orientations (r,c) and (c,r) ---
    within_dims <- list(c(1, plots_per_block))
    
    dims <- factor_subsets(plots_per_block, augmented = TRUE)$combos
    if (!is.null(dims)) {
      for (k in seq_along(dims)) {
        rc <- as.vector(dims[[k]])
        within_dims[[length(within_dims) + 1]] <- rc
        if (rc[1] != rc[2]) {
          within_dims[[length(within_dims) + 1]] <- rev(rc)
        }
      }
    }
    
    # unique within dims
    within_dims <- unique(lapply(within_dims, function(x) paste(x[1], x[2], sep = "x")))
    within_dims <- lapply(within_dims, function(s) as.numeric(strsplit(s, "x", fixed = TRUE)[[1]]))
    # --- end minimal change ---
    
    # Candidate block grid factors (blocks_per_col x blocks_per_row) with product = i
    grid_pairs <- factor_pairs(i)
    
    # Build all field dims: (r_block*blocks_per_col) x (c_block*blocks_per_row)
    options_dims <- list()
    for (wd in within_dims) {
      r_block <- wd[1]
      c_block <- wd[2]
      
      # Filler feasibility must be evaluated against the within-block columns
      # if (Fillers > (c_block - checks - 1)) next
      if (Fillers > 0 && Fillers > (c_block - checks - 1)) next
      
      for (gp in grid_pairs) {
        blocks_per_col <- gp[1]
        blocks_per_row <- gp[2]
        field_r <- r_block * blocks_per_col
        field_c <- c_block * blocks_per_row
        options_dims[[length(options_dims) + 1]] <- c(field_r, field_c)
      }
    }
    
    # # Unique options
    # options_dims <- unique(lapply(options_dims, function(x) paste(x[1], x[2], sep = "x")))
    # options_dims <- lapply(options_dims, function(s) as.numeric(strsplit(s, "x", fixed = TRUE)[[1]]))
    
    options_dims <- unique(lapply(options_dims, function(x) paste(x[1], x[2], sep = "x")))
    options_dims <- lapply(options_dims, function(s) as.numeric(strsplit(s, "x", fixed = TRUE)[[1]]))
    
    # ---- FILTER OUT DEGENERATE FIELD DIMS: 1xN or Nx1 ----
    options_dims <- Filter(function(v) {
      length(v) == 2 && all(!is.na(v)) && v[1] > 1 && v[2] > 1
    }, options_dims)
    # ------------------------------------------------------
    
    for (m in seq_along(options_dims)) {
      dim_option <- options_dims[[m]]
      dims_expt <- paste(dim_option[1], "x", dim_option[2], sep = " ")
      checked_dims[[n]] <- dims_expt
      b_out[n] <- i
      blocks_dims <- rbind(blocks_dims, c(i, dims_expt))
      n <- n + 1
    }
  }
  
  blocks_and_dims <- blocks_dims[-1, ]
  if (!is.matrix(blocks_and_dims)) {
    blocks_and_dims <- matrix(data = blocks_and_dims, ncol = 2, byrow = TRUE)
  }
  
  return(list(
    b = b_out,
    option_dims = checked_dims,
    blocks_dims = blocks_and_dims
  ))
}

#' @noRd
factor_pairs <- function(n) {
  out <- list()
  k <- 1
  for (a in seq_len(n)) {
    if (n %% a == 0) {
      out[[k]] <- c(a, n / a)
      k <- k + 1
    }
  }
  out
}

#' @noRd
infer_arcbd_grid_dims <- function(nrows, ncols, b, plots_per_block) {
  # --- minimal change: include BOTH orientations (r,c) and (c,r) ---
  within_dims <- list(c(1, plots_per_block))
  
  dims <- factor_subsets(plots_per_block, augmented = TRUE)$combos
  if (!is.null(dims)) {
    for (k in seq_along(dims)) {
      rc <- as.vector(dims[[k]])
      within_dims[[length(within_dims) + 1]] <- rc
      if (rc[1] != rc[2]) {
        within_dims[[length(within_dims) + 1]] <- rev(rc)
      }
    }
  }
  
  within_dims <- unique(lapply(within_dims, function(x) paste(x[1], x[2], sep = "x")))
  within_dims <- lapply(within_dims, function(s) as.numeric(strsplit(s, "x", fixed = TRUE)[[1]]))
  # --- end minimal change ---
  
  grid_pairs <- factor_pairs(b)
  
  for (wd in within_dims) {
    r_block <- wd[1]
    c_block <- wd[2]
    for (gp in grid_pairs) {
      blocks_per_col <- gp[1]
      blocks_per_row <- gp[2]
      if (r_block * blocks_per_col == nrows && c_block * blocks_per_row == ncols) {
        return(list(
          rows_within_block = r_block,
          cols_within_block = c_block,
          blocks_per_col = blocks_per_col,
          blocks_per_row = blocks_per_row
        ))
      }
    }
  }
  
  NULL
}

#' @noRd
assemble_arcbd_blocks <- function(block_list, blocks_per_col, blocks_per_row) {
  # block_list can be:
  # - list of data.frames/matrices, or
  # - list returned by setNames(list(...), paste0("Block", 1:b))
  blocks <- unname(block_list)
  b <- length(blocks)
  if (blocks_per_col * blocks_per_row != b) {
    stop("Internal error: blocks_per_col * blocks_per_row must equal b.")
  }
  
  # Ensure each block is a matrix
  blocks <- lapply(blocks, function(x) {
    if (is.data.frame(x)) return(as.matrix(x))
    as.matrix(x)
  })
  
  rows_out <- vector(mode = "list", length = blocks_per_col)
  idx <- 1
  for (r in seq_len(blocks_per_col)) {
    row_blocks <- blocks[idx:(idx + blocks_per_row - 1)]
    idx <- idx + blocks_per_row
    rows_out[[r]] <- do.call(cbind, row_blocks)
  }
  
  do.call(rbind, rows_out)
}
