#' Spatial Un-replicated Diagonal Arrangement Design
#' 
#' Randomly generates an spatial un-replicated diagonal arrangement design.
#'
#' @param nrows Number of rows in the field.
#' @param ncols Number of columns in the field.
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param checks Integer number of genotypes checks or a numeric vector. 
#' @param kindExpt Type of diagonal design, with single options: Single Un-replicated Diagonal Checks
#' \code{'SUDC'} and Decision Blocks Un-replicated Design with Diagonal Checks \code{'DBUDC'} 
#' for multiple experiments. By default \code{kindExpt = 'SUDC'}.
#' @param planter Option for \code{serpentine} or \code{cartesian} plot arrangement. 
#' By default  \code{planter = 'serpentine'}.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param plotNumber Numeric vector with the starting plot number for each location. 
#' By default \code{plotNumber = 101}.
#' @param splitBy Option to split the field when \code{kindExpt = 'DBUDC'} is selected. 
#' By default \code{splitBy = 'row'}.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#' @param blocks Number of experiments or blocks to generate an \code{DBUDC} design. 
#' If \code{kindExpt = 'DBUDC'} and data is null, \code{blocks} are mandatory.
#' @param exptName (optional) Name of the experiment.
#' @param locationNames (optional) Names each location.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME | BLOCK} or only 2 
#' columns \code{ENTRY | NAME} if \code{kindExpt = 'SUDC'}.
#' 
#' 
#' @return A list with information on the design parameters.
#' @return A matrix with the randomization layout.
#' @return A matrix with the layout plot number.
#' @return A data frame with field book design. This includes the index (Row, Column).
#'
#'
#' @references
#' Clarke, G. P. Y., & Stefanova, K. T. (2011). Optimal design for early-generation plant
#' breeding trials with unreplicated or partially replicated test lines. Australian & New
#' Zealand Journal of Statistics, 53(4), 461–480.
#'
#'
#' @examples
#' 
#' # Example 1: Generates a spatial single diagonal arrangement design in one location
#' # with 270 treatments and 30 check plots for a field with dimensions 15 rows x 20 cols
#' # in a serpentine arrangement.
#' spatd <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, 
#'                               checks = 4, 
#'                               plotNumber = 101, 
#'                               kindExpt = "SUDC", 
#'                               planter = "serpentine", 
#'                               seed = 1987,
#'                               exptName = "20WRY1", 
#'                               locationNames = "MINOT")
#' spatd$infoDesign
#' spatd$layoutRandom
#' spatd$plotsNumber
#' head(spatd$fieldBook, 12)
#' 
#' # Example 2: Generates a spatial decision block diagonal arrangement design in one location
#' # with 720 treatments allocated in 5 experiments or blocks for a field with dimensions
#' # 30 rows x 26 cols in a serpentine arrangement. In this case, we show how to set up the data 
#' # option with the entries list.
#' checks <- 5;expts <- 5
#' list_checks <- paste("CH", 1:checks, sep = "")
#' treatments <- paste("G", 6:725, sep = "")
#' BLOCK <- c(rep("ALL", checks), rep(1:expts, c(150,155,95,200,120)))
#' treatment_list <- data.frame(list(ENTRY = 1:725, NAME = c(list_checks, treatments), BLOCK = BLOCK))
#' head(treatment_list, 12) 
#' tail(treatment_list, 12)
#' spatDB <- diagonal_arrangement(nrows = 30, ncols = 26,
#'                                checks = 5, 
#'                                plotNumber = 1, 
#'                                kindExpt = "DBUDC", 
#'                                planter = "serpentine", 
#'                                splitBy = "row", 
#'                                data = treatment_list)
#' spatDB$infoDesign
#' spatDB$layoutRandom
#' spatDB$plotsNumber
#' head(spatDB$fieldBook,12)
#'
#' # Example 3: Generates a spatial decision block diagonal arrangement design in one location
#' # with 270 treatments allocated in 3 experiments or blocks for a field with dimensions
#' # 20 rows x 15 cols in a serpentine arrangement. Which in turn is an augmented block (3 blocks).
#' spatAB <- diagonal_arrangement(nrows = 20, ncols = 15, lines = 270, 
#'                                checks = 4, 
#'                                plotNumber = c(1,1001,2001), 
#'                                kindExpt = "DBUDC", 
#'                                planter = "serpentine",
#'                                exptName = c("20WRA", "20WRB", "20WRC"), 
#'                                blocks = c(90, 90, 90),
#'                                splitBy = "column")
#' spatAB$infoDesign
#' spatAB$layoutRandom
#' spatAB$plotsNumber
#' head(spatAB$fieldBook,12)
#' 
#' @export
diagonal_arrangement <- function(nrows = NULL, ncols = NULL, lines = NULL, checks = NULL, planter = "serpentine", 
                                 l = 1, plotNumber = 101, kindExpt = "SUDC", splitBy = "row", seed = NULL, 
                                 blocks = NULL, exptName = NULL, locationNames = NULL, data = NULL) {
  
  if (all(c("serpentine", "cartesian") != planter)) {
    base::stop('Input for planter is unknown. Please, choose one: "serpentine" or "cartesian"')
  }
  if (all(c("SUDC", "DBUDC") != kindExpt)) {
    base::stop('Input for kindExpt is unknown. Please, choose one: "SUDC" or "DBUDC"')
  }
  if (all(c("column", "row") != splitBy)) {
    base::stop('Input for splitBy is unknown. Please, choose one: "column" or "row"')
  }
  if (!is.null(plotNumber) && is.numeric(plotNumber)) {
    if(any(plotNumber %% 1 != 0) || any(plotNumber < 1) || any(diff(plotNumber) < 0)) {
      shiny::validate('diagonal_arrangement() requires plotNumber to be positive and sorted integers.')
    }
  }
  if (is.null(plotNumber)) {
    plotNumber <- 1001
    warning("Since plotNumber was missing, it was set up as: 1001")
  }else if(!is.numeric(plotNumber)) {
    stop("Input for plotNumber can only be an integer or a numeric vector.")
  }

  if(is.null(data) && !is.null(plotNumber) && kindExpt == "DBUDC") {
    if(length(plotNumber) > 1 && length(blocks) != length(plotNumber)) {
      base::stop('The input plotNumber and blocks need to be of the same length. You can consider plotNumber as an integer.')
    }
  }

  if (!is.null(checks) && is.numeric(checks) && all(checks %% 1 == 0)) {
    if(!is.null(data)) {
      if (length(checks) == 1 && checks >= 1) {
        checksEntries <- as.numeric(data[1:checks,1])
        checks <- checks
      }else if (length(checks) > 1) {
        checksEntries <- checks
        checks <- length(checks)
      } 
    }else {
      if (length(checks) == 1 && checks >= 1) {
        checksEntries <- 1:checks
        checks <- checks
      }else if (length(checks) > 1) {
        checksEntries <- checks
        checks <- length(checks)
      }
    }
  }else base::stop("'diagonal_arrangement()' requires input checks to be an integer greater than 0.")
  
  if(any(diff(checksEntries) > 1) || any(diff(checksEntries) < 0)) {
    base::stop(paste("'diagonal_arrangement()' requires input checks to be a continuous range."))
  }
  
  if (!is.null(data)) {
    arg1 <- list(nrows, ncols, l);arg2 <- c(nrows, ncols, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      base::stop("'diagonal_arrangement()' requires input nrows, ncols, and l to be numeric and distint of NULL")
    }
  }else {
    arg1 <- list(nrows, ncols, lines, l);arg2 <- c(nrows, ncols, lines, l)
    if (base::any(lengths(arg1) != 1) || base::any(arg2 %% 1 != 0) || base::any(arg2 < 1)) {
      base::stop("'diagonal_arrangement()' requires input nrows, ncols, and l to be numeric and distint of NULL")
    }
  } 
  
  if (is.null(seed)) {seed <- runif(1, min = -10000, max = 10000)}
  set.seed(seed)
  
  Option_NCD <- TRUE
  
  if (splitBy == "column") {
    Way <- "By Column"
  }else {
    Way <- "By Row"
  }
  
  getData <- function() {
    Blocks <- 1
    if (!is.null(data)) {
      data_entry <- data
      data_entry_UP <- na.omit(data_entry[,1:2]) 
      colnames(data_entry_UP) <- c("ENTRY", "NAME")
      print(nrow(data_entry_UP))
      if (kindExpt == "DBUDC") {
        data_entry_UP <- na.omit(data_entry[,1:3]) 
        colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
        B <- data_entry_UP[(checks + 1):nrow(data_entry_UP),]
        Block_levels <- as.numeric(levels(as.factor(B$BLOCK)))
        Blocks <- length(Block_levels)
        if (Option_NCD == TRUE) {
          data_entry1 <- data_entry_UP[(checks + 1):nrow(data_entry_UP), ]
          Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
          Block_levels <- na.omit(Block_levels)
          data_dim_each_block <- numeric()
          for (i in Block_levels){ 
            data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
          }
          dim_data <- sum(data_dim_each_block)
        }
      }
    }else {
      if (kindExpt != "DBUDC") {
        NAME <- c(paste(rep("Check", checks), 1:checks),
                  paste(rep("gen", lines), (checksEntries[checks] + 1):(checksEntries[1] + lines + checks - 1)))
        data_entry_UP <- data.frame(list(ENTRY = checksEntries[1]:(checksEntries[1] + lines + checks - 1),	NAME = NAME))
        colnames(data_entry_UP) <- c("ENTRY", "NAME")
        if (nrow(data_entry_UP) != (lines + checks)) base::stop("nrows data != of lines + checks")
      }else if (kindExpt == "DBUDC") {
        if(is.null(blocks)) stop("'diagonal_arrangement()' requires blocks when kindExpt = 'DBUDC' and data is null.")
        if(sum(blocks) != lines) stop("In 'diagonal_arrangement()' number of lines and total lines in 'blocks' do not match.")
        NAME <- c(paste(rep("Check", checks), 1:checks),
                  paste(rep("gen", lines), (checksEntries[checks] + 1):(checksEntries[1] + lines + checks - 1)))
        data_entry_UP <- data.frame(list(ENTRY = checksEntries[1]:(checksEntries[1] + lines + checks - 1),	NAME = NAME))
        data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
        colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
        Blocks <- length(blocks)
        if (Option_NCD == TRUE) {
          data_entry1 <- data_entry_UP[(length(checks) + 1):nrow(data_entry_UP), ]
          Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
          Block_levels <- na.omit(Block_levels)
          data_dim_each_block <- numeric()
          for (i in Block_levels) {
            data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
          }
          dim_data <- sum(data_dim_each_block)
        }
      }
    }
    
    dim_data_entry <- nrow(data_entry_UP)
    dim_data_1 <- nrow(data_entry_UP[(checks + 1):nrow(data_entry_UP), ])
    
    return(list(data_entry = data_entry_UP, dim_data_entry = dim_data_entry,
                dim_data_1 = dim_data_1, Blocks = Blocks))
    
  }
  
  available_percent1 <- available_percent(n_rows = nrows, n_cols = ncols, checks = checksEntries, Option_NCD = FALSE,
                                          Visual_ch = NULL, visualCheck = FALSE, kindExpt = kindExpt, myWay = Way,
                                          planter_mov1 = planter, data = getData()$data_entry, dim_data = getData()$dim_data_entry,
                                          dim_data_1 = getData()$dim_data_1, Block_Fillers = NULL)
  
  new_lines <- nrow(getData()$data_entry) - checks
  infoP <- as.data.frame(available_percent1$P)
  infoP$V7 <- nrows*ncols - infoP[,6]
  minLines <- min(infoP$V7)
  maxLines <- max(infoP$V7)
  exptlines <- new_lines
  if (exptlines >= minLines & exptlines <= maxLines) {
    if (exptlines %in% as.vector(infoP[,7])) {
      Option_NCD <- FALSE
      Exptlines <- exptlines
    }else if (all(as.vector(infoP[,7]) != exptlines)) {
      if(kindExpt == "DBUDC" && Way != "By Row") {
        shiny::validate("Fillers are only allowed when you split the field by rows :(")
      }
      Option_NCD <- TRUE
      Exptlines <- exptlines
      if (kindExpt == "DBUDC") {
        
        Block_Fillers <- as.numeric(getData()$Blocks)
        available_percent1 <- available_percent(n_rows = nrows, n_cols = ncols, checks = checksEntries, Option_NCD = Option_NCD,
                                                Visual_ch = NULL, visualCheck = FALSE, kindExpt = kindExpt, myWay = Way,
                                                planter_mov1 = planter, data = getData()$data_entry, dim_data = getData()$dim_data_entry,
                                                dim_data_1 = getData()$dim_data_1, Block_Fillers = Block_Fillers)
        infoP <- as.data.frame(available_percent1$P)
        infoP$V7 <- Exptlines
      }else {
        Block_Fillers <- NULL
        available_percent1 <- available_percent(n_rows = nrows, n_cols = ncols, checks = checksEntries, Option_NCD = Option_NCD,
                                                Visual_ch = NULL, visualCheck = FALSE, kindExpt = kindExpt, myWay = Way,
                                                planter_mov1 = planter, data = getData()$data_entry, dim_data = getData()$dim_data_entry,
                                                dim_data_1 = getData()$dim_data_1, Block_Fillers = Block_Fillers)
        infoP <- as.data.frame(available_percent1$P)
        infoP$V7 <- Exptlines
      }  
    }
  }else shiny::validate("The total number of entries do not fit into the specified field.")
  
  if(kindExpt == "DBUDC") {
    if(length(plotNumber) > 1 && length(available_percent1$data_dim_each_block) != length(plotNumber)) {
      base::stop('The input plotNumber and blocks need to be of the same length. You can consider plotNumber as an integer.')
    }
  }
  
  rand_checks <- random_checks(dt = available_percent1$dt, d_checks = available_percent1$d_checks, p = infoP, percent = NULL,
                               exptlines = Exptlines, kindExpt = kindExpt, planter_mov = planter, Checks = checksEntries, myWay = Way,
                               data = getData()$data_entry, data_dim_each_block = available_percent1$data_dim_each_block,
                               n_reps = NULL, Option_NCD = Option_NCD, seed = seed)
  
  data_entry <- getData()$data_entry
  w_map <- rand_checks$map_checks
  n_rows = nrows; n_cols = ncols
  my_split_r <- rand_checks$map_checks
  multi <- kindExpt == "RDC" || kindExpt == "DBUDC"
  if (multi == TRUE) {
    map_checks <- rand_checks$map_checks
    req(getData()$data_entry)
    data_entry <- getData()$data_entry
    if (kindExpt == "DBUDC" && Way == "By Row") {
      data_dim_each_block <- available_percent1$data_dim_each_block
      my_row_sets <- automatically_cuts(data = map_checks, planter_mov = planter,
                                        way = "By Row", dim_data = data_dim_each_block)[[1]]
      if(is.null(my_row_sets)) return(NULL)
      n_blocks <- length(my_row_sets)
    }else if (kindExpt == "DBUDC" && Way == "By Column") {
      data_dim_each_block <- available_percent1$data_dim_each_block
      cuts_by_c <- automatically_cuts(data = map_checks, planter_mov = planter, way = "By Column",
                                      dim_data = data_dim_each_block)
      if(is.null(cuts_by_c)) return(NULL)
      n_blocks <- length(cuts_by_c)
      m = diff(cuts_by_c)
      my_col_sets = c(cuts_by_c[1], m)
    }
    if(Way == "By Column") {
      if(kindExpt == "DBUDC" && Option_NCD == FALSE){
        data_random <- get_random(n_rows = nrows, n_cols = ncols, d_checks = my_split_r,
                                  reps = NULL, Fillers = FALSE, col_sets = my_col_sets, row_sets = NULL,
                                  checks = checksEntries, data = data_entry, data_dim_each_block = data_dim_each_block)
      }else if(kindExpt == "DBUDC" && Option_NCD == TRUE){
        req(available_percent1$data_dim_each_block)
        data_random <- get_random(n_rows = nrows, n_cols = ncols, d_checks = my_split_r,
                                  reps = NULL, Fillers = TRUE, col_sets = my_col_sets, row_sets = NULL,
                                  checks = checksEntries,data = data_entry)
      }
    }else {
      if (kindExpt == "DBUDC" && Option_NCD == FALSE) {
        # data_random <- get_random(n_rows = nrows, n_cols = ncols, d_checks = my_split_r,
        #                           reps = NULL, Fillers = FALSE, col_sets = NULL, row_sets = my_row_sets,
        #                           checks = 1:checks,data = data_entry, data_dim_each_block = data_dim_each_block)
        data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
        data_random <- get_DBrandom(binaryMap = w_map, data_dim_each_block = data_dim_each_block, data_entries = data_entry1,
                                    planter = planter)
      }else if(kindExpt == "DBUDC" && Option_NCD == TRUE) {
        Block_Fillers <- as.numeric(getData()$Blocks)
        data_random <- get_random(n_rows = nrows, n_cols = ncols, d_checks = my_split_r,
                                  reps = NULL, Fillers = FALSE, col_sets = NULL, row_sets = my_row_sets,
                                  checks = checksEntries, data = data_entry, planter_mov  = planter,
                                  Multi.Fillers = TRUE, which.blocks = Block_Fillers)
      }
    }
  }else {
    n_blocks <- 1
    if(Option_NCD == TRUE) {
      data_random <- get_random(n_rows = nrows, n_cols = ncols, d_checks = my_split_r,
                                reps = NULL, Fillers = TRUE, col_sets = ncols, row_sets = NULL,
                                checks = checksEntries, data = data_entry, planter_mov  = planter)
    }else {
      data_random <- get_random(n_rows = nrows, n_cols = ncols, d_checks = my_split_r,
                                reps = NULL, Fillers = FALSE, col_sets = ncols, row_sets = NULL,
                                checks = checksEntries, data = data_entry, planter_mov  = planter)
    }
  }
  
  if (Way == "By Row" && kindExpt == "DBUDC") {
    map_letters <- data_random$w_map_letters
    split_name_diagonal1 <- names_dbrows(w_map = w_map, myWay = "By Row", kindExpt = "DBUDC", w_map_letters = map_letters,
                                         data_dim_each_block = data_dim_each_block, expt_name = exptName, Checks = checksEntries)
  }else {
    w_map_letters1 <- data_random$w_map_letters
    split_name_diagonal1 <- names_diagonal(nrows = nrows, ncols = ncols, randomChecksMap = w_map, kindExpt = kindExpt,
                                           checks = checksEntries,myWay = Way, Option_NCD = Option_NCD, expt_name = exptName, 
                                           data_entry = data_entry, reps = NULL, data_dim_each_block = data_dim_each_block, 
                                           w_map_letters1 = w_map_letters1)
  }
  
  put_Filler_in_names <- function() {
    if (multi == FALSE && Option_NCD == TRUE) {
      blocks <- 1
      if (!is.null(exptName)) {
        Name_expt <- exptName 
      }else Name_expt = paste0(rep("Expt", times = blocks), 1:blocks)
      split_names <- matrix(data = Name_expt, ncol = ncols, nrow = nrows)
      r_map <- data_random$rand
      Fillers <- sum(r_map == "Filler")
      if (nrows %% 2 == 0) {
        if(planter == "serpentine") {
          split_names[1, 1:Fillers] <- "Filler"
        }else{
          split_names[1,((ncols + 1) - Fillers):ncols] <- "Filler"
        }
      }else{
        split_names[1,((ncols + 1) - Fillers):ncols] <- "Filler"
      }
    }
    return(list(name_with_Fillers = split_names))
  }
  
  w_map <- rand_checks$map_checks
  if (multi == TRUE && Option_NCD == FALSE) { 
    if (kindExpt == "DBUDC") { 
      if (Way == "By Row") { 
        my_row_sets <- automatically_cuts(data = w_map, planter_mov = planter,
                                          way = "By Row", dim_data = data_dim_each_block)[[1]]
        n_blocks <- length(my_row_sets) 
      }else { 
        cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                        dim_data = data_dim_each_block)  
        n_blocks <- length(cuts_by_c) 
        m = diff(cuts_by_c) 
        my_col_sets = c(cuts_by_c[1], m) 
      } 
      Name_expt <- exptName
      if (length(Name_expt) == n_blocks || !is.null(Name_expt)) { 
        expe_names <- Name_expt 
      }else{ 
        expe_names = paste0(rep("Expt", times = n_blocks), 1:n_blocks) 
      } 
      plot_n_start <- as.numeric(plotNumber)
      if (length(plot_n_start) > 1 && length(plot_n_start) < n_blocks) return(NULL) 
      if(Way == "By Column"){
        datos_name <- split_name_diagonal1$my_names 
        datos_name = as.matrix(datos_name)
        my_split_plot_nub <- plot_number(movement_planter = planter, n_blocks = n_blocks,
                                         n_rows = nrows, n_cols = ncols, plot_n_start = plot_n_start,
                                         datos = datos_name, expe_name = expe_names, ByRow = FALSE,
                                         my_row_sets = NULL, ByCol = TRUE, my_col_sets = my_col_sets) 
      }else{
        datos_name <- split_name_diagonal1$my_names 
        plot_n_start <- plotNumber
        data.dim.each <- data_dim_each_block
        Block_Fillers <- as.numeric(getData()$Blocks) 
        
        my_split_plot_nub <- plot_number_fillers(movement_planter = planter, plot_n_start = plot_n_start,
                                                 datos = datos_name, expe_names = expe_names, ByRow = TRUE,
                                                 my_row_sets = my_row_sets, ByCol = FALSE, my_col_sets = NULL,
                                                 which.blocks = Block_Fillers, n_blocks = n_blocks,
                                                 data.dim.each = data.dim.each) 
      }
    }
  }else if(multi == TRUE && Option_NCD == TRUE) {
    if (Way == "By Row") { 
      my_row_sets <- automatically_cuts(data = w_map, planter_mov = planter,
                                        way = "By Row", dim_data = data_dim_each_block)[[1]]
      n_blocks <- length(my_row_sets) 
    }else { 
      cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                      dim_data = data_dim_each_block)  
      n_blocks <- length(cuts_by_c) 
      m = diff(cuts_by_c)
      my_col_sets = c(cuts_by_c[1], m) 
    } 
    w_map_letters1 <- data_random$w_map_letters
    Name_expt <- exptName
    if (length(Name_expt) == n_blocks || !is.null(Name_expt)) { 
      expe_names <- Name_expt 
    }else { 
      expe_names = paste0(rep("Expt", times = n_blocks), 1:n_blocks) 
    } 
    if(Way == "By Row") { 
      datos_name <- split_name_diagonal1$my_names 
      plot_n_start <- plotNumber
      data.dim.each <- data_dim_each_block
      Block_Fillers <- as.numeric(Block_Fillers) 
      
      my_split_plot_nub <- plot_number_fillers(movement_planter = planter, plot_n_start = plot_n_start,
                                               datos = datos_name, expe_names = expe_names, ByRow = TRUE,
                                               my_row_sets = my_row_sets, ByCol = FALSE, my_col_sets = NULL,
                                               which.blocks = Block_Fillers, n_blocks = n_blocks,
                                               data.dim.each = data.dim.each) 
    }else { 
      return(NULL) 
    } 
  }else { 
    plot_n_start <- as.numeric(plotNumber) 
    n_blocks <- 1 
    if (!is.null(exptName)) { 
      Name_expt <- exptName  
    }else Name_expt = paste0(rep("Expt", times = n_blocks), 1:n_blocks)
    datos_name <- split_name_diagonal1$my_names 
    my_split_plot_nub <- plot_number(movement_planter = planter, n_blocks = 1, n_rows = nrows,
                                     n_cols = ncols, plot_n_start = plot_n_start, datos = datos_name,
                                     expe_name =  Name_expt, ByRow = NULL, my_row_sets = NULL, ByCol = NULL,
                                     my_col_sets = NULL)
    if (Option_NCD == TRUE) { 
      r_map <- data_random$rand
      Fillers <- sum(r_map == "Filler") 
      if (nrows %% 2 == 0) { 
        if(planter == "serpentine") { 
          my_split_plot_nub[[1]][1, 1:Fillers] <- 0 
        }else{ 
          my_split_plot_nub[[1]][1,((ncols + 1) - Fillers):ncols] <- 0 
        } 
      }else { 
        my_split_plot_nub[[1]][1,((ncols + 1) - Fillers):ncols] <- 0 
      } 
    } 
    my_split_plot_nub 
  } 
  
  if (is.null(locationNames)) locationNames <- 1:l
  
  my_export_design <- function(){
    year <- format(Sys.Date(), "%Y")

    if(is.null(data_random$rand)) base::stop("Random matrix is missing.")
    if(is.null(rand_checks$col_checks)) base::stop("checks matrix is missing.")
    if(is.null(my_split_plot_nub$w_map_letters1)) base::stop("Plot numbers matrix is missing.")
    if(is.null(split_name_diagonal1$my_names)) base::stop("Names matrix is missing.")
   
    movement_planter <- planter
    random_entries_map <- data_random$rand
    random_entries_map[random_entries_map == "Filler"] <- 0
    random_entries_map <- apply(random_entries_map, 2, as.numeric)
    my_data_VLOOKUP <- getData()$data_entry
    COLNAMES_DATA <- colnames(my_data_VLOOKUP)
    if(Option_NCD == TRUE) {
      if(kindExpt != "DBUDC") {
        Entry_Fillers <- data.frame(list(0, "Filler"))
      }else {
        Entry_Fillers <- data.frame(list(0, "Filler", "NA"))
      }
      colnames(Entry_Fillers) <- COLNAMES_DATA
      my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
    }
    Col_checks <- as.matrix(rand_checks$col_checks)
    plot_number <- as.matrix(my_split_plot_nub$w_map_letters1)
    plot_number <- apply(plot_number, 2 ,as.numeric)
    my_names <- split_name_diagonal1$my_names
    
    if (multi == FALSE && Option_NCD == TRUE) {
      my_names <- put_Filler_in_names()$name_with_Fillers
    }
    
    results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
    final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                       location = locationNames, Year = year,
                                       data_file = my_data_VLOOKUP, reps = FALSE)
    if(Option_NCD == TRUE) {
      final_expt_export$CHECKS <- ifelse(final_expt_export$NAME == "Filler", "NA", final_expt_export$CHECKS)
      final_expt_export$EXPT <- ifelse(final_expt_export$EXPT == "Filler", "NA", final_expt_export$EXPT)
    } 
    
    return(list(final_expt = final_expt_export))
  }
  
  fieldBook <- as.data.frame(my_export_design()$final_expt)
  if(is.null(fieldBook) || !is.data.frame(fieldBook)) base::stop("fieldBook is NULL or != data frame.")
  if(dim(fieldBook)[1]*dim(fieldBook)[2] == 0) base::stop("fieldBook is NULL or != data frame or length 0.")
  fieldBook <- fieldBook[,-11]
  
  ID <- 1:nrow(fieldBook)
  fieldBook <- fieldBook[, c(6,7,9,4,2,3,5,1,10)]
  fieldBook <- cbind(ID, fieldBook)
  colnames(fieldBook)[10] <- "TREATMENT"
  
  #fieldBook <- fieldBook[, c(2,3,1,4:10)]
  
  rownames(fieldBook) <- 1:nrow(fieldBook)
  
  linesexpt <- data_random$Lines
  plot_num <- as.matrix(my_split_plot_nub$w_map_letters1)
  layoutR <- data_random$rand
  rownames(layoutR) <- paste("Row", nrow(layoutR):1, sep = "")
  colnames(layoutR) <- paste("Col", 1:ncol(layoutR), sep = "")
  rownames(plot_num) <- paste("Row", nrow(plot_num):1, sep = "")
  colnames(plot_num) <- paste("Col", 1:ncol(plot_num), sep = "")
  dataEntry <- getData()$data_entry
  infoD <- as.data.frame(available_percent1$dt)
  exptNames <- split_name_diagonal1$my_names
  Col_checks <- as.matrix(rand_checks$col_checks)
  Fillers <- 0
  RepChecks <- numeric()
  for (i in factor(checksEntries)) {RepChecks[i] <- sum(layoutR == i)}
  if (any(as.vector(layoutR) == "Filler")) Fillers <- sum(layoutR == "Filler") 
  percentChecks <- round(sum(RepChecks)/(nrows*ncols),3) * 100
  percentChecks <- paste(percentChecks, "%", sep = "")
  infoDesign <- list(Lines = linesexpt, checks = checks, RepChecks = RepChecks, percentChecks = percentChecks,
                     Fillers = Fillers, seed = seed)
  return(list(infoDesign = infoDesign, layoutRandom = layoutR, plotsNumber = plot_num,
              fieldBook = fieldBook, data_entry = getData()$data_entry))
}