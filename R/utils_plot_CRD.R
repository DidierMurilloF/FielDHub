#' plot_CRD 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_CRD <- function(x = NULL, n_TrtGen = NULL, n_Reps = NULL, layout = 1, 
                     planter = "serpentine", l = 1) {
  
  site <- l
  locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
  nlocs <- length(levels(locations))
  newBooksLocs <- vector(mode = "list", length = nlocs)
  countLocs <- 1
  books0 <- list(NULL)
  books1 <- list(NULL)
  for (locs in levels(locations)) {
    NewBook <- x$fieldBook %>%
      dplyr::filter(LOCATION == locs)
    
    cols <- rep(1:n_TrtGen, times = n_Reps)
    breaks <- split_vectors(x = cols, len_cuts = rep(n_TrtGen, each = n_Reps))
    lngt <- 1:length(breaks)
    new_breaks <- vector(mode = "list", length = n_Reps)
    for (n in lngt) {
      if (n %% 2 == 0) {
        new_breaks[[n]] <- rev(breaks[[n]])
      } else new_breaks[[n]] <- breaks[[n]]
    }
    COLUMN_serp <- unlist(new_breaks)
    if (planter == "serpentine") {
      COLUMN <- COLUMN_serp
    } else COLUMN <- rep(1:n_TrtGen, times = n_Reps)
    books0[[1]] <- NewBook %>% 
      dplyr::mutate(ROW = rep(1:n_Reps, each = n_TrtGen),
                    COLUMN = COLUMN)
    cols <- rep(1:n_Reps, times = n_TrtGen)
    breaks <- split_vectors(x = cols, len_cuts = rep(n_Reps, each = n_TrtGen))
    lngt <- 1:length(breaks)
    new_breaks <- vector(mode = "list", length = n_TrtGen)
    for (n in lngt) {
      if (n %% 2 == 0) {
        new_breaks[[n]] <- rev(breaks[[n]])
      } else new_breaks[[n]] <- breaks[[n]]
    }
    COLUMN_serp <- unlist(new_breaks)
    if (planter == "serpentine") {
      COLUMN <- COLUMN_serp
    } else COLUMN <- rep(1:n_Reps, times = n_TrtGen)
    books1[[1]] <- NewBook %>% 
      dplyr::mutate(ROW = rep(1:n_TrtGen, each = n_Reps),
                    COLUMN = COLUMN)
    books_crd <- c(books0, books1)
    newBooks <- books_crd[!sapply(books_crd,is.null)]
    newBooksLocs[[countLocs]] <- newBooks
    countLocs <- countLocs + 1
  }
  
  opt <- layout
  newBooksSelected <- newBooksLocs[[site]]
  opt_available <- 1:length(newBooksSelected)
  if (all(opt_available != opt)) {
    message(cat("\n",
                " Option for layout is not available!", "\n", "\n",
                "*********************************************", "\n",
                "*********************************************", "\n", "\n",
                "Layout options available for this design are:", "\n", "\n",
                opt_available, "\n", "\n",
                "*********************************************", "\n",
                "*********************************************"))
    return(NULL)
  }
  df1 <- newBooksSelected[opt]
  df <- as.data.frame(df1)
  
  if (x$infoDesign$id_design == 1) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,6,7,4:5)]
    df <- df[,c(1:3,6,7,4:5)]
    # Plot field layout
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    ds <- "Completely Randomized Design " 
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    p1 <- desplot::desplot(TREATMENT ~ COLUMN + ROW, flip = FALSE,
                           out2.gpar = list(col = "black", lty = 3), 
                           text = TREATMENT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE, 
                           gg=TRUE)
    df$REP <- as.factor(df$REP)
    df$PLOT <- as.character(df$PLOT)
    p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                           #out1 = REP,
                           #out2.gpar = list(col = "black", lty = 3), 
                           text = PLOT, 
                           cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE, 
                           gg=TRUE)
  } else if (x$infoDesign$id_design == 4) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    nc <- ncol(df)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,(nc-1),nc,4:(nc-2))]
    df <- df[, c(1:3,(nc-1),nc,4:(nc-2))]
    if (x$infoDesign$kind == "CRD") {
      # nc <- ncol(df)
      # df <- df[,c(1:3,(nc-1),nc,4:(nc-2))]
      # Plot field layout
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Full Factorial Design (CRD) " 
      main <- paste0(ds, rows, "X", cols)
      # Plot field layout
      p1 <- desplot::desplot(TRT_COMB ~ COLUMN + ROW, flip = FALSE,
                             out2.gpar = list(col = "black", lty = 3), 
                             text = TRT_COMB, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
      df$REP <- as.factor(df$REP)
      df$PLOT <- as.factor(df$PLOT)
      p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
    }
  } else if (x$infoDesign$id_design == 5) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    nc <- ncol(df)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,(nc-1),nc,4:(nc-2))]
    df <- df[,c(1:3,(nc-1),nc,4:(nc-2))]
    if (x$infoDesign$typeDesign == "CRD") {
      # nc <- ncol(df)
      # df <- df[,c(1:3,(nc-1),nc,4:(nc-2))]
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Split Plot Design (CRD) " 
      main <- paste0(ds, rows, "X", cols)
      df$REP <- as.factor(df$REP)
      p1 <- desplot::desplot(TRT_COMB ~ COLUMN + ROW, flip = FALSE,
                             out1 = REP,
                             out2.gpar=list(col = "gray50", lwd = 1, lty = 1),
                             text = TRT_COMB, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = TRUE, 
                             gg=TRUE)
      
      df$REP <- as.factor(df$REP)
      df$PLOT <- as.factor(df$PLOT)
      p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
    }
  } else if (x$infoDesign$id_design == 6) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    nc <- ncol(df)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,9,10,4:8)]
    df <- df[,c(1:3,9,10,4:8)]
    if (x$infoDesign$typeDesign == "CRD") {
      # df <- df[,c(1:3,9,10,4:8)]
      df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
      df$SUB_PLOT <- as.factor(df$SUB_PLOT)
      df$SUB_SUB_PLOT <- as.factor(df$SUB_SUB_PLOT)
      df$REP <- as.factor(df$REP)
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Split-Split Plot Design (CRD) " 
      main <- paste0(ds, rows, "X", cols)
      p1 <- desplot::desplot(REP ~ COLUMN + ROW, flip = FALSE,
                             out1 = REP,
                             out2 = WHOLE_PLOT,
                             out2.gpar=list(col = "gray50", lwd = 1, lty = 1),
                             text = WHOLE_PLOT, cex = 1, shorten = "no",
                             col = SUB_PLOT,
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = TRUE, 
                             gg=TRUE)
      
      df$PLOT <- as.factor(df$PLOT)
      p2 <- desplot::desplot(PLOT ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE, 
                             gg=TRUE)
    }
  }
  return(list(p1 = p1, p2 = p2, df = df, newBooks = newBooksSelected, 
              allSitesFieldbook = allSitesFieldbook))
}