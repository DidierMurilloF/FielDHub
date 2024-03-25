#' plot_latinSQ 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_latinSQ <- function(x = NULL, dims = NULL, n_Reps = NULL, layout = 1, 
                         stacked = "horizontal", 
                         planter = "serpentine", 
                         l = 1) {
  rsRep <- dims[1]
  csRep <- dims[2]
  site <- l
  locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
  nlocs <- length(levels(locations))
  newBooksLocs <- vector(mode = "list", length = nlocs)
  countLocs <- 1
  books0 <- list(NULL)
  books1 <- list(NULL)
  
  for (locs in levels(locations)) {
    NewBook <- x$fieldBook |>
      dplyr::filter(LOCATION == locs)
    plots <- NewBook$PLOT
    if (x$infoDesign$id_design == 9) {
      plots <- NewBook$PLOT
      NewROWS1 <- rep(1:(rsRep*n_Reps), each = csRep)
      NewCOLUMNS1 <- NewBook$COLUMN
      NewROWS2 <- NewBook$ROW
    } else if (x$infoDesign$id_design == 7) {
      plots <- sort(NewBook$PLOT)
      t <- split_vectors(x = 1:(rsRep * n_Reps), len_cuts = rep(rsRep, n_Reps))
      rows <- list()
      for (k in 1:n_Reps) {
        rows[[k]] <- rep(t[[k]], each = csRep)
      }
      NewROWS1 <- unlist(rows)
      NewCOLUMNS1 <- rep(rep(1:csRep, times = rsRep), times = n_Reps)
      NewROWS2 <- rep(rep(1:rsRep, each = csRep), times = n_Reps)
    } else if (x$infoDesign$id_design == 3) {
      plots <- sort(NewBook$PLOT)
      t <- split_vectors(x = 1:(rsRep * n_Reps), len_cuts = rep(rsRep, n_Reps))
      rows <- list()
      for (k in 1:n_Reps) {
        rows[[k]] <- rep(t[[k]], each = csRep)
      }
      NewROWS1 <- unlist(rows)
      NewCOLUMNS1 <- rep(rep(1:csRep, times = rsRep), times = n_Reps)
      NewROWS2 <- rep(rep(1:rsRep, each = csRep), times = n_Reps)
    }
    
    if (stacked == "vertical") {
      df1 <- NewBook |> 
        dplyr::mutate(NewROW = NewROWS1,
                      NewCOLUMNS = NewCOLUMNS1)
      
      df1 <- df1[order(df1$NewROW, decreasing = FALSE), ]
      nCols <- max(df1$NewCOLUMNS)
      newPlots <- planter_transform(plots = plots, planter = planter, reps = n_Reps,
                                    cols = nCols, units = csRep)
      df1$PLOT <- newPlots
      books0[[1]] <- df1
    } else if (stacked == "horizontal") {
      w <- 1:(csRep * n_Reps)
      u <- seq(1, length(w), by = csRep)
      v <- seq(csRep, length(w), by = csRep)
      z <- vector(mode = "list", length = n_Reps)
      for (j in 1:n_Reps) {
        z[[j]] <- c(rep(u[j]:v[j], times = rsRep))
      }
      z <- unlist(z)
      df2 <- NewBook |> 
        dplyr::mutate(NewROW = NewROWS2,
                      NewCOLUMNS = z )
      nCols <- max(df2$NewCOLUMNS)
      newPlots <- planter_transform(plots = plots, planter = planter, reps = n_Reps,
                                    cols = nCols, units = NULL,
                                    mode = "horizontal")
      df2$PLOT <- newPlots
      books1[[1]] <- df2
    }
 
    books_sq <- c(books0, books1)
    newBooks <- books_sq[!sapply(books_sq, is.null)]
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
  
  if (x$infoDesign$id_design == 9) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,9,10,4:8)]
    colnames(allSitesFieldbook) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "ROW_REP",
                      "COLUMN_REP", "ENTRY", "TREATMENT")
    df <- df[,c(1:3,9,10,4:8)]
    colnames(df) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "ROW_REP",
                      "COLUMN_REP", "ENTRY", "TREATMENT")
    df <- df[ , -c(7,8)]
    allSitesFieldbook <- allSitesFieldbook[ , -c(7,8)]
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    df$ENTRY <- as.factor(df$ENTRY)
    ds <- "Row-Column Design "
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    p1 <- desplot::desplot(ENTRY ~ COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           out2.gpar = list(col = "black", lty = 3),
                           text = ENTRY, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE,
                           gg = TRUE)
    df$REP <- as.factor(df$REP)
    p2 <- desplot::desplot(REP ~  COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           text = PLOT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE,
                           gg = TRUE)
  } else  if (x$infoDesign$id_design == 7) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,8,9,4:7)]
    colnames(allSitesFieldbook) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", 
                                      "REP", "HSTRIP", "VSTRIP", "TRT_COMB")
    
    df <- df[,c(1:3,8,9,4:7)]
    colnames(df) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", 
                      "HSTRIP", "VSTRIP", "TRT_COMB")
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    df$TRT_COMB <- as.factor(df$TRT_COMB)
    ds <- "Strip-Plot Design "
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    p1 <- desplot::desplot(TRT_COMB ~ COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           out2.gpar = list(col = "black", lty = 3),
                           text = TRT_COMB, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE,
                           gg = TRUE)
    df$REP <- as.factor(df$REP)
    p2 <- desplot::desplot(REP ~  COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           text = PLOT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE,
                           gg = TRUE)
  } else  if (x$infoDesign$id_design == 3) {
    allSites <- vector(mode = "list", length = nlocs)
    for (st in 1:nlocs) {
      newBooksSelected_1 <- newBooksLocs[[st]]
      df_1 <- newBooksSelected_1[opt]
      allSites[[st]] <- as.data.frame(df_1)
    }
    allSitesFieldbook <- dplyr::bind_rows(allSites)
    allSitesFieldbook <- allSitesFieldbook[,c(1:3,8,9,4:7)]
    colnames(allSitesFieldbook) <- c("ID", "LOCATION", "PLOT", "ROW", 
                                    "COLUMN", "SQUARE", "ROW_SQ", 
                                    "COLUMN_SQ", "TREATMENT")
    df <- df[,c(1:3,8,9,4:7)]
    colnames(df) <- c("ID", "LOCATION", "PLOT", "ROW", 
                      "COLUMN", "SQUARE", "ROW_SQ", 
                      "COLUMN_SQ", "TREATMENT")
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    ds <- "Latin Square Design " 
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    df$TREATMENT <- as.factor(df$TREATMENT)
    if (n_Reps > 1) {
      p1 <- desplot::desplot(TREATMENT ~ COLUMN + ROW, flip = FALSE,
                             out1 = SQUARE,
                             out2.gpar = list(col = "black", lty = 3), 
                             text = TREATMENT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE,
                             gg = TRUE)
      
      df$SQUARE <- as.factor(df$SQUARE)
      p2 <- desplot::desplot(SQUARE ~ COLUMN + ROW, flip = FALSE,
                             out1 = SQUARE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main,
                             show.key = FALSE,
                             gg = TRUE)
    } else {
      p1 <- desplot::desplot(TREATMENT ~ COLUMN + ROW, flip = FALSE,
                             out2.gpar = list(col = "black", lty = 3), 
                             text = TREATMENT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main, 
                             show.key = FALSE,
                             gg = TRUE)
      
      df$SQUARE <- as.factor(df$SQUARE)
      p2 <- desplot::desplot(SQUARE ~ COLUMN + ROW, flip = FALSE,
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main,
                             show.key = FALSE,
                             gg = TRUE)
    }

  }
  return(list(p1 = p1, p2 = p2, df = df, newBooks = newBooksSelected, 
              allSitesFieldbook = allSitesFieldbook))
} 
