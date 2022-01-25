#' plot_latinSQ 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_latinSQ <- function(x = NULL, dims = NULL, n_Reps = NULL, optionLayout = 1, 
                         planter = "serpentine") {
  rsRep <- dims[1]
  csRep <- dims[2]
  
  books0 <- list(NULL)
  if (x$infoDesign$idDesign == 9) {
    NewROWS1 <- rep(1:(rsRep*n_Reps), each = csRep)
    NewCOLUMNS1 <- x$fieldBook$COLUMN
    NewROWS2 <- x$fieldBook$ROW
  } else if (x$infoDesign$idDesign == 7) {
    NewROWS1 <- rep(1:(rsRep*n_Reps), each = csRep)
    NewCOLUMNS1 <- rep(rep(1:csRep, times = rsRep), times = n_Reps)
    NewROWS2 <- rep(rep(1:rsRep, each = csRep), times = n_Reps)
  } else {
    NewROWS1 <- rep(1:(rsRep*n_Reps), each = csRep)
    NewCOLUMNS1 <- rep(rep(1:csRep, times = rsRep), times = n_Reps)
    NewROWS2 <- rep(rep(1:rsRep, each = csRep), times = n_Reps)
  }
  books0[[1]] <- x$fieldBook %>% 
    dplyr::mutate(NewROW = NewROWS1,
                  NewCOLUMNS = NewCOLUMNS1)
  
  books1 <- list(NULL)
  w <- 1:(csRep*n_Reps)
  u <- seq(1, length(w), by = csRep)
  v <- seq(csRep, length(w), by = csRep)
  z <- vector(mode = "list", length = n_Reps)
  for (j in 1:n_Reps) {
    z[[j]] <- c(rep(u[j]:v[j], times = rsRep))
  }
  z <- unlist(z)
  books1[[1]] <- x$fieldBook %>% 
    dplyr::mutate(NewROW = NewROWS2,
                  NewCOLUMNS = z )
  
  books <- c(books0, books1)
  newBooks <- books[!sapply(books,is.null)]
  opt <- optionLayout
  df <- as.data.frame(newBooks[[opt]])
  
  if (x$infoDesign$idDesign == 9){
    df <- df[,c(1:3,8,9,4:7)]
    colnames(df) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "ROW_REP", 
                      "COLUMN_REP", "ENTRY")
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
                           show.key = FALSE)
    df$REP <- as.factor(df$REP)
    p2 <- desplot::desplot(REP ~  COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           # out2 = IBLOCK,
                           # col = IBLOCK,
                           # out2.gpar = list(col = "black", lty = 3),
                           #out2.gpar = list(col = "black", lty = 3),
                           text = PLOT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE)
  } else  if (x$infoDesign$idDesign == 7) {
    df <- df[,c(1:3,8,9,4:7)]
    colnames(df) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "HSTRIP", "VSTRIP", "TRT_COMB")
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
                           show.key = FALSE)
    df$REP <- as.factor(df$REP)
    p2 <- desplot::desplot(REP ~  COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           text = PLOT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE)
  } else {
    df <- df[,c(1:3,8,9,4:7)]
    colnames(df) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "SQUARE", "ROW_SQ", 
                      "COLUMN_SQ", "TREATMENT")
    rows <- max(as.numeric(df$ROW))
    cols <- max(as.numeric(df$COLUMN))
    ds <- "Row-Column Design " 
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    df$TREATMENT <- as.factor(df$TREATMENT)
    p1 <- desplot::desplot(TREATMENT ~ COLUMN + ROW, flip = FALSE,
                           out1 = SQUARE,
                           out2.gpar = list(col = "black", lty = 3), 
                           text = TREATMENT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE)
    df$SQUARE <- as.factor(df$SQUARE)
    p2 <- desplot::desplot(SQUARE ~ COLUMN + ROW, flip = FALSE,
                           out1 = SQUARE,
                           text = PLOT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main,
                           show.key = FALSE)
  }
  return(list(p1 = p1, p2 = p2, df = df, newBooks = newBooks))
}
