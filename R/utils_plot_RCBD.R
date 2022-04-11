#' plot_RCBD 
#'allSitesFieldbook
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_RCBD <- function(x = NULL, n_TrtGen = NULL, n_Reps = NULL, optionLayout = 1, orderReps = "horizontal_stack_panel", 
                      planter = "serpentine", l = 1) {
  site <- l
  locations <- factor(x$fieldBook$LOCATION, levels = unique(x$fieldBook$LOCATION))
  nlocs <- length(locations)
  newBooksLocs <- vector(mode = "list", length = nlocs)
  countLocs <- 1
  books0 <- list(NULL)
  books1 <- list(NULL)
  books2 <- list(NULL)
  books3 <- list(NULL)
  books4 <- list(NULL)
  books5 <- list(NULL)
  books6 <- list(NULL)
  for (locs in levels(locations)) {
    NewBook <- x$fieldBook %>%
      dplyr::filter(LOCATION == locs)
    plots <- NewBook$PLOT
    if (orderReps == "vertical_stack_panel") {
      books0 <- list(NULL)
      COLUMN <- rep(1:n_TrtGen, times = n_Reps)
      books0[[1]] <- NewBook %>% 
        dplyr::mutate(ROW = rep(1:n_Reps, each = n_TrtGen),
                      COLUMN = COLUMN)
      y <- numbers::primeFactors(n_TrtGen)
      if (length(y) >= 2) {
        if (length(y) == 2) {
          if (sqrt(n_TrtGen) %% 1 == 0) {
            y1 <- y
            sq <- sqrt(n_TrtGen)
            y2 <- c(sq, sq)
            Y <- unique(data.frame(rbind(y1, y2)))
            print(Y)
            dm <- nrow(Y)
          } else {
            y1 <- y
            Y <- data.frame(rbind(y1))
            print(Y)
            dm <- nrow(Y)
          }
        }else if (length(y) == 3) {
          if (sqrt(n_TrtGen) %% 1 == 0) {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2+1):length(y)]))
            sq <- sqrt(n_TrtGen)
            y3 <- c(sq, sq)
            Y <- unique(data.frame(rbind(y1, y2, y3)))
            print(print(Y))
            dm <- nrow(Y)
          } else {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2 + 1):length(y)]))
            Y <- unique(data.frame(rbind(y1, y2)))
            dm <- nrow(Y)
          }
        } else if (length(y) >= 4) {
          if (sqrt(n_TrtGen) %% 1 == 0) {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2+1):length(y)]))
            y3 <- rev(c(prod(y[1:3]), y[4]))
            sq <- sqrt(n_TrtGen)
            y4 <- c(sq, sq)
            Y <- unique(data.frame(rbind(y4, y3, y2, y1)))
            print(print(Y))
            dm <- nrow(Y)
          } else {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2+1):length(y)]))
            y3 <- rev(c(prod(y[1:3]), y[4]))
            Y <- unique(data.frame(rbind(y3, y2, y1)))
            dm <- nrow(Y)
          }
        }
        # if (length(y) == 2) {
        #   y1 <- y
        #   Y <- unique(data.frame(rbind(y1)))
        #   dm <- nrow(Y)
        # }
        # if (length(y) > 2) {
        #   y1 <- c(y[1], prod(y[2:length(y)]))
        #   y2 <- c(prod(y[1:length(y)-1]), y[length(y)])
        #   y2 <- sort(y2, decreasing = FALSE)
        #   Y <- unique(data.frame(rbind(y1, y2)))
        #   dm <- nrow(Y)
        # }
        print(Y)
        books1 <- vector(mode = "list", length = dm)
        for (k in 1:dm) {
          s1 <- as.numeric(Y[k,][1])
          s2 <- as.numeric(Y[k,][2])
          COLUMNS <- rep(rep(1:s2, times = s1), times = n_Reps)
          x$bookROWCol <- NewBook %>% 
            dplyr::mutate(ROW = rep(1:(s1*n_Reps), each = s2),
                          COLUMN = COLUMNS)
          df <- x$bookROWCol
          nCols <- max(df$COLUMN)
          newPlots <- planter_transform(plots = plots, planter = planter, reps = n_Reps, 
                                        cols = nCols, units = NULL)
          df$PLOT <- newPlots
          books1[[k]] <- df
        }
      }
    } else if (orderReps == "horizontal_stack_panel") {
      y <- numbers::primeFactors(n_TrtGen)
      if (length(y) >= 2) {
        if (length(y) == 2) {
          if (sqrt(n_TrtGen) %% 1 == 0) {
            y1 <- y
            sq <- sqrt(n_TrtGen)
            y2 <- c(sq, sq)
            Y <- unique(data.frame(rbind(y1, y2)))
            print(Y)
            dm <- nrow(Y)
          } else {
            y1 <- y
            Y <- data.frame(rbind(y1))
            print(Y)
            dm <- nrow(Y)
          }
        }else if (length(y) == 3) {
          if (sqrt(n_TrtGen) %% 1 == 0) {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2+1):length(y)]))
            sq <- sqrt(n_TrtGen)
            y3 <- c(sq, sq)
            Y <- unique(data.frame(rbind(y1, y2, y3)))
            print(print(Y))
            dm <- nrow(Y)
          } else {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2 + 1):length(y)]))
            Y <- unique(data.frame(rbind(y1, y2)))
            dm <- nrow(Y)
          }
        } else if (length(y) >= 4) {
          if (sqrt(n_TrtGen) %% 1 == 0) {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2+1):length(y)]))
            y3 <- rev(c(prod(y[1:3]), y[4]))
            sq <- sqrt(n_TrtGen)
            y4 <- c(sq, sq)
            Y <- unique(data.frame(rbind(y4, y3, y2, y1)))
            print(print(Y))
            dm <- nrow(Y)
          } else {
            y1 <- c(y[1], prod(y[2:length(y)]))
            y2 <- c(y[1] * y[2], prod(y[(2+1):length(y)]))
            y3 <- rev(c(prod(y[1:3]), y[4]))
            Y <- unique(data.frame(rbind(y3, y2, y1)))
            dm <- nrow(Y)
          }
        }
        books2 <- vector(mode = "list", length = dm)
        for (h_panel in 1:dm) {
          s1 <- as.numeric(Y[h_panel,][1])
          s2 <- as.numeric(Y[h_panel,][2])
          w <- 1:(s1*n_Reps)
          u <- seq(1, length(w), by = s1)
          v <- seq(s1, length(w), by = s1)
          z <- vector(mode = "list", length = n_Reps)
          for (j in 1:n_Reps) {
            z[[j]] <- c(rep(u[j]:v[j], times = s2))
          }
          z <- unlist(z)
          x$bookROWCol <- NewBook %>%
            dplyr::mutate(ROW = rep(rep(1:s2, each = s1), n_Reps),
                          COLUMN = z)
          df2 <- x$bookROWCol
          # nRows <- max(df2$ROW)
          nCols <- max(df2$COLUMN)
          newPlots <- planter_transform(plots = plots, planter = planter, reps = n_Reps, cols = nCols,
                                        mode = "Horizontal", units = NULL)
          df2$PLOT <- newPlots
          books2[[h_panel]] <- df2
        } 
      }else if (length(y) == 1) {
        books3 <- list(NULL)
        ROWS <- rep(1:n_TrtGen, times = n_Reps)
        books3[[1]] <- NewBook %>% 
          dplyr::mutate(ROW = ROWS,
                        COLUMN = rep(1:n_Reps, each = n_TrtGen))
      }
    }
    books_rcbd <- c(books3, books2, books1, books4, books5, books6, books0)
    newBooks <- books_rcbd[!sapply(books_rcbd,is.null)]
    newBooksLocs[[countLocs]] <- newBooks
    countLocs <- countLocs + 1
  }
  opt <- optionLayout
  newBooksSelected <- newBooksLocs[[site]]
  df1 <- newBooksSelected[opt]
  df <- as.data.frame(df1)
  
  
  if (x$infoDesign$idDesign == 2) {
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
    ds <- "Randomized Complete Block Design " 
    main <- paste0(ds, rows, "X", cols)
    # Plot field layout
    p1 <- desplot::desplot(TREATMENT ~ COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           out2.gpar = list(col = "black", lty = 3), 
                           text = TREATMENT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE, 
                           gg = TRUE)
    #df$PLOT <- as.factor(df$PLOT)
    df$REP <- as.factor(df$REP)
    p2 <- desplot::desplot(REP ~ COLUMN + ROW, flip = FALSE,
                           out1 = REP,
                           #out2.gpar = list(col = "black", lty = 3), 
                           text = PLOT, cex = 1, shorten = "no",
                           data = df, xlab = "COLUMNS", ylab = "ROWS",
                           main = main, 
                           show.key = FALSE,
                           gg = TRUE)
  } else if (x$infoDesign$idDesign == 4) {
    if (x$infoDesign$kind == "RCBD") {
      allSites <- vector(mode = "list", length = nlocs)
      for (st in 1:nlocs) {
        newBooksSelected_1 <- newBooksLocs[[st]]
        df_1 <- newBooksSelected_1[opt]
        allSites[[st]] <- as.data.frame(df_1)
      }
      nc <- ncol(df)
      allSitesFieldbook <- dplyr::bind_rows(allSites)
      allSitesFieldbook <- allSitesFieldbook[,c(1:3, (nc-1), nc, 4:(nc-2))]
      df <- df[,c(1:3, (nc-1), nc, 4:(nc-2))]
      # Plot field layout
      rows <- max(as.numeric(df$ROW))
      cols <- max(as.numeric(df$COLUMN))
      ds <- "Full Factorial Design (RCBD) " 
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
      
      df$PLOT <- as.factor(df$PLOT)
      df$REP <- as.factor(df$REP)
      p2 <- desplot::desplot(REP ~ COLUMN + ROW, flip = FALSE,
                             out1 = REP,
                             #out2.gpar = list(col = "black", lty = 3),
                             text = PLOT, cex = 1, shorten = "no",
                             data = df, xlab = "COLUMNS", ylab = "ROWS",
                             main = main,
                             show.key = FALSE,
                             gg = TRUE)
    }
  } else if (x$infoDesign$idDesign == 7) {
      allSites <- vector(mode = "list", length = nlocs)
      for (st in 1:nlocs) {
        newBooksSelected_1 <- newBooksLocs[[st]]
        df_1 <- newBooksSelected_1[opt]
        allSites[[st]] <- as.data.frame(df_1)
      }
      allSitesFieldbook <- dplyr::bind_rows(allSites)
      allSitesFieldbook <- allSitesFieldbook[,c(1:3,8,9,4:7)]
      colnames(allSitesFieldbook) <- c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "HSTRIP", "VSTRIP", "TRT_COMB")
      
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
  } 
  return(list(p1 = p1, p2 = p2, df = df, newBooks = newBooksSelected, 
              allSitesFieldbook = allSitesFieldbook))
}
