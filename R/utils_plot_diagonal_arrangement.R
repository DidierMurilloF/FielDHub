#' @noRd
plot_diagonal_arrangement <- function(x, l) {
    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))

    loc_field_book$ENTRY <- as.numeric(loc_field_book$ENTRY)
    
    main <- paste0("Un-replicated Diagonal Arrangement ", rows, " x ", cols)
    p1 <- desplot::ggdesplot(
        loc_field_book, 
        EXPT ~ COLUMN + ROW,  
        text = ENTRY, 
        col = CHECKS, 
        cex = 1, 
        out1 = EXPT,
        out2 = CHECKS, 
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        show.key = FALSE, 
        gg = TRUE,
        out2.gpar=list(col = "gray50", lwd = 1, lty = 1)
    )
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_prep <- function(x, l) {

    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))

    loc_field_book$ENTRY <- as.character(loc_field_book$ENTRY)
    
    loc_field_book$binay_checks <- ifelse(loc_field_book$CHECKS != 0, 1, 0)
    
    main <- paste0("Partially Replicated Design ", rows, " x ", cols)
    p1 <- desplot::ggdesplot(
        data = loc_field_book, 
        binay_checks ~ COLUMN + ROW,  
        text = ENTRY,  
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        cex = 1,
        show.key = FALSE, 
        gg = TRUE,
        col.regions = c("gray", "seagreen")
    )
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_optim <- function(x, l) {
  
    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))
    
    loc_field_book$ENTRY <- as.character(loc_field_book$ENTRY)
    loc_field_book$CHECKS <- as.character(loc_field_book$CHECKS)
    
    main <- paste0("Un-replicated Optimized Arrangement ", rows, " x ", cols)
    
    p1 <- desplot::ggdesplot(
        loc_field_book,
        CHECKS ~ COLUMN + ROW,
        text= ENTRY,
        cex=1,
        main = main,
        show.key=FALSE,
        xlab = "COLUMNS",
        ylab = "ROWS",
        gg = TRUE)
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_augmented_RCBD <- function(x, l) {
  
    fieldbook <- x$fieldBook
    
    sites <- factor(fieldbook$LOCATION, levels = unique(fieldbook$LOCATION))
    
    site_levels <- levels(sites)
    
    loc_field_book <- fieldbook |> 
        dplyr::filter(LOCATION == site_levels[l]) |> 
        as.data.frame()
    
    cols <- max(as.numeric(loc_field_book$COLUMN))
    rows <- max(as.numeric(loc_field_book$ROW))
    
    loc_field_book$ENTRY <- as.character(loc_field_book$ENTRY)
    loc_field_book$CHECKS <- as.character(loc_field_book$CHECKS)
    loc_field_book$BLOCK <- as.character(loc_field_book$BLOCK)
    main <- paste0("Augmented RCBD Layout ", rows, " x ", cols)
    p1 <- desplot::ggdesplot(
        BLOCK ~ COLUMN + ROW,  
        text = ENTRY, 
        col = CHECKS, 
        cex = 1, 
        out1 = EXPT,
        out2 = BLOCK,
        data = loc_field_book, 
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        show.key = FALSE, 
        gg = TRUE,
        out2.gpar=list(col = "gray50", lwd = 1, lty = 1))
    
    p1 <- add_gg_features(p1)
    
    main_plot <- paste0("Augmented RCBD Plot Number Layout ", rows, " x ", cols)
    p2 <- desplot::ggdesplot(
      BLOCK ~ COLUMN + ROW,  
      text = PLOT, 
      cex = 1.1, 
      out1 = EXPT,
      out2 = BLOCK,
      data = loc_field_book, 
      xlab = "COLUMNS", 
      ylab = "ROWS",
      main = main_plot,
      show.key = FALSE, 
      gg = TRUE,
      out2.gpar=list(col = "gray50", lwd = 1, lty = 1))
    
    p2 <- add_gg_features(p2)
    
    return(list(p1 = p1, p2 = p2, allSitesFieldbook = fieldbook))
}
