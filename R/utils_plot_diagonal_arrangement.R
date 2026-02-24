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
  
  # labels that must NEVER be reformatted by desplot
  loc_field_book$PLOT_TXT <- sprintf("%d", as.integer(loc_field_book$PLOT))
  
  # flag checks (used only for p1 text color)
  loc_field_book$IS_CHECK <- loc_field_book$CHECKS == "1"
  
  # -----------------------------
  # Muted palette for BLOCK bg
  # -----------------------------
  block_levels <- sort(unique(loc_field_book$BLOCK))
  muted6 <- c("#F2F2F2", "#E6EEF5", "#E9F2EC", "#F3EEE6", "#EDE7F2", "#F1E9E9")
  if (length(block_levels) > length(muted6)) {
    muted6 <- grDevices::colorRampPalette(muted6)(length(block_levels))
  } else {
    muted6 <- muted6[seq_along(block_levels)]
  }
  fill_vals <- stats::setNames(muted6, block_levels)
  
  # -----------------------------
  # p1: layout (ENTRY labels)
  # -----------------------------
  main <- paste0("Augmented RCBD Layout ", rows, " x ", cols)
  
  p1_bg <- desplot::ggdesplot(
    BLOCK ~ COLUMN + ROW,
    text = "",
    cex = 0,
    col = BLOCK,   # bg by block (muted palette)
    out1 = EXPT,
    out2 = BLOCK,
    data = loc_field_book,
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = main,
    show.key = FALSE,
    gg = TRUE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  )
  
  p1 <- p1_bg +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_text(
      data = dplyr::filter(loc_field_book, !IS_CHECK),
      ggplot2::aes(
        x = as.numeric(COLUMN),
        y = as.numeric(ROW),
        label = ENTRY
      ),
      inherit.aes = FALSE,
      color = "gray10",
      size = 3.2
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(loc_field_book, IS_CHECK),
      ggplot2::aes(
        x = as.numeric(COLUMN),
        y = as.numeric(ROW),
        label = ENTRY
      ),
      inherit.aes = FALSE,
      color = "red3",
      fontface = "bold",
      size = 3.2
    )
  
  p1 <- add_gg_features(p1)
  
  # -----------------------------
  # p2: plot numbers (NO check highlighting)
  # -----------------------------
  main_plot <- paste0("Augmented RCBD Plot Number Layout ", rows, " x ", cols)
  
  p2_bg <- desplot::ggdesplot(
    BLOCK ~ COLUMN + ROW,
    text = "",
    cex = 0,
    col = BLOCK,   # keep same muted bg by block
    out1 = EXPT,
    out2 = BLOCK,
    data = loc_field_book,
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = main_plot,
    show.key = FALSE,
    gg = TRUE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  )
  
  p2 <- p2_bg +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_text(
      data = loc_field_book,
      ggplot2::aes(
        x = as.numeric(COLUMN),
        y = as.numeric(ROW),
        label = PLOT_TXT
      ),
      inherit.aes = FALSE,
      color = "gray10",
      size = 3.2
    )
  
  p2 <- add_gg_features(p2)
  
  return(list(p1 = p1, p2 = p2, allSitesFieldbook = fieldbook))
}
