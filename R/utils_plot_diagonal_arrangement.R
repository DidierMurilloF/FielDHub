#' @noRd
plot_diagonal_arrangement <- function(x, l, ...) {
    dots <- list(...)
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
    p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
        data = loc_field_book,
        form = EXPT ~ COLUMN + ROW,
        text.string = "ENTRY",
        col.string = "CHECKS",
        cex = 1, 
        shorten = "no",
        out1.string = "EXPT",
        out2.string = "CHECKS",
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        show.key = FALSE, 
        gg = TRUE,
        out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
    ), dots))
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_prep <- function(x, l, ...) {
    dots <- list(...)
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
    p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
        data = loc_field_book, 
        form = binay_checks ~ COLUMN + ROW,
        text.string = "ENTRY",
        xlab = "COLUMNS", 
        ylab = "ROWS",
        main = main,
        cex = 1,
        shorten = "no",
        show.key = FALSE, 
        gg = TRUE,
        col.regions = c("gray", "seagreen")
    ), dots))
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}

#' @noRd
plot_optim <- function(x, l, ...) {
    dots <- list(...)
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
    
    p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
        data = loc_field_book,
        form = CHECKS ~ COLUMN + ROW,
        text.string = "ENTRY",
        cex = 1,
        shorten = "no",
        main = main,
        show.key = FALSE,
        xlab = "COLUMNS",
        ylab = "ROWS",
        gg = TRUE
    ), dots))
    
    return(list(p1 = p1, allSitesFieldbook = fieldbook))
}


#' @noRd
plot_augmented_RCBD <- function(x, l, ...) {
  dots <- list(...)
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
  
  # plot numbers as plain integers, whatever PLOT is stored as
  loc_field_book$PLOT_TXT <- sprintf("%d", as.integer(loc_field_book$PLOT))
  
  # text color groups for p1: checks are highlighted, test lines are not
  loc_field_book$CHECK_TEXT <- ifelse(loc_field_book$CHECKS == "1",
                                      "check", "test")
  check_text_cols <- c(check = "red3", test = "gray10")
  
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
  
  p1 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = loc_field_book,
    form = BLOCK ~ COLUMN + ROW,
    text.string = "ENTRY",
    col.string = "CHECK_TEXT",
    col.text = check_text_cols,
    cex = 0.8,
    shorten = "no",
    out1.string = "EXPT",
    out2.string = "BLOCK",
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = main,
    show.key = FALSE,
    gg = TRUE,
    ticks = "all",
    panel.border = FALSE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  ), dots)) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none")
  
  p1 <- p1 + fieldhub_layout_theme()
  
  # -----------------------------
  # p2: plot numbers (NO check highlighting)
  # -----------------------------
  main_plot <- paste0("Augmented RCBD Plot Number Layout ", rows, " x ", cols)
  
  p2 <- do.call(desplot::ggdesplot, utils::modifyList(list(
    data = loc_field_book,
    form = BLOCK ~ COLUMN + ROW,
    text.string = "PLOT_TXT",
    col.text = "gray10",
    cex = 0.8,
    shorten = "no",
    out1.string = "EXPT",
    out2.string = "BLOCK",
    xlab = "COLUMNS",
    ylab = "ROWS",
    main = main_plot,
    show.key = FALSE,
    gg = TRUE,
    ticks = "all",
    panel.border = FALSE,
    out2.gpar = list(col = "gray50", lwd = 1, lty = 1)
  ), dots)) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none")
  
  p2 <- p2 + fieldhub_layout_theme()
  
  return(list(p1 = p1, p2 = p2, allSitesFieldbook = fieldbook))
}
