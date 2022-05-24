#' Function to export a formatted .csv table from data in the Fieldbook
#'
#' @param Fieldbook A list from a FielDHub design. 
#' @param selected A number, to select which location to view.
#' @noRd
export_layout <- function(Fieldbook, selected) {

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop(
      "Package \"Matrix\" must be installed to use this function.",
      call. = FALSE
    )
  }

  dataIn <- Fieldbook
  
  locs <- levels(factor(dataIn$LOCATION))
  
  df_site_one <- subset(dataIn, dataIn$LOCATION == locs[selected])
  df_site_one$ROW <- factor(df_site_one$ROW, 
                            levels = unique(sort(df_site_one$ROW, 
                                                 decreasing = TRUE)))
  treats <- c(1:nrow(dataIn))
  
  if ("ENTRY" %in% colnames(dataIn)) {
    layout_entries <- with(
      df_site_one, 
      as.matrix(Matrix::sparseMatrix(i = as.numeric(ROW), 
                                     j=as.numeric(COLUMN), 
                                     x=ENTRY,
                                     dimnames=list(levels(ROW), 
                                                   levels(COLUMN)))))
    df <- as.data.frame(layout_entries)
    df <- as.data.frame(layout_entries)[nrow(df):1,]
    
  } else if("TREATMENT" %in% colnames(dataIn)) {
    M <- matrix(data = "", 
                nrow = length(levels(df_site_one$ROW)), 
                ncol = length(levels(factor(df_site_one$COLUMN))), 
                byrow = TRUE)
    layout_entries <- with(
      df_site_one, 
      as.matrix(Matrix::sparseMatrix(i = as.numeric(ROW),
                                     j = as.numeric(COLUMN), 
                                     x = treats, 
                                     dimnames = list(levels(ROW), 
                                                   levels(COLUMN)))))
    lookup <- dataIn$TREATMENT
    names(lookup) <- treats
    for (index in nrow(dataIn):1) {
      layout_entries[index] <- unname(lookup[index])
    }
    layout_entries <- matrix(data = lookup, 
                             nrow = length(levels(df_site_one$ROW)), 
                             ncol = length(levels(factor(df_site_one$COLUMN))), 
                             byrow = TRUE)
    df <- as.data.frame(layout_entries)
  } else {
    M <- matrix(data = "", 
                nrow = length(levels(df_site_one$ROW)), 
                ncol = length(levels(factor(df_site_one$COLUMN))), 
                byrow = TRUE)
    layout_entries <- with(
      df_site_one, 
      as.matrix(Matrix::sparseMatrix(i = as.numeric(ROW),
                                     j = as.numeric(COLUMN), 
                                     x = treats, 
                                     dimnames = list(levels(ROW), 
                                                   levels(COLUMN)))))
    lookup <- dataIn$TRT_COMB
    names(lookup) <- treats
    for (index in nrow(dataIn):1) {
      layout_entries[index] <- unname(lookup[index])
    }
    layout_entries <- matrix(data = lookup, 
                             nrow = length(levels(df_site_one$ROW)), 
                             ncol = length(levels(factor(df_site_one$COLUMN))), 
                             byrow = TRUE)
    df <- as.data.frame(layout_entries)
  }
  
  leftHead <- c("Location",locs[selected],1:(nrow(layout_entries)))
  blanks <- as.data.frame(matrix("", ncol = ncol(layout_entries)), nrow = 2)
  col_labels <- as.data.frame(matrix(c(1:ncol(layout_entries)), 
                                     ncol = ncol(layout_entries)), nrow = 1)
  
  names(blanks) <- names(df)
  names(col_labels) <- names(df)
  blanks2 <- rbind(blanks,col_labels)
  
  layout_entries3 <- rbind(blanks2, df)
  
  layout_entries2 <- cbind(leftHead, as.data.frame(layout_entries3))
  
  rownames(layout_entries2) <- 1:(nrow(layout_entries) + 2)
  #layout_entries2 <- rev(layout_entries2)
  #layout_entries2 <- as.data.frame(layout_entries2)
  return(list(file = layout_entries2))
  
  # filename <- paste0("Fieldbook_location", selected, "_", Sys.Date(), ".csv")
  # 
  # write.table(layout_entries2, file = filename, row.names = FALSE, 
  #             col.names = FALSE, sep = ",")
}