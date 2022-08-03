#' Function to export a formatted .csv table from data in the Fieldbook
#'
#' @param Fieldbook A list from a FielDHub design. 
#' @param selected A number, to select which location to view.
#' @importFrom utils tail
#' @noRd
export_layout <- function(Fieldbook, selected, plotOn = FALSE) {
  
  dataIn <- Fieldbook
  
  locs <- levels(factor(dataIn$LOCATION))
  
  df_site_one <- subset(dataIn, dataIn$LOCATION == locs[selected])
  
  if (!plotOn) {if ("ENTRY" %in% colnames(dataIn)) {
    type="ENTRY"
    
  } else if("TREATMENT" %in% colnames(dataIn)) {
    type="TREATMENT"
    
  } else {
    type="TRT_COMB"
  }} else {
    type = "PLOT"
  }
  
  cols <- length(levels(factor(df_site_one$COLUMN)))
  rows <- length(levels(factor(df_site_one$ROW)))
  mtx <- matrix(nrow = rows, ncol = cols)
  # k <- rows
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      mtx[i,j] <- subset(df_site_one, ROW == i & COLUMN == j)[[type]]
    }
    # k <- k - 1
  } 
  df <- as.data.frame(mtx)
  
  leftHead <- c("Location",locs[selected],1:(nrow(mtx)))
  blanks <- as.data.frame(matrix("", ncol = ncol(mtx)), nrow = 2)
  col_labels <- as.data.frame(matrix(c(1:ncol(mtx)), 
                                     ncol = ncol(mtx)), nrow = 1)
  
  names(blanks) <- names(df)
  names(col_labels) <- names(df)
  blanks2 <- rbind(blanks,col_labels)
  
  layout_entries3 <- rbind(blanks2, df)
  
  layout_entries2 <- cbind(leftHead, as.data.frame(layout_entries3))
  layout_entries2 <- layout_entries2[order(nrow(layout_entries2):1),]
  layout_entries2 <- rbind(tail(layout_entries2, 2)[2:1, ], head(layout_entries2, -2))
  rownames(layout_entries2) <- 1:(nrow(mtx) + 2)
  
  return(list(file = layout_entries2))
}