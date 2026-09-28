#' Function to export a formatted .csv table from data in the Fieldbook
#'
#' @param Fieldbook A list from a FielDHub design.
#' @param selected Location number, in field-book appearance order, matching
#'   the layout plot and heatmap.
#' @param type_pref (optional) Column name to prefer as the exported cell
#'   value, when present in \code{Fieldbook} (e.g. \code{"TREATMENT"}). By
#'   default \code{NULL}, which preserves the original ENTRY-first behaviour.
#'   Designs whose on-screen map already labels plots by ENTRY (e.g.
#'   \code{plot_diagonal_arrangement}, which uses \code{text.string = "ENTRY"})
#'   should leave this at the default so the export keeps matching the map.
#' @importFrom utils tail
#' @noRd
export_layout <- function(Fieldbook, selected, plotOn = FALSE, type_pref = NULL) {

  dataIn <- Fieldbook
  if (!is.data.frame(dataIn) || !"LOCATION" %in% names(dataIn) ||
      !is.atomic(dataIn$LOCATION) || !is.null(dim(dataIn$LOCATION)) ||
      anyNA(dataIn$LOCATION)) {
    fieldhub_abort("The layout field book must contain nonmissing LOCATION identifiers.")
  }
  locs <- field_book_locations(dataIn)
  if (!is.numeric(selected) || is.complex(selected) || length(selected) != 1L ||
      !is.finite(selected) || !selected %in% seq_along(locs)) {
    fieldhub_abort("Select one available location for the layout export.")
  }
  df_site_one <- dataIn[dataIn$LOCATION == locs[selected], , drop = FALSE]

  if (!plotOn) {if (!is.null(type_pref) && type_pref %in% colnames(dataIn)) {
    type <- type_pref

  } else if ("ENTRY" %in% colnames(dataIn)) {
    type="ENTRY"

  } else if("TREATMENT" %in% colnames(dataIn)) {
    type="TREATMENT"

  } else {
    type="TRT_COMB"
  }} else {
    type = "PLOT"
  }
  
  mtx <- field_book_export_grid(df_site_one, type)
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
