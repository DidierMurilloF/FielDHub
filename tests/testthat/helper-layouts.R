# Field layouts that plot_layout() builds for the catalogue designs whose
# field map is not in the field book (id_design 1 to 12).

layout_planters <- c("serpentine", "cartesian")
layout_stackings <- c("vertical", "horizontal", "grid_panel")

# Catalogue entries with layout options
layout_entries <- function() {
  Filter(function(name) {
    id <- catalogue_design(name)$infoDesign$id_design
    is.numeric(id) && id <= 12
  }, names(catalogue))
}

# One column of a location's field book at its coordinates, row 1 first
layout_grid <- function(book, column) {
  grid <- matrix(".", nrow = max(book$ROW), ncol = max(book$COLUMN))
  grid[cbind(book$ROW, book$COLUMN)] <- as.character(book[[column]])
  width <- max(nchar(grid))
  apply(grid, 1, function(row) paste(formatC(row, width = width), collapse = " "))
}

quiet_layout <- function(...) {
  tryCatch(suppressWarnings(suppressMessages(plot_layout(...))), error = function(e) e)
}

# For every planter, stacking and layout option: the title of the plot, the
# columns of the field book with coordinates, and for each location the
# field-book row (ID) and the plot number at each coordinate.
print_layouts <- function(design) {
  for (stacked in layout_stackings) {
    for (planter in layout_planters) {
      cat("## ", planter, ", ", stacked, "\n", sep = "")
      first <- quiet_layout(design, layout = 1, planter = planter, stacked = stacked)
      if (inherits(first, "error")) {
        cat("Error:", conditionMessage(first), "\n")
        next
      }
      if (is.null(first)) {
        cat("No layout\n")
        next
      }
      for (k in seq_along(first$newBooks)) {
        p <- quiet_layout(design, layout = k, planter = planter, stacked = stacked)
        book <- p$allSitesFieldbook
        cat("### Layout ", k, ": ", p$out_layout$labels$title, "\n", sep = "")
        cat("Columns:", names(book), "\n")
        for (loc in unique(book$LOCATION)) {
          site <- book[book$LOCATION == loc, ]
          repeated <- sum(duplicated(site[c("ROW", "COLUMN")]))
          if (repeated > 0) cat("Coordinates used more than once:", repeated, "\n")
          cat("Location ", loc, ", ID:\n", sep = "")
          cat(layout_grid(site, "ID"), sep = "\n")
          cat("PLOT:\n")
          cat(layout_grid(site, "PLOT"), sep = "\n")
        }
      }
    }
  }
}
