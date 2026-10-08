#-----------------------------------------------------------------------
# print() and summary() of each design. Each design result has the class
# c("fieldhub_<design>", "FielDHub"), and summary() gives it the class
# c("summary.fieldhub_<design>", "summary.FielDHub").
#-----------------------------------------------------------------------

# The design parameters, without the internal design identifier
str_parameters <- function(x) {
  str(x$infoDesign[names(x$infoDesign) != "id_design"])
}

# The first n rows of a data frame, under a line that names it
print_head <- function(data, what, n, ...) {
  nhead_print <- infoPrint(n, nrow(data))
  cat("\n", nhead_print, paste("First observations of the data frame with", what), "\n")
  print(head(data, n = nhead_print, ...))
}

# print() of most designs: the title, the design parameters and the first
# rows of the field book, after the efficiency of the design if given
print_design <- function(x, title, book, n, ..., efficiency = NULL) {
  cat(title, "\n\n")
  if (!is.null(efficiency)) {
    cat("Efficiency of design:", "\n")
    print(efficiency)
    cat("\n")
  }
  cat("Information on the design parameters:", "\n")
  str_parameters(x)
  print_head(x$fieldBook, paste("the", book, "field book:"), n, ...)
  invisible(x)
}

# A section of a summary that shows a list, such as the design parameters
summary_list <- function(heading, value) {
  cat(heading, "\n")
  str(value)
  cat("\n")
}

# A section of a summary that prints a value, such as a layout
summary_print <- function(heading, value) {
  cat(heading, "\n")
  print(value)
  cat("\n")
}

# A section of a summary that shows the structure of a data frame
summary_str <- function(heading, value) {
  cat(heading, "\n\n")
  str(value)
}

#-----------------------------------------------------------------------
# CRD
#-----------------------------------------------------------------------
#' @export
print.fieldhub_crd <- function(x, n = 10, ...) {
  print_design(x, "Completely Randomized Design (CRD)", "CRD", n, ...)
}

#' @export
print.summary.fieldhub_crd <- function(x, ...) {
  cat("Completely Randomized Design (CRD):", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the CRD field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# RCBD
#-----------------------------------------------------------------------
#' @export
print.fieldhub_rcbd <- function(x, n = 10, ...) {
  print_design(x, "Randomized Complete Block Design (RCBD):", "RCBD", n, ...)
}

#' @export
print.summary.fieldhub_rcbd <- function(x, ...) {
  cat("Randomized Complete Block Design (RCBD):", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Layout randomization for each location:", x$layoutRandom)
  summary_print("3. Plot numbers layout:", x$plotNumber)
  summary_str("4. Structure of the data frame with the RCBD field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Latin square
#-----------------------------------------------------------------------
#' @export
print.fieldhub_latin_square <- function(x, n = 10, ...) {
  print_design(x, "Latin Square Design:", "latin_square", n, ...)
}

#' @export
print.summary.fieldhub_latin_square <- function(x, ...) {
  cat("Latin Square Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Squares:", x$squares)
  summary_print("3. Plot squares:", x$plotSquares)
  summary_str("4. Structure of the data frame with the latin_square field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Full factorial
#-----------------------------------------------------------------------
#' @export
print.fieldhub_full_factorial <- function(x, n = 10, ...) {
  print_design(x, "Full Factorial Design", "full_factorial", n, ...)
}

#' @export
print.summary.fieldhub_full_factorial <- function(x, ...) {
  cat("Full Factorial Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the full_factorial field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Split plot
#-----------------------------------------------------------------------
#' @export
print.fieldhub_split_plot <- function(x, n = 10, ...) {
  print_design(x, "Split Plot Design", "split_plot", n, ...)
}

#' @export
print.summary.fieldhub_split_plot <- function(x, ...) {
  cat("Split Plot Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Layout randomization for each location:", x$layoutlocations)
  summary_str("3. Structure of the data frame with the split_plot field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Split-split plot
#-----------------------------------------------------------------------
#' @export
print.fieldhub_split_split_plot <- function(x, n = 10, ...) {
  print_design(x, "Split-Split Plot Design", "split_split_plot", n, ...)
}

#' @export
print.summary.fieldhub_split_split_plot <- function(x, ...) {
  cat("Split-Split Plot Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the split_split_plot field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Strip plot
#-----------------------------------------------------------------------
#' @export
print.fieldhub_strip_plot <- function(x, n = 10, ...) {
  print_design(x, "Strip Plot Design", "strip_plot", n, ...)
}

#' @export
print.summary.fieldhub_strip_plot <- function(x, ...) {
  cat("Strip Plot Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Layout randomization for each location:", x$stripsBlockLoc)
  summary_print("3. Plot number layout for each location:", x$plotLayouts)
  summary_str("4. Structure of the data frame with the strip_plot field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Incomplete blocks
#-----------------------------------------------------------------------
#' @export
print.fieldhub_incomplete_blocks <- function(x, n = 10, ...) {
  print_design(x, "Incomplete Blocks Design", "incomplete_blocks", n, ...,
               efficiency = x$blocksModel)
}

#' @export
print.summary.fieldhub_incomplete_blocks <- function(x, ...) {
  cat("Incomplete Blocks Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the incomplete_blocks field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Row-column
#-----------------------------------------------------------------------
#' @export
print.fieldhub_row_column <- function(x, n = 10, ...) {
  if (!is.null(x$infoDesign$optimization) && x$infoDesign$optimization == "onestage") {
    title <- "Resolvable Row-Column Design (One-Step Optimization)"
  } else {
    title <- "Resolvable Row-Column Design (Two-Step Optimization)"
  }
  print_design(x, title, "row_column", n, ..., efficiency = x$blocksModel[[1]])
}

#' @export
print.summary.fieldhub_row_column <- function(x, ...) {
  cat("Row Column Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Resolvable row column blocks", x$resolvableBlocks)
  summary_print("3. Concurrence matrix:", x$concurrence)
  summary_str("4. Structure of the data frame with the row_column field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Square lattice
#-----------------------------------------------------------------------
#' @export
print.fieldhub_square_lattice <- function(x, n = 10, ...) {
  print_design(x, "Square Lattice Design", "square_lattice", n, ...,
               efficiency = x$blocksModel)
}

#' @export
print.summary.fieldhub_square_lattice <- function(x, ...) {
  cat("Square Lattice:", "\n\n")
  summary_list("1. Efficiency of design:", x$blocksModel)
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the square_lattice field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Rectangular lattice
#-----------------------------------------------------------------------
#' @export
print.fieldhub_rectangular_lattice <- function(x, n = 10, ...) {
  print_design(x, "Rectangular Lattice Design", "rectangular_lattice", n, ...,
               efficiency = x$blocksModel)
}

#' @export
print.summary.fieldhub_rectangular_lattice <- function(x, ...) {
  cat("Rectangular Lattice Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the rectangular_lattice field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Alpha lattice
#-----------------------------------------------------------------------
#' @export
print.fieldhub_alpha_lattice <- function(x, n = 10, ...) {
  print_design(x, "Alpha Lattice Design", "alpha_lattice", n, ...,
               efficiency = x$blocksModel)
}

#' @export
print.summary.fieldhub_alpha_lattice <- function(x, ...) {
  cat("Alpha Lattice Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_str("2. Structure of the data frame with the alpha_lattice field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Partially replicated
#-----------------------------------------------------------------------
#' @export
print.fieldhub_partially_replicated <- function(x, n = 10, ...) {
  cat("Partially Replicated Design", "\n\n")
  cat("Replications within location:", "\n")
  print(x$reps_info)
  cat("\n", "Information on the design parameters:", "\n")
  str_parameters(x)
  print_head(x$fieldBook, "the partially_replicated field book:", n, ...)
  invisible(x)
}

#' @export
print.summary.fieldhub_partially_replicated <- function(x, ...) {
  cat("Partially Replicated Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Layout randomization:", x$layoutRandom)
  summary_print("3. Plot number layout:", x$plotNumber)
  summary_str("4. Structure of the data frame with the data input:", x$dataEntry)
  summary_str("5. Structure of the data frame with the partially_replicated field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Multi-location partially replicated
#-----------------------------------------------------------------------
#' @export
print.fieldhub_multi_location_prep <- function(x, n = 10, ...) {
  cat("Multi-Location Partially Replicated Design", "\n")
  cat("\n", "Replications within location:", "\n")
  print(x$reps_info)
  cat("\n", "Information on the design parameters:", "\n")
  str_parameters(x)
  print_head(x$fieldBook, "the partially_replicated field book:", n, ...)
  invisible(x)
}

#' @export
print.summary.fieldhub_multi_location_prep <- function(x, ...) {
  cat("Multi-Location Partially Replicated Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Replications within location:", x$reps_info)
  summary_str("3. Structure of the data frame with the data input:", x$dataEntry)
  summary_str("4. Structure of the data frame with the multi_location_prep field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Augmented RCBD
#-----------------------------------------------------------------------
#' @export
print.fieldhub_rcbd_augmented <- function(x, n = 10, ...) {
  print_design(x, "Augmented Randomized Complete Block Design:", "RCBD_augmented", n, ...)
}

#' @export
print.summary.fieldhub_rcbd_augmented <- function(x, ...) {
  cat("Augmented Randomized Complete Block Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Layout randomization:", x$layoutRandom)
  summary_print("3. Plot number layout:", x$plotNumber)
  summary_print("4. Experiments name layout:", x$exptNames)
  summary_str("5. Structure of the data frame with the data input:", x$data_entry)
  summary_str("6. Structure of the data frame with the RCBD_augmented field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Diagonal arrangement
#-----------------------------------------------------------------------
#' @export
print.fieldhub_diagonal_arrangement <- function(x, n = 10, ...) {
  print_design(x, "Un-replicated Diagonal Arrangement Design", "diagonal_arrangement", n, ...)
}

#' @export
print.summary.fieldhub_diagonal_arrangement <- function(x, ...) {
  cat("Un-replicated Diagonal Arrangement Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Layout randomization:", x$layoutRandom)
  summary_print("3. Plot number layout:", x$plotsNumber)
  summary_str("4. Structure of the data frame with the data input:", x$data_entry)
  summary_str("5. Structure of the data frame with the diagonal_arrangement field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Sparse allocation
#-----------------------------------------------------------------------
#' @export
print.fieldhub_sparse_allocation <- function(x, n = 10, ...) {
  print_design(x, "Sparse Allocation: Un-replicated Diagonal Arrangement Design",
               "diagonal_arrangement", n, ...)
}

#' @export
print.summary.fieldhub_sparse_allocation <- function(x, ...) {
  cat("Sparse Allocation: Un-replicated Diagonal Arrangement Design:", "\n\n")
  summary_list("1. Information on the design parameters:", x$infoDesign)
  summary_print("2. Number of lines allocated to each location:", x$size_locations)
  summary_str("3. Structure of the data frame with the data input:", x$data_entry)
  summary_str("4. Structure of the data frame with the sparse_allocation field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Optimized arrangement
#-----------------------------------------------------------------------
#' @export
print.fieldhub_optimized_arrangement <- function(x, n = 10, ...) {
  print_design(x, "Un-replicated Optimized Arrangement Design", "optimized_arrangement", n, ...)
}

#' @export
print.summary.fieldhub_optimized_arrangement <- function(x, ...) {
  cat("Un-replicated Optimized Arrangement Design:", "\n\n")
  cat("1. Information on the design parameters:", "\n")
  str_parameters(x)
  cat("\n")
  summary_print("2. Layout randomization:", x$layoutRandom)
  summary_print("3. Plot number layout:", x$plotNumber)
  summary_str("4. Structure of the data frame with the data input:", x$dataEntry)
  summary_str("5. Structure of the data frame with the optimized_arrangement field book:", x$fieldBook)
  invisible(x)
}

#-----------------------------------------------------------------------
# Split families
#-----------------------------------------------------------------------
#' @export
print.fieldhub_split_families <- function(x, n = 10, ...) {
  cat("Split families:", "\n\n")
  cat("\n", "Data frame with the summary of cases by location:", "\n")
  print(x$rowsEachlist)
  print_head(x$data_locations, "the entries for each location:", n, ...)
  invisible(x)
}

#' @export
print.summary.fieldhub_split_families <- function(x, ...) {
  cat("Split families:", "\n\n")
  summary_str("1. Structure of the data frame with the summary of entries by location:", x$rowsEachlist)
  summary_str("2. Structure of the data frame with the entries for each location:", x$data_locations)
  invisible(x)
}
