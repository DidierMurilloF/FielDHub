#' @title Compute Index Ranges
#'
#' @description
#' Computes the index ranges (starting and ending positions) of elements in a numeric vector 
#' or elements contained in a list of vectors. 
#'
#' @param x A numeric vector or a list of numeric vectors.
#' @return A list containing two vectors: 'from' and 'to', representing the starting
#' and ending positions respectively.
#' @noRd
compute_index_ranges <- function(x) {
  if (is.numeric(x) && is.vector(x)) {
    # Handling numeric vector
    to = cumsum(x)
    from = to - x + 1
    return(list(from = from, to = to))
  } else if (is.list(x) && all(sapply(x, function(elem) is.vector(elem) && is.numeric(elem)))) {
    # Handling list of numeric vectors
    lengths = unlist(lapply(x, length))
    to = cumsum(lengths)
    from = to - lengths + 1
    return(list(from = from, to = to))
  } else {
    fieldhub_abort("'x' must be a numeric vector or a list of numeric vectors")
  }
}

#' @title Total elements in a list
#'
#' @description
#' Counts the total number of elements within a list, including those within nested lists or vectors.
#'
#' @param alist A list for which the total number of elements is desired.
#' @return The total number of elements
#' @noRd
total_elements <- function(alist) {
  if (!is.list(alist)) {
    fieldhub_abort("The 'total_elements' function requires a list as input.")
  }
  
  length(unlist(alist))
}

#' @title Split matrix Into sub matrices
#' 
#' @description
#' Splits a matrix into a list of blocks, either by rows or by columns, based on the specified sizes of the blocks.
#'
#' @param matrix_object A matrix to be split.
#' @param blocks Either a list or a vector indicating the sizes of the blocks to be split into. 
#' If \code{blocks} is a list of vectors, each vector's length defines the size of the blocks. 
#' If \code{blocks} is a vector, each element represents the size of a block.
#' @param byrow A logical value. If \code{TRUE} (the default), the matrix is split 
#' by rows; otherwise, it is split by columns.
#' @return A list of matrices, each representing a block.
#' @noRd
split_matrix_into_blocks <- function(matrix_object, blocks, byrow = TRUE) {

  if (!is.matrix(matrix_object)) {
    fieldhub_abort("Input must be a matrix.")
  }
    
  if (!is.list(blocks) && !is.numeric(blocks)) {
    fieldhub_abort("Blocks must be a numeric vector or a list of numeric vectors.")
  }
  
  num_blocks = length(blocks)
  if (is.list(blocks)) {
    size = total_elements(blocks)
    start_end = compute_index_ranges(blocks)
    from = start_end$from
    to = start_end$to    
  }
  if (is.numeric(blocks)) {
    size = sum(blocks)
    to = cumsum(blocks)
    from = to - blocks + 1
  }
  
  # empty list to store results
  blocks_list = vector(mode="list", length=num_blocks)
  
  # Validate the total size against the matrix dimension before the loop
  if (byrow && size != nrow(matrix_object)) {
    fieldhub_abort("Number of rows in 'matrix_object' does not match 'blocks'")
  } else if (!byrow && size != ncol(matrix_object)) {
    fieldhub_abort("Number of columns in 'matrix_object' does not match 'blocks'")
  }
  
  # Use a loop to populate the blocks_list based on the 'byrow' flag
  for (k in 1:num_blocks) {
    if (byrow) {
      blocks_list[[k]] = matrix_object[from[k]:to[k], , drop = FALSE]  # Ensuring the result is always a matrix
    } else {
      blocks_list[[k]] = matrix_object[, from[k]:to[k], drop = FALSE]  # Ensuring the result is always a matrix
    }
  }
  
  return(blocks_list)
}
