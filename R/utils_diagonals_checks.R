#' @importFrom utils capture.output
#' @noRd
diagonals_checks <- function(n_rows = NULL, n_cols = NULL, jump_by_cols = 5, jump_by_rows = 4,
                             name_file = "Car2020", checks = c(1,2,3,4), p_start = NULL){
  
  di <- jump_by_cols - n_rows
  
  if (n_rows < jump_by_cols){
    
    n_rows <- n_rows + di
    
  }
  w_map <- matrix(data = 0, ncol = n_cols, nrow = n_rows, byrow = F)
  
  breaks_points <- seq(1, (jump_by_cols + p_start)*jump_by_rows, jump_by_rows)
  breaks_points <- rev(breaks_points)
  min_breaks_points <- numeric(length = jump_by_cols)
  
  for (i in 1:jump_by_cols){
    min_breaks_points[i] <- min(seq(breaks_points[i], 1, by = -jump_by_cols))
  }
  
  star_break_points <- list()
  for (l in 1:length(min_breaks_points)) {
    star_break_points[[l]] <- seq(min_breaks_points[l], n_rows, by = jump_by_cols)
  }
  
  ################ some test ####################
  rem <- (n_cols) %% jump_by_cols
  divs <- (n_cols - rem) / jump_by_cols
  ###############################################
  
  for(i in 1:length(star_break_points)) {
    w_map[star_break_points[[i]], i] <- 1
  }
  
  if ( divs > 1){
    new_divs <- 1:divs
    for (k in 2:length(new_divs)) {
      w_map[, (jump_by_cols + 1):(new_divs[k]*jump_by_cols)] <- w_map[, 1:jump_by_cols]
    }
  }
  ######## Final cheking ###################################### 
  if (rem > 0){
    w_map[, (divs*jump_by_cols + 1):n_cols] <- w_map[, 1:rem]
  }
  ############################################################
  
  if (di > 0){
    w_map <- w_map[-((n_rows-di+1):n_rows),]
  }
  n_Checks <- length(which(w_map == 1))
  pots <- dim(w_map)[1] * dim(w_map)[2]
  a <- capture.output(print(paste("You have",  n_Checks,"(",round((n_Checks/pots)*100,2),
                                  "%) pots available for Check of the", pots)))
  b <- capture.output(print(c(jump_by_cols, jump_by_rows)))
  
  ## Get random the checks into the whole experiment.
  res <- n_Checks %% length(checks)
  divv <- (n_Checks - res) / length(checks)
  if (res == 0){
    s <- rep(checks, n_Checks/length(checks))
    random_checks <- sample(s)
  }else{
    v <- c(rep(checks,divv), sample(checks,res))
    random_checks <- sample(v)
  }
  
  w_map[w_map == 1] <- random_checks
  output_list <- list(a, w_map, table(random_checks), b)
  
  return(output_list)
}

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
    stop("'x' must be a numeric vector or a list of numeric vectors")
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
    stop("The 'total_elements' function requires a list as input.")
  }
  
  length(unlist(alist))
}

#' @title Split Matrix Into Blocks
#' 
#' @description
#' Splits a matrix into a list of blocks, either by rows or by columns, based on the specified sizes of the blocks.
#'
#' @param Matrix A matrix to be split.
#' @param blocks Either a list or a vector indicating the sizes of the blocks to be split into. 
#' If \code{blocks} is a list of vectors, each vector's length defines the size of the blocks. 
#' If \code{blocks} is a vector, each element represents the size of a block.
#' @param byrow A logical value. If \code{TRUE} (the default), the matrix is split 
#' by rows; otherwise, it is split by columns.
#' @return A list of matrices, each representing a block.
#' @noRd
split_matrix_into_blocks <- function(Matrix, blocks, byrow = TRUE) {

  if (!is.matrix(Matrix)) {
    stop("Input must be a matrix.")
  }
    
  if (!is.list(blocks) && !is.numeric(blocks)) {
    stop("Blocks must be a numeric vector or a list of numeric vectors.")
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
  if (byrow && size != nrow(Matrix)) {
    stop("Number of rows in 'Matrix' does not match 'blocks'")
  } else if (!byrow && size != ncol(Matrix)) {
    stop("Number of columns in 'Matrix' does not match 'blocks'")
  }
  
  # Use a loop to populate the blocks_list based on the 'byrow' flag
  for (k in 1:num_blocks) {
    if (byrow) {
      blocks_list[[k]] = Matrix[from[k]:to[k], , drop = FALSE]  # Ensuring the result is always a matrix
    } else {
      blocks_list[[k]] = Matrix[, from[k]:to[k], drop = FALSE]  # Ensuring the result is always a matrix
    }
  }
  
  return(blocks_list)
}

# for (k in 1:num_blocks) {
#   if (byrow) {
#     if (size != nrow(Matrix))
#       stop("\nNumber of rows in 'Matrix' doesn't match 'blocks'")
#     blocks_list[[k]] = Matrix[from[k]:to[k],]
#   } else {
#     if (size != ncol(Matrix))
#       stop("\nNumber of columns in 'Matrix' doesn't match 'blocks'")
#     blocks_list[[k]] = Matrix[,from[k]:to[k]]      
#   }
# }