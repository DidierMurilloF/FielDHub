#' @title Calculate pairwise distances between all elements in a matrix that appears twice or more.
#'
#' @description Given a matrix of integers, this function calculates the pairwise Euclidean 
#' distance between all possible pairs of elements in the matrix that appear two or more times. 
#' If no element appears two or more times, the function will return an error message.
#'
#' 
#' @param X a matrix of integers
#' 
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{geno}: the integer value for which the pairwise distances are calculated
#'   \item \code{Pos1}: the row index of the first element in the pair
#'   \item \code{Pos2}: the row index of the second element in the pair
#'   \item \code{DIST}: the Euclidean distance between the two elements in the pair
#'   \item \code{rA}: the row index of the first element in the pair
#'   \item \code{cA}: the column index of the first element in the pair
#'   \item \code{rB}: the row index of the second element in the pair
#'   \item \code{cB}: the column index of the second element in the pair
#' }
#' 
#' @author Jean-Marc Montpetit [aut]
#'  
#' @noRd 
pairs_distance <- function(X) {
    # check if the input X is a matrix
    if (!is.matrix(X)) {
        stop("Input must be a matrix")
    }
    # check if the input matrix X is numeric
    if (!is.numeric(X)) {
        stop("Matrix elements must be numeric")
    }
    possPairs <- function(Z) {
        PAIRS <- base::expand.grid(Z, Z, KEEP.OUT.ATTRS = FALSE)
        colnames(PAIRS) <- c("P1","P2")
        # removing self PAIRS
        PAIRS <- PAIRS[!(PAIRS$P1==PAIRS$P2),]
        # removing reciprocals 
        # Relying on rownames to remove dups
        rownames(PAIRS) <- 1:nrow(PAIRS)
        c_id <- as.integer(rownames(unique(t(apply(PAIRS[, 1:2], MARGIN = 1, FUN = sort)))))
        PAIRS <- PAIRS[c_id,]
        # row name reset
        rownames(PAIRS) <- 1:nrow(PAIRS)
        # row-wise sort
        PAIRS <- as.data.frame(t(apply(X=PAIRS,MARGIN=1,FUN=sort)))
        
        return(PAIRS)
    }
    dups <- base::table(as.vector(X))
    if (sum(dups > 1) == 0) {
        stop("All elements in X appear only once")
    }
    dupsI <- as.numeric(rownames(dups)[dups > 1])
    pair_points <- matrix(data = NA, nrow = 0, ncol = 2)
    int_reps <- numeric()
    colnames(pair_points) <- c("P2", "P1")
    for (i in seq_along(dupsI)) {
        id <- which(X == dupsI[i])
        id <- which(X==dupsI[i])
        loopPairs <- possPairs(id)
        int_reps[i] <- nrow(loopPairs)
        pair_points <- rbind(pair_points, loopPairs)
    }
    #preloop DF creation
    plotDist <- data.frame(geno = rep(dupsI, times = int_reps),
                           Pos1=as.integer(NA),
                           Pos2=as.integer(NA),
                           DIST=as.numeric(NA),
                           rA=as.integer(NA),
                           cA=as.integer(NA),
                           rB=as.integer(NA),
                           cB=as.integer(NA)) 
    loopPairs <- pair_points
    for (z in 1:nrow(loopPairs)) {
        coord.a <- which(
            matrix((1:length(X)) == loopPairs[z, 1, drop = TRUE], dim(X), byrow = FALSE),
            arr.ind = TRUE
        )
        coord.b <- which(
            matrix((1:length(X)) == loopPairs[z, 2, drop = TRUE], dim(X), byrow = FALSE),
            arr.ind = TRUE
        )
        loopDist <- sqrt(sum(abs(coord.a-coord.b)^2))
        plotDist[z, 4] <- loopDist
        plotDist[z, 2:3] <- loopPairs[z,]
        plotDist[z, 5:6] <- coord.a
        plotDist[z, 7:8] <- coord.b
    }
    plotDist <- plotDist[order(plotDist$DIST), ]
    return(plotDist)
}

#' @title Swap pairs in a matrix of integers
#' 
#' @description Modifies the input matrix \code{X} to ensure that the distance between any two occurrences
#' of the same integer is at least a dist \code{d}, by swapping one of the occurrences with a
#' random occurrence of a different integer that is at least \code{d} away. The function
#' starts with \code{starting_dist = 3} and increases it by \code{1} until the algorithm no longer 
#' converges or \code{stop_iter} iterations have been performed. 
#' 
#' 
#' @param X A matrix of integers.
#' @param starting_dist The minimum starting distance to enforce between pairs of occurrences
#'  of the same integer. Default is 3.
#' @param stop_iter The maximum number of iterations to perform. Default is 100.
#' 
#' @return A list containing the following elements:
#' \item{optim_design}{The modified matrix.}
#' \item{designs}{A list of all intermediate designs, starting from the input matrix.}
#' \item{distances}{A list of all pair distances for each intermediate design.}
#' \item{min_distance}{An integer indicating the minimum distance between pairs of occurrences of the same integer.}
#' \item{pairswise_distance}{A data frame with the pairwise distances for the final design.}
#' 
#' @author Jean-Marc Montpetit [aut], Didier Murillo [aut]
#' 
#' @examples
#' # Create a matrix X with the numbers 1 to 10 are twice and 11 to 50 are once.
#' # The matrix has 6 rows and 10 columns
#' set.seed(123)
#' X <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
#' X
#' # Swap pairs
#' B <- swap_pairs(X, starting_dist = 3)
#' B$optim_design
#' B$designs
#' B$distances
#' 
#' 
#' @export
swap_pairs <- function(X, starting_dist = 3, stop_iter = 100) {
    # check if the input X is a matrix
    if (!is.matrix(X)) {
        stop("Input must be a matrix")
    }
    # check if the input matrix X is numeric
    if (!is.numeric(X)) {
        stop("Matrix elements must be numeric")
    }
    swap_succeed <- FALSE
    input_X <- X
    input_freq <- table(input_X)
    minDist <- sqrt(sum(dim(X)^2))
    designs <- list()
    designs[[1]] <- X
    distances <- list()
    rows_incidence <- numeric()
    init_dist <- pairs_distance(X = X)
    genos <- unique(init_dist$geno)
    distances[[1]] <- init_dist
    w <- 2
    for (min_dist in seq(starting_dist, minDist, 1)) {
        n_iter <- 1
        while (n_iter <= stop_iter) {
            plotDist <- pairs_distance(X)
            rownames(plotDist) <- NULL  #reset for latert use
            LowID <- which(plotDist$DIST < min_dist)
            low_dist_gens <- unique(plotDist$geno[LowID])
            LS <- length(LowID)
            if (LS == 0) {
                n_iter <- stop_iter + 1
                break
            }
            for (genotype in low_dist_gens) {
                indices <- which(X == genotype, arr.ind = TRUE)
                # Get the row and column indices of the cells that contain other values
                other_indices <- which(X != genotype, arr.ind = TRUE)
                for (i in seq_len(nrow(indices))) {
                    # Calculate the Euclidean distances to the fixed point
                    dist <- apply(other_indices, 1, function(x) sum((x - indices[i, ])^2))
                    other_indices <- as.data.frame(other_indices)
                    # other_indices <- other_indices[order(other_indices[, 1]),]
                    valid_indices <- other_indices[sqrt(dist) >= min_dist, ]
                    if (nrow(valid_indices) == 0) break
                    valid_indices <- valid_indices[order(valid_indices[, 1]),]
                    # Pick a random cell to swap with
                    k <- sample(nrow(valid_indices), size = 1)
                    # Swap the two occurrences
                    X[indices[i,1], indices[i,2]] <- X[valid_indices[k,1], valid_indices[k,2]]
                    X[valid_indices[k,1], valid_indices[k,2]] <- genotype
                }
            }
            n_iter <- n_iter + 1
        }
        if (min(pairs_distance(X)$DIST) < min_dist) {
            break
        } else {
            swap_succeed <- TRUE
            output_freq <- table(X)
            if (!all(input_freq == output_freq)) {
              stop("The swap function changed the frequency of some integers.")
            }
            frequency_rows <- as.data.frame(search_matrix_values(X = X, values_search = genos))
            df <- frequency_rows %>% 
                dplyr::filter(Times >= 2)
            rows_incidence[w - 1] <- nrow(df)
            designs[[w]] <- X
            distances[[w]] <- pairs_distance(X)
            w <- w + 1
        }
    }
    optim_design = designs[[length(designs)]] # return the last (better) design
    pairswise_distance <- pairs_distance(optim_design)
    min_distance = min(pairswise_distance$DIST)
    if (!swap_succeed) {
        optim_design = designs[[1]] # return the last (better) design
        distances[[1]] <- pairs_distance(optim_design)
        pairswise_distance <- pairs_distance(optim_design)
        min_distance = min(pairswise_distance$DIST)
        frequency_rows <- as.data.frame(search_matrix_values(X = optim_design, values_search = genos))
        df <- frequency_rows %>% 
            dplyr::filter(Times >= 2)
        rows_incidence[1] <- nrow(df)
    }
    return(
        list(
            rows_incidence = rows_incidence,
            optim_design = optim_design, 
            designs = designs, 
            distances = distances,
            min_distance = min_distance,
            pairswise_distance = pairswise_distance
        )
    )
}

#' @title Search Matrix Values
#'
#' @description Search for values in a matrix and return the row number, value, and frequency.
#' 
#' @author Jean-Marc Montpetit [aut], Didier Murillo [aut]
#'
#' @param X A matrix.
#' @param values_search A vector of values to search for in the matrix.
#' @return A data frame with three columns: Row (the row number where the value is found), 
#' Value (the searched value), and Times (the frequency of the searched value in the row).
#' @examples
#' A <- matrix(c(1, 2, 3, 2, 3, 4, 3, 4, 5), nrow = 3, byrow = TRUE)
#' search_matrix_values( X = A, values_search = c(2, 3, 5))
#' @noRd
search_matrix_values <- function(X, values_search) {
    # Initialize an empty list to store the results
    result <- list()
    # Loop through each row of X
    for (i in 1:nrow(X)) {
        # Get the unique values and their frequency in the current row
        row_vals <- unique(X[i,])
        row_counts <- tabulate(match(X[i,], row_vals))
        # Find the values that are in the search list
        search_vals <- row_vals[row_vals %in% values_search]
        # XAd the row number, search values, and their frequency to the result list
        for (val in search_vals) {
            freq <- sum(X[i,] == val)
            result[[length(result)+1]] <- c(i, val, freq)
        }
    }
    # Convert the result list to a data frame
    result_df <- do.call(rbind, result)
    colnames(result_df) <- c("Row", "Value", "Times")
    # Return the final data frame
    return(result_df)
}

