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
#' @author Jean-Marc Montpetit, \email{jeanmarc.montpetit@videotron.ca}
#'  
#' @noRd 
pairs_distance <- function(X) {
    # check if the input X is a matrix
    if (!is.matrix(input_matrix)) {
        stop("Input must be a matrix")
    }
    # check if the input matrix X is numeric
    if (!is.numeric(input_matrix)) {
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
        c_id <- as.integer(rownames(unique(t(apply(PAIRS[,1:2],MARGIN = 1,FUN=sort)))))
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
    #preloop DF creation
    plotDist <- data.frame(geno=dupsI,
                            Pos1=as.integer(NA),
                            Pos2=as.integer(NA),
                            DIST=as.numeric(NA),
                            rA=as.integer(NA),
                            cA=as.integer(NA),
                            rB=as.integer(NA),
                            cB=as.integer(NA))
    for (i in seq_along(dupsI)) {
        minDist <- sqrt(sum(dim(X)^2))
        id <- which(X==dupsI[i])
        loopPairs <- possPairs(id)
        for (z in 1:nrow(loopPairs)){
            coord.a <- which(
                matrix((1:length(X))==loopPairs[z,1,drop=TRUE],dim(X),byrow = FALSE),
                arr.ind = TRUE
            )
            coord.b <- which(
                matrix((1:length(X))==loopPairs[z,2,drop=TRUE],dim(X),byrow = FALSE),
                arr.ind = TRUE
            )
            loopDist <- sqrt(sum(abs(coord.a-coord.b)^2))
            if (loopDist < minDist){
                minDist <- loopDist
                plotDist$DIST[i] <- loopDist
                plotDist[i,2:3] <- loopPairs[z,]
                plotDist[i,5:6] <- coord.a
                plotDist[i,7:8] <- coord.b
            }
        }
    }
    plotDist <- plotDist[order(plotDist$DIST),]
    return(plotDist)
}

#' @title Swap pairs in a matrix of integers
#' 
#' @description Modifies the input matrix X to ensure that the distance between 
#' any two occurrences of the same integer is at least min_dist, by swapping 
#' one of the occurrences with a random occurrence of a different integer that 
#' is at least min_dist away. Where min_dist starts with starting_dist = 3, and then 
#' increase it by 1 until the algorithm no longer converges.
#' 
#' @param X A matrix of integers.
#' @param starting_dist The minimum starting distance to enforce between pairs 
#' of occurrences of the same integer. Default is 3.
#' @param stop_iter The maximum number of iterations to perform. Default is 100.
#' 
#' @return A list containing the following elements:
#' \describe{
#'   \item{X}{The modified matrix.}
#'   \item{designs}{A list of all intermediate designs, starting from the input matrix.}
#'   \item{distances}{A list of all pair distances for each intermediate design.}
#' }
#' 
#' @author Jean-Marc Montpetit, \email{jeanmarc.montpetit@videotron.ca} [aut]
#'         Didier Murillo, \email{didier.murilloflorez@ndsu.edu} [ctb]
#' 
#' @examples
#' # Create a matrix X with the numbers 1 to 10 are twice and 11 to 50 are once.
#' # The matrix has 6 rows and 10 columns
#' set.seed(123)
#' X <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
#' X
#' # Swap pairs
#' B <- swap_pairs(X, starting_dist = 2)
#' B$X
#' B$designs
#' B$distances
#' 
#' 
#' @export
#' @seealso \code{\link{pairs_distance}}
swap_pairs <- function(X, starting_dist = 3, stop_iter = 100) {
    # check if the input X is a matrix
    if (!is.matrix(input_matrix)) {
        stop("Input must be a matrix")
    }
    # check if the input matrix X is numeric
    if (!is.numeric(input_matrix)) {
        stop("Matrix elements must be numeric")
    }
    field_layout <- X
    minDist <- sqrt(sum(dim(field_layout)^2))
    designs <- list()
    designs[[1]] <- field_layout
    distances <- list()
    distances[[1]] <- pairs_distance(X = field_layout)
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
                    valid_indices <- other_indices[sqrt(dist) >= min_dist, ]
                    if (nrow(valid_indices) == 0) break
                    if (nrow(valid_indices) > 0) {
                        # Pick a random cell to swap with
                        k <- sample(nrow(valid_indices), size = 1)
                        # Swap the two occurrences
                        X[indices[i,1], indices[i,2]] <- X[valid_indices[k,1], valid_indices[k,2]]
                        X[valid_indices[k,1], valid_indices[k,2]] <- genotype
                    } else break
                }
            }
            n_iter <- n_iter + 1
        }
        if (min(pairs_distance(X)$DIST) < min_dist) {
            break
        } else {
            print(paste0("Maximum distance achieved: ", min_dist))
            designs[[w]] <- X
            distances[[w]] <- pairs_distance(X)
            w <- w + 1
        }
    }
    return(
        list(
            X = designs[length(designs)], 
            designs = designs, 
            distances = distances
        )
    )
}