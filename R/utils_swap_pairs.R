swap_pairs <- function(X, min_dist) {
    while (TRUE) {
        # Keep track of whether any swaps were made in this iteration
        any_swaps <- FALSE
        # Loop through each integer in the matrix
        for (value in unique(X)) {
            indices <- which(X == value, arr.ind = TRUE)
            if (nrow(indices) > 1) {
                # Calculate the pairwise distances between the indices
                distances <- proxy::dist(indices)
                # distances <- matrix(data = c(0, distances, distances, 0), 
                #                     nrow = nrow(indices), 
                #                     ncol = nrow(indices),
                #                     byrow = TRUE)
                for (i in seq_len(nrow(indices))) {
                    #for (j in nrow(indices):1) {
                    if (distances < min_dist) {
                        other_values <- setdiff(unique(X), value)
                        other_indices <- matrix(
                            data = c(rep(1:dim(X)[1], each = dim(X)[2]), rep(1:dim(X)[2], times = dim(X)[1])), 
                            nrow = prod(dim(X)),
                            ncol = 2, 
                            byrow = FALSE
                        )
                        other_indices <- cbind(other_indices, as.vector(t(X)) %in% other_values)
                        colnames(other_indices) <- c("i", "j", "value")
                        other_indices <- other_indices[other_indices[,3] != 0, ][, 1:2]
                        dists <- apply(other_indices, 1, function(x) sum((x - as.vector(indices[i,]))^2))
                        valid_indices <- other_indices[sqrt(dists) >= min_dist, ]
                        if (nrow(valid_indices) > 0) {
                            k <- sample(nrow(valid_indices), size = 1)
                            # Swap the two occurrences
                            X[indices[i,1], indices[i,2]] <- X[valid_indices[k,1], valid_indices[k,2]]
                            X[valid_indices[k,1], valid_indices[k,2]] <- value
                            any_swaps <- TRUE
                        }
                    }
                    # }
                }
            }
        }
        # If no swaps were made in this iteration, break out of the loop
        if (!any_swaps) {
            break
        }
    }
    return(X)
}