library(FielDHub)
library(testthat)
# Test pairs_distance function
test_that("pairs_distance returns expected output", {
  # Test with a matrix that contains integers that appear two or more times
  X1 <- matrix(c(1, 2, 3, 4, 2, 5, 6, 1, 7, 8, 2, 9), ncol = 3)
  X1[2,2] <- 2
  X1[2,2] <- 2
  res1 <- FielDHub:::pairs_distance(X1)
  expect_equal(ncol(res1), 8)
  expect_equal(nrow(res1), 6)
  expect_equal(sum(duplicated(res1[, 2:3])), 0)
  
  # Test with a matrix that contains only unique integers
  X2 <- matrix(1:9, ncol = 3)
  expect_error(FielDHub:::pairs_distance(X2), "All elements in X appear only once")
})

# Test swap_pairs function
test_that("swap_pairs returns expected output", {
  # Test with a matrix that contains integers that appear two or more times
  set.seed(123)
  X1 <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
  res1 <- swap_pairs(X1, starting_dist = 3, stop_iter = 100)
  expect_equal(dim(res1$optim_design), dim(X1))
  expect_equal(length(res1$designs), length(res1$distances))
  expect_equal(length(res1$distances), length(res1$rows_incidence))
  expect_true(res1$min_distance >= 3)
  expect_equal(ncol(res1$pairswise_distance), 8)
  
  # Test with a matrix that contains only unique integers
  X2 <- matrix(1:9, ncol = 3)
  expect_warning(
    swap_pairs(X2, starting_dist = 3, stop_iter = 100), 
    "All elements in X appear only once")
})

###################################################################################33

X1 <- matrix(c(1, 2, 3, 4, 2, 5, 6, 1, 7, 8, 2, 9), ncol = 3)
X1
X1[2,2] <- 2
X1[3,3] <- 5
X <- X1
X
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
    c_id <- as.integer(rownames(unique(t(apply(PAIRS[,1:2],MARGIN = 1,FUN=sort)))))
    PAIRS <- PAIRS[c_id,]
    # row name reset
    rownames(PAIRS) <- 1:nrow(PAIRS)
    # row-wise sort
    PAIRS <- as.data.frame(t(apply(X=PAIRS,MARGIN=1,FUN=sort)))
    
    return(PAIRS)
}
dups <- base::table(as.vector(X))
dups
if (sum(dups > 1) == 0) {
    stop("All elements in X appear only once")
}
dupsI <- as.numeric(rownames(dups)[dups > 1])
dupsI
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
    id <- which(X == dupsI[i])
    pair_points <- matrix(data = NA, nrow = 0, ncol = 2)
    colnames(pair_points) <- c("P2", "P1")
    for (i in seq_along(dupsI)) {
        id <- which(X==dupsI[i])
        loopPairs <- possPairs(id)
        pair_points <- rbind(pair_points, loopPairs)
    }
    loopPairs <- pair_points
    for (z in 1:nrow(loopPairs)) {
        print(z)
        coord.a <- which(
            matrix((1:length(X))==loopPairs[z,1,drop=TRUE],dim(X),byrow = FALSE),
            arr.ind = TRUE
        )
        print(coord.a)
        coord.b <- which(
            matrix((1:length(X))==loopPairs[z,2,drop=TRUE],dim(X),byrow = FALSE),
            arr.ind = TRUE
        )
        print(coord.b)
        loopDist <- sqrt(sum(abs(coord.a-coord.b)^2))
        print(loopDist)
        plotDist[z, 4] <- loopDist
        plotDist[z,2:3] <- loopPairs[z,]
        plotDist[z,5:6] <- coord.a
        plotDist[z,7:8] <- coord.b
    }
}
plotDist
