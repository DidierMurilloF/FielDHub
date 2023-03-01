library(blocksdesign)
nt <- 36
r <- 3
k <- 6
b <- nt/k

d <- blocksdesign::blocks(treatments = nt, replicates = r, blocks = list(r, b), searches = 1000, seed = 5, jumps = 20)
d

nt <- 40
r <- 3
k <- 5
b <- nt/k

d <- blocksdesign::blocks(
    treatments = nt, 
    replicates = r, 
    blocks = list(r, b), 
    searches = 1000, 
    seed = 5, 
    jumps = 10
    )
d

# Load required packages
library(MASS)
library(matrixcalc)

# Set up design parameters
t <- 4
n <- 6
b <- 3

# Create the design matrix
design <- matrix(NA, nrow = n, ncol = t)

# Create the block design matrix
block_design <- matrix(NA, nrow = b, ncol = t)

# Randomize the treatments within each block
for (i in 1:b) {
  block_design[i, ] <- sample(1:t, t, replace = FALSE)
}

# Assign treatments to the experimental units
for (i in 1:n) {
  block <- sample(1:b, 1)
  treatments <- block_design[block, ]
  treatments_assigned <- sample(treatments, length(treatments), replace = FALSE)
  design[i, ] <- treatments_assigned
}

# Calculate the D-efficiency
x <- matrix(0, nrow = t, ncol = n)
for (i in 1:n) {
  x[design[i, ], i] <- 1
}
D <- det(t(x) %*% x)
D_efficiency <- D^(1/n)

# Calculate the A-efficiency
X <- model.matrix(~0+design)
H <- X %*% solve(t(X) %*% X) %*% t(X)
A <- diag(H) + rep(1, n)
A_efficiency <- min(A)

# Print the results
print(design)
cat("D-efficiency:", D_efficiency, "\n")
cat("A-efficiency:", A_efficiency, "\n")

# Set up design parameters
t <- 15
n <- 30
b <- 5

# Set the number of random designs to generate
n_designs <- 1000

# Set the number of iterations for each design
n_iterations <- 100

# Initialize the best D- and A-efficiency
best_D_efficiency <- 0
best_A_efficiency <- Inf

# Generate random designs and evaluate their efficiency
for (i in 1:n_designs) {
  # Generate a random design
  design <- matrix(sample(1:t, n, replace = TRUE), ncol = b)
  
  # Evaluate the D-efficiency
  x <- matrix(0, nrow = t, ncol = n)
  for (j in 1:n) {
    x[design[j, ], j] <- 1
  }
  D <- det(t(x) %*% x)
  D_efficiency <- D^(1/n)
  
  # Evaluate the A-efficiency
  H <- diag(hatMatrix(lm(rep(1, n) ~ x - 1))) + rep(1, n)
  A_efficiency <- min(H)
  
  # Update the best D- and A-efficiency
  if (D_efficiency > best_D_efficiency) {
    best_D_efficiency <- D_efficiency
    best_design_D <- design
  }
  if (A_efficiency < best_A_efficiency) {
    best_A_efficiency <- A_efficiency
    best_design_A <- design
  }
}

# Print the best designs and their efficiency
print(best_design_D)
print(paste0("D-efficiency: ", round(best_D_efficiency, 3)))
print(best_design_A)
print(paste0("A-efficiency: ", round(best_A_efficiency, 3)))


# Load the AlgDesign package
library(AlgDesign)

# Set the number of treatments and the number of blocks
num_treatments <- 8
num_blocks <- 3

# Generate the design
design <- gen.block.design(num_treatments, num_blocks, type = "incomplete")

# Calculate the D-efficiency
d_efficiency <- Ds(design)

# Calculate the A-efficiency
a_efficiency <- As(design)

# Print the design, D-efficiency, and A-efficiency
cat("Design:\n")
print(design)
cat("D-efficiency:", d_efficiency, "\n")
cat("A-efficiency:", a_efficiency, "\n")

library(MASS)
library(matrixcalc)

# set up the design parameters
t <- 4
n <- 6
b <- 3

# create the design matrix
design <- matrix(NA, nrow = n, ncol = t)

# create the block design matrix
block_design <- matrix(NA, nrow = b, ncol = t)

# randomize the treatments within each block
for (i in 1:b) {
  block_design[i, ] <- sample(1:t, t, replace = FALSE)
}

# assign treatments to the experimental units
for (i in 1:n) {
  block <- sample(1:b, 1)
  treatments <- block_design[block, ]
  treatments_assigned <- sample(treatments, length(treatments), replace = FALSE)
  design[i, ] <- treatments_assigned
}

# calculate the D-efficiency
x <- matrix(0, nrow = t, ncol = n)
for (i in 1:n) {
  x[design[i, ], i] <- 1
}
D <- det(t(x) %*% x)
D_efficiency <- D^(1/n)

# calculate the A-efficiency
H <- hatMatrix(lm(rep(1, n) ~ design - 1))
A <- diag(H) + rep(1, n)
A_efficiency <- min(A)

# print the results
print(design)
cat("D-efficiency:", D_efficiency, "\n")
cat("A-efficiency:", A_efficiency, "\n")



y <- 25
sqrt(y) == round(sqrt(y))


#######################################################
x <- diagonal_arrangement(nrows = 52, ncols = 11, lines = 480, checks = 4)





set.seed(1)
v <- matrix(rbinom(16, 1, 0.5), 4, 4)
v
sum(v == 0)


 checks <- 5;expts <- 5
 list_checks <- paste("CH", 1:checks, sep = "")
 treatments <- paste("G", 6:725, sep = "")
 treatment_list <- data.frame(list(ENTRY = 1:725, NAME = c(list_checks, treatments)))
 head(treatment_list, 12) 
 tail(treatment_list, 12)
 spatDB <- diagonal_arrangement(
   nrows = 30, 
   ncols = 26,
   checks = 5, 
   plotNumber = 1, 
   kindExpt = "DBUDC", 
   planter = "serpentine", 
   splitBy = "row", 
   blocks = c(150,155,95,200,120),
   data = treatment_list
  )
 spatDB$infoDesign
 spatDB$layoutRandom
 spatDB$plotsNumber
 head(spatDB$fieldBook,12)
 
 
  spatd <- diagonal_arrangement(
    nrows = 18, 
    ncols = 18, 
    lines = 270, 
    checks = 4, 
    plotNumber = 101, 
    kindExpt = "SUDC", 
    planter = "serpentine", 
    seed = 1987,
    exptName = "20WRY1", 
    locationNames = "MINOT"
  )
  spatd$infoDesign
  spatd$layoutRandom
  spatd$plotsNumber
  head(spatd$fieldBook, 12)
