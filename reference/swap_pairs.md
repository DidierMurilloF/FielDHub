# Swap pairs in a matrix of integers

Modifies the input matrix `X` to ensure that the distance between any
two occurrences of the same integer is at least a distance `d`, by
swapping one of the occurrences with a candidate cell of a different
integer. The function starts with `starting_dist = 3` and increases it
by `1` until the algorithm no longer converges or `stop_iter` iterations
have been performed. This version evaluates candidate swaps using both
the mean pairwise distance and a centrality penalty, and it uses
candidate sampling to reduce computation.

## Usage

``` r
swap_pairs(
  X,
  starting_dist = 3,
  stop_iter = 10,
  lambda = 0.5,
  dist_method = "euclidean",
  candidate_sample_size = 4
)
```

## Arguments

- X:

  A matrix of integers.

- starting_dist:

  The minimum starting distance to enforce between pairs of occurrences
  of the same integer. Default is 3.

- stop_iter:

  The maximum number of iterations to perform. Default is 50.

- lambda:

  A tuning parameter for the centrality penalty. Default is 0.1.

- dist_method:

  The method used for distance calculation. Options are "euclidean"
  (default) and "manhattan".

- candidate_sample_size:

  Maximum number of candidate cells to evaluate per swap. Default is 5.

## Value

A list containing:

- optim_design:

  The modified matrix.

- designs:

  A list of all intermediate designs, starting from the input matrix.

- distances:

  A list of all pair distances for each intermediate design.

- min_distance:

  The minimum distance between pairs of occurrences of the same integer
  in the final design.

- pairwise_distance:

  A data frame with the pairwise distances for the final design.

- rows_incidence:

  A vector recording the number of rows with repeated integers for each
  iteration.

## Examples

``` r
set.seed(123)
X <- matrix(sample(c(rep(1:10, 2), 11:50), replace = FALSE), ncol = 10)
B <- swap_pairs(
  X,
  starting_dist = 3,
  stop_iter = 50,
  lambda = 0.5,
  dist_method = "euclidean",
  candidate_sample_size = 3
)
B$optim_design
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    6    1   24   44   27   19   15    3   39     2
#> [2,]    3   45   20   32   48   29   43   33    7     6
#> [3,]   10    4   36   30   46   28   14    9   22    10
#> [4,]    7    2    8   31   16   40   47   17   34     5
#> [5,]   42   38    5   26   49   50   35   37    1     8
#> [6,]    9   21   12   23   25   13   41   11    4    18
```
