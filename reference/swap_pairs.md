# Swap pairs in a matrix of integers

Attempts to separate repeated entries by swapping cells of different
entries. Distance thresholds increase from `starting_dist` in steps of
one. Candidate sampling, mean pairwise distance, and a centrality
penalty guide the search. The last successful threshold layout is
returned; if none succeeds, the original matrix is returned. A requested
minimum distance is not guaranteed.

## Usage

``` r
swap_pairs(
  X,
  starting_dist = 3,
  stop_iter = 10,
  lambda = 0.5,
  dist_method = "euclidean",
  candidate_sample_size = 4,
  seed = NULL
)
```

## Arguments

- X:

  A numeric matrix of whole-number entry identifiers within R's integer
  range. Missing cells are inactive positions and never move.

- starting_dist:

  First distance threshold; a finite nonnegative number. Default is 3.

- stop_iter:

  Maximum complete swap sweeps per threshold, as a nonnegative whole
  number within R's integer range. Default is 10.

- lambda:

  Finite nonnegative weight for the centrality penalty. Default is 0.5.

- dist_method:

  Coordinate distance used for candidate filtering, scoring, stopping,
  and reported distances: "euclidean" (default) or "manhattan".

- candidate_sample_size:

  Maximum candidates evaluated per swap, as a positive whole number
  within R's integer range. Default is 4.

- seed:

  Optional randomization seed. When omitted, one integer is drawn from
  the current random-number stream and recorded; the swap's own
  randomization does not change the caller's stream.

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

  Row-repetition counts for retained threshold steps, or for the
  original matrix when no step succeeds.

- diagnostics:

  The distance metric, stop reason, total completed sweeps
  (`iterations`), per-threshold budget, number of attempted thresholds,
  last threshold, last attempted minimum distance, and retained minimum
  distance. Stop reasons are `iteration_limit`,
  `distance_range_complete`, and `no_distance_thresholds`. A failed
  attempt is not retained in `optim_design`.

- metadata:

  The design identifier, schema and package versions, RNG settings,
  resolved seed and complete input parameters, including `X`. The result
  inherits from `fieldhub_optimization`.

## Details

The finite threshold range is bounded by the field geometry. For
Euclidean distance without missing cells, the historical bound is
`sqrt(nrow(X)^2 + ncol(X)^2)`; with missing cells it is the maximum
distance between active positions. For Manhattan distance it is the
maximum Manhattan distance between active positions. There are at most
`floor(bound - starting_dist) + 1` thresholds when the bound is at least
`starting_dist`, and none otherwise. Each threshold permits at most
`stop_iter` complete sweeps. Diagnostics distinguish an exhausted
threshold budget from completing or skipping the distance range.

Standalone calls use the shared seed contract. To reproduce a previous
`set.seed(s); swap_pairs(X)` call, with `X` already constructed, pass
`seed = s`. Calls without a seed now select an automatic seed, so their
layouts can differ from previous versions. Optimization performed inside
field-design engines retains its existing draw sequence. Use
[`reproduce_design()`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md)
to replay a recorded optimization result.

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
  candidate_sample_size = 3,
  seed = 123
)
B$optim_design
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    3   37   50   49   26   17   41   47    5    10
#> [2,]    1   20   12   32   16   29   43   34    7     6
#> [3,]    8    6   10   39   31   28   14   11    9     4
#> [4,]    4   23    2   30   25   40   18    3   35    46
#> [5,]    5   15   22   45   21   36   27   19    8    38
#> [6,]    9    7   42   33   24   13    1   48   44     2
```
