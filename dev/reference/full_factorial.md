# Generates a Full Factorial Design

It randomly generates a full factorial design across locations.

## Usage

``` r
full_factorial(
  setfactors = NULL,
  reps = NULL,
  l = 1,
  type = 2,
  plotNumber = 101,
  continuous = FALSE,
  planter = "serpentine",
  seed = NULL,
  locationNames = NULL,
  factorLabels = TRUE,
  data = NULL
)
```

## Arguments

- setfactors:

  Numeric vector with levels of each factor.

- reps:

  Number of replicates (full blocks).

- l:

  Number of locations. By default `l = 1`.

- type:

  Option for CRD or RCBD designs. Values are `type = 1` (CRD) or
  `type = 2` (RCBD). By default `type = 2`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- continuous:

  Logical for plot number continuous or not. By default
  `continuous = FALSE`.

- planter:

  Option for `serpentine` or `cartesian` plot arrangement. By default
  `planter = 'serpentine'`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- locationNames:

  (optional) Names for each location.

- factorLabels:

  (optional) If `TRUE` retain the levels labels from the original data
  set otherwise, numeric labels will be assigned. Default is
  `factorLabels =TRUE`.

- data:

  (optional) Data frame with the labels of factors.

## Value

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the full factorial field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a full factorial with 3 factors each with 2 levels.
# This in an RCBD arrangement with 3 reps.
fullFact1 <- full_factorial(setfactors = c(2,2,2), reps = 3, l = 1, type = 2,
                            plotNumber = 101,
                            continuous = TRUE,
                            planter = "serpentine",
                            seed = 325,
                            locationNames = "FARGO")
fullFact1$infoDesign
#> $factors
#> [1] "A" "B" "C"
#> 
#> $levels
#> [1] 0 1 0 1 0 1
#> 
#> $runs
#> [1] 8
#> 
#> $all_treatments
#>   A B C
#> 1 0 0 0
#> 2 1 0 0
#> 3 0 1 0
#> 4 1 1 0
#> 5 0 0 1
#> 6 1 0 1
#> 7 0 1 1
#> 8 1 1 1
#> 
#> $reps
#> [1] 3
#> 
#> $locations
#> [1] 1
#> 
#> $location_names
#> [1] "FARGO"
#> 
#> $kind
#> [1] "RCBD"
#> 
#> $levels_each_factor
#> [1] 2 2 2
#> 
#> $id_design
#> [1] 4
#> 
head(fullFact1$fieldBook,10)
#>    ID LOCATION PLOT REP FACTOR_A FACTOR_B FACTOR_C TRT_COMB
#> 1   1    FARGO  101   1        0        1        1    0*1*1
#> 2   2    FARGO  102   1        1        1        1    1*1*1
#> 3   3    FARGO  103   1        1        0        0    1*0*0
#> 4   4    FARGO  104   1        0        1        0    0*1*0
#> 5   5    FARGO  105   1        1        1        0    1*1*0
#> 6   6    FARGO  106   1        1        0        1    1*0*1
#> 7   7    FARGO  107   1        0        0        0    0*0*0
#> 8   8    FARGO  108   1        0        0        1    0*0*1
#> 16  9    FARGO  109   2        1        1        0    1*1*0
#> 15 10    FARGO  110   2        0        0        0    0*0*0

# Example 2: Generates a full factorial with 3 factors and each with levels: 2,3,
# and 2, respectively. In this case, we show how to use the option data
FACTORS <- rep(c("A", "B", "C"), c(2,3,2))
LEVELS <- c("a0", "a1", "b0", "b1", "b2", "c0", "c1")
data_factorial <- data.frame(list(FACTOR = FACTORS, LEVEL = LEVELS))
print(data_factorial)
#>   FACTOR LEVEL
#> 1      A    a0
#> 2      A    a1
#> 3      B    b0
#> 4      B    b1
#> 5      B    b2
#> 6      C    c0
#> 7      C    c1
# This in an RCBD arrangement with 5 reps in 3 locations.
fullFact2 <- full_factorial(setfactors = NULL, reps = 5, l = 3, type = 2,
                            plotNumber = c(101,1001,2001),
                            continuous = FALSE,
                            planter = "serpentine",
                            seed = 326,
                            locationNames = c("Loc1","Loc2","Loc3"),
                            data = data_factorial)
fullFact2$infoDesign
#> $factors
#> [1] "A" "B" "C"
#> 
#> $levels
#> $levels[[1]]
#> [1] "a0" "a1"
#> 
#> $levels[[2]]
#> [1] "b0" "b1" "b2"
#> 
#> $levels[[3]]
#> [1] "c0" "c1"
#> 
#> 
#> $runs
#> [1] 12
#> 
#> $all_treatments
#>     A  B  C
#> 1  a0 b0 c0
#> 2  a1 b0 c0
#> 3  a0 b1 c0
#> 4  a1 b1 c0
#> 5  a0 b2 c0
#> 6  a1 b2 c0
#> 7  a0 b0 c1
#> 8  a1 b0 c1
#> 9  a0 b1 c1
#> 10 a1 b1 c1
#> 11 a0 b2 c1
#> 12 a1 b2 c1
#> 
#> $reps
#> [1] 5
#> 
#> $locations
#> [1] 3
#> 
#> $location_names
#> [1] "Loc1" "Loc2" "Loc3"
#> 
#> $kind
#> [1] "RCBD"
#> 
#> $levels_each_factor
#> [1] 2 3 2
#> 
#> $id_design
#> [1] 4
#> 
head(fullFact2$fieldBook,10)
#>    ID LOCATION PLOT REP FACTOR_A FACTOR_B FACTOR_C TRT_COMB
#> 1   1     Loc1  101   1       a0       b1       c0 a0*b1*c0
#> 2   2     Loc1  102   1       a1       b0       c1 a1*b0*c1
#> 3   3     Loc1  103   1       a1       b2       c1 a1*b2*c1
#> 4   4     Loc1  104   1       a0       b1       c1 a0*b1*c1
#> 5   5     Loc1  105   1       a1       b0       c0 a1*b0*c0
#> 6   6     Loc1  106   1       a0       b0       c1 a0*b0*c1
#> 7   7     Loc1  107   1       a1       b1       c0 a1*b1*c0
#> 8   8     Loc1  108   1       a0       b2       c1 a0*b2*c1
#> 9   9     Loc1  109   1       a1       b1       c1 a1*b1*c1
#> 10 10     Loc1  110   1       a0       b0       c0 a0*b0*c0
```
