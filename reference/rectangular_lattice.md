# Generates a Rectangular Lattice Design.

It randomly generates a rectangular lattice design across locations.

## Usage

``` r
rectangular_lattice(
  t = NULL,
  k = NULL,
  r = NULL,
  l = 1,
  plotNumber = 101,
  locationNames = NULL,
  seed = NULL,
  data = NULL
)
```

## Arguments

- t:

  Number of treatments.

- k:

  Size of incomplete blocks (number of units per incomplete block).

- r:

  Number of blocks (full resolvable replicates).

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- locationNames:

  (optional) Names for each location.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- data:

  (optional) Data frame with label list of treatments.

## Value

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the rectangular lattice design field
  book.

## References

Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs
for factorial and unstructured treatment sets.
https://CRAN.R-project.org/package=blocksdesign

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a rectangular lattice design with 6 full blocks, 4 units per IBlock (k)
# and 20 treatments in one location.
rectangularLattice1 <- rectangular_lattice(t = 20, k = 4, r = 6, l = 1, 
                                           plotNumber = 101,
                                           locationNames = "FARGO", 
                                           seed = 126)
rectangularLattice1$infoDesign
#> $Reps
#> [1] 6
#> 
#> $iBlocks
#> [1] 5
#> 
#> $NumberTreatments
#> [1] 20
#> 
#> $NumberLocations
#> [1] 1
#> 
#> $Locations
#> [1] "FARGO"
#> 
#> $seed
#> [1] 126
#> 
#> $lambda
#> [1] 0.9473684
#> 
#> $id_design
#> [1] 11
#> 
head(rectangularLattice1$fieldBook,12)
#>    ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1   1    FARGO  101   1      1    1     5       G-5
#> 2   2    FARGO  102   1      1    2    15      G-15
#> 3   3    FARGO  103   1      1    3     3       G-3
#> 4   4    FARGO  104   1      1    4    14      G-14
#> 5   5    FARGO  105   1      2    1    12      G-12
#> 6   6    FARGO  106   1      2    2     1       G-1
#> 7   7    FARGO  107   1      2    3    10      G-10
#> 8   8    FARGO  108   1      2    4    16      G-16
#> 9   9    FARGO  109   1      3    1     7       G-7
#> 10 10    FARGO  110   1      3    2    19      G-19
#> 11 11    FARGO  111   1      3    3    11      G-11
#> 12 12    FARGO  112   1      3    4     6       G-6

# Example 2: Generates a rectangular lattice design with 5 full blocks, 7 units per IBlock (k)
# and 56 treatments across 2 locations.
# In this case, we show how to use the option data.
treatments <- paste("ND-", 1:56, sep = "")
ENTRY <- 1:56
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list) 
#>   ENTRY TREATMENT
#> 1     1      ND-1
#> 2     2      ND-2
#> 3     3      ND-3
#> 4     4      ND-4
#> 5     5      ND-5
#> 6     6      ND-6
rectangularLattice2 <- rectangular_lattice(t = 56, k = 7, r = 5, l = 2, 
                                           plotNumber = c(1001,2001),
                                           locationNames = c("Loc1", "Loc2"), 
                                           seed = 127,
                                           data = treatment_list)
rectangularLattice2$infoDesign
#> $Reps
#> [1] 5
#> 
#> $iBlocks
#> [1] 8
#> 
#> $NumberTreatments
#> [1] 56
#> 
#> $NumberLocations
#> [1] 2
#> 
#> $Locations
#> [1] "LOC1" "LOC2"
#> 
#> $seed
#> [1] 127
#> 
#> $lambda
#> [1] 0.5454545
#> 
#> $id_design
#> [1] 11
#> 
head(rectangularLattice2$fieldBook,12)
#>    ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1   1     LOC1 1001   1      1    1    43     ND-43
#> 2   2     LOC1 1002   1      1    2     6      ND-6
#> 3   3     LOC1 1003   1      1    3     5      ND-5
#> 4   4     LOC1 1004   1      1    4    27     ND-27
#> 5   5     LOC1 1005   1      1    5    54     ND-54
#> 6   6     LOC1 1006   1      1    6    41     ND-41
#> 7   7     LOC1 1007   1      1    7    26     ND-26
#> 8   8     LOC1 1008   1      2    1    24     ND-24
#> 9   9     LOC1 1009   1      2    2    51     ND-51
#> 10 10     LOC1 1010   1      2    3    21     ND-21
#> 11 11     LOC1 1011   1      2    4    20     ND-20
#> 12 12     LOC1 1012   1      2    5    11     ND-11
```
