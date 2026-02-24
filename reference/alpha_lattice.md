# Generates an Alpha Design

Randomly generates an alpha design like `alpha(0,1)` across multiple
locations.

## Usage

``` r
alpha_lattice(
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

  Number of full blocks (or resolvable replicates) (also number of
  replicates per treatment).

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- locationNames:

  (optional) String with names for each of the `l` locations.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- data:

  (optional) Data frame with label list of treatments.

## Value

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the alpha design field book.

## References

Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs
for factorial and unstructured treatment sets.
https://CRAN.R-project.org/package=blocksdesign

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates an alpha design with 4 full blocks and 15 treatments.
# Size of IBlocks k = 3.
alphalattice1 <- alpha_lattice(t = 15, 
                               k = 3, 
                               r = 4, 
                               l = 1, 
                               plotNumber = 101, 
                               locationNames = "GreenHouse", 
                               seed = 1247)
alphalattice1$infoDesign
#> $Reps
#> [1] 4
#> 
#> $iBlocks
#> [1] 5
#> 
#> $NumberTreatments
#> [1] 15
#> 
#> $NumberLocations
#> [1] 1
#> 
#> $Locations
#> [1] "GREENHOUSE"
#> 
#> $seed
#> [1] 1247
#> 
#> $lambda
#> [1] 0.5714286
#> 
#> $id_design
#> [1] 12
#> 
head(alphalattice1$fieldBook, 10)
#>    ID   LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1   1 GREENHOUSE  101   1      1    1     8       G-8
#> 2   2 GREENHOUSE  102   1      1    2     3       G-3
#> 3   3 GREENHOUSE  103   1      1    3     2       G-2
#> 4   4 GREENHOUSE  104   1      2    1     6       G-6
#> 5   5 GREENHOUSE  105   1      2    2     9       G-9
#> 6   6 GREENHOUSE  106   1      2    3    12      G-12
#> 7   7 GREENHOUSE  107   1      3    1    14      G-14
#> 8   8 GREENHOUSE  108   1      3    2     1       G-1
#> 9   9 GREENHOUSE  109   1      3    3     5       G-5
#> 10 10 GREENHOUSE  110   1      4    1    15      G-15

# Example 2: Generates an alpha design with 3 full blocks and 25 treatment.
# Size of IBlocks k = 5. 
# In this case, we show how to use the option data.
treatments <- paste("G-", 1:25, sep = "")
ENTRY <- 1:25
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list) 
#>   ENTRY TREATMENT
#> 1     1       G-1
#> 2     2       G-2
#> 3     3       G-3
#> 4     4       G-4
#> 5     5       G-5
#> 6     6       G-6
alphalattice2 <- alpha_lattice(t = 25,
                               k = 5,
                               r = 3, 
                               l = 1, 
                               plotNumber = 1001, 
                               locationNames = "A", 
                               seed = 1945,
                               data = treatment_list)
alphalattice2$infoDesign
#> $Reps
#> [1] 3
#> 
#> $iBlocks
#> [1] 5
#> 
#> $NumberTreatments
#> [1] 25
#> 
#> $NumberLocations
#> [1] 1
#> 
#> $Locations
#> [1] "A"
#> 
#> $seed
#> [1] 1945
#> 
#> $lambda
#> [1] 0.5
#> 
#> $id_design
#> [1] 12
#> 
head(alphalattice2$fieldBook, 10)
#>    ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1   1        A 1001   1      1    1    20      G-20
#> 2   2        A 1002   1      1    2     5       G-5
#> 3   3        A 1003   1      1    3    10      G-10
#> 4   4        A 1004   1      1    4     1       G-1
#> 5   5        A 1005   1      1    5    12      G-12
#> 6   6        A 1006   1      2    1    19      G-19
#> 7   7        A 1007   1      2    2     8       G-8
#> 8   8        A 1008   1      2    3    13      G-13
#> 9   9        A 1009   1      2    4     9       G-9
#> 10 10        A 1010   1      2    5    17      G-17
```
