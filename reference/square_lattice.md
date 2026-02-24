# Generates a Square Lattice Design.

It randomly generates a square lattice design across locations.

## Usage

``` r
square_lattice(
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

- `fieldBook` is a data frame with the square lattice design field book.

## References

Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs
for factorial and unstructured treatment sets.
https://CRAN.R-project.org/package=blocksdesign

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a square lattice design with 5 full blocks, 8 units per IBlock,
# 8 IBlocks for a square number of treatmens of 64 in two locations.
squareLattice1 <- square_lattice(t = 64, k = 8, r = 5, l = 2, 
                                 plotNumber = c(1001, 2001),
                                 locationNames = c("FARGO", "MINOT"), 
                                 seed = 1986)
squareLattice1$infoDesign
#> $Reps
#> [1] 5
#> 
#> $IBlocks
#> [1] 8
#> 
#> $NumberTreatments
#> [1] 64
#> 
#> $NumberLocations
#> [1] 2
#> 
#> $Locations
#> [1] "FARGO" "MINOT"
#> 
#> $seed
#> [1] 1986
#> 
#> $lambda
#> [1] 0.5555556
#> 
#> $id_design
#> [1] 10
#> 
head(squareLattice1$fieldBook,12)
#>    ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1   1    FARGO 1001   1      1    1    43      G-43
#> 2   2    FARGO 1002   1      1    2    49      G-49
#> 3   3    FARGO 1003   1      1    3    35      G-35
#> 4   4    FARGO 1004   1      1    4    15      G-15
#> 5   5    FARGO 1005   1      1    5    45      G-45
#> 6   6    FARGO 1006   1      1    6    42      G-42
#> 7   7    FARGO 1007   1      1    7    40      G-40
#> 8   8    FARGO 1008   1      1    8    10      G-10
#> 9   9    FARGO 1009   1      2    1    61      G-61
#> 10 10    FARGO 1010   1      2    2    21      G-21
#> 11 11    FARGO 1011   1      2    3    62      G-62
#> 12 12    FARGO 1012   1      2    4    34      G-34

# Example 2: Generates a square lattice design with 3 full blocks, 7 units per IBlock,
# 7 IBlocks for a square number of treatmens of 49 in one location.
# In this case, we show how to use the option data.
treatments <- paste("G", 1:49, sep = "")
ENTRY <- 1:49
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list) 
#>   ENTRY TREATMENT
#> 1     1        G1
#> 2     2        G2
#> 3     3        G3
#> 4     4        G4
#> 5     5        G5
#> 6     6        G6
squareLattice2 <- square_lattice(t = 49, k = 7, r = 3, l = 1, 
                                 plotNumber = 1001,
                                 locationNames = "CASSELTON", 
                                 seed = 1986,
                                 data = treatment_list)
squareLattice2$infoDesign
#> $Reps
#> [1] 3
#> 
#> $IBlocks
#> [1] 7
#> 
#> $NumberTreatments
#> [1] 49
#> 
#> $NumberLocations
#> [1] 1
#> 
#> $Locations
#> [1] "CASSELTON"
#> 
#> $seed
#> [1] 1986
#> 
#> $lambda
#> [1] 0.375
#> 
#> $id_design
#> [1] 10
#> 
head(squareLattice2$fieldBook,12)
#>    ID  LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1   1 CASSELTON 1001   1      1    1    27       G27
#> 2   2 CASSELTON 1002   1      1    2    30       G30
#> 3   3 CASSELTON 1003   1      1    3    42       G42
#> 4   4 CASSELTON 1004   1      1    4     1        G1
#> 5   5 CASSELTON 1005   1      1    5    20       G20
#> 6   6 CASSELTON 1006   1      1    6    26       G26
#> 7   7 CASSELTON 1007   1      1    7    48       G48
#> 8   8 CASSELTON 1008   1      2    1    49       G49
#> 9   9 CASSELTON 1009   1      2    2    29       G29
#> 10 10 CASSELTON 1010   1      2    3    24       G24
#> 11 11 CASSELTON 1011   1      2    4    34       G34
#> 12 12 CASSELTON 1012   1      2    5    47       G47
```
