# Generates a Resolvable Incomplete Block Design

Randomly generates a resolvable incomplete block design (IBD) of
characteristics (t, k, r). The randomization can be done across
locations.

## Usage

``` r
incomplete_blocks(
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

  (optional) Names for each location.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- data:

  (optional) Data frame with label list of treatments.

## Value

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the incomplete block design field
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
# Example 1: Generates a resolvable IBD of characteristics (t,k,r) = (12,4,2).
# 1-resolvable IBDs
ibd1 <- incomplete_blocks(t = 12,
                          k = 4,
                          r = 2,
                          seed = 1984)
ibd1$infoDesign
#> $Reps
#> [1] 2
#> 
#> $iBlocks
#> [1] 3
#> 
#> $NumberTreatments
#> [1] 12
#> 
#> $NumberLocations
#> [1] 1
#> 
#> $Locations
#> [1] 1
#> 
#> $seed
#> [1] 1984
#> 
#> $lambda
#> [1] 0.5454545
#> 
#> $id_design
#> [1] 8
#> 
head(ibd1$fieldBook)
#>   ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1  1        1  101   1      1    1     9       G-9
#> 2  2        1  102   1      1    2     5       G-5
#> 3  3        1  103   1      1    3     6       G-6
#> 4  4        1  104   1      1    4    12      G-12
#> 5  5        1  105   1      2    1     2       G-2
#> 6  6        1  106   1      2    2    11      G-11

# Example 2: Generates a balanced resolvable IBD of characteristics (t,k,r) = (15,3,7).
# In this case, we show how to use the option data.
treatments <- paste("TX-", 1:15, sep = "")
ENTRY <- 1:15
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list)
#>   ENTRY TREATMENT
#> 1     1      TX-1
#> 2     2      TX-2
#> 3     3      TX-3
#> 4     4      TX-4
#> 5     5      TX-5
#> 6     6      TX-6
ibd2 <- incomplete_blocks(t = 15,
                          k = 3,
                          r = 7,
                          seed = 1985,
                          data = treatment_list)
ibd2$infoDesign
#> $Reps
#> [1] 7
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
#> [1] 1
#> 
#> $seed
#> [1] 1985
#> 
#> $lambda
#> [1] 1
#> 
#> $id_design
#> [1] 8
#> 
head(ibd2$fieldBook)
#>   ID LOCATION PLOT REP IBLOCK UNIT ENTRY TREATMENT
#> 1  1        1  101   1      1    1     1      TX-1
#> 2  2        1  102   1      1    2    11     TX-11
#> 3  3        1  103   1      1    3    13     TX-13
#> 4  4        1  104   1      2    1     3      TX-3
#> 5  5        1  105   1      2    2    14     TX-14
#> 6  6        1  106   1      2    3     4      TX-4
```
