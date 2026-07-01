# Generates a Resolvable Row-Column Design (RowColD)

It randomly generates a resolvable row-column design (RowColD). The
design is optimized in both rows and columns blocking factors. The
randomization can be done across multiple locations.

## Usage

``` r
row_column(
  t = NULL,
  nrows = NULL,
  r = NULL,
  l = 1,
  plotNumber = 101,
  locationNames = NULL,
  seed = NULL,
  iterations = 1000,
  data = NULL
)
```

## Arguments

- t:

  Number of treatments.

- nrows:

  Number of rows of a full resolvable replicate.

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

- iterations:

  Number of iterations for design optimization. By default
  `iterations = 1000`.

- data:

  (optional) Data frame with label list of treatments

## Value

A list with four elements.

- `infoDesign` is a list with information on the design parameters.

- `resolvableBlocks` a list with the resolvable row columns blocks.

- `concurrence` is the concurrence matrix.

- `fieldBook` is a data frame with the row-column field book.

## Details

The Row-Column design in FielDHub is built in two stages. The first step
constructs the blocking factor `Columns` using Incomplete Block Units
from an incomplete block design that sets the number of incomplete
blocks as the number of `Columns` in the design, each of which has a
dimension equal to the number of `Rows`. Once this design is generated,
the `Rows` are used as the `Row` blocking factor that is optimized for
A-Efficiency, but levels within the original `Columns` are fixed. To
optimize the `Rows` while maintaining the current optimized `Columns`,
we use a heuristic algorithm that swaps at random treatment positions
within a given `Column (Block)` also selected at random. The algorithm
begins by calculating the A-Efficiency on the initial design, performs a
swap iteration, recalculates the A-Efficiency on the resulting design,
and compares it with the previous one to decide whether to keep or
discard the new design. This iterative process is repeated, by default,
1,000 times.

## References

Edmondson., R. N. (2021). blocksdesign: Nested and crossed block designs
for factorial and unstructured treatment sets.
https://CRAN.R-project.org/package=blocksdesign

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r

# Example 1: Generates a row-column design with 2 full blocks and 24 treatments
# and 6 rows. This for one location. This example uses 100 iterations for the optimization
# but 1000 is the default and recomended value.
rowcold1 <- row_column(
  t = 24, 
  nrows = 6, 
  r = 2, 
  l = 1, 
  plotNumber= 101, 
  locationNames = "Loc1",
  iterations = 100,
  seed = 21
)
rowcold1$infoDesign
#> $rows
#> [1] 6
#> 
#> $columns
#> [1] 4
#> 
#> $reps
#> [1] 2
#> 
#> $treatments
#> [1] 24
#> 
#> $locations
#> [1] 1
#> 
#> $location_names
#> [1] "Loc1"
#> 
#> $seed
#> [1] 21
#> 
#> $id_design
#> [1] 9
#> 
rowcold1$resolvableBlocks
#> $Loc_Loc1
#> $Loc_Loc1$rep1
#>      [,1] [,2] [,3] [,4]
#> [1,]   NA   NA   NA   NA
#> [2,]   NA   NA   NA   NA
#> [3,]   NA   NA   NA   NA
#> [4,]   NA   NA   NA   NA
#> [5,]   NA   NA   NA   NA
#> [6,]   NA   NA   NA   NA
#> 
#> $Loc_Loc1$rep2
#>      [,1] [,2] [,3] [,4]
#> [1,]   NA   NA   NA   NA
#> [2,]   NA   NA   NA   NA
#> [3,]   NA   NA   NA   NA
#> [4,]   NA   NA   NA   NA
#> [5,]   NA   NA   NA   NA
#> [6,]   NA   NA   NA   NA
#> 
#> 
head(rowcold1$fieldBook,12)
#>    ID LOCATION PLOT REP ROW COLUMN ENTRY TREATMENT
#> 1   1     Loc1  101   1   1      1    13      G-13
#> 7   2     Loc1  102   1   1      2    23      G-23
#> 13  3     Loc1  103   1   1      3    10      G-10
#> 19  4     Loc1  104   1   1      4    12      G-12
#> 2   5     Loc1  105   1   2      1    20      G-20
#> 8   6     Loc1  106   1   2      2     8       G-8
#> 14  7     Loc1  107   1   2      3     6       G-6
#> 20  8     Loc1  108   1   2      4    19      G-19
#> 3   9     Loc1  109   1   3      1    24      G-24
#> 9  10     Loc1  110   1   3      2    11      G-11
#> 15 11     Loc1  111   1   3      3    21      G-21
#> 21 12     Loc1  112   1   3      4     5       G-5

# Example 2: Generates a row-column design with 2 full blocks and 30 treatments
# and 5 rows, for one location. This example uses 100 iterations for the optimization
# but 1000 is the default and recommended value.
# In this case, we show how to use the option data.
treatments <- paste("ND-", 1:30, sep = "")
ENTRY <- 1:30
treatment_list <- data.frame(list(ENTRY = ENTRY, TREATMENT = treatments))
head(treatment_list)
#>   ENTRY TREATMENT
#> 1     1      ND-1
#> 2     2      ND-2
#> 3     3      ND-3
#> 4     4      ND-4
#> 5     5      ND-5
#> 6     6      ND-6
rowcold2 <- row_column(
  t = 30, 
  nrows = 5, 
  r = 2, 
  l = 1, 
  plotNumber= 1001, 
  locationNames = "A",
  seed = 15,
  iterations = 100,
  data = treatment_list
)
rowcold2$infoDesign
#> $rows
#> [1] 5
#> 
#> $columns
#> [1] 6
#> 
#> $reps
#> [1] 2
#> 
#> $treatments
#> [1] 30
#> 
#> $locations
#> [1] 1
#> 
#> $location_names
#> [1] "A"
#> 
#> $seed
#> [1] 15
#> 
#> $id_design
#> [1] 9
#> 
rowcold2$resolvableBlocks
#> $Loc_A
#> $Loc_A$rep1
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] NA   NA   NA   NA   NA   NA  
#> [2,] NA   NA   NA   NA   NA   NA  
#> [3,] NA   NA   NA   NA   NA   NA  
#> [4,] NA   NA   NA   NA   NA   NA  
#> [5,] NA   NA   NA   NA   NA   NA  
#> 
#> $Loc_A$rep2
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] NA   NA   NA   NA   NA   NA  
#> [2,] NA   NA   NA   NA   NA   NA  
#> [3,] NA   NA   NA   NA   NA   NA  
#> [4,] NA   NA   NA   NA   NA   NA  
#> [5,] NA   NA   NA   NA   NA   NA  
#> 
#> 
head(rowcold2$fieldBook,12)
#>    ID LOCATION PLOT REP ROW COLUMN ENTRY TREATMENT
#> 1   1        A 1001   1   1      1     5      ND-5
#> 6   2        A 1002   1   1      2     7      ND-7
#> 11  3        A 1003   1   1      3    14     ND-14
#> 16  4        A 1004   1   1      4    23     ND-23
#> 21  5        A 1005   1   1      5     9      ND-9
#> 26  6        A 1006   1   1      6    15     ND-15
#> 2   7        A 1007   1   2      1    10     ND-10
#> 7   8        A 1008   1   2      2    17     ND-17
#> 12  9        A 1009   1   2      3    13     ND-13
#> 17 10        A 1010   1   2      4     6      ND-6
#> 22 11        A 1011   1   2      5    29     ND-29
#> 27 12        A 1012   1   2      6    20     ND-20
  
```
