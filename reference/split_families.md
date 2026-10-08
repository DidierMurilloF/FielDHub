# Split a population of genotypes randomly into several locations.

Split a population of genotypes randomly into several locations, with
the aim of having approximatelly the same number of replicates of each
genotype, line or treatment per location.

## Usage

``` r
split_families(l = NULL, data = NULL, seed = NULL)
```

## Arguments

- l:

  Number of locations.

- data:

  Data frame with the entry (ENTRY) and the labels of each treatment
  (NAME) and number of individuals per family group (FAMILY).

- seed:

  (optional) A single real number specifying the random seed. When
  omitted, one integer is drawn from the current random-number stream
  and recorded in `infoDesign$seed` and `metadata$seed`; the
  allocation's own randomization does not change the caller's stream.

## Value

A list with two elements.

- `rowsEachlist` is a table with a summary of cases.

- `data_locations` is a data frame with the entries for each location

## Details

To reproduce an allocation previously made with
`set.seed(s); split_families(l, data)`, use
`split_families(l, data, seed = s)`.

## Reproducibility

The result records effective inputs and the resolved seed in
`metadata$parameters`. Under the same package versions and RNG settings,
rebuild a result `x` with
`do.call(split_families, x$metadata$parameters)`.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Split a population of 3000 and 200 families into 8 locations. 
# Original dataset is been simulated.
set.seed(77)
N <- 2000; families <- 100
ENTRY <- 1:N
NAME <- paste0("SB-", 1:N)
FAMILY <- vector(mode = "numeric", length = N)
x <- 1:N
for (i in x) { FAMILY[i] <- sample(1:families, size = 1, replace = TRUE) }
gen.list <- data.frame(list(ENTRY = ENTRY, NAME = NAME, FAMILY = FAMILY))
head(gen.list)
#>   ENTRY NAME FAMILY
#> 1     1 SB-1     18
#> 2     2 SB-2     45
#> 3     3 SB-3     69
#> 4     4 SB-4     57
#> 5     5 SB-5     37
#> 6     6 SB-6     29
# Now we are going to use the split_families() function.
split_population <- split_families(l = 8, data = gen.list, seed = 77)
print(split_population)
#> Split families: 
#> 
#> 
#>  Data frame with the summary of cases by location: 
#>     Location   n
#> 1 Location 1 247
#> 2 Location 2 247
#> 3 Location 3 262
#> 4 Location 4 249
#> 5 Location 5 251
#> 6 Location 6 243
#> 7 Location 7 249
#> 8 Location 8 252
#> 
#>  10 First observations of the data frame with the entries for each location: 
#>    ENTRY    NAME FAMILY   LOCATION
#> 1    168  SB-168      1 Location 1
#> 2    337  SB-337      1 Location 1
#> 3   1529 SB-1529      2 Location 1
#> 4    171  SB-171      2 Location 1
#> 5   1317 SB-1317      2 Location 1
#> 6    673  SB-673      3 Location 1
#> 7   1647 SB-1647      3 Location 1
#> 8    196  SB-196      4 Location 1
#> 9    829  SB-829      4 Location 1
#> 10  1379 SB-1379      5 Location 1
summary(split_population)
#> Split families: 
#> 
#> 1. Structure of the data frame with the summary of entries by location: 
#> 
#> 'data.frame':    8 obs. of  2 variables:
#>  $ Location: chr  "Location 1" "Location 2" "Location 3" "Location 4" ...
#>  $ n       : num  247 247 262 249 251 243 249 252
#> 2. Structure of the data frame with the entries for each location: 
#> 
#> 'data.frame':    2000 obs. of  4 variables:
#>  $ ENTRY   : int  168 337 1529 171 1317 673 1647 196 829 1379 ...
#>  $ NAME    : chr  "SB-168" "SB-337" "SB-1529" "SB-171" ...
#>  $ FAMILY  : num  1 1 2 2 2 3 3 4 4 5 ...
#>  $ LOCATION: chr  "Location 1" "Location 1" "Location 1" "Location 1" ...
head(split_population$data_locations,12)
#>    ENTRY    NAME FAMILY   LOCATION
#> 1    168  SB-168      1 Location 1
#> 2    337  SB-337      1 Location 1
#> 3   1529 SB-1529      2 Location 1
#> 4    171  SB-171      2 Location 1
#> 5   1317 SB-1317      2 Location 1
#> 6    673  SB-673      3 Location 1
#> 7   1647 SB-1647      3 Location 1
#> 8    196  SB-196      4 Location 1
#> 9    829  SB-829      4 Location 1
#> 10  1379 SB-1379      5 Location 1
#> 11  1384 SB-1384      5 Location 1
#> 12  1631 SB-1631      6 Location 1
```
