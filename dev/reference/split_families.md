# Split a population of genotypes randomly into several locations.

Split a population of genotypes randomly into several locations, with
the aim of having approximatelly the same number of replicates of each
genotype, line or treatment per location.

## Usage

``` r
split_families(l = NULL, data = NULL)
```

## Arguments

- l:

  Number of locations.

- data:

  Data frame with the entry (ENTRY) and the labels of each treatment
  (NAME) and number of individuals per family group (FAMILY).

## Value

A list with two elements.

- `rowsEachlist` is a table with a summary of cases.

- `data_locations` is a data frame with the entries for each location

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
split_population <- split_families(l = 8, data = gen.list)
print(split_population)
#> Split families: 
#> 
#> 
#>  Data frame with the summary of cases by location: 
#>     Location   n
#> 1 Location 1 244
#> 2 Location 2 256
#> 3 Location 3 250
#> 4 Location 4 245
#> 5 Location 5 258
#> 6 Location 6 245
#> 7 Location 7 246
#> 8 Location 8 256
#> 
#>  10 First observations of the data frame with the entries for each location: 
#>    ENTRY    NAME FAMILY   LOCATION
#> 1    967  SB-967      1 Location 1
#> 2   1565 SB-1565      2 Location 1
#> 3   1030 SB-1030      2 Location 1
#> 4   1276 SB-1276      2 Location 1
#> 5   1953 SB-1953      3 Location 1
#> 6    673  SB-673      3 Location 1
#> 7    423  SB-423      4 Location 1
#> 8   1977 SB-1977      4 Location 1
#> 9    882  SB-882      5 Location 1
#> 10  1379 SB-1379      5 Location 1
summary(split_population)
#> Split families: 
#> 
#> 1. Structure of the data frame with the summary of entries by location: 
#> 
#> 'data.frame':    8 obs. of  2 variables:
#>  $ Location: chr  "Location 1" "Location 2" "Location 3" "Location 4" ...
#>  $ n       : num  244 256 250 245 258 245 246 256
#> 2. Structure of the data frame with the entries for each location: 
#> 
#> 'data.frame':    2000 obs. of  4 variables:
#>  $ ENTRY   : int  967 1565 1030 1276 1953 673 423 1977 882 1379 ...
#>  $ NAME    : chr  "SB-967" "SB-1565" "SB-1030" "SB-1276" ...
#>  $ FAMILY  : num  1 2 2 2 3 3 4 4 5 5 ...
#>  $ LOCATION: chr  "Location 1" "Location 1" "Location 1" "Location 1" ...
head(split_population$data_locations,12)
#>    ENTRY    NAME FAMILY   LOCATION
#> 1    967  SB-967      1 Location 1
#> 2   1565 SB-1565      2 Location 1
#> 3   1030 SB-1030      2 Location 1
#> 4   1276 SB-1276      2 Location 1
#> 5   1953 SB-1953      3 Location 1
#> 6    673  SB-673      3 Location 1
#> 7    423  SB-423      4 Location 1
#> 8   1977 SB-1977      4 Location 1
#> 9    882  SB-882      5 Location 1
#> 10  1379 SB-1379      5 Location 1
#> 11   740  SB-740      6 Location 1
#> 12   910  SB-910      6 Location 1
```
