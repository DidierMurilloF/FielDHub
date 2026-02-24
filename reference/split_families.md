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
#> Error: object 'gen.list' not found
print(split_population)
#> Error: object 'split_population' not found
summary(split_population)
#> Error: object 'split_population' not found
head(split_population$data_locations,12)
#> Error: object 'split_population' not found
```
