# Print a `FielDHub` object

Prints information about any `FielDHub` function.

## Usage

``` r
# S3 method for class 'FielDHub'
print(x, n, ...)
```

## Arguments

- x:

  an object inheriting from class

- n:

  a single integer. If positive or zero, size for the resulting object:
  number of elements for a vector (including lists), rows for a matrix
  or data frame or lines for a function. If negative, all but the n
  last/first number of elements of x.

- ...:

  further arguments passed to
  [`head`](https://rdrr.io/r/utils/head.html).

## Value

an object inheriting from class `FielDHub`

## Author

Thiago de Paula Oliveira, <thiago.paula.oliveira@alumni.usp.br> \[aut\],
Didier Murillo \[aut\]

## Examples

``` r
# Example 1: Generates a CRD design with 5 treatments and 5 reps each.
crd1 <- CRD(t = 5, reps = 5, plotNumber = 101,
seed = 1985, locationName = "Fargo")
crd1$infoDesign
#> $numberofTreatments
#> [1] 5
#> 
#> $treatments
#> [1] "T1" "T2" "T3" "T4" "T5"
#> 
#> $Reps
#> [1] 5
#> 
#> $locationName
#> [1] "Fargo"
#> 
#> $seed
#> [1] 1985
#> 
#> $id_design
#> [1] 1
#> 
print(crd1)
#> Completely Randomized Design (CRD) 
#> 
#> Information on the design parameters: 
#> List of 5
#>  $ numberofTreatments: num 5
#>  $ treatments        : chr [1:5] "T1" "T2" "T3" "T4" ...
#>  $ Reps              : num 5
#>  $ locationName      : chr "Fargo"
#>  $ seed              : num 1985
#> 
#>  10 First observations of the data frame with the CRD field book: 
#>    ID LOCATION PLOT REP TREATMENT
#> 1   1    Fargo  101   3        T3
#> 2   2    Fargo  102   4        T2
#> 3   3    Fargo  103   2        T1
#> 4   4    Fargo  104   3        T5
#> 5   5    Fargo  105   2        T5
#> 6   6    Fargo  106   2        T4
#> 7   7    Fargo  107   4        T3
#> 8   8    Fargo  108   5        T4
#> 9   9    Fargo  109   1        T2
#> 10 10    Fargo  110   5        T1
```
