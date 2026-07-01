# Summary a `FielDHub` object

Summarise information on the design parameters, and data frame structure

## Usage

``` r
# S3 method for class 'FielDHub'
summary(object, ...)
```

## Arguments

- object:

  an object inheriting from class `FielDHub`

- ...:

  Unused, for extensibility

## Value

an object inheriting from class `summary.FielDHub`

## Author

Thiago de Paula Oliveira, <thiago.paula.oliveira@alumni.usp.br>

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
summary(crd1)
#> Completely Randomized Design (CRD): 
#> 
#> 1. Information on the design parameters: 
#> List of 6
#>  $ numberofTreatments: num 5
#>  $ treatments        : chr [1:5] "T1" "T2" "T3" "T4" ...
#>  $ Reps              : num 5
#>  $ locationName      : chr "Fargo"
#>  $ seed              : num 1985
#>  $ id_design         : num 1
#> 
#> 2. Structure of the data frame with the CRD field book: 
#> 
#> 'data.frame':    25 obs. of  5 variables:
#>  $ ID       : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ LOCATION : chr  "Fargo" "Fargo" "Fargo" "Fargo" ...
#>  $ PLOT     : num  101 102 103 104 105 106 107 108 109 110 ...
#>  $ REP      : Factor w/ 5 levels "1","2","3","4",..: 3 4 2 3 2 2 4 5 1 5 ...
#>  $ TREATMENT: chr  "T3" "T2" "T1" "T5" ...
```
