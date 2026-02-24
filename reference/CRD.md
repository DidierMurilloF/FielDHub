# Generates a Completely Randomized Design (CRD)

It randomly generates a completely randomized design.

## Usage

``` r
CRD(
  t = NULL,
  reps = NULL,
  plotNumber = 101,
  locationName = NULL,
  seed = NULL,
  data = NULL
)
```

## Arguments

- t:

  An integer number with total number of treatments or a vector of
  dimension t with labels.

- reps:

  Number of replicates of each treatment.

- plotNumber:

  Starting plot number. By default `plotNumber = 101`.

- locationName:

  (optional) Name of the location.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- data:

  (optional) Data frame with the 2 columns with labels of each
  treatments and its number of replicates.

## Value

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the CRD field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a CRD design with 10 treatments and 5 reps each.
crd1 <- CRD(
  t = 10,
  reps = 5,
  plotNumber = 101,
  seed = 1987,
  locationName = "Fargo"
)
crd1$infoDesign
#> $numberofTreatments
#> [1] 10
#> 
#> $treatments
#>  [1] "T1"  "T2"  "T3"  "T4"  "T5"  "T6"  "T7"  "T8"  "T9"  "T10"
#> 
#> $Reps
#> [1] 5
#> 
#> $locationName
#> [1] "Fargo"
#> 
#> $seed
#> [1] 1987
#> 
#> $id_design
#> [1] 1
#> 
head(crd1$fieldBook, 10)
#>    ID LOCATION PLOT REP TREATMENT
#> 1   1    Fargo  101   1       T10
#> 2   2    Fargo  102   4        T1
#> 3   3    Fargo  103   3        T3
#> 4   4    Fargo  104   3        T9
#> 5   5    Fargo  105   4        T3
#> 6   6    Fargo  106   5        T3
#> 7   7    Fargo  107   1        T2
#> 8   8    Fargo  108   3        T4
#> 9   9    Fargo  109   1        T1
#> 10 10    Fargo  110   3        T1

# Example 2: Generates a CRD design with 15 treatments and 6 reps each.
Gens <- paste("Wheat", 1:15, sep = "")
crd2 <- CRD(
  t = Gens,
  reps = 6,
  plotNumber = 1001,
  seed = 1654,
  locationName = "Fargo"
)
crd2$infoDesign
#> $numberofTreatments
#> [1] 15
#> 
#> $treatments
#>  [1] "Wheat1"  "Wheat2"  "Wheat3"  "Wheat4"  "Wheat5"  "Wheat6"  "Wheat7" 
#>  [8] "Wheat8"  "Wheat9"  "Wheat10" "Wheat11" "Wheat12" "Wheat13" "Wheat14"
#> [15] "Wheat15"
#> 
#> $Reps
#> [1] 6
#> 
#> $locationName
#> [1] "Fargo"
#> 
#> $seed
#> [1] 1654
#> 
#> $id_design
#> [1] 1
#> 
head(crd2$fieldBook, 10)
#>    ID LOCATION PLOT REP TREATMENT
#> 1   1    Fargo 1001   6    Wheat3
#> 2   2    Fargo 1002   2    Wheat8
#> 3   3    Fargo 1003   2    Wheat2
#> 4   4    Fargo 1004   4    Wheat4
#> 5   5    Fargo 1005   1    Wheat1
#> 6   6    Fargo 1006   1    Wheat4
#> 7   7    Fargo 1007   1   Wheat13
#> 8   8    Fargo 1008   1    Wheat1
#> 9   9    Fargo 1009   6   Wheat15
#> 10 10    Fargo 1010   4    Wheat7

# Example 3: Generates a CRD design with 12 treatments and 4 reps each.
# In this case, we show how to use the option data.
treatments <- paste("ND-", 1:12, sep = "")
treatment_list <- data.frame(list(TREATMENT = treatments, REP = 4))
head(treatment_list)
#>   TREATMENT REP
#> 1      ND-1   4
#> 2      ND-2   4
#> 3      ND-3   4
#> 4      ND-4   4
#> 5      ND-5   4
#> 6      ND-6   4
crd3 <- CRD(
  t = NULL,
  reps = NULL,
  plotNumber = 2001,
  seed = 1655,
  locationName = "Cali",
  data = treatment_list
)
crd3$infoDesign
#> $numberofTreatments
#> [1] 12
#> 
#> $treatments
#>  [1] "ND-1"  "ND-2"  "ND-3"  "ND-4"  "ND-5"  "ND-6"  "ND-7"  "ND-8"  "ND-9" 
#> [10] "ND-10" "ND-11" "ND-12"
#> 
#> $Reps
#>  [1] 4 4 4 4 4 4 4 4 4 4 4 4
#> 
#> $locationName
#> [1] "Cali"
#> 
#> $seed
#> [1] 1655
#> 
#> $id_design
#> [1] 1
#> 
head(crd3$fieldBook, 10)
#>    ID LOCATION PLOT REP TREATMENT
#> 1   1     Cali 2001   4      ND-3
#> 2   2     Cali 2002   1      ND-7
#> 3   3     Cali 2003   2      ND-2
#> 4   4     Cali 2004   3      ND-8
#> 5   5     Cali 2005   3      ND-2
#> 6   6     Cali 2006   1     ND-10
#> 7   7     Cali 2007   3      ND-7
#> 8   8     Cali 2008   4      ND-9
#> 9   9     Cali 2009   3      ND-4
#> 10 10     Cali 2010   1      ND-3
```
