# Generates a Split Split Plot Design

It randomly generates a split split plot design (SSPD) across locations.

## Usage

``` r
split_split_plot(
  wp = NULL,
  sp = NULL,
  ssp = NULL,
  reps = NULL,
  type = 2,
  l = 1,
  plotNumber = 101,
  seed = NULL,
  locationNames = NULL,
  factorLabels = TRUE,
  data = NULL
)
```

## Arguments

- wp:

  Number of whole plots, as an integer or a vector.

- sp:

  Number of sub plots per whole plot, as an integer or a vector.

- ssp:

  Number of sub-sub plots, as an integer or a vector.

- reps:

  Number of blocks (full replicates).

- type:

  Option for CRD or RCBD designs. Values are `type = 1` (CRD) or
  `type = 2` (RCBD). By default `type = 2`.

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- locationNames:

  (optional) Names for each location.

- factorLabels:

  (optional) If `TRUE` retain the levels labels from the original data
  set otherwise, numeric labels will be assigned. Default is
  `factorLabels =TRUE`.

- data:

  (optional) Data frame with label list of treatments.

## Value

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the split split plot field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a split split plot design SSPD with 5 whole plots, 2 sub-plots,
# 3 sub-sub plots, and 3 reps in an RCBD arrangement. This is for one location.
SSPD1 <- split_split_plot(wp = 4, sp = 2, ssp = 3, reps = 5, l = 1, 
                          plotNumber = 101, 
                          seed = 23, 
                          type = 2, 
                          locationNames = "FARGO")
SSPD1$infoDesign
#> $Whole.Plots
#> [1] 1 2 3 4
#> 
#> $Sub.Plots
#> [1] 1 2
#> 
#> $Sub.Sub.Plots
#> [1] 1 2 3
#> 
#> $Locations
#> [1] 1
#> 
#> $typeDesign
#> [1] "RCBD"
#> 
#> $seed
#> [1] 23
#> 
#> $id_design
#> [1] 6
#> 
head(SSPD1$fieldBook,12)
#>    ID LOCATION PLOT REP WHOLE_PLOT SUB_PLOT SUB_SUB_PLOT TRT_COMB
#> 1   1    FARGO  101   1          1        2            2    1|2|2
#> 2   2    FARGO  101   1          1        2            1    1|2|1
#> 3   3    FARGO  101   1          1        2            3    1|2|3
#> 4   4    FARGO  101   1          1        1            2    1|1|2
#> 5   5    FARGO  101   1          1        1            1    1|1|1
#> 6   6    FARGO  101   1          1        1            3    1|1|3
#> 7   7    FARGO  102   1          3        1            2    3|1|2
#> 8   8    FARGO  102   1          3        1            1    3|1|1
#> 9   9    FARGO  102   1          3        1            3    3|1|3
#> 10 10    FARGO  102   1          3        2            2    3|2|2
#> 11 11    FARGO  102   1          3        2            1    3|2|1
#> 12 12    FARGO  102   1          3        2            3    3|2|3

# Example 2: Generates a split split plot design SSPD with 2 whole plost 
# (Irrigation, No irrigation), 5 sub plots (4 types of fungicide + one control), and 
# 10 sub-sub plots (Ten varieties of beans), and 4 reps in an RCBD arrangement.
# This is for 3 locations. In this case, we show how to use the option data.
wp <- paste("IRR_", c("NO", "Yes"), sep = "") #Irrigation (2 Whole plots)
sp <- c("NFung", paste("Fung", 1:4, sep = "")) #Fungicides (5 Sub plots)
ssp <- paste("Beans", 1:10, sep = "") #Beans varieties (10 Sub-sub plots)
split_split_plot_Data <- data.frame(list(WHOLPLOT = c(wp, rep(NA, 8)), 
                                         SUBPLOT = c(sp, rep(NA, 5)),
                                         SUB_SUBPLOTS = ssp))
head(split_split_plot_Data, 10)
#>    WHOLPLOT SUBPLOT SUB_SUBPLOTS
#> 1    IRR_NO   NFung       Beans1
#> 2   IRR_Yes   Fung1       Beans2
#> 3      <NA>   Fung2       Beans3
#> 4      <NA>   Fung3       Beans4
#> 5      <NA>   Fung4       Beans5
#> 6      <NA>    <NA>       Beans6
#> 7      <NA>    <NA>       Beans7
#> 8      <NA>    <NA>       Beans8
#> 9      <NA>    <NA>       Beans9
#> 10     <NA>    <NA>      Beans10
SSPD2 <- split_split_plot(reps = 4, l = 3, 
                          plotNumber = c(101, 1001, 2001),
                          seed = 23, 
                          type = 2, 
                          locationNames = c("A", "B", "C"),
                          data = split_split_plot_Data)
SSPD2$infoDesign
#> $Whole.Plots
#> [1] "IRR_NO"  "IRR_Yes"
#> 
#> $Sub.Plots
#> [1] "NFung" "Fung1" "Fung2" "Fung3" "Fung4"
#> 
#> $Sub.Sub.Plots
#>  [1] "Beans1"  "Beans2"  "Beans3"  "Beans4"  "Beans5"  "Beans6"  "Beans7" 
#>  [8] "Beans8"  "Beans9"  "Beans10"
#> 
#> $Locations
#> [1] 3
#> 
#> $typeDesign
#> [1] "RCBD"
#> 
#> $seed
#> [1] 23
#> 
#> $id_design
#> [1] 6
#> 
head(SSPD2$fieldBook,12)
#>    ID LOCATION PLOT REP WHOLE_PLOT SUB_PLOT SUB_SUB_PLOT             TRT_COMB
#> 1   1        A  101   1     IRR_NO    Fung3       Beans3  IRR_NO|Fung3|Beans3
#> 2   2        A  101   1     IRR_NO    Fung3       Beans9  IRR_NO|Fung3|Beans9
#> 3   3        A  101   1     IRR_NO    Fung3      Beans10 IRR_NO|Fung3|Beans10
#> 4   4        A  101   1     IRR_NO    Fung3       Beans7  IRR_NO|Fung3|Beans7
#> 5   5        A  101   1     IRR_NO    Fung3       Beans8  IRR_NO|Fung3|Beans8
#> 6   6        A  101   1     IRR_NO    Fung3       Beans5  IRR_NO|Fung3|Beans5
#> 7   7        A  101   1     IRR_NO    Fung3       Beans2  IRR_NO|Fung3|Beans2
#> 8   8        A  101   1     IRR_NO    Fung3       Beans6  IRR_NO|Fung3|Beans6
#> 9   9        A  101   1     IRR_NO    Fung3       Beans4  IRR_NO|Fung3|Beans4
#> 10 10        A  101   1     IRR_NO    Fung3       Beans1  IRR_NO|Fung3|Beans1
#> 11 11        A  101   1     IRR_NO    Fung1       Beans9  IRR_NO|Fung1|Beans9
#> 12 12        A  101   1     IRR_NO    Fung1       Beans2  IRR_NO|Fung1|Beans2
             
```
