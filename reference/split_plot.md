# Generates a Split Plot Design

It randomly generates a split plot design (SPD) across locations.

## Usage

``` r
split_plot(
  wp = NULL,
  sp = NULL,
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

- `fieldBook` is a data frame with the split plot field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a split plot design SPD with 4 whole plots, 2 sub plots per whole plot,
# and 4 reps in an RCBD arrangement. This in for a single location.
SPDExample1 <- split_plot(wp = 4, sp = 2, reps = 5, l = 1, 
                          plotNumber = 101, 
                          seed = 14,
                          type = 2, 
                          locationNames = "FARGO")
SPDExample1$infoDesign
#> $WholePlots
#> [1] 1 2 3 4
#> 
#> $SubPlots
#> [1] 1 2
#> 
#> $locationNumber
#> [1] 1
#> 
#> $locationNames
#> [1] "FARGO"
#> 
#> $plotNumbers
#> [1] 101
#> 
#> $typeDesign
#> [1] "RCBD"
#> 
#> $seed
#> [1] 14
#> 
#> $id_design
#> [1] 5
#> 
SPDExample1$layoutlocations
#> [[1]]
#>       PLOT  REP Whole-plot Sub-plot
#>  [1,] "101" "1" "1"        "1 2"   
#>  [2,] "102" "1" "4"        "2 1"   
#>  [3,] "103" "1" "3"        "2 1"   
#>  [4,] "104" "1" "2"        "2 1"   
#>  [5,] "201" "2" "3"        "1 2"   
#>  [6,] "202" "2" "2"        "2 1"   
#>  [7,] "203" "2" "4"        "2 1"   
#>  [8,] "204" "2" "1"        "2 1"   
#>  [9,] "301" "3" "4"        "2 1"   
#> [10,] "302" "3" "2"        "1 2"   
#> [11,] "303" "3" "1"        "2 1"   
#> [12,] "304" "3" "3"        "2 1"   
#> [13,] "401" "4" "1"        "2 1"   
#> [14,] "402" "4" "3"        "2 1"   
#> [15,] "403" "4" "2"        "2 1"   
#> [16,] "404" "4" "4"        "1 2"   
#> [17,] "501" "5" "3"        "1 2"   
#> [18,] "502" "5" "1"        "2 1"   
#> [19,] "503" "5" "2"        "2 1"   
#> [20,] "504" "5" "4"        "2 1"   
#> 
head(SPDExample1$fieldBook,12)
#>    ID LOCATION PLOT REP WHOLE_PLOT SUB_PLOT TRT_COMB
#> 1   1    FARGO  101   1          1        1      1|1
#> 2   2    FARGO  101   1          1        2      1|2
#> 3   3    FARGO  102   1          4        2      4|2
#> 4   4    FARGO  102   1          4        1      4|1
#> 5   5    FARGO  103   1          3        2      3|2
#> 6   6    FARGO  103   1          3        1      3|1
#> 7   7    FARGO  104   1          2        2      2|2
#> 8   8    FARGO  104   1          2        1      2|1
#> 9   9    FARGO  201   2          3        1      3|1
#> 10 10    FARGO  201   2          3        2      3|2
#> 11 11    FARGO  202   2          2        2      2|2
#> 12 12    FARGO  202   2          2        1      2|1

# Example 2: Generates a split plot design SPD with 5 whole plots 
# (4 types of fungicide + one control), 10 sub plots per whole plot (10 bean varieties), 
# and 6 reps in an RCBD arrangement. This in 3 locations or sites.
# In this case, we show how to use the option data.
wp <- c("NFung", paste("Fung", 1:4, sep = ""))  # Fungicides (5 Whole plots)
sp <- paste("Beans", 1:10, sep = "")            # Beans varieties (10 sub plots)
split_plot_Data <- data.frame(list(WHOLPLOT = c(wp, rep(NA, 5)), SUBPLOT = sp))
head(split_plot_Data, 12)
#>    WHOLPLOT SUBPLOT
#> 1     NFung  Beans1
#> 2     Fung1  Beans2
#> 3     Fung2  Beans3
#> 4     Fung3  Beans4
#> 5     Fung4  Beans5
#> 6      <NA>  Beans6
#> 7      <NA>  Beans7
#> 8      <NA>  Beans8
#> 9      <NA>  Beans9
#> 10     <NA> Beans10
SPDExample2 <- split_plot(reps = 6, l = 3, 
                          plotNumber = c(101, 1001, 2001),
                          seed = 23, 
                          type = 2, 
                          locationNames = c("A", "B", "C"),
                          data = split_plot_Data)
SPDExample2$infoDesign
#> $WholePlots
#> [1] "NFung" "Fung1" "Fung2" "Fung3" "Fung4"
#> 
#> $SubPlots
#>  [1] "Beans1"  "Beans2"  "Beans3"  "Beans4"  "Beans5"  "Beans6"  "Beans7" 
#>  [8] "Beans8"  "Beans9"  "Beans10"
#> 
#> $locationNumber
#> [1] 3
#> 
#> $locationNames
#> [1] "A" "B" "C"
#> 
#> $plotNumbers
#> [1]  101 1001 2001
#> 
#> $typeDesign
#> [1] "RCBD"
#> 
#> $seed
#> [1] 23
#> 
#> $id_design
#> [1] 5
#> 
SPDExample2$layoutlocations
#> [[1]]
#>       PLOT  REP Whole-plot
#>  [1,] "101" "1" "Fung4"   
#>  [2,] "102" "1" "Fung3"   
#>  [3,] "103" "1" "Fung2"   
#>  [4,] "104" "1" "Fung1"   
#>  [5,] "105" "1" "NFung"   
#>  [6,] "201" "2" "Fung2"   
#>  [7,] "202" "2" "Fung4"   
#>  [8,] "203" "2" "NFung"   
#>  [9,] "204" "2" "Fung1"   
#> [10,] "205" "2" "Fung3"   
#> [11,] "301" "3" "NFung"   
#> [12,] "302" "3" "Fung2"   
#> [13,] "303" "3" "Fung4"   
#> [14,] "304" "3" "Fung3"   
#> [15,] "305" "3" "Fung1"   
#> [16,] "401" "4" "Fung3"   
#> [17,] "402" "4" "Fung2"   
#> [18,] "403" "4" "Fung1"   
#> [19,] "404" "4" "Fung4"   
#> [20,] "405" "4" "NFung"   
#> [21,] "501" "5" "Fung4"   
#> [22,] "502" "5" "Fung1"   
#> [23,] "503" "5" "Fung3"   
#> [24,] "504" "5" "NFung"   
#> [25,] "505" "5" "Fung2"   
#> [26,] "601" "6" "Fung1"   
#> [27,] "602" "6" "Fung2"   
#> [28,] "603" "6" "Fung3"   
#> [29,] "604" "6" "Fung4"   
#> [30,] "605" "6" "NFung"   
#>       Sub-plot                                                                
#>  [1,] "Beans5 Beans1 Beans2 Beans3 Beans10 Beans6 Beans7 Beans9 Beans4 Beans8"
#>  [2,] "Beans9 Beans10 Beans8 Beans5 Beans7 Beans4 Beans2 Beans6 Beans3 Beans1"
#>  [3,] "Beans7 Beans10 Beans6 Beans2 Beans8 Beans3 Beans1 Beans4 Beans9 Beans5"
#>  [4,] "Beans7 Beans9 Beans8 Beans2 Beans3 Beans1 Beans5 Beans4 Beans6 Beans10"
#>  [5,] "Beans9 Beans10 Beans4 Beans3 Beans6 Beans7 Beans1 Beans2 Beans8 Beans5"
#>  [6,] "Beans4 Beans3 Beans9 Beans10 Beans1 Beans8 Beans5 Beans7 Beans6 Beans2"
#>  [7,] "Beans8 Beans7 Beans1 Beans5 Beans2 Beans10 Beans9 Beans6 Beans4 Beans3"
#>  [8,] "Beans8 Beans4 Beans1 Beans2 Beans9 Beans6 Beans3 Beans5 Beans10 Beans7"
#>  [9,] "Beans1 Beans7 Beans5 Beans4 Beans6 Beans9 Beans2 Beans8 Beans10 Beans3"
#> [10,] "Beans6 Beans1 Beans4 Beans2 Beans7 Beans10 Beans3 Beans8 Beans9 Beans5"
#> [11,] "Beans5 Beans3 Beans6 Beans9 Beans4 Beans1 Beans10 Beans7 Beans2 Beans8"
#> [12,] "Beans3 Beans7 Beans4 Beans2 Beans8 Beans6 Beans5 Beans9 Beans10 Beans1"
#> [13,] "Beans1 Beans6 Beans7 Beans9 Beans3 Beans2 Beans4 Beans5 Beans8 Beans10"
#> [14,] "Beans8 Beans4 Beans2 Beans6 Beans10 Beans3 Beans5 Beans1 Beans9 Beans7"
#> [15,] "Beans3 Beans8 Beans5 Beans1 Beans7 Beans10 Beans6 Beans2 Beans9 Beans4"
#> [16,] "Beans4 Beans7 Beans5 Beans8 Beans9 Beans2 Beans10 Beans6 Beans1 Beans3"
#> [17,] "Beans8 Beans9 Beans2 Beans1 Beans7 Beans6 Beans5 Beans10 Beans4 Beans3"
#> [18,] "Beans9 Beans8 Beans2 Beans5 Beans1 Beans6 Beans10 Beans7 Beans4 Beans3"
#> [19,] "Beans10 Beans3 Beans6 Beans1 Beans5 Beans8 Beans7 Beans2 Beans4 Beans9"
#> [20,] "Beans6 Beans8 Beans10 Beans1 Beans7 Beans3 Beans5 Beans4 Beans2 Beans9"
#> [21,] "Beans8 Beans7 Beans9 Beans6 Beans1 Beans5 Beans2 Beans3 Beans10 Beans4"
#> [22,] "Beans3 Beans9 Beans8 Beans4 Beans1 Beans7 Beans10 Beans6 Beans2 Beans5"
#> [23,] "Beans2 Beans5 Beans10 Beans1 Beans7 Beans6 Beans9 Beans4 Beans8 Beans3"
#> [24,] "Beans8 Beans5 Beans7 Beans1 Beans9 Beans6 Beans2 Beans4 Beans3 Beans10"
#> [25,] "Beans7 Beans8 Beans10 Beans4 Beans1 Beans9 Beans3 Beans2 Beans5 Beans6"
#> [26,] "Beans3 Beans8 Beans4 Beans9 Beans2 Beans6 Beans1 Beans7 Beans10 Beans5"
#> [27,] "Beans4 Beans10 Beans1 Beans8 Beans3 Beans9 Beans7 Beans5 Beans6 Beans2"
#> [28,] "Beans5 Beans3 Beans6 Beans4 Beans2 Beans10 Beans8 Beans1 Beans9 Beans7"
#> [29,] "Beans3 Beans7 Beans2 Beans5 Beans1 Beans9 Beans4 Beans10 Beans8 Beans6"
#> [30,] "Beans8 Beans6 Beans2 Beans5 Beans9 Beans10 Beans1 Beans3 Beans4 Beans7"
#> 
#> [[2]]
#>       PLOT   REP Whole-plot
#>  [1,] "1001" "1" "Fung1"   
#>  [2,] "1002" "1" "Fung3"   
#>  [3,] "1003" "1" "NFung"   
#>  [4,] "1004" "1" "Fung2"   
#>  [5,] "1005" "1" "Fung4"   
#>  [6,] "1101" "2" "Fung3"   
#>  [7,] "1102" "2" "Fung2"   
#>  [8,] "1103" "2" "Fung4"   
#>  [9,] "1104" "2" "Fung1"   
#> [10,] "1105" "2" "NFung"   
#> [11,] "1201" "3" "NFung"   
#> [12,] "1202" "3" "Fung2"   
#> [13,] "1203" "3" "Fung1"   
#> [14,] "1204" "3" "Fung4"   
#> [15,] "1205" "3" "Fung3"   
#> [16,] "1301" "4" "Fung3"   
#> [17,] "1302" "4" "NFung"   
#> [18,] "1303" "4" "Fung2"   
#> [19,] "1304" "4" "Fung4"   
#> [20,] "1305" "4" "Fung1"   
#> [21,] "1401" "5" "Fung2"   
#> [22,] "1402" "5" "NFung"   
#> [23,] "1403" "5" "Fung1"   
#> [24,] "1404" "5" "Fung4"   
#> [25,] "1405" "5" "Fung3"   
#> [26,] "1501" "6" "Fung2"   
#> [27,] "1502" "6" "Fung1"   
#> [28,] "1503" "6" "NFung"   
#> [29,] "1504" "6" "Fung4"   
#> [30,] "1505" "6" "Fung3"   
#>       Sub-plot                                                                
#>  [1,] "Beans3 Beans6 Beans8 Beans9 Beans4 Beans5 Beans7 Beans2 Beans1 Beans10"
#>  [2,] "Beans2 Beans4 Beans9 Beans10 Beans8 Beans3 Beans5 Beans6 Beans1 Beans7"
#>  [3,] "Beans3 Beans7 Beans1 Beans6 Beans5 Beans2 Beans4 Beans10 Beans8 Beans9"
#>  [4,] "Beans3 Beans5 Beans7 Beans6 Beans4 Beans10 Beans2 Beans9 Beans8 Beans1"
#>  [5,] "Beans4 Beans9 Beans8 Beans3 Beans6 Beans7 Beans5 Beans1 Beans2 Beans10"
#>  [6,] "Beans6 Beans3 Beans5 Beans2 Beans7 Beans10 Beans9 Beans1 Beans8 Beans4"
#>  [7,] "Beans8 Beans5 Beans6 Beans7 Beans10 Beans2 Beans3 Beans9 Beans4 Beans1"
#>  [8,] "Beans3 Beans1 Beans10 Beans4 Beans7 Beans9 Beans5 Beans2 Beans8 Beans6"
#>  [9,] "Beans7 Beans3 Beans9 Beans10 Beans1 Beans5 Beans6 Beans4 Beans8 Beans2"
#> [10,] "Beans10 Beans1 Beans5 Beans9 Beans6 Beans3 Beans8 Beans7 Beans4 Beans2"
#> [11,] "Beans6 Beans7 Beans8 Beans3 Beans5 Beans4 Beans2 Beans1 Beans9 Beans10"
#> [12,] "Beans3 Beans9 Beans8 Beans5 Beans2 Beans1 Beans4 Beans6 Beans10 Beans7"
#> [13,] "Beans2 Beans5 Beans9 Beans1 Beans8 Beans3 Beans4 Beans6 Beans10 Beans7"
#> [14,] "Beans10 Beans7 Beans9 Beans8 Beans5 Beans1 Beans4 Beans3 Beans2 Beans6"
#> [15,] "Beans1 Beans8 Beans2 Beans3 Beans7 Beans6 Beans5 Beans10 Beans4 Beans9"
#> [16,] "Beans1 Beans4 Beans3 Beans9 Beans10 Beans5 Beans6 Beans7 Beans2 Beans8"
#> [17,] "Beans1 Beans8 Beans6 Beans9 Beans7 Beans2 Beans3 Beans5 Beans10 Beans4"
#> [18,] "Beans2 Beans3 Beans1 Beans8 Beans7 Beans6 Beans4 Beans9 Beans5 Beans10"
#> [19,] "Beans9 Beans1 Beans10 Beans8 Beans7 Beans3 Beans5 Beans6 Beans4 Beans2"
#> [20,] "Beans2 Beans1 Beans3 Beans7 Beans4 Beans10 Beans8 Beans6 Beans9 Beans5"
#> [21,] "Beans10 Beans9 Beans6 Beans7 Beans4 Beans3 Beans5 Beans8 Beans1 Beans2"
#> [22,] "Beans3 Beans10 Beans4 Beans7 Beans1 Beans8 Beans2 Beans9 Beans5 Beans6"
#> [23,] "Beans8 Beans7 Beans2 Beans3 Beans10 Beans6 Beans5 Beans4 Beans1 Beans9"
#> [24,] "Beans3 Beans10 Beans5 Beans8 Beans9 Beans4 Beans2 Beans1 Beans7 Beans6"
#> [25,] "Beans2 Beans8 Beans4 Beans1 Beans5 Beans6 Beans7 Beans10 Beans3 Beans9"
#> [26,] "Beans10 Beans1 Beans6 Beans2 Beans9 Beans8 Beans3 Beans5 Beans7 Beans4"
#> [27,] "Beans4 Beans8 Beans7 Beans5 Beans10 Beans9 Beans2 Beans3 Beans1 Beans6"
#> [28,] "Beans7 Beans9 Beans6 Beans5 Beans1 Beans8 Beans3 Beans10 Beans4 Beans2"
#> [29,] "Beans3 Beans9 Beans8 Beans1 Beans7 Beans10 Beans6 Beans2 Beans4 Beans5"
#> [30,] "Beans3 Beans2 Beans1 Beans8 Beans9 Beans6 Beans4 Beans7 Beans10 Beans5"
#> 
#> [[3]]
#>       PLOT   REP Whole-plot
#>  [1,] "2001" "1" "NFung"   
#>  [2,] "2002" "1" "Fung2"   
#>  [3,] "2003" "1" "Fung1"   
#>  [4,] "2004" "1" "Fung4"   
#>  [5,] "2005" "1" "Fung3"   
#>  [6,] "2101" "2" "Fung1"   
#>  [7,] "2102" "2" "Fung2"   
#>  [8,] "2103" "2" "Fung4"   
#>  [9,] "2104" "2" "NFung"   
#> [10,] "2105" "2" "Fung3"   
#> [11,] "2201" "3" "Fung3"   
#> [12,] "2202" "3" "NFung"   
#> [13,] "2203" "3" "Fung4"   
#> [14,] "2204" "3" "Fung2"   
#> [15,] "2205" "3" "Fung1"   
#> [16,] "2301" "4" "Fung3"   
#> [17,] "2302" "4" "Fung4"   
#> [18,] "2303" "4" "Fung1"   
#> [19,] "2304" "4" "Fung2"   
#> [20,] "2305" "4" "NFung"   
#> [21,] "2401" "5" "Fung3"   
#> [22,] "2402" "5" "Fung2"   
#> [23,] "2403" "5" "Fung1"   
#> [24,] "2404" "5" "Fung4"   
#> [25,] "2405" "5" "NFung"   
#> [26,] "2501" "6" "Fung1"   
#> [27,] "2502" "6" "Fung3"   
#> [28,] "2503" "6" "Fung2"   
#> [29,] "2504" "6" "Fung4"   
#> [30,] "2505" "6" "NFung"   
#>       Sub-plot                                                                
#>  [1,] "Beans3 Beans2 Beans6 Beans4 Beans5 Beans10 Beans7 Beans1 Beans8 Beans9"
#>  [2,] "Beans5 Beans4 Beans9 Beans1 Beans6 Beans2 Beans10 Beans7 Beans8 Beans3"
#>  [3,] "Beans4 Beans7 Beans6 Beans1 Beans2 Beans10 Beans9 Beans3 Beans8 Beans5"
#>  [4,] "Beans9 Beans8 Beans3 Beans6 Beans1 Beans7 Beans10 Beans5 Beans2 Beans4"
#>  [5,] "Beans7 Beans10 Beans4 Beans8 Beans2 Beans5 Beans9 Beans1 Beans3 Beans6"
#>  [6,] "Beans8 Beans1 Beans3 Beans10 Beans6 Beans4 Beans9 Beans5 Beans7 Beans2"
#>  [7,] "Beans3 Beans6 Beans1 Beans5 Beans7 Beans10 Beans9 Beans4 Beans8 Beans2"
#>  [8,] "Beans4 Beans2 Beans7 Beans1 Beans10 Beans9 Beans6 Beans5 Beans3 Beans8"
#>  [9,] "Beans2 Beans7 Beans9 Beans3 Beans4 Beans1 Beans5 Beans6 Beans8 Beans10"
#> [10,] "Beans9 Beans2 Beans4 Beans5 Beans6 Beans3 Beans1 Beans8 Beans10 Beans7"
#> [11,] "Beans6 Beans10 Beans1 Beans7 Beans2 Beans9 Beans4 Beans5 Beans8 Beans3"
#> [12,] "Beans5 Beans3 Beans10 Beans9 Beans4 Beans2 Beans7 Beans6 Beans1 Beans8"
#> [13,] "Beans1 Beans2 Beans7 Beans8 Beans5 Beans3 Beans10 Beans6 Beans9 Beans4"
#> [14,] "Beans5 Beans8 Beans1 Beans3 Beans10 Beans6 Beans2 Beans9 Beans4 Beans7"
#> [15,] "Beans5 Beans8 Beans3 Beans4 Beans9 Beans1 Beans10 Beans7 Beans2 Beans6"
#> [16,] "Beans9 Beans5 Beans8 Beans1 Beans3 Beans7 Beans2 Beans6 Beans10 Beans4"
#> [17,] "Beans2 Beans5 Beans3 Beans9 Beans6 Beans7 Beans1 Beans4 Beans8 Beans10"
#> [18,] "Beans3 Beans10 Beans1 Beans8 Beans2 Beans6 Beans9 Beans4 Beans5 Beans7"
#> [19,] "Beans9 Beans10 Beans1 Beans7 Beans3 Beans6 Beans5 Beans4 Beans2 Beans8"
#> [20,] "Beans9 Beans1 Beans4 Beans3 Beans6 Beans2 Beans5 Beans10 Beans8 Beans7"
#> [21,] "Beans1 Beans9 Beans4 Beans8 Beans2 Beans5 Beans7 Beans6 Beans3 Beans10"
#> [22,] "Beans4 Beans5 Beans10 Beans7 Beans6 Beans1 Beans2 Beans9 Beans8 Beans3"
#> [23,] "Beans6 Beans3 Beans10 Beans8 Beans1 Beans9 Beans2 Beans4 Beans5 Beans7"
#> [24,] "Beans6 Beans7 Beans8 Beans4 Beans3 Beans1 Beans9 Beans2 Beans5 Beans10"
#> [25,] "Beans3 Beans9 Beans8 Beans7 Beans1 Beans5 Beans10 Beans2 Beans4 Beans6"
#> [26,] "Beans8 Beans5 Beans7 Beans10 Beans1 Beans3 Beans2 Beans6 Beans4 Beans9"
#> [27,] "Beans9 Beans4 Beans10 Beans7 Beans1 Beans6 Beans8 Beans5 Beans3 Beans2"
#> [28,] "Beans7 Beans8 Beans1 Beans10 Beans3 Beans4 Beans2 Beans6 Beans9 Beans5"
#> [29,] "Beans3 Beans5 Beans4 Beans2 Beans7 Beans10 Beans9 Beans8 Beans1 Beans6"
#> [30,] "Beans3 Beans6 Beans2 Beans9 Beans1 Beans4 Beans8 Beans7 Beans5 Beans10"
#> 
head(SPDExample2$fieldBook,12)
#>    ID LOCATION PLOT REP WHOLE_PLOT SUB_PLOT      TRT_COMB
#> 1   1        A  101   1      Fung4   Beans5  Fung4|Beans5
#> 2   2        A  101   1      Fung4   Beans1  Fung4|Beans1
#> 3   3        A  101   1      Fung4   Beans2  Fung4|Beans2
#> 4   4        A  101   1      Fung4   Beans3  Fung4|Beans3
#> 5   5        A  101   1      Fung4  Beans10 Fung4|Beans10
#> 6   6        A  101   1      Fung4   Beans6  Fung4|Beans6
#> 7   7        A  101   1      Fung4   Beans7  Fung4|Beans7
#> 8   8        A  101   1      Fung4   Beans9  Fung4|Beans9
#> 9   9        A  101   1      Fung4   Beans4  Fung4|Beans4
#> 10 10        A  101   1      Fung4   Beans8  Fung4|Beans8
#> 11 11        A  102   1      Fung3   Beans9  Fung3|Beans9
#> 12 12        A  102   1      Fung3  Beans10 Fung3|Beans10
             
                  
```
