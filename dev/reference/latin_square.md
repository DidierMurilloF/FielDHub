# Generates a Latin Square Design

Randomly generates a latin square design of up 10 treatments.

## Usage

``` r
latin_square(
  t = NULL,
  reps = 1,
  plotNumber = 101,
  planter = "serpentine",
  seed = NULL,
  locationNames = NULL,
  data = NULL
)
```

## Arguments

- t:

  Number of treatments.

- reps:

  Number of full resolvable squares. By default `reps = 1`.

- plotNumber:

  Starting plot number. By default `plotNumber = 101`.

- planter:

  Option for `serpentine` or `cartesian` arrangement. By default
  `planter = 'serpentine'`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- locationNames:

  (optional) Name for the location.

- data:

  (optional) Data frame with label list of treatments.

## Value

A list with information on the design parameters.

Data frame with the latin square field book.

A list with two elements.

- `infoDesign` is a list with information on the design parameters.

- `fieldBook` is a data frame with the latin square field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Thiago de Paula
Oliveira\[ctb\] Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a latin square design with 4 treatments and 2 reps.
latinSq1 <- latin_square(t = 4,
                         reps = 2,
                         plotNumber = 101,
                         planter = "cartesian",
                         seed = 1980)
print(latinSq1)
#> Latin Square Design: 
#> 
#> Information on the design parameters: 
#> List of 4
#>  $ treatments  : int 4
#>  $ squares     : num 2
#>  $ locationName: NULL
#>  $ seed        : num 1980
#> 
#>  10 First observations of the data frame with the latin_square field book: 
#>    ID LOCATION PLOT SQUARE   ROW   COLUMN TREATMENT
#> 1   1        1  101      1 Row 1 Column 1        T1
#> 2   2        1  102      1 Row 1 Column 2        T4
#> 3   3        1  103      1 Row 1 Column 3        T2
#> 4   4        1  104      1 Row 1 Column 4        T3
#> 5   5        1  105      1 Row 2 Column 1        T3
#> 6   6        1  106      1 Row 2 Column 2        T1
#> 7   7        1  107      1 Row 2 Column 3        T4
#> 8   8        1  108      1 Row 2 Column 4        T2
#> 9   9        1  109      1 Row 3 Column 1        T4
#> 10 10        1  110      1 Row 3 Column 2        T2
summary(latinSq1)
#> Latin Square Design: 
#> 
#> 1. Information on the design parameters: 
#> List of 5
#>  $ treatments  : int 4
#>  $ squares     : num 2
#>  $ locationName: NULL
#>  $ seed        : num 1980
#>  $ id_design   : num 3
#> 
#> 2. Squares: 
#> $rep1
#>       Column 1 Column 2 Column 3 Column 4
#> Row 1 "T1"     "T4"     "T2"     "T3"    
#> Row 2 "T3"     "T1"     "T4"     "T2"    
#> Row 3 "T4"     "T2"     "T3"     "T1"    
#> Row 4 "T2"     "T3"     "T1"     "T4"    
#> 
#> $rep2
#>       Column 1 Column 2 Column 3 Column 4
#> Row 1 "T1"     "T3"     "T4"     "T2"    
#> Row 2 "T2"     "T4"     "T3"     "T1"    
#> Row 3 "T4"     "T1"     "T2"     "T3"    
#> Row 4 "T3"     "T2"     "T1"     "T4"    
#> 
#> 
#> 3. Plot squares: 
#> $rep1
#>      [,1] [,2] [,3] [,4]
#> [1,]  101  102  103  104
#> [2,]  105  106  107  108
#> [3,]  109  110  111  112
#> [4,]  113  114  115  116
#> 
#> $rep2
#>      [,1] [,2] [,3] [,4]
#> [1,]  201  202  203  204
#> [2,]  205  206  207  208
#> [3,]  209  210  211  212
#> [4,]  213  214  215  216
#> 
#> 
#> 4. Structure of the data frame with the latin_square field book: 
#> 
#> 'data.frame':    32 obs. of  7 variables:
#>  $ ID       : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ LOCATION : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ PLOT     : int  101 102 103 104 105 106 107 108 109 110 ...
#>  $ SQUARE   : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ ROW      : Factor w/ 4 levels "Row 1","Row 2",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ COLUMN   : Factor w/ 4 levels "Column 1","Column 2",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ TREATMENT: chr  "T1" "T4" "T2" "T3" ...
head(latinSq1$fieldBook)
#>   ID LOCATION PLOT SQUARE   ROW   COLUMN TREATMENT
#> 1  1        1  101      1 Row 1 Column 1        T1
#> 2  2        1  102      1 Row 1 Column 2        T4
#> 3  3        1  103      1 Row 1 Column 3        T2
#> 4  4        1  104      1 Row 1 Column 4        T3
#> 5  5        1  105      1 Row 2 Column 1        T3
#> 6  6        1  106      1 Row 2 Column 2        T1

# Example 2: Generates a latin square design with 5 treatments and 3 reps.
latin_data <- data.frame(list(ROW = paste("Period", 1:5, sep = ""),
                              COLUMN = paste("Cow", 1:5, sep = ""),
                              TREATMENT = paste("Diet", 1:5, sep = "")))
print(latin_data)
#>       ROW COLUMN TREATMENT
#> 1 Period1   Cow1     Diet1
#> 2 Period2   Cow2     Diet2
#> 3 Period3   Cow3     Diet3
#> 4 Period4   Cow4     Diet4
#> 5 Period5   Cow5     Diet5
latinSq2 <- latin_square(t = NULL,
                         reps = 3,
                         plotNumber = 101,
                         planter = "cartesian",
                         seed = 1981,
                         data = latin_data)
latinSq2$squares
#> $rep1
#>         Cow1    Cow2    Cow3    Cow4    Cow5   
#> Period1 "Diet4" "Diet3" "Diet2" "Diet1" "Diet5"
#> Period2 "Diet1" "Diet2" "Diet4" "Diet5" "Diet3"
#> Period3 "Diet3" "Diet5" "Diet1" "Diet2" "Diet4"
#> Period4 "Diet2" "Diet4" "Diet5" "Diet3" "Diet1"
#> Period5 "Diet5" "Diet1" "Diet3" "Diet4" "Diet2"
#> 
#> $rep2
#>         Cow1    Cow2    Cow3    Cow4    Cow5   
#> Period1 "Diet2" "Diet4" "Diet5" "Diet3" "Diet1"
#> Period2 "Diet1" "Diet2" "Diet3" "Diet4" "Diet5"
#> Period3 "Diet3" "Diet5" "Diet1" "Diet2" "Diet4"
#> Period4 "Diet4" "Diet1" "Diet2" "Diet5" "Diet3"
#> Period5 "Diet5" "Diet3" "Diet4" "Diet1" "Diet2"
#> 
#> $rep3
#>         Cow1    Cow2    Cow3    Cow4    Cow5   
#> Period1 "Diet4" "Diet2" "Diet1" "Diet3" "Diet5"
#> Period2 "Diet1" "Diet3" "Diet2" "Diet5" "Diet4"
#> Period3 "Diet3" "Diet5" "Diet4" "Diet2" "Diet1"
#> Period4 "Diet5" "Diet1" "Diet3" "Diet4" "Diet2"
#> Period5 "Diet2" "Diet4" "Diet5" "Diet1" "Diet3"
#> 
latinSq2$plotSquares
#> $rep1
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  101  102  103  104  105
#> [2,]  106  107  108  109  110
#> [3,]  111  112  113  114  115
#> [4,]  116  117  118  119  120
#> [5,]  121  122  123  124  125
#> 
#> $rep2
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  201  202  203  204  205
#> [2,]  206  207  208  209  210
#> [3,]  211  212  213  214  215
#> [4,]  216  217  218  219  220
#> [5,]  221  222  223  224  225
#> 
#> $rep3
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  301  302  303  304  305
#> [2,]  306  307  308  309  310
#> [3,]  311  312  313  314  315
#> [4,]  316  317  318  319  320
#> [5,]  321  322  323  324  325
#> 
head(latinSq2$fieldBook)
#>   ID LOCATION PLOT SQUARE     ROW COLUMN TREATMENT
#> 1  1        1  101      1 Period1   Cow1     Diet4
#> 2  2        1  102      1 Period1   Cow2     Diet3
#> 3  3        1  103      1 Period1   Cow3     Diet2
#> 4  4        1  104      1 Period1   Cow4     Diet1
#> 5  5        1  105      1 Period1   Cow5     Diet5
#> 6  6        1  106      1 Period2   Cow1     Diet1
```
