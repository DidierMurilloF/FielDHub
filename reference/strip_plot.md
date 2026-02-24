# Strip Plot Design

It randomly generates a strip plot design across locations.

## Usage

``` r
strip_plot(
  Hplots = NULL,
  Vplots = NULL,
  b = 1,
  l = 1,
  plotNumber = NULL,
  planter = "serpentine",
  locationNames = NULL,
  seed = NULL,
  factorLabels = TRUE,
  randomizeH = TRUE,
  randomizeV = FALSE,
  data = NULL
)
```

## Arguments

- Hplots:

  Number of horizontal factors, as an integer or a vector.

- Vplots:

  Number of vertical factors, as an integer or a vector.

- b:

  Number of blocks (full replicates).

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- planter:

  Option for `serpentine` or `cartesian` arrangement. By default
  `planter = 'serpentine'`.

- locationNames:

  (optional) Names for each location.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- factorLabels:

  (optional) If `TRUE` retain the levels labels from the original data
  set otherwise, numeric labels will be assigned. Default is
  `factorLabels =TRUE`.

- randomizeH:

  Logical flag: `TRUE` to randomize horizontal strips separately in each
  replicate; `FALSE` to randomize once per location and reuse.

- randomizeV:

  Logical flag: `TRUE` to randomize vertical strips separately in each
  replicate; `FALSE` to randomize once per location and reuse.

- data:

  (optional) data frame with the labels of vertical and horizontal
  plots.

## Value

A list with four elements.

- `infoDesign` is a list with information on the design parameters.

- `stripsBlockLoc` is a list with the strip blocks for each location.

- `plotLayouts` is a list with the layout plot numbers for each
  location.

- `fieldBook` is a data frame with the strip plot field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a strip plot design with 5 vertical strips and 4 horizontal strips,
# with 3 reps in one location.
H <- paste("H", 1:4, sep = "")
V <- paste("V", 1:5, sep = "")
strip1 <- strip_plot(
  Hplots = H,
  Vplots = V,
  b = 3,
  l = 1,
  plotNumber = 101,
  planter = "serpentine",
  locationNames = "A",
  seed = 333
)
strip1$infoDesign
#> $Hplots
#> [1] 4
#> 
#> $Vplots
#> [1] 5
#> 
#> $blocks
#> [1] 3
#> 
#> $numberLocations
#> [1] 1
#> 
#> $nameLocations
#> [1] "A"
#> 
#> $seed
#> [1] 333
#> 
#> $id_design
#> [1] 7
#> 
strip1$stripsBlockLoc
#> $Loc_A
#> $Loc_A$rep1
#>    V1      V3      V2      V5      V4     
#> H2 "H2|V1" "H2|V3" "H2|V2" "H2|V5" "H2|V4"
#> H4 "H4|V1" "H4|V3" "H4|V2" "H4|V5" "H4|V4"
#> H1 "H1|V1" "H1|V3" "H1|V2" "H1|V5" "H1|V4"
#> H3 "H3|V1" "H3|V3" "H3|V2" "H3|V5" "H3|V4"
#> 
#> $Loc_A$rep2
#>    V1      V3      V2      V5      V4     
#> H3 "H3|V1" "H3|V3" "H3|V2" "H3|V5" "H3|V4"
#> H4 "H4|V1" "H4|V3" "H4|V2" "H4|V5" "H4|V4"
#> H2 "H2|V1" "H2|V3" "H2|V2" "H2|V5" "H2|V4"
#> H1 "H1|V1" "H1|V3" "H1|V2" "H1|V5" "H1|V4"
#> 
#> $Loc_A$rep3
#>    V1      V3      V2      V5      V4     
#> H1 "H1|V1" "H1|V3" "H1|V2" "H1|V5" "H1|V4"
#> H3 "H3|V1" "H3|V3" "H3|V2" "H3|V5" "H3|V4"
#> H4 "H4|V1" "H4|V3" "H4|V2" "H4|V5" "H4|V4"
#> H2 "H2|V1" "H2|V3" "H2|V2" "H2|V5" "H2|V4"
#> 
#> 
strip1$plotLayouts
#> $Loc_A
#> $Loc_A$rep1
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  101  102  103  104  105
#> [2,]  110  109  108  107  106
#> [3,]  111  112  113  114  115
#> [4,]  120  119  118  117  116
#> 
#> $Loc_A$rep2
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  201  202  203  204  205
#> [2,]  210  209  208  207  206
#> [3,]  211  212  213  214  215
#> [4,]  220  219  218  217  216
#> 
#> $Loc_A$rep3
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  301  302  303  304  305
#> [2,]  310  309  308  307  306
#> [3,]  311  312  313  314  315
#> [4,]  320  319  318  317  316
#> 
#> 
head(strip1$fieldBook, 12)
#>    ID LOCATION PLOT REP HSTRIP VSTRIP TRT_COMB
#> 1   1        A  101   1     H2     V1    H2|V1
#> 2   2        A  102   1     H2     V3    H2|V3
#> 3   3        A  103   1     H2     V2    H2|V2
#> 4   4        A  104   1     H2     V5    H2|V5
#> 5   5        A  105   1     H2     V4    H2|V4
#> 6   6        A  110   1     H4     V1    H4|V1
#> 7   7        A  109   1     H4     V3    H4|V3
#> 8   8        A  108   1     H4     V2    H4|V2
#> 9   9        A  107   1     H4     V5    H4|V5
#> 10 10        A  106   1     H4     V4    H4|V4
#> 11 11        A  111   1     H1     V1    H1|V1
#> 12 12        A  112   1     H1     V3    H1|V3

# Example 2: Generates a strip plot design with 5 vertical strips and 5 horizontal strips,
# with 6 reps across to 3 locations. In this case, we show how to use the option data.
Hplots <- LETTERS[1:5]
Vplots <- LETTERS[1:4]
strip_data <- data.frame(list(HPLOTS = Hplots, VPLOTS = c(Vplots, NA)))
head(strip_data)
#>   HPLOTS VPLOTS
#> 1      A      A
#> 2      B      B
#> 3      C      C
#> 4      D      D
#> 5      E   <NA>
strip2 <- strip_plot(
  Hplots = 5,
  Vplots = 5,
  b = 6,
  l = 3,
  plotNumber = c(101, 1001, 2001),
  planter = "cartesian",
  locationNames = c("A", "B", "C"),
  seed = 222,
  data = strip_data
)
strip2$infoDesign
#> $Hplots
#> [1] 5
#> 
#> $Vplots
#> [1] 4
#> 
#> $blocks
#> [1] 6
#> 
#> $numberLocations
#> [1] 3
#> 
#> $nameLocations
#> [1] "A" "B" "C"
#> 
#> $seed
#> [1] 222
#> 
#> $id_design
#> [1] 7
#> 
strip2$stripsBlockLoc
#> $Loc_A
#> $Loc_A$rep1
#>   A     B     D     C    
#> B "B|A" "B|B" "B|D" "B|C"
#> D "D|A" "D|B" "D|D" "D|C"
#> A "A|A" "A|B" "A|D" "A|C"
#> E "E|A" "E|B" "E|D" "E|C"
#> C "C|A" "C|B" "C|D" "C|C"
#> 
#> $Loc_A$rep2
#>   A     B     D     C    
#> B "B|A" "B|B" "B|D" "B|C"
#> A "A|A" "A|B" "A|D" "A|C"
#> E "E|A" "E|B" "E|D" "E|C"
#> C "C|A" "C|B" "C|D" "C|C"
#> D "D|A" "D|B" "D|D" "D|C"
#> 
#> $Loc_A$rep3
#>   A     B     D     C    
#> A "A|A" "A|B" "A|D" "A|C"
#> D "D|A" "D|B" "D|D" "D|C"
#> E "E|A" "E|B" "E|D" "E|C"
#> B "B|A" "B|B" "B|D" "B|C"
#> C "C|A" "C|B" "C|D" "C|C"
#> 
#> $Loc_A$rep4
#>   A     B     D     C    
#> E "E|A" "E|B" "E|D" "E|C"
#> A "A|A" "A|B" "A|D" "A|C"
#> D "D|A" "D|B" "D|D" "D|C"
#> C "C|A" "C|B" "C|D" "C|C"
#> B "B|A" "B|B" "B|D" "B|C"
#> 
#> $Loc_A$rep5
#>   A     B     D     C    
#> A "A|A" "A|B" "A|D" "A|C"
#> C "C|A" "C|B" "C|D" "C|C"
#> E "E|A" "E|B" "E|D" "E|C"
#> B "B|A" "B|B" "B|D" "B|C"
#> D "D|A" "D|B" "D|D" "D|C"
#> 
#> $Loc_A$rep6
#>   A     B     D     C    
#> E "E|A" "E|B" "E|D" "E|C"
#> B "B|A" "B|B" "B|D" "B|C"
#> A "A|A" "A|B" "A|D" "A|C"
#> D "D|A" "D|B" "D|D" "D|C"
#> C "C|A" "C|B" "C|D" "C|C"
#> 
#> 
#> $Loc_B
#> $Loc_B$rep1
#>   C     B     A     D    
#> A "A|C" "A|B" "A|A" "A|D"
#> C "C|C" "C|B" "C|A" "C|D"
#> D "D|C" "D|B" "D|A" "D|D"
#> E "E|C" "E|B" "E|A" "E|D"
#> B "B|C" "B|B" "B|A" "B|D"
#> 
#> $Loc_B$rep2
#>   C     B     A     D    
#> D "D|C" "D|B" "D|A" "D|D"
#> E "E|C" "E|B" "E|A" "E|D"
#> B "B|C" "B|B" "B|A" "B|D"
#> C "C|C" "C|B" "C|A" "C|D"
#> A "A|C" "A|B" "A|A" "A|D"
#> 
#> $Loc_B$rep3
#>   C     B     A     D    
#> B "B|C" "B|B" "B|A" "B|D"
#> D "D|C" "D|B" "D|A" "D|D"
#> C "C|C" "C|B" "C|A" "C|D"
#> E "E|C" "E|B" "E|A" "E|D"
#> A "A|C" "A|B" "A|A" "A|D"
#> 
#> $Loc_B$rep4
#>   C     B     A     D    
#> B "B|C" "B|B" "B|A" "B|D"
#> D "D|C" "D|B" "D|A" "D|D"
#> E "E|C" "E|B" "E|A" "E|D"
#> A "A|C" "A|B" "A|A" "A|D"
#> C "C|C" "C|B" "C|A" "C|D"
#> 
#> $Loc_B$rep5
#>   C     B     A     D    
#> E "E|C" "E|B" "E|A" "E|D"
#> D "D|C" "D|B" "D|A" "D|D"
#> A "A|C" "A|B" "A|A" "A|D"
#> C "C|C" "C|B" "C|A" "C|D"
#> B "B|C" "B|B" "B|A" "B|D"
#> 
#> $Loc_B$rep6
#>   C     B     A     D    
#> D "D|C" "D|B" "D|A" "D|D"
#> E "E|C" "E|B" "E|A" "E|D"
#> C "C|C" "C|B" "C|A" "C|D"
#> A "A|C" "A|B" "A|A" "A|D"
#> B "B|C" "B|B" "B|A" "B|D"
#> 
#> 
#> $Loc_C
#> $Loc_C$rep1
#>   B     D     A     C    
#> A "A|B" "A|D" "A|A" "A|C"
#> D "D|B" "D|D" "D|A" "D|C"
#> E "E|B" "E|D" "E|A" "E|C"
#> C "C|B" "C|D" "C|A" "C|C"
#> B "B|B" "B|D" "B|A" "B|C"
#> 
#> $Loc_C$rep2
#>   B     D     A     C    
#> D "D|B" "D|D" "D|A" "D|C"
#> C "C|B" "C|D" "C|A" "C|C"
#> E "E|B" "E|D" "E|A" "E|C"
#> A "A|B" "A|D" "A|A" "A|C"
#> B "B|B" "B|D" "B|A" "B|C"
#> 
#> $Loc_C$rep3
#>   B     D     A     C    
#> C "C|B" "C|D" "C|A" "C|C"
#> A "A|B" "A|D" "A|A" "A|C"
#> D "D|B" "D|D" "D|A" "D|C"
#> B "B|B" "B|D" "B|A" "B|C"
#> E "E|B" "E|D" "E|A" "E|C"
#> 
#> $Loc_C$rep4
#>   B     D     A     C    
#> E "E|B" "E|D" "E|A" "E|C"
#> D "D|B" "D|D" "D|A" "D|C"
#> B "B|B" "B|D" "B|A" "B|C"
#> A "A|B" "A|D" "A|A" "A|C"
#> C "C|B" "C|D" "C|A" "C|C"
#> 
#> $Loc_C$rep5
#>   B     D     A     C    
#> A "A|B" "A|D" "A|A" "A|C"
#> D "D|B" "D|D" "D|A" "D|C"
#> C "C|B" "C|D" "C|A" "C|C"
#> B "B|B" "B|D" "B|A" "B|C"
#> E "E|B" "E|D" "E|A" "E|C"
#> 
#> $Loc_C$rep6
#>   B     D     A     C    
#> E "E|B" "E|D" "E|A" "E|C"
#> A "A|B" "A|D" "A|A" "A|C"
#> D "D|B" "D|D" "D|A" "D|C"
#> C "C|B" "C|D" "C|A" "C|C"
#> B "B|B" "B|D" "B|A" "B|C"
#> 
#> 
strip2$plotLayouts
#> $Loc_A
#> $Loc_A$rep1
#>      [,1] [,2] [,3] [,4]
#> [1,]  101  102  103  104
#> [2,]  105  106  107  108
#> [3,]  109  110  111  112
#> [4,]  113  114  115  116
#> [5,]  117  118  119  120
#> 
#> $Loc_A$rep2
#>      [,1] [,2] [,3] [,4]
#> [1,]  201  202  203  204
#> [2,]  205  206  207  208
#> [3,]  209  210  211  212
#> [4,]  213  214  215  216
#> [5,]  217  218  219  220
#> 
#> $Loc_A$rep3
#>      [,1] [,2] [,3] [,4]
#> [1,]  301  302  303  304
#> [2,]  305  306  307  308
#> [3,]  309  310  311  312
#> [4,]  313  314  315  316
#> [5,]  317  318  319  320
#> 
#> $Loc_A$rep4
#>      [,1] [,2] [,3] [,4]
#> [1,]  401  402  403  404
#> [2,]  405  406  407  408
#> [3,]  409  410  411  412
#> [4,]  413  414  415  416
#> [5,]  417  418  419  420
#> 
#> $Loc_A$rep5
#>      [,1] [,2] [,3] [,4]
#> [1,]  501  502  503  504
#> [2,]  505  506  507  508
#> [3,]  509  510  511  512
#> [4,]  513  514  515  516
#> [5,]  517  518  519  520
#> 
#> $Loc_A$rep6
#>      [,1] [,2] [,3] [,4]
#> [1,]  601  602  603  604
#> [2,]  605  606  607  608
#> [3,]  609  610  611  612
#> [4,]  613  614  615  616
#> [5,]  617  618  619  620
#> 
#> 
#> $Loc_B
#> $Loc_B$rep1
#>      [,1] [,2] [,3] [,4]
#> [1,] 1001 1002 1003 1004
#> [2,] 1005 1006 1007 1008
#> [3,] 1009 1010 1011 1012
#> [4,] 1013 1014 1015 1016
#> [5,] 1017 1018 1019 1020
#> 
#> $Loc_B$rep2
#>      [,1] [,2] [,3] [,4]
#> [1,] 1101 1102 1103 1104
#> [2,] 1105 1106 1107 1108
#> [3,] 1109 1110 1111 1112
#> [4,] 1113 1114 1115 1116
#> [5,] 1117 1118 1119 1120
#> 
#> $Loc_B$rep3
#>      [,1] [,2] [,3] [,4]
#> [1,] 1201 1202 1203 1204
#> [2,] 1205 1206 1207 1208
#> [3,] 1209 1210 1211 1212
#> [4,] 1213 1214 1215 1216
#> [5,] 1217 1218 1219 1220
#> 
#> $Loc_B$rep4
#>      [,1] [,2] [,3] [,4]
#> [1,] 1301 1302 1303 1304
#> [2,] 1305 1306 1307 1308
#> [3,] 1309 1310 1311 1312
#> [4,] 1313 1314 1315 1316
#> [5,] 1317 1318 1319 1320
#> 
#> $Loc_B$rep5
#>      [,1] [,2] [,3] [,4]
#> [1,] 1401 1402 1403 1404
#> [2,] 1405 1406 1407 1408
#> [3,] 1409 1410 1411 1412
#> [4,] 1413 1414 1415 1416
#> [5,] 1417 1418 1419 1420
#> 
#> $Loc_B$rep6
#>      [,1] [,2] [,3] [,4]
#> [1,] 1501 1502 1503 1504
#> [2,] 1505 1506 1507 1508
#> [3,] 1509 1510 1511 1512
#> [4,] 1513 1514 1515 1516
#> [5,] 1517 1518 1519 1520
#> 
#> 
#> $Loc_C
#> $Loc_C$rep1
#>      [,1] [,2] [,3] [,4]
#> [1,] 2001 2002 2003 2004
#> [2,] 2005 2006 2007 2008
#> [3,] 2009 2010 2011 2012
#> [4,] 2013 2014 2015 2016
#> [5,] 2017 2018 2019 2020
#> 
#> $Loc_C$rep2
#>      [,1] [,2] [,3] [,4]
#> [1,] 2101 2102 2103 2104
#> [2,] 2105 2106 2107 2108
#> [3,] 2109 2110 2111 2112
#> [4,] 2113 2114 2115 2116
#> [5,] 2117 2118 2119 2120
#> 
#> $Loc_C$rep3
#>      [,1] [,2] [,3] [,4]
#> [1,] 2201 2202 2203 2204
#> [2,] 2205 2206 2207 2208
#> [3,] 2209 2210 2211 2212
#> [4,] 2213 2214 2215 2216
#> [5,] 2217 2218 2219 2220
#> 
#> $Loc_C$rep4
#>      [,1] [,2] [,3] [,4]
#> [1,] 2301 2302 2303 2304
#> [2,] 2305 2306 2307 2308
#> [3,] 2309 2310 2311 2312
#> [4,] 2313 2314 2315 2316
#> [5,] 2317 2318 2319 2320
#> 
#> $Loc_C$rep5
#>      [,1] [,2] [,3] [,4]
#> [1,] 2401 2402 2403 2404
#> [2,] 2405 2406 2407 2408
#> [3,] 2409 2410 2411 2412
#> [4,] 2413 2414 2415 2416
#> [5,] 2417 2418 2419 2420
#> 
#> $Loc_C$rep6
#>      [,1] [,2] [,3] [,4]
#> [1,] 2501 2502 2503 2504
#> [2,] 2505 2506 2507 2508
#> [3,] 2509 2510 2511 2512
#> [4,] 2513 2514 2515 2516
#> [5,] 2517 2518 2519 2520
#> 
#> 
head(strip2$fieldBook, 12)
#>    ID LOCATION PLOT REP HSTRIP VSTRIP TRT_COMB
#> 1   1        A  101   1      B      A      B|A
#> 2   2        A  102   1      B      B      B|B
#> 3   3        A  103   1      B      D      B|D
#> 4   4        A  104   1      B      C      B|C
#> 5   5        A  105   1      D      A      D|A
#> 6   6        A  106   1      D      B      D|B
#> 7   7        A  107   1      D      D      D|D
#> 8   8        A  108   1      D      C      D|C
#> 9   9        A  109   1      A      A      A|A
#> 10 10        A  110   1      A      B      A|B
#> 11 11        A  111   1      A      D      A|D
#> 12 12        A  112   1      A      C      A|C
```
