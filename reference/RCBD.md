# Generates a Randomized Complete Block Design (RCBD)

It randomly generates a randomized complete block design (RCBD) across
locations.

## Usage

``` r
RCBD(
  t = NULL,
  reps = NULL,
  l = 1,
  plotNumber = 101,
  continuous = FALSE,
  planter = "serpentine",
  seed = NULL,
  locationNames = NULL,
  data = NULL
)
```

## Arguments

- t:

  An integer number with total number of treatments or a vector of
  dimension t with labels.

- reps:

  Number of replicates (full blocks) of each treatment.

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- continuous:

  Logical value for plot number continuous or not. By default
  `continuous = FALSE`.

- planter:

  Option for `serpentine` or `cartesian` arrangement. By default
  `planter = 'serpentine'`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- locationNames:

  (optional) Names for each location.

- data:

  (optional) Data frame with the labels of treatments.

## Value

A list with five elements.

- `infoDesign` is a list with information on the design parameters.

- `layoutRandom` is the RCBD layout randomization for each location.

- `plotNumber` is the plot number layout for each location.

- `fieldBook` is a data frame with the RCBD field book design.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a RCBD design with 3 blocks and 20 treatments across 3 locations.
rcbd1 <- RCBD(t = LETTERS[1:20], reps = 5, l = 3, 
              plotNumber = c(101,1001, 2001), 
              continuous = TRUE,
              planter = "serpentine", 
              seed = 1020, 
              locationNames = c("FARGO", "MINOT", "CASSELTON"))
rcbd1$infoDesign                  
#> $blocks
#> [1] 5
#> 
#> $number.of.treatments
#> [1] 20
#> 
#> $treatments
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#> [20] "T"
#> 
#> $locations
#> [1] 3
#> 
#> $plotNumber
#>  [1]  101  201  301  401  501 1001 1101 1201 1301 1401 2001 2101 2201 2301 2401
#> 
#> $locationNames
#> [1] "FARGO"     "MINOT"     "CASSELTON"
#> 
#> $seed
#> [1] 1020
#> 
#> $id_design
#> [1] 2
#> 
rcbd1$layoutRandom
#> $Loc_FARGO
#>      Block --Treatments--                           
#> [1,] "1"   "P R L T E A J O M C K F I Q G D S H N B"
#> [2,] "2"   "Q H G M F D L P E B J N A I K C T R O S"
#> [3,] "3"   "R B G K H E S C F D I T P N Q M A O J L"
#> [4,] "4"   "M I T B N G O J Q C A L P E S R D K H F"
#> [5,] "5"   "M C Q O E H I A P S R L J G F B T D K N"
#> 
#> $Loc_MINOT
#>      Block --Treatments--                           
#> [1,] "1"   "F O C A G D L B I S P T H K M E N R Q J"
#> [2,] "2"   "Q H K A G D E M N O C S J I T L P F B R"
#> [3,] "3"   "B K D L O E A R F S I P G T C Q J N M H"
#> [4,] "4"   "C P L O B K E H Q G N A T R J F S M D I"
#> [5,] "5"   "G S D B H L Q K A P E J T R I C O F M N"
#> 
#> $Loc_CASSELTON
#>      Block --Treatments--                           
#> [1,] "1"   "P G T E L O K H D N S C M I A J Q R B F"
#> [2,] "2"   "C D L F A T I G S O B J M E R P H N Q K"
#> [3,] "3"   "C G K N B A L Q I F D H J M O P S T E R"
#> [4,] "4"   "E L H D F J A T S N B G Q M I O P C K R"
#> [5,] "5"   "T I M A H K E C Q L D J R B G S N O F P"
#> 
rcbd1$plotNumber
#> $Loc_FARGO
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]  101  102  103  104  105  106  107  108  109   110   111   112   113   114
#> [2,]  140  139  138  137  136  135  134  133  132   131   130   129   128   127
#> [3,]  141  142  143  144  145  146  147  148  149   150   151   152   153   154
#> [4,]  180  179  178  177  176  175  174  173  172   171   170   169   168   167
#> [5,]  181  182  183  184  185  186  187  188  189   190   191   192   193   194
#>      [,15] [,16] [,17] [,18] [,19] [,20]
#> [1,]   115   116   117   118   119   120
#> [2,]   126   125   124   123   122   121
#> [3,]   155   156   157   158   159   160
#> [4,]   166   165   164   163   162   161
#> [5,]   195   196   197   198   199   200
#> 
#> $Loc_MINOT
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] 1001 1002 1003 1004 1005 1006 1007 1008 1009  1010  1011  1012  1013  1014
#> [2,] 1040 1039 1038 1037 1036 1035 1034 1033 1032  1031  1030  1029  1028  1027
#> [3,] 1041 1042 1043 1044 1045 1046 1047 1048 1049  1050  1051  1052  1053  1054
#> [4,] 1080 1079 1078 1077 1076 1075 1074 1073 1072  1071  1070  1069  1068  1067
#> [5,] 1081 1082 1083 1084 1085 1086 1087 1088 1089  1090  1091  1092  1093  1094
#>      [,15] [,16] [,17] [,18] [,19] [,20]
#> [1,]  1015  1016  1017  1018  1019  1020
#> [2,]  1026  1025  1024  1023  1022  1021
#> [3,]  1055  1056  1057  1058  1059  1060
#> [4,]  1066  1065  1064  1063  1062  1061
#> [5,]  1095  1096  1097  1098  1099  1100
#> 
#> $Loc_CASSELTON
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] 2001 2002 2003 2004 2005 2006 2007 2008 2009  2010  2011  2012  2013  2014
#> [2,] 2040 2039 2038 2037 2036 2035 2034 2033 2032  2031  2030  2029  2028  2027
#> [3,] 2041 2042 2043 2044 2045 2046 2047 2048 2049  2050  2051  2052  2053  2054
#> [4,] 2080 2079 2078 2077 2076 2075 2074 2073 2072  2071  2070  2069  2068  2067
#> [5,] 2081 2082 2083 2084 2085 2086 2087 2088 2089  2090  2091  2092  2093  2094
#>      [,15] [,16] [,17] [,18] [,19] [,20]
#> [1,]  2015  2016  2017  2018  2019  2020
#> [2,]  2026  2025  2024  2023  2022  2021
#> [3,]  2055  2056  2057  2058  2059  2060
#> [4,]  2066  2065  2064  2063  2062  2061
#> [5,]  2095  2096  2097  2098  2099  2100
#> 
head(rcbd1$fieldBook)
#>   ID LOCATION PLOT REP TREATMENT
#> 1  1    FARGO  101   1         P
#> 2  2    FARGO  102   1         R
#> 3  3    FARGO  103   1         L
#> 4  4    FARGO  104   1         T
#> 5  5    FARGO  105   1         E
#> 6  6    FARGO  106   1         A

# Example 2: Generates a RCBD design with 6 blocks and 18 treatments in one location.
# In this case, we show how to use the option data.
treatments <- paste("ND-", 1:18, sep = "")
treatment_list <- data.frame(list(TREATMENT = treatments))
head(treatment_list)
#>   TREATMENT
#> 1      ND-1
#> 2      ND-2
#> 3      ND-3
#> 4      ND-4
#> 5      ND-5
#> 6      ND-6
rcbd2 <- RCBD(reps = 6, l = 1, 
              plotNumber = 101, 
              continuous = FALSE, 
              planter = "serpentine", 
              seed = 13, 
              locationNames = "IBAGUE",
              data = treatment_list)
rcbd2$infoDesign                  
#> $blocks
#> [1] 6
#> 
#> $number.of.treatments
#> [1] 18
#> 
#> $treatments
#>  [1] "ND-1"  "ND-2"  "ND-3"  "ND-4"  "ND-5"  "ND-6"  "ND-7"  "ND-8"  "ND-9" 
#> [10] "ND-10" "ND-11" "ND-12" "ND-13" "ND-14" "ND-15" "ND-16" "ND-17" "ND-18"
#> 
#> $locations
#> [1] 1
#> 
#> $plotNumber
#> [1] 101 201 301 401 501 601
#> 
#> $locationNames
#> [1] "IBAGUE"
#> 
#> $seed
#> [1] 13
#> 
#> $id_design
#> [1] 2
#> 
rcbd2$layoutRandom
#> $Loc_IBAGUE
#>      Block
#> [1,] "1"  
#> [2,] "2"  
#> [3,] "3"  
#> [4,] "4"  
#> [5,] "5"  
#> [6,] "6"  
#>      --Treatments--                                                                                      
#> [1,] "ND-3 ND-5 ND-10 ND-13 ND-6 ND-14 ND-4 ND-8 ND-18 ND-1 ND-11 ND-2 ND-17 ND-12 ND-9 ND-7 ND-16 ND-15"
#> [2,] "ND-15 ND-17 ND-12 ND-1 ND-11 ND-4 ND-8 ND-7 ND-5 ND-3 ND-14 ND-9 ND-10 ND-13 ND-2 ND-6 ND-18 ND-16"
#> [3,] "ND-17 ND-12 ND-8 ND-14 ND-10 ND-6 ND-7 ND-18 ND-2 ND-1 ND-13 ND-9 ND-11 ND-15 ND-16 ND-3 ND-4 ND-5"
#> [4,] "ND-14 ND-13 ND-16 ND-1 ND-8 ND-9 ND-15 ND-6 ND-7 ND-12 ND-10 ND-18 ND-11 ND-4 ND-3 ND-5 ND-2 ND-17"
#> [5,] "ND-14 ND-11 ND-9 ND-4 ND-1 ND-16 ND-3 ND-8 ND-5 ND-7 ND-10 ND-18 ND-12 ND-6 ND-2 ND-15 ND-13 ND-17"
#> [6,] "ND-3 ND-5 ND-17 ND-9 ND-6 ND-18 ND-1 ND-14 ND-12 ND-8 ND-4 ND-11 ND-15 ND-2 ND-10 ND-16 ND-13 ND-7"
#> 
rcbd2$plotNumber
#> $Loc_IBAGUE
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,]  101  102  103  104  105  106  107  108  109   110   111   112   113   114
#> [2,]  218  217  216  215  214  213  212  211  210   209   208   207   206   205
#> [3,]  301  302  303  304  305  306  307  308  309   310   311   312   313   314
#> [4,]  418  417  416  415  414  413  412  411  410   409   408   407   406   405
#> [5,]  501  502  503  504  505  506  507  508  509   510   511   512   513   514
#> [6,]  618  617  616  615  614  613  612  611  610   609   608   607   606   605
#>      [,15] [,16] [,17] [,18]
#> [1,]   115   116   117   118
#> [2,]   204   203   202   201
#> [3,]   315   316   317   318
#> [4,]   404   403   402   401
#> [5,]   515   516   517   518
#> [6,]   604   603   602   601
#> 
head(rcbd2$fieldBook)
#>   ID LOCATION PLOT REP TREATMENT
#> 1  1   IBAGUE  101   1      ND-3
#> 2  2   IBAGUE  102   1      ND-5
#> 3  3   IBAGUE  103   1     ND-10
#> 4  4   IBAGUE  104   1     ND-13
#> 5  5   IBAGUE  105   1      ND-6
#> 6  6   IBAGUE  106   1     ND-14

```
