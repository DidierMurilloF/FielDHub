# Generates an Augmented Randomized Complete Block Design (ARCBD)

It randomly generates an augmented randomized complete block design
across locations (ARCBD).

## Usage

``` r
RCBD_augmented(
  lines = NULL,
  checks = NULL,
  b = NULL,
  l = 1,
  planter = "serpentine",
  plotNumber = 101,
  repsStack = c("vertical", "horizontal"),
  exptName = NULL,
  seed = NULL,
  locationNames = NULL,
  repsExpt = 1,
  random = TRUE,
  data = NULL,
  nrows = NULL,
  ncols = NULL
)
```

## Arguments

- lines:

  Treatments, number of lines for test.

- checks:

  Number of checks per augmented block.

- b:

  Number of augmented blocks.

- l:

  Number of locations. By default `l = 1`.

- planter:

  Option for `serpentine` or `cartesian` arrangement. By default
  `planter = 'serpentine'`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- repsStack:

  Option for `horizontal` or `vertical` layout By default
  `repsStack = 'vertical'`.

- exptName:

  (optional) Name of experiment.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- locationNames:

  (optional) Name for each location.

- repsExpt:

  (optional) Number of reps of experiment. By default `repsExpt = 1`.

- random:

  Logical value to randomize treatments or not. By default
  `random = TRUE`.

- data:

  (optional) Data frame with the labels of treatments.

- nrows:

  (optional) Number of rows in the field.

- ncols:

  (optional) Number of columns in the field.

## Value

A list with five elements.

- `infoDesign` is a list with information on the design parameters.

- `layoutRandom` is the ARCBD layout randomization for the first
  location.

- `plotNumber` is the plot number layout for the first location.

- `exptNames` is the experiment names layout.

- `data_entry` is a data frame with the data input.

- `fieldBook` is a data frame with the ARCBD field book.

## References

Federer, W. T. (1955). Experimental Design. Theory and Application. New
York, USA. The Macmillan Company.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates an ARCBD with 6 blocks, 3 checks for each, and 50 treatments 
# in two locations.
ARCBD1 <- RCBD_augmented(lines = 50, checks = 3, b = 6, l = 2, 
                         planter = "cartesian", 
                         plotNumber = c(1,1001),
                         seed = 23, 
                         locationNames = c("FARGO", "MINOT"))
ARCBD1$infoDesign
#> $rows
#> [1] 6
#> 
#> $columns
#> [1] 12
#> 
#> $rows_within_blocks
#> [1] 1
#> 
#> $columns_within_blocks
#> [1] 12
#> 
#> $treatments
#> [1] 50
#> 
#> $checks
#> [1] 3
#> 
#> $blocks
#> [1] 6
#> 
#> $plots_per_block
#> [1] 12 12 12 12 12  8
#> 
#> $locations
#> [1] 2
#> 
#> $fillers
#> [1] 4
#> 
#> $seed
#> [1] 23
#> 
#> $id_design
#> [1] 14
#> 
ARCBD1$layoutRandom
#>      Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12
#> Row6    2   15   38    3   21   36   26    1    0     0     0     0
#> Row5    3    1   24   46   11    2   48   37   32    31    20    42
#> Row4   34   25   16   41    9   50    2   43   39     1    13     3
#> Row3   18   28    5    2   40    8   30   17   53    10     3     1
#> Row2    7   29   12    2    3   33   22   23    4    47    19     1
#> Row1   49   14   27    3    2   45    6   35   52    44    51     1
ARCBD1$exptNames
#>      V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11   V12
#> 1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 2 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 3 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 4 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 5 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 6 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
ARCBD1$plotNumber
#>      V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12
#> [1,] 61 62 63 64 65 66 67 68  0   0   0   0
#> [2,] 49 50 51 52 53 54 55 56 57  58  59  60
#> [3,] 37 38 39 40 41 42 43 44 45  46  47  48
#> [4,] 25 26 27 28 29 30 31 32 33  34  35  36
#> [5,] 13 14 15 16 17 18 19 20 21  22  23  24
#> [6,]  1  2  3  4  5  6  7  8  9  10  11  12
head(ARCBD1$fieldBook, 12)
#>    ID  EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS BLOCK ENTRY TREATMENT
#> 1   1 Expt1    FARGO 2026    1   1      1      0     1    49       G49
#> 2   2 Expt1    FARGO 2026    2   1      2      0     1    14       G14
#> 3   3 Expt1    FARGO 2026    3   1      3      0     1    27       G27
#> 4   4 Expt1    FARGO 2026    4   1      4      1     1     3       CH3
#> 5   5 Expt1    FARGO 2026    5   1      5      1     1     2       CH2
#> 6   6 Expt1    FARGO 2026    6   1      6      0     1    45       G45
#> 7   7 Expt1    FARGO 2026    7   1      7      0     1     6        G6
#> 8   8 Expt1    FARGO 2026    8   1      8      0     1    35       G35
#> 9   9 Expt1    FARGO 2026    9   1      9      0     1    52       G52
#> 10 10 Expt1    FARGO 2026   10   1     10      0     1    44       G44
#> 11 11 Expt1    FARGO 2026   11   1     11      0     1    51       G51
#> 12 12 Expt1    FARGO 2026   12   1     12      1     1     1       CH1
                   
# Example 2: Generates an ARCBD with 17 blocks, 4 checks for each, and 350 treatments 
# in 3 locations.
# In this case, we show how to use the option data.
checks <- 4;
list_checks <- paste("CH", 1:checks, sep = "")
treatments <- paste("G", 5:354, sep = "")
treatment_list <- data.frame(list(ENTRY = 1:354, NAME = c(list_checks, treatments)))
head(treatment_list, 12)
#>    ENTRY NAME
#> 1      1  CH1
#> 2      2  CH2
#> 3      3  CH3
#> 4      4  CH4
#> 5      5   G5
#> 6      6   G6
#> 7      7   G7
#> 8      8   G8
#> 9      9   G9
#> 10    10  G10
#> 11    11  G11
#> 12    12  G12
ARCBD2 <- RCBD_augmented(lines = 350, checks = 4, b = 17, l = 3, 
                         planter = "serpentine", 
                         plotNumber = c(101,1001,2001), 
                         seed = 24, 
                         locationNames = LETTERS[1:3],
                         data = treatment_list)
ARCBD2$infoDesign
#> $rows
#> [1] 17
#> 
#> $columns
#> [1] 25
#> 
#> $rows_within_blocks
#> [1] 1
#> 
#> $columns_within_blocks
#> [1] 25
#> 
#> $treatments
#> [1] 350
#> 
#> $checks
#> [1] 4
#> 
#> $blocks
#> [1] 17
#> 
#> $plots_per_block
#>  [1] 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 18
#> 
#> $locations
#> [1] 3
#> 
#> $fillers
#> [1] 7
#> 
#> $seed
#> [1] 24
#> 
#> $id_design
#> [1] 14
#> 
ARCBD2$layoutRandom
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row17  257  259    1  198  331   66    3  238  170   176   126   207   225
#> Row16   17   12    1  314   22  235   77  340  188    76   101     2    16
#> Row15  229  231   54    3  305    4  128   50   30    55     1   337    24
#> Row14   63   45   62   40  140  322   82  228  283   142    53   211     7
#> Row13  253    2   68  113   13  279   47   57    4   132     3   167   159
#> Row12  282  205  192  324  315    2  247  124  179    58   105   273    31
#> Row11  110  125   85  332  250  248  265  255    2   251    52    42   236
#> Row10  173  154  338  327   78    3   96  177  193     4   244   191   348
#> Row9     4    2   25  103   36  155  260  246  189    49   197   284   242
#> Row8   107  321  186    4  163   33   71  109  100   174   309    18   135
#> Row7     2  239  252  213  261  150    3  266  277   307     4    95   311
#> Row6   133   75  153  102  274    2    4    1  270   285     3   240   276
#> Row5   234   56  349  288  202  300   79   87  157    64   168     1     4
#> Row4   160  195    2  289  161   83  143  271  141   144    94   320     3
#> Row3   268  209    4  185  308  115   81  342  249   258   120     1     2
#> Row2   172  347  346  215  298   86    1  116  328   224   139     3     4
#> Row1   345  130    4  162    1  123    2   39    9   302   210   352   138
#>       Col14 Col15 Col16 Col17 Col18 Col19 Col20 Col21 Col22 Col23 Col24 Col25
#> Row17   122     4   208   187     2     0     0     0     0     0     0     0
#> Row16     3   219   111   291   316     4   341   169   222   237    65   281
#> Row15   329   263     2    74   108   318   350   147   306   325    37    43
#> Row14   136   310     5     2   199     1     4   164    46     3   158   223
#> Row13    23     1   148   117   201    28    11   119   190    73    72    99
#> Row12    32     1    27   243   241    21     3   303     4   106   127   254
#> Row11    35     1   216    61     3     4   230    69   245   339    98    14
#> Row10     1   227   323     2   203   118   181    88   104    10   272   175
#> Row9    335   217   319   200     3   152    97   267    44   275    92     1
#> Row8    221   333   121     2     1     3   214   226   183    15   194   351
#> Row7    313    60   293    38    59    67   232   134     1   178    93   114
#> Row6    156    41   165   146    51   317   292   280   343   171   334    84
#> Row5    220    34   131   262     3   180   129   145     2   212    91   278
#> Row4     19   353   301     6     4   206   304     1   233   354   166    20
#> Row3    294    89   269    29    26   286   290   336    80     3   149   312
#> Row2     48   295   151   287     2   326    70   264   204   137   296     8
#> Row1    297   330     3   256    90   184   196   218   344   299   182   112
ARCBD2$exptNames
#>       V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11   V12
#> 1  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 2  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 3  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 4  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 5  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 6  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 7  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 8  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 9  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 10 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 11 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 12 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 13 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 14 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 15 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 16 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 17 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#>      V13   V14   V15   V16   V17   V18   V19   V20   V21   V22   V23   V24
#> 1  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 2  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 3  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 4  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 5  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 6  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 7  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 8  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 9  Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 10 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 11 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 12 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 13 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 14 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 15 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 16 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#> 17 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1 Expt1
#>      V25
#> 1  Expt1
#> 2  Expt1
#> 3  Expt1
#> 4  Expt1
#> 5  Expt1
#> 6  Expt1
#> 7  Expt1
#> 8  Expt1
#> 9  Expt1
#> 10 Expt1
#> 11 Expt1
#> 12 Expt1
#> 13 Expt1
#> 14 Expt1
#> 15 Expt1
#> 16 Expt1
#> 17 Expt1
ARCBD2$plotNumber
#>        V1  V2  V3  V4  V5  V6  V7  V8  V9 V10 V11 V12 V13 V14 V15 V16 V17 V18
#>  [1,] 501 502 503 504 505 506 507 508 509 510 511 512 513 514 515 516 517 518
#>  [2,] 500 499 498 497 496 495 494 493 492 491 490 489 488 487 486 485 484 483
#>  [3,] 451 452 453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468
#>  [4,] 450 449 448 447 446 445 444 443 442 441 440 439 438 437 436 435 434 433
#>  [5,] 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418
#>  [6,] 400 399 398 397 396 395 394 393 392 391 390 389 388 387 386 385 384 383
#>  [7,] 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368
#>  [8,] 350 349 348 347 346 345 344 343 342 341 340 339 338 337 336 335 334 333
#>  [9,] 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318
#> [10,] 300 299 298 297 296 295 294 293 292 291 290 289 288 287 286 285 284 283
#> [11,] 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268
#> [12,] 250 249 248 247 246 245 244 243 242 241 240 239 238 237 236 235 234 233
#> [13,] 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218
#> [14,] 200 199 198 197 196 195 194 193 192 191 190 189 188 187 186 185 184 183
#> [15,] 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
#> [16,] 150 149 148 147 146 145 144 143 142 141 140 139 138 137 136 135 134 133
#> [17,] 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
#>       V19 V20 V21 V22 V23 V24 V25
#>  [1,]   0   0   0   0   0   0   0
#>  [2,] 482 481 480 479 478 477 476
#>  [3,] 469 470 471 472 473 474 475
#>  [4,] 432 431 430 429 428 427 426
#>  [5,] 419 420 421 422 423 424 425
#>  [6,] 382 381 380 379 378 377 376
#>  [7,] 369 370 371 372 373 374 375
#>  [8,] 332 331 330 329 328 327 326
#>  [9,] 319 320 321 322 323 324 325
#> [10,] 282 281 280 279 278 277 276
#> [11,] 269 270 271 272 273 274 275
#> [12,] 232 231 230 229 228 227 226
#> [13,] 219 220 221 222 223 224 225
#> [14,] 182 181 180 179 178 177 176
#> [15,] 169 170 171 172 173 174 175
#> [16,] 132 131 130 129 128 127 126
#> [17,] 119 120 121 122 123 124 125
head(ARCBD2$fieldBook, 12)
#>    ID  EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS BLOCK ENTRY TREATMENT
#> 1   1 Expt1        A 2026  101   1      1      0     1   345      G345
#> 2   2 Expt1        A 2026  102   1      2      0     1   130      G130
#> 3   3 Expt1        A 2026  103   1      3      1     1     4       CH4
#> 4   4 Expt1        A 2026  104   1      4      0     1   162      G162
#> 5   5 Expt1        A 2026  105   1      5      1     1     1       CH1
#> 6   6 Expt1        A 2026  106   1      6      0     1   123      G123
#> 7   7 Expt1        A 2026  107   1      7      1     1     2       CH2
#> 8   8 Expt1        A 2026  108   1      8      0     1    39       G39
#> 9   9 Expt1        A 2026  109   1      9      0     1     9        G9
#> 10 10 Expt1        A 2026  110   1     10      0     1   302      G302
#> 11 11 Expt1        A 2026  111   1     11      0     1   210      G210
#> 12 12 Expt1        A 2026  112   1     12      0     1   352      G352
                                       
```
