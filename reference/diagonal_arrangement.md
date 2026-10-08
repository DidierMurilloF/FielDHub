# Spatial Un-replicated Diagonal Arrangement Design

Randomly generates an spatial un-replicated diagonal arrangement design.

## Usage

``` r
diagonal_arrangement(
  nrows = NULL,
  ncols = NULL,
  lines = NULL,
  checks = NULL,
  planter = "serpentine",
  l = 1,
  plotNumber = 101,
  kindExpt = "SUDC",
  splitBy = "row",
  seed = NULL,
  blocks = NULL,
  exptName = NULL,
  locationNames = NULL,
  multiLocationData = FALSE,
  data = NULL,
  year = NULL,
  checksPercent = NULL,
  sameEntries = FALSE
)
```

## Arguments

- nrows:

  Number of rows in the field.

- ncols:

  Number of columns in the field.

- lines:

  Number of genotypes, experimental lines or treatments.

- checks:

  Number of genotypes checks.

- planter:

  Option for `serpentine` or `cartesian` plot arrangement. By default
  `planter = 'serpentine'`.

- l:

  Number of locations or sites. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- kindExpt:

  Type of diagonal design, with single options: Single Un-replicated
  Diagonal Checks `'SUDC'` and Decision Blocks Un-replicated Design with
  Diagonal Checks `'DBUDC'` for multiple experiments. By default
  `kindExpt = 'SUDC'`.

- splitBy:

  Option to split the field when `kindExpt = 'DBUDC'` is selected. By
  default `splitBy = 'row'`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- blocks:

  Number of experiments or blocks to generate an `DBUDC` design. If
  `kindExpt = 'DBUDC'` and data is null, `blocks` are mandatory.

- exptName:

  (optional) Name of the experiment.

- locationNames:

  (optional) Names each location.

- multiLocationData:

  (optional) Option to pass an entry list for multiple locations. By
  default `multiLocationData = FALSE`.

- data:

  (optional) Data frame with 2 columns: `ENTRY | NAME `.

- year:

  (optional) Year recorded in the `YEAR` column of the field book. By
  default the current year.

- checksPercent:

  (optional) Percentage of checks, one of the options available for the
  field dimensions. By default the last (largest) option.

- sameEntries:

  (optional) Logical. When `TRUE` and `kindExpt = "DBUDC"`, every block
  holds the same entries, and all blocks must have the same size. By
  default `sameEntries = FALSE`.

## Value

A list with five elements.

- `infoDesign` is a list with information on the design parameters.

- `layoutRandom` is a matrix with the randomization layout.

- `plotsNumber` is a matrix with the layout plot number.

- `data_entry` is a data frame with the data input.

- `fieldBook` is a data frame with field book design. This includes the
  index (Row, Column).

## Reproducibility

The result records effective inputs and the resolved seed in
`metadata$parameters`. Under the same package versions and RNG settings,
rebuild a result `x` with
`do.call(diagonal_arrangement, x$metadata$parameters)`.

## References

Clarke, G. P. Y., & Stefanova, K. T. (2011). Optimal design for
early-generation plant breeding trials with unreplicated or partially
replicated test lines. Australian & New Zealand Journal of Statistics,
53(4), 461–480.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r

# Example 1: Generates a spatial single diagonal arrangement design in one location
# with 270 treatments and 30 check plots for a field with dimensions 15 rows x 20 cols
# in a serpentine arrangement.
spatd <- diagonal_arrangement(
  nrows = 15, 
  ncols = 20, 
  lines = 270, 
  checks = 4, 
  plotNumber = 101, 
  kindExpt = "SUDC", 
  planter = "serpentine", 
  seed = 1987,
  exptName = "20WRY1", 
  locationNames = "MINOT"
)
spatd$infoDesign
#> $rows
#> [1] 15
#> 
#> $columns
#> [1] 20
#> 
#> $treatments
#> [1] 270
#> 
#> $checks
#> [1] 4
#> 
#> $entry_checks
#> $entry_checks[[1]]
#> [1] 1 2 3 4
#> 
#> 
#> $rep_checks
#> $rep_checks[[1]]
#> [1] 8 7 8 7
#> 
#> 
#> $locations
#> [1] 1
#> 
#> $planter
#> [1] "serpentine"
#> 
#> $percent_checks
#> [1] "10%"
#> 
#> $fillers
#> [1] 0
#> 
#> $seed
#> [1] 1987
#> 
#> $id_design
#> [1] 15
#> 
spatd$layoutRandom
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row15  164    3  153   11  221  179  151  139   58    22   266     2   129
#> Row14   89  182  185   38    1  253  156  241  160   252   214    86   130
#> Row13   15  148   82  213   44  194  269    2  265   169    48   245   210
#> Row12    1  124   52  177    5  261   47   40   17    87     3   104   147
#> Row11  100  127  136    4   19   65  158   46   18   229   157   274    59
#> Row10   94   50   27   31  220  166    3  172  170    12    16   176   137
#> Row9   205  212  115  142  110  208  224  216  222     2   246    42   251
#> Row8   175   92    1  197  243  234  236   99  211    67   140    39     3
#> Row7    75   76    8  122  200    1  264   25  138   199   107   120   131
#> Row6   132   93  254    7  247   60   45  171    3   117   103   116   190
#> Row5   181    2   70   79   85  133  203  134  184   273    34     1   174
#> Row4    71  204  159   29    2   83   26   64  119   145   240   223   225
#> Row3   144  231   80  255   43  187  112    4  168    98    32    41    96
#> Row2     4  196  238  235   97  183  111  143  186   237     2   232   263
#> Row1    55  108  248    4  250  217  123  249  126    28    23   118    20
#>       Col14 Col15 Col16 Col17 Col18 Col19 Col20
#> Row15    33   109   154    88    30    53    95
#> Row14   163     4   219    68   270   173    90
#> Row13   244   125   149   226     1    54    56
#> Row12   259   233   267   201   193     6    10
#> Row11     2   114    21    77   272    72    24
#> Row10   102   155    36     3     9   162   191
#> Row9    218   106   228   258   167    84     1
#> Row8    230   192    62   135   198    14    69
#> Row7    161    81     3   165   189   268    57
#> Row6    128   146   206   141   215     4   195
#> Row5     61   202    51   242    73    63   207
#> Row4    113     1    78   178   152    37   180
#> Row3    101    74    66   239     4   105   256
#> Row2     49   262    91   257   121   260   209
#> Row1      3    13   150   188    35   227   271
#> 
spatd$plotsNumber
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row15  381  382  383  384  385  386  387  388  389   390   391   392   393
#> Row14  380  379  378  377  376  375  374  373  372   371   370   369   368
#> Row13  341  342  343  344  345  346  347  348  349   350   351   352   353
#> Row12  340  339  338  337  336  335  334  333  332   331   330   329   328
#> Row11  301  302  303  304  305  306  307  308  309   310   311   312   313
#> Row10  300  299  298  297  296  295  294  293  292   291   290   289   288
#> Row9   261  262  263  264  265  266  267  268  269   270   271   272   273
#> Row8   260  259  258  257  256  255  254  253  252   251   250   249   248
#> Row7   221  222  223  224  225  226  227  228  229   230   231   232   233
#> Row6   220  219  218  217  216  215  214  213  212   211   210   209   208
#> Row5   181  182  183  184  185  186  187  188  189   190   191   192   193
#> Row4   180  179  178  177  176  175  174  173  172   171   170   169   168
#> Row3   141  142  143  144  145  146  147  148  149   150   151   152   153
#> Row2   140  139  138  137  136  135  134  133  132   131   130   129   128
#> Row1   101  102  103  104  105  106  107  108  109   110   111   112   113
#>       Col14 Col15 Col16 Col17 Col18 Col19 Col20
#> Row15   394   395   396   397   398   399   400
#> Row14   367   366   365   364   363   362   361
#> Row13   354   355   356   357   358   359   360
#> Row12   327   326   325   324   323   322   321
#> Row11   314   315   316   317   318   319   320
#> Row10   287   286   285   284   283   282   281
#> Row9    274   275   276   277   278   279   280
#> Row8    247   246   245   244   243   242   241
#> Row7    234   235   236   237   238   239   240
#> Row6    207   206   205   204   203   202   201
#> Row5    194   195   196   197   198   199   200
#> Row4    167   166   165   164   163   162   161
#> Row3    154   155   156   157   158   159   160
#> Row2    127   126   125   124   123   122   121
#> Row1    114   115   116   117   118   119   120
#> 
head(spatd$fieldBook, 12)
#>    ID   EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
#> 1   1 20WRY1    MINOT 2026  101   1      1      0    55    Gen-55
#> 2   2 20WRY1    MINOT 2026  102   1      2      0   108   Gen-108
#> 3   3 20WRY1    MINOT 2026  103   1      3      0   248   Gen-248
#> 4   4 20WRY1    MINOT 2026  104   1      4      4     4   Check-4
#> 5   5 20WRY1    MINOT 2026  105   1      5      0   250   Gen-250
#> 6   6 20WRY1    MINOT 2026  106   1      6      0   217   Gen-217
#> 7   7 20WRY1    MINOT 2026  107   1      7      0   123   Gen-123
#> 8   8 20WRY1    MINOT 2026  108   1      8      0   249   Gen-249
#> 9   9 20WRY1    MINOT 2026  109   1      9      0   126   Gen-126
#> 10 10 20WRY1    MINOT 2026  110   1     10      0    28    Gen-28
#> 11 11 20WRY1    MINOT 2026  111   1     11      0    23    Gen-23
#> 12 12 20WRY1    MINOT 2026  112   1     12      0   118   Gen-118

# Example 2: Generates a spatial decision block diagonal arrangement design in one location
# with 720 treatments allocated in 5 experiments or blocks for a field with dimensions
# 30 rows x 26 cols in a serpentine arrangement. In this case, we show how to set up the data 
# option with the entries list.
checks <- 5;expts <- 5
list_checks <- paste("CH", 1:checks, sep = "")
treatments <- paste("G", 6:725, sep = "")
treatment_list <- data.frame(list(ENTRY = 1:725, NAME = c(list_checks, treatments)))
head(treatment_list, 12) 
#>    ENTRY NAME
#> 1      1  CH1
#> 2      2  CH2
#> 3      3  CH3
#> 4      4  CH4
#> 5      5  CH5
#> 6      6   G6
#> 7      7   G7
#> 8      8   G8
#> 9      9   G9
#> 10    10  G10
#> 11    11  G11
#> 12    12  G12
tail(treatment_list, 12)
#>     ENTRY NAME
#> 714   714 G714
#> 715   715 G715
#> 716   716 G716
#> 717   717 G717
#> 718   718 G718
#> 719   719 G719
#> 720   720 G720
#> 721   721 G721
#> 722   722 G722
#> 723   723 G723
#> 724   724 G724
#> 725   725 G725
spatDB <- diagonal_arrangement(
  nrows = 30, 
  ncols = 26,
  checks = 5, 
  plotNumber = 1, 
  kindExpt = "DBUDC", 
  planter = "serpentine", 
  splitBy = "row", 
  blocks = c(150,155,95,200,120),
  data = treatment_list
)
spatDB$infoDesign
#> $rows
#> [1] 30
#> 
#> $columns
#> [1] 26
#> 
#> $treatments
#> [1] 150 155  95 200 120
#> 
#> $checks
#> [1] 5
#> 
#> $entry_checks
#> $entry_checks[[1]]
#> [1] 1 2 3 4 5
#> 
#> 
#> $rep_checks
#> $rep_checks[[1]]
#> [1] 10 13 11 13 13
#> 
#> 
#> $locations
#> [1] 1
#> 
#> $planter
#> [1] "serpentine"
#> 
#> $percent_checks
#> [1] "7.7%"
#> 
#> $fillers
#> [1] 0
#> 
#> $seed
#> [1] 346871191
#> 
#> $id_design
#> [1] 15
#> 
spatDB$layoutRandom
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row30  616    2  651  672  614  619  698  668  667   673   661   617   669
#> Row29  607  706  658  720  645    4  717  626  722   696   638   612   681
#> Row28  719  646  622  697  662  693  631  629  680     4   640   688   630
#> Row27    1  682  663  633  708  723  623  639  624   642   704   613   636
#> Row26  625  606  678  628    3  644  632  709  677   683   649   711   610
#> Row25  424  581  476  474  493  512  595  572    2   472   487   471   590
#> Row24  526  525  599  570  446  421  464  585  555   428   604   538     3
#> Row23  508  408  486    3  591  447  495  436  506   443   568   573   554
#> Row22  583  513  593  537  435  445  575    1  454   600   420   517   569
#> Row21  541  489  565  571  587  545  494  566  459   499   481     2   515
#> Row20  484  546    2  514  532  504  533  539  559   488   507   550   516
#> Row19  584  455  458  406  556  597    1  553  462   438   518   592   485
#> Row18  535  582  429  498  414  534  564  431  441   475     1   449   528
#> Row17  336    3  385  333  328  340  346  349  320   342   330   375   339
#> Row16  376  354  350  338  404    1  345  316  365   391   369   373   360
#> Row15  374  368  384  361  386  372  337  396  364     2   347   370   405
#> Row14    4  311  335  378  402  358  380  319  343   383   388   317   381
#> Row13  284  251  300  239    4  303  175  197  237   191   206   201   308
#> Row12  193  214  198  257  157  171  192  177    5   236   291   249   294
#> Row11  278  186  245  285  187  222  256  269  176   217   233   163     4
#> Row10  169  231  210    1  272  304  258  277  179   261   164   196   204
#> Row9   194  185  274  299  215  178  158    2  161   209   298   288   170
#> Row8   281  276  259  306  221  211  265  205  286   166   310     4   181
#> Row7   140   84    4  105   32  150   91  301  238   199   173   219   225
#> Row6    66   51  138   90  107   65    5   89   92   153    19    26    56
#> Row5   130  119  134   41   33   50  104  116   28   127     4   100    37
#> Row4   111    1  133  114   43   95   60   64   52   123   128   137   143
#> Row3    94  102   80  129  146    2  141   82  142   125   122    18    62
#> Row2    21  149  139  144   22  101   16   25   24     5    79    86    10
#> Row1     3   76  103   35   27   55  151   58  152    23   112    85   135
#>       Col14 Col15 Col16 Col17 Col18 Col19 Col20 Col21 Col22 Col23 Col24 Col25
#> Row30   657     5   676   725   724   675   684   702   618   641   615   654
#> Row29   671   656   716   664   701     3   721   647   691   713   687   659
#> Row28   650   715   634   705   635   666   611   692   660     1   718   689
#> Row27     5   655   710   707   621   714   653   608   690   703   699   670
#> Row26   665   620   694   643     2   700   609   679   712   695   627   674
#> Row25   560   416   467   492   501   477   588   502     3   567   478   557
#> Row24   470   529   530   463   543   407   509   523   521   549   548   510
#> Row23   480   520   437     5   425   542   589   439   433   426   603   598
#> Row22   453   551   418   427   519   578   432     4   457   579   596   490
#> Row21   417   448   574   444   503   451   411   586   544   410   469     5
#> Row20   452   522     4   468   442   562   430   413   576   601   412   563
#> Row19   440   505   500   409   580   479     4   419   482   461   483   465
#> Row18   602   547   496   558   456   422   577   450   497   423     5   491
#> Row17   403     4   392   329   366   524   531   466   540   536   434   605
#> Row16   344   351   395   326   393     5   356   314   379   353   332   398
#> Row15   318   327   352   315   363   313   322   390   371     5   367   400
#> Row14     2   357   324   359   401   323   399   334   348   362   382   394
#> Row13   267   268   230   309     5   195   389   321   355   377   312   331
#> Row12   297   242   275   282   240   247   293   254     3   235   182   270
#> Row11   167   279   213   307   156   244   218   202   212   207   264   262
#> Row10   174   203   246     1   271   292   253   250   227   302   266   287
#> Row9    243   188   296   180   305   248   283     3   189   232   165   290
#> Row8    255   273   226   183   228   224   280   184   229   289   160     5
#> Row7    172   220     4   159   260   208   252   190   234   200   241   216
#> Row6     69    54    31    87    57   126     1   117   148    71    13    74
#> Row5    118    81    45   154    12    83    68    53    49    46     2    11
#> Row4     47     3    98    73    20    88    30   121   108    72   136    29
#> Row3     34     7   132   147    39     2   109    15     8    77   106    14
#> Row2     97    17    44     9    75   145   113    63   131     3   110    36
#> Row1      5    78    96    59    48    99    40    61    67    42   115    38
#>       Col26
#> Row30   685
#> Row29   648
#> Row28   652
#> Row27   637
#> Row26   686
#> Row25   460
#> Row24     2
#> Row23   561
#> Row22   511
#> Row21   415
#> Row20   473
#> Row19   594
#> Row18   552
#> Row17   527
#> Row16   325
#> Row15   341
#> Row14   397
#> Row13   387
#> Row12   263
#> Row11     2
#> Row10   162
#> Row9    168
#> Row8    223
#> Row7    295
#> Row6      6
#> Row5    120
#> Row4     70
#> Row3     93
#> Row2    124
#> Row1    155
#> 
spatDB$plotsNumber
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row30  780  779  778  777  776  775  774  773  772   771   770   769   768
#> Row29  729  730  731  732  733  734  735  736  737   738   739   740   741
#> Row28  728  727  726  725  724  723  722  721  720   719   718   717   716
#> Row27  677  678  679  680  681  682  683  684  685   686   687   688   689
#> Row26  676  675  674  673  672  671  670  669  668   667   666   665   664
#> Row25  625  626  627  628  629  630  631  632  633   634   635   636   637
#> Row24  624  623  622  621  620  619  618  617  616   615   614   613   612
#> Row23  573  574  575  576  577  578  579  580  581   582   583   584   585
#> Row22  572  571  570  569  568  567  566  565  564   563   562   561   560
#> Row21  521  522  523  524  525  526  527  528  529   530   531   532   533
#> Row20  520  519  518  517  516  515  514  513  512   511   510   509   508
#> Row19  469  470  471  472  473  474  475  476  477   478   479   480   481
#> Row18  468  467  466  465  464  463  462  461  460   459   458   457   456
#> Row17  417  418  419  420  421  422  423  424  425   426   427   428   429
#> Row16  416  415  414  413  412  411  410  409  408   407   406   405   404
#> Row15  365  366  367  368  369  370  371  372  373   374   375   376   377
#> Row14  364  363  362  361  360  359  358  357  356   355   354   353   352
#> Row13  313  314  315  316  317  318  319  320  321   322   323   324   325
#> Row12  312  311  310  309  308  307  306  305  304   303   302   301   300
#> Row11  261  262  263  264  265  266  267  268  269   270   271   272   273
#> Row10  260  259  258  257  256  255  254  253  252   251   250   249   248
#> Row9   209  210  211  212  213  214  215  216  217   218   219   220   221
#> Row8   208  207  206  205  204  203  202  201  200   199   198   197   196
#> Row7   157  158  159  160  161  162  163  164  165   166   167   168   169
#> Row6   156  155  154  153  152  151  150  149  148   147   146   145   144
#> Row5   105  106  107  108  109  110  111  112  113   114   115   116   117
#> Row4   104  103  102  101  100   99   98   97   96    95    94    93    92
#> Row3    53   54   55   56   57   58   59   60   61    62    63    64    65
#> Row2    52   51   50   49   48   47   46   45   44    43    42    41    40
#> Row1     1    2    3    4    5    6    7    8    9    10    11    12    13
#>       Col14 Col15 Col16 Col17 Col18 Col19 Col20 Col21 Col22 Col23 Col24 Col25
#> Row30   767   766   765   764   763   762   761   760   759   758   757   756
#> Row29   742   743   744   745   746   747   748   749   750   751   752   753
#> Row28   715   714   713   712   711   710   709   708   707   706   705   704
#> Row27   690   691   692   693   694   695   696   697   698   699   700   701
#> Row26   663   662   661   660   659   658   657   656   655   654   653   652
#> Row25   638   639   640   641   642   643   644   645   646   647   648   649
#> Row24   611   610   609   608   607   606   605   604   603   602   601   600
#> Row23   586   587   588   589   590   591   592   593   594   595   596   597
#> Row22   559   558   557   556   555   554   553   552   551   550   549   548
#> Row21   534   535   536   537   538   539   540   541   542   543   544   545
#> Row20   507   506   505   504   503   502   501   500   499   498   497   496
#> Row19   482   483   484   485   486   487   488   489   490   491   492   493
#> Row18   455   454   453   452   451   450   449   448   447   446   445   444
#> Row17   430   431   432   433   434   435   436   437   438   439   440   441
#> Row16   403   402   401   400   399   398   397   396   395   394   393   392
#> Row15   378   379   380   381   382   383   384   385   386   387   388   389
#> Row14   351   350   349   348   347   346   345   344   343   342   341   340
#> Row13   326   327   328   329   330   331   332   333   334   335   336   337
#> Row12   299   298   297   296   295   294   293   292   291   290   289   288
#> Row11   274   275   276   277   278   279   280   281   282   283   284   285
#> Row10   247   246   245   244   243   242   241   240   239   238   237   236
#> Row9    222   223   224   225   226   227   228   229   230   231   232   233
#> Row8    195   194   193   192   191   190   189   188   187   186   185   184
#> Row7    170   171   172   173   174   175   176   177   178   179   180   181
#> Row6    143   142   141   140   139   138   137   136   135   134   133   132
#> Row5    118   119   120   121   122   123   124   125   126   127   128   129
#> Row4     91    90    89    88    87    86    85    84    83    82    81    80
#> Row3     66    67    68    69    70    71    72    73    74    75    76    77
#> Row2     39    38    37    36    35    34    33    32    31    30    29    28
#> Row1     14    15    16    17    18    19    20    21    22    23    24    25
#>       Col26
#> Row30   755
#> Row29   754
#> Row28   703
#> Row27   702
#> Row26   651
#> Row25   650
#> Row24   599
#> Row23   598
#> Row22   547
#> Row21   546
#> Row20   495
#> Row19   494
#> Row18   443
#> Row17   442
#> Row16   391
#> Row15   390
#> Row14   339
#> Row13   338
#> Row12   287
#> Row11   286
#> Row10   235
#> Row9    234
#> Row8    183
#> Row7    182
#> Row6    131
#> Row5    130
#> Row4     79
#> Row3     78
#> Row2     27
#> Row1     26
#> 
head(spatDB$fieldBook,12)
#>    ID   EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
#> 1   1 Block1        1 2026    1   1      1      3     3       CH3
#> 2   2 Block1        1 2026    2   1      2      0    76       G76
#> 3   3 Block1        1 2026    3   1      3      0   103      G103
#> 4   4 Block1        1 2026    4   1      4      0    35       G35
#> 5   5 Block1        1 2026    5   1      5      0    27       G27
#> 6   6 Block1        1 2026    6   1      6      0    55       G55
#> 7   7 Block1        1 2026    7   1      7      0   151      G151
#> 8   8 Block1        1 2026    8   1      8      0    58       G58
#> 9   9 Block1        1 2026    9   1      9      0   152      G152
#> 10 10 Block1        1 2026   10   1     10      0    23       G23
#> 11 11 Block1        1 2026   11   1     11      0   112      G112
#> 12 12 Block1        1 2026   12   1     12      0    85       G85

# Example 3: Generates a spatial decision block diagonal arrangement design in one location
# with 270 treatments allocated in 3 experiments or blocks for a field with dimensions
# 20 rows x 15 cols in a serpentine arrangement. Which in turn is an augmented block (3 blocks).
spatAB <- diagonal_arrangement(
  nrows = 20, 
  ncols = 15, 
  lines = 270, 
  checks = 4, 
  plotNumber = c(1,1001,2001), 
  kindExpt = "DBUDC", 
  planter = "serpentine",
  exptName = c("20WRA", "20WRB", "20WRC"), 
  blocks = c(90, 90, 90),
  splitBy = "column"
)
spatAB$infoDesign
#> $rows
#> [1] 20
#> 
#> $columns
#> [1] 15
#> 
#> $treatments
#> [1] 90 90 90
#> 
#> $checks
#> [1] 4
#> 
#> $entry_checks
#> $entry_checks[[1]]
#> [1] 1 2 3 4
#> 
#> 
#> $rep_checks
#> $rep_checks[[1]]
#> [1] 8 7 7 8
#> 
#> 
#> $locations
#> [1] 1
#> 
#> $planter
#> [1] "serpentine"
#> 
#> $percent_checks
#> [1] "10%"
#> 
#> $fillers
#> [1] 0
#> 
#> $seed
#> [1] 432744511
#> 
#> $id_design
#> [1] 15
#> 
spatAB$layoutRandom
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row20   18    1   29   75   82  159  107  117  125   112   220     4   185
#> Row19   70   94   30   26    1  162  144  113  105   122   230   187   257
#> Row18   68    5   59   79   90  153  130    1  139   135   241   231   218
#> Row17    3   41   76   53   13  142  179   97  121   145     4   222   201
#> Row16   45   31   50    3    8  161  151  111  109   155   223   215   263
#> Row15   80   21   83   44   48  170    2  164  183   143   217   236   268
#> Row14   17   51   81   38   16  158  177  150  132     3   208   232   199
#> Row13    9   69    4   64   23  178  148  120  106   126   243   258     3
#> Row12    6   15   87   20   55    1  119  156  147   129   196   235   214
#> Row11   11   12   74   66   91  131   95  140    2   167   211   186   206
#> Row10   42    1   32   22   61  137  123   99  110   101   244     1   188
#> Row9    62   85   63   73    4  146  171  128  166    96   253   247   212
#> Row8    34   37   28   33   46  154  168    4  133   163   272   260   224
#> Row7     3   10   25   24   43  138  184  100  116   149     1   250   216
#> Row6    93   36   47    2   65  118  157  172  165   127   193   213   239
#> Row5    40   39   78   88   19  102    3  134  181   108   238   269   240
#> Row4    89   57   27   72   86   98  180  115  174     4   194   242   254
#> Row3    58   84    2   52   71  104  173  176  124   136   195   262     2
#> Row2    49   54   14   60   56    4  175  169  103   152   197   245   233
#> Row1     7   35   77   67   92  114  160  182    2   141   237   249   265
#>       Col14 Col15
#> Row20   190   227
#> Row19   256     3
#> Row18   252   259
#> Row17   226   203
#> Row16     1   255
#> Row15   198   273
#> Row14   251   205
#> Row13   221   266
#> Row12   271   207
#> Row11   261   225
#> Row10   267   204
#> Row9    264     4
#> Row8    274   248
#> Row7    200   234
#> Row6      2   189
#> Row5    229   219
#> Row4    270   192
#> Row3    202   210
#> Row2    246   209
#> Row1    228   191
#> 
spatAB$plotsNumber
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row20  100   99   98   97   96 1100 1099 1098 1097  1096  2100  2099  2098
#> Row19   91   92   93   94   95 1091 1092 1093 1094  1095  2091  2092  2093
#> Row18   90   89   88   87   86 1090 1089 1088 1087  1086  2090  2089  2088
#> Row17   81   82   83   84   85 1081 1082 1083 1084  1085  2081  2082  2083
#> Row16   80   79   78   77   76 1080 1079 1078 1077  1076  2080  2079  2078
#> Row15   71   72   73   74   75 1071 1072 1073 1074  1075  2071  2072  2073
#> Row14   70   69   68   67   66 1070 1069 1068 1067  1066  2070  2069  2068
#> Row13   61   62   63   64   65 1061 1062 1063 1064  1065  2061  2062  2063
#> Row12   60   59   58   57   56 1060 1059 1058 1057  1056  2060  2059  2058
#> Row11   51   52   53   54   55 1051 1052 1053 1054  1055  2051  2052  2053
#> Row10   50   49   48   47   46 1050 1049 1048 1047  1046  2050  2049  2048
#> Row9    41   42   43   44   45 1041 1042 1043 1044  1045  2041  2042  2043
#> Row8    40   39   38   37   36 1040 1039 1038 1037  1036  2040  2039  2038
#> Row7    31   32   33   34   35 1031 1032 1033 1034  1035  2031  2032  2033
#> Row6    30   29   28   27   26 1030 1029 1028 1027  1026  2030  2029  2028
#> Row5    21   22   23   24   25 1021 1022 1023 1024  1025  2021  2022  2023
#> Row4    20   19   18   17   16 1020 1019 1018 1017  1016  2020  2019  2018
#> Row3    11   12   13   14   15 1011 1012 1013 1014  1015  2011  2012  2013
#> Row2    10    9    8    7    6 1010 1009 1008 1007  1006  2010  2009  2008
#> Row1     1    2    3    4    5 1001 1002 1003 1004  1005  2001  2002  2003
#>       Col14 Col15
#> Row20  2097  2096
#> Row19  2094  2095
#> Row18  2087  2086
#> Row17  2084  2085
#> Row16  2077  2076
#> Row15  2074  2075
#> Row14  2067  2066
#> Row13  2064  2065
#> Row12  2057  2056
#> Row11  2054  2055
#> Row10  2047  2046
#> Row9   2044  2045
#> Row8   2037  2036
#> Row7   2034  2035
#> Row6   2027  2026
#> Row5   2024  2025
#> Row4   2017  2016
#> Row3   2014  2015
#> Row2   2007  2006
#> Row1   2004  2005
#> 
head(spatAB$fieldBook,12)
#>    ID  EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
#> 1   1 20WRA        1 2026    1   1      1      0     7     Gen-7
#> 2   2 20WRA        1 2026    2   1      2      0    35    Gen-35
#> 3   3 20WRA        1 2026    3   1      3      0    77    Gen-77
#> 4   4 20WRA        1 2026    4   1      4      0    67    Gen-67
#> 5   5 20WRA        1 2026    5   1      5      0    92    Gen-92
#> 6   6 20WRB        1 2026 1001   1      6      0   114   Gen-114
#> 7   7 20WRB        1 2026 1002   1      7      0   160   Gen-160
#> 8   8 20WRB        1 2026 1003   1      8      0   182   Gen-182
#> 9   9 20WRB        1 2026 1004   1      9      2     2   Check-2
#> 10 10 20WRB        1 2026 1005   1     10      0   141   Gen-141
#> 11 11 20WRC        1 2026 2001   1     11      0   237   Gen-237
#> 12 12 20WRC        1 2026 2002   1     12      0   249   Gen-249
```
