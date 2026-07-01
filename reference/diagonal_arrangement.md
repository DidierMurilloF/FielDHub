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
  data = NULL
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

## Value

A list with five elements.

- `infoDesign` is a list with information on the design parameters.

- `layoutRandom` is a matrix with the randomization layout.

- `plotsNumber` is a matrix with the layout plot number.

- `data_entry` is a data frame with the data input.

- `fieldBook` is a data frame with field book design. This includes the
  index (Row, Column).

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
#> [1] 10 13 13 11 13
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
#> [1] 24210
#> 
#> $id_design
#> [1] 15
#> 
spatDB$layoutRandom
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row30  702    3  686  699  642  709  701  689  664   720   696   633   708
#> Row29  722  649  616  627  716    4  673  639  711   641   680   688   710
#> Row28  698  615  631  674  672  636  626  685  608     4   651   697   650
#> Row27    5  679  629  677  606  621  692  662  694   725   663   669   666
#> Row26  661  622  610  678    3  612  687  657  713   609   675   670   704
#> Row25  566  576  522  514  491  575  433  598    4   432   473   567   454
#> Row24  529  500  488  518  580  458  526  525  419   480   548   605     5
#> Row23  508  410  602    3  471  588  470  498  492   474   437   472   558
#> Row22  442  541  468  552  463  482  449    2  584   443   423   599   535
#> Row21  446  475  589  467  537  422  542  416  572   435   411     3   487
#> Row20  560  547    3  460  597  429  448  469  590   409   464   506   478
#> Row19  600  530  550  504  520  521    1  461  536   556   486   509   519
#> Row18  436  544  447  424  415  545  543  438  512   595     4   578   534
#> Row17  334    5  340  361  396  345  365  342  384   373   390   392   316
#> Row16  367  366  335  387  404    2  311  395  389   348   328   394   380
#> Row15  397  320  356  351  314  327  339  403  383     5   377   319   374
#> Row14    1  321  331  337  353  402  352  364  358   322   369   329   405
#> Row13  209  256  242  296    5  201  272  237  310   279   158   243   274
#> Row12  186  292  222  193  275  179  200  261    3   252   204   250   289
#> Row11  221  168  176  301  297  184  224  271  244   263   161   188     2
#> Row10  306  206  307    2  300  298  255  278  284   295   259   173   241
#> Row9   302  190  251  170  187  178  293    2  157   230   260   240   159
#> Row8   246  181  189  277  192  232  162  228  305   167   245     5   194
#> Row7    58   41    1  124   57   55   11  199  171   254   291   182   304
#> Row6    96  133   44   81   98  139    2   66   62     7    53    70    20
#> Row5   140   85   65   31   39  106   73   33   76   112     4    34    54
#> Row4   145    3   50  128   64  137   95   42  144   120    92   118   115
#> Row3   107   67   61  149   80    5  101   47   27    77   151   127    74
#> Row2   138  116  122   88  154  117   29  110   78     4    60    83   113
#> Row1     2   94  102  114   26   79   91  131   25   109     8     6    49
#>       Col14 Col15 Col16 Col17 Col18 Col19 Col20 Col21 Col22 Col23 Col24 Col25
#> Row30   723     5   655   611   667   700   619   617   721   623   624   635
#> Row29   658   714   706   643   684     1   647   638   648   705   625   719
#> Row28   681   640   652   654   630   715   646   724   637     2   620   718
#> Row27     1   690   607   682   668   613   659   644   628   653   693   634
#> Row26   671   691   614   676     2   632   717   660   665   703   707   618
#> Row25   462   455   408   596   406   479   451   591     3   494   503   513
#> Row24   583   426   453   483   561   496   456   571   430   440   570   459
#> Row23   527   553   466     5   418   524   445   420   450   477   563   452
#> Row22   431   555   585   538   413   417   577     4   485   546   489   551
#> Row21   516   407   594   439   523   604   414   481   532   539   510     1
#> Row20   562   586     2   581   573   515   531   425   501   444   587   421
#> Row19   517   499   507   465   484   495     5   528   559   434   574   579
#> Row18   412   603   476   490   565   511   457   540   582   593     2   568
#> Row17   349     3   336   368   341   569   564   557   493   428   554   502
#> Row16   385   355   323   378   375     4   318   381   399   333   362   401
#> Row15   376   354   391   398   350   338   332   346   330     2   382   313
#> Row14     3   370   386   315   325   371   324   372   379   326   317   312
#> Row13   203   247   285   281     5   215   400   347   363   393   357   388
#> Row12   264   191   286   174   225   269   197   238     1   202   217   164
#> Row11   282   268   223   235   165   180   163   231   183   308   198   299
#> Row10   216   273   177     4   294   156   276   207   169   160   195   229
#> Row9    210   196   233   267   249   227   290     4   205   266   211   258
#> Row8    219   208   280   175   172   309   236   234   239   283   212     1
#> Row7    270   287     5   166   185   213   218   226   220   288   248   265
#> Row6    105    32    84    12   103    16     4    35    19    69    71    87
#> Row5    153   130    30    63   152   150    46   141    68    38     3    10
#> Row4    155     2   125   147    56   132     9   119    59    13   146   121
#> Row3     45    52    21    72   129     1    14    18    43    90    36    75
#> Row2     86   134    24    22    15   143    93    17    97     3    99   100
#> Row1      5   136    37    48    40   111   135   104   123    28    82   148
#>       Col26
#> Row30   645
#> Row29   695
#> Row28   712
#> Row27   683
#> Row26   656
#> Row25   497
#> Row24     1
#> Row23   533
#> Row22   427
#> Row21   505
#> Row20   549
#> Row19   592
#> Row18   601
#> Row17   441
#> Row16   343
#> Row15   359
#> Row14   360
#> Row13   344
#> Row12   303
#> Row11     3
#> Row10   214
#> Row9    253
#> Row8    257
#> Row7    262
#> Row6    108
#> Row5     89
#> Row4     51
#> Row3    142
#> Row2    126
#> Row1     23
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
#> 1   1 Block1        1 2026    1   1      1      2     2       CH2
#> 2   2 Block1        1 2026    2   1      2      0    94       G94
#> 3   3 Block1        1 2026    3   1      3      0   102      G102
#> 4   4 Block1        1 2026    4   1      4      0   114      G114
#> 5   5 Block1        1 2026    5   1      5      0    26       G26
#> 6   6 Block1        1 2026    6   1      6      0    79       G79
#> 7   7 Block1        1 2026    7   1      7      0    91       G91
#> 8   8 Block1        1 2026    8   1      8      0   131      G131
#> 9   9 Block1        1 2026    9   1      9      0    25       G25
#> 10 10 Block1        1 2026   10   1     10      0   109      G109
#> 11 11 Block1        1 2026   11   1     11      0     8        G8
#> 12 12 Block1        1 2026   12   1     12      0     6        G6

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
#> [1] 7 6 8 9
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
#> [1] 72391
#> 
#> $id_design
#> [1] 15
#> 
spatAB$layoutRandom
#> [[1]]
#>       Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13
#> Row20   82    1   56   57   70  144  172  148  146   173   219     4   229
#> Row19   49   42   69   21    3  103  111  151  102   157   237   249   221
#> Row18   44   10    9   29   12  109  177    3  106   149   262   225   226
#> Row17    2   43   90   16   71  162  176  136  104   138     4   191   185
#> Row16   75   88   36    1   30  110  133  175  180   158   193   253   186
#> Row15   85   45   91   19   13   97    2  171  154   105   252   204   245
#> Row14   86   67   14   83   93  134  161  100  140     4   211   217   251
#> Row13   33   92    3    6    5  116  160  101  117    98   227   263     2
#> Row12   54   31   74   89    8    3  181  145   99   114   254   223   267
#> Row11   23    7   50   25   76  137  163  168    2   159   242   260   206
#> Row10   63    4   11   24   61  139  182  167  153   130   208     3   216
#> Row9    80   65   58   52    1  118  135  125  122   147   192   264   234
#> Row8    32   26   41   48   39  152  183    4  165    95   233   220   240
#> Row7     4   66   37   68   46  178  142  132   96   115     1   198   258
#> Row6    55   27   35    2   77  155  166  131  127   169   209   212   256
#> Row5    94   18   60   34   40  141    3  119  126   184   231   224   241
#> Row4    59   15   73   38   84  179  113  170  164     1   232   189   235
#> Row3    28   47    4   53   51  143  123  120  129   108   244   207     4
#> Row2    64   20   17   72   81    4  107  150  174   156   210   230   222
#> Row1    79   62   78   22   87  121  124  112    1   128   228   261   213
#>       Col14 Col15
#> Row20   243   259
#> Row19   248     3
#> Row18   246   196
#> Row17   269   265
#> Row16     2   190
#> Row15   214   257
#> Row14   188   266
#> Row13   272   255
#> Row12   201   203
#> Row11   202   270
#> Row10   205   215
#> Row9    268     3
#> Row8    218   197
#> Row7    247   250
#> Row6      1   187
#> Row5    273   239
#> Row4    236   274
#> Row3    271   195
#> Row2    200   199
#> Row1    238   194
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
#> 1   1 20WRA        1 2026    1   1      1      0    79    Gen-79
#> 2   2 20WRA        1 2026    2   1      2      0    62    Gen-62
#> 3   3 20WRA        1 2026    3   1      3      0    78    Gen-78
#> 4   4 20WRA        1 2026    4   1      4      0    22    Gen-22
#> 5   5 20WRA        1 2026    5   1      5      0    87    Gen-87
#> 6   6 20WRB        1 2026 1001   1      6      0   121   Gen-121
#> 7   7 20WRB        1 2026 1002   1      7      0   124   Gen-124
#> 8   8 20WRB        1 2026 1003   1      8      0   112   Gen-112
#> 9   9 20WRB        1 2026 1004   1      9      1     1   Check-1
#> 10 10 20WRB        1 2026 1005   1     10      0   128   Gen-128
#> 11 11 20WRC        1 2026 2001   1     11      0   228   Gen-228
#> 12 12 20WRC        1 2026 2002   1     12      0   261   Gen-261
```
