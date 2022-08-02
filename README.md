
<!-- README.md is generated from README.Rmd. Please edit that file -->

<br>

# FielDHub <a href=''><img src='man/figures/logo.gif' align="right" height="139" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/FielDHub)](https://cran.r-project.org/web//packages/FielDHub/)
[![R-CMD-check](https://github.com/DidierMurilloF/FielDHub/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DidierMurilloF/FielDHub/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![cranlogs](https://cranlogs.r-pkg.org/badges/FielDHub)](https://cranlogs.r-pkg.org/badges/FielDHub)
[![cranlogs](https://cranlogs.r-pkg.org/badges/grand-total/FielDHub)](https://cranlogs.r-pkg.org/badges/grand-total/FielDHub)

## A Shiny App for Design of Experiments in Life Sciences

## Installation

### Stable version from CRAN

``` r
install.packages("FielDHub")
```

### Development version from GitHub

``` r
remotes::install_github("DidierMurilloF/FielDHub")
```

## FielDHub Paper

[![DOI](https://joss.theoj.org/papers/10.21105/joss.03122/status.svg)](https://doi.org/10.21105/joss.03122)

## Overview

A shiny design of experiments (DOE) app that aids in the creation of
traditional, un-replicated, augmented and partially-replicated designs
applied to agriculture, plant breeding, forestry, animal and biological
sciences.

For more details and examples of all functions present in the FielDHub
package. Please, go to
<https://didiermurillof.github.io/FielDHub/reference/index.html>.

![](FielDHub_Overview.png)

## Usage

This is a basic example which shows you how to launch the app:

``` r
library(FielDHub)
run_app()
```

### Diagonal Arrangement Example

A project needs to test 280 genotypes in a field containing 16 rows and
20 columns of plots. In this example, these 280 genotypes are divided
among three different experiments. In addition, four checks are included
in a systematic diagonal arrangement across experiments to fill 40 plots
representing 12.5% of the total number of experimental plots. An option
to include filler plots is also available for fields where the number of
experimental plots does not equal the number of available field plots.

![](DExample.png)

The figure above shows a map of an experiment randomized as a Decision
Block Unreplicated Design with Checks on Diagonals. Yellow, gray, and
green shade the blocks of unreplicated experiments, while distinctively
colored check plots are replicated throughout the field in a systematic
diagonal arrangement.

To illustrate using FielDHub to build experimental designs through R
code, the design produced in the R Shiny interface described above can
also be created using the function `diagonal_arrangement()` in the R
script below. Note, that to obtain identical results, users must include
the same seed number in the script as was used in the Shiny app. In this
case, the seed number is 1249.

``` r
diagonal <- diagonal_arrangement(
  nrows = 16, 
  ncols = 20, 
  lines = 280, 
  checks = 4, 
  plotNumber = 101, 
  splitBy = "row", 
  seed = 1249, 
  kindExpt = "DBUDC", 
  blocks = c(100, 100, 80)
)
```

Users can access the returned values from `diagonal_arrangement()` as
follow,

``` r
diagonal$infoDesign
$rows
[1] 16

$columns
[1] 20

$treatments
[1] 100 100  80

$checks
[1] 4

$entry_checks
[1] 1 2 3 4

$rep_checks
[1] 11  9  9 11

$locations
[1] 1

$planter
[1] "serpentine"

$percent_checks
[1] "12.5%"

$fillers
[1] 0

$seed
[1] 1249

$id_design
[1] 15
```

``` r
head(diagonal$fieldBook, 12)
   ID   EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
1   1 Block1        1 2022  101   1      1      0    51    Gen-51
2   2 Block1        1 2022  102   1      2      0    67    Gen-67
3   3 Block1        1 2022  103   1      3      0    50    Gen-50
4   4 Block1        1 2022  104   1      4      0    29    Gen-29
5   5 Block1        1 2022  105   1      5      0    39    Gen-39
6   6 Block1        1 2022  106   1      6      0    92    Gen-92
7   7 Block1        1 2022  107   1      7      1     1   Check-1
8   8 Block1        1 2022  108   1      8      0    58    Gen-58
9   9 Block1        1 2022  109   1      9      0    23    Gen-23
10 10 Block1        1 2022  110   1     10      0    54    Gen-54
11 11 Block1        1 2022  111   1     11      0    55    Gen-55
12 12 Block1        1 2022  112   1     12      0     9     Gen-9
```

Users can plot the layout design from `diagonal_arrangement()` using the
function `plot()` as follows,

``` r
plot(diagonal)
$field_book
     ID   EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
1     1 Block1        1 2022  101   1      1      0    51    Gen-51
2     2 Block1        1 2022  102   1      2      0    67    Gen-67
3     3 Block1        1 2022  103   1      3      0    50    Gen-50
4     4 Block1        1 2022  104   1      4      0    29    Gen-29
5     5 Block1        1 2022  105   1      5      0    39    Gen-39
6     6 Block1        1 2022  106   1      6      0    92    Gen-92
7     7 Block1        1 2022  107   1      7      1     1   Check-1
8     8 Block1        1 2022  108   1      8      0    58    Gen-58
9     9 Block1        1 2022  109   1      9      0    23    Gen-23
10   10 Block1        1 2022  110   1     10      0    54    Gen-54
11   11 Block1        1 2022  111   1     11      0    55    Gen-55
12   12 Block1        1 2022  112   1     12      0     9     Gen-9
13   13 Block1        1 2022  113   1     13      0    46    Gen-46
14   14 Block1        1 2022  114   1     14      0    27    Gen-27
15   15 Block1        1 2022  115   1     15      2     2   Check-2
16   16 Block1        1 2022  116   1     16      0    37    Gen-37
17   17 Block1        1 2022  117   1     17      0    61    Gen-61
18   18 Block1        1 2022  118   1     18      0    72    Gen-72
19   19 Block1        1 2022  119   1     19      0    15    Gen-15
20   20 Block1        1 2022  120   1     20      0   104   Gen-104
21   21 Block1        1 2022  121   2     20      3     3   Check-3
22   22 Block1        1 2022  122   2     19      0    91    Gen-91
23   23 Block1        1 2022  123   2     18      0    65    Gen-65
24   24 Block1        1 2022  124   2     17      0    99    Gen-99
25   25 Block1        1 2022  125   2     16      0    83    Gen-83
26   26 Block1        1 2022  126   2     15      0    66    Gen-66
27   27 Block1        1 2022  127   2     14      0    12    Gen-12
28   28 Block1        1 2022  128   2     13      0    59    Gen-59
29   29 Block1        1 2022  129   2     12      4     4   Check-4
30   30 Block1        1 2022  130   2     11      0    81    Gen-81
31   31 Block1        1 2022  131   2     10      0    69    Gen-69
32   32 Block1        1 2022  132   2      9      0    86    Gen-86
33   33 Block1        1 2022  133   2      8      0    52    Gen-52
34   34 Block1        1 2022  134   2      7      0    90    Gen-90
35   35 Block1        1 2022  135   2      6      0    41    Gen-41
36   36 Block1        1 2022  136   2      5      0    68    Gen-68
37   37 Block1        1 2022  137   2      4      3     3   Check-3
38   38 Block1        1 2022  138   2      3      0    18    Gen-18
39   39 Block1        1 2022  139   2      2      0    19    Gen-19
40   40 Block1        1 2022  140   2      1      0    31    Gen-31
41   41 Block1        1 2022  141   3      1      2     2   Check-2
42   42 Block1        1 2022  142   3      2      0    13    Gen-13
43   43 Block1        1 2022  143   3      3      0    22    Gen-22
44   44 Block1        1 2022  144   3      4      0    85    Gen-85
45   45 Block1        1 2022  145   3      5      0    16    Gen-16
46   46 Block1        1 2022  146   3      6      0    96    Gen-96
47   47 Block1        1 2022  147   3      7      0    43    Gen-43
48   48 Block1        1 2022  148   3      8      0    63    Gen-63
49   49 Block1        1 2022  149   3      9      2     2   Check-2
50   50 Block1        1 2022  150   3     10      0    33    Gen-33
51   51 Block1        1 2022  151   3     11      0    44    Gen-44
52   52 Block1        1 2022  152   3     12      0    57    Gen-57
53   53 Block1        1 2022  153   3     13      0    36    Gen-36
54   54 Block1        1 2022  154   3     14      0    14    Gen-14
55   55 Block1        1 2022  155   3     15      0    25    Gen-25
56   56 Block1        1 2022  156   3     16      0    93    Gen-93
57   57 Block1        1 2022  157   3     17      1     1   Check-1
58   58 Block1        1 2022  158   3     18      0    75    Gen-75
59   59 Block1        1 2022  159   3     19      0    48    Gen-48
60   60 Block1        1 2022  160   3     20      0    24    Gen-24
61   61 Block1        1 2022  161   4     20      0    98    Gen-98
62   62 Block1        1 2022  162   4     19      0    80    Gen-80
63   63 Block1        1 2022  163   4     18      0    56    Gen-56
64   64 Block1        1 2022  164   4     17      0    94    Gen-94
65   65 Block1        1 2022  165   4     16      0     6     Gen-6
66   66 Block1        1 2022  166   4     15      0    73    Gen-73
67   67 Block1        1 2022  167   4     14      1     1   Check-1
68   68 Block1        1 2022  168   4     13      0   100   Gen-100
69   69 Block1        1 2022  169   4     12      0    45    Gen-45
70   70 Block1        1 2022  170   4     11      0    79    Gen-79
71   71 Block1        1 2022  171   4     10      0    35    Gen-35
72   72 Block1        1 2022  172   4      9      0    20    Gen-20
73   73 Block1        1 2022  173   4      8      0    11    Gen-11
74   74 Block1        1 2022  174   4      7      0    53    Gen-53
75   75 Block1        1 2022  175   4      6      1     1   Check-1
76   76 Block1        1 2022  176   4      5      0    82    Gen-82
77   77 Block1        1 2022  177   4      4      0    95    Gen-95
78   78 Block1        1 2022  178   4      3      0    87    Gen-87
79   79 Block1        1 2022  179   4      2      0    70    Gen-70
80   80 Block1        1 2022  180   4      1      0   102   Gen-102
81   81 Block1        1 2022  181   5      1      0    32    Gen-32
82   82 Block1        1 2022  182   5      2      0    76    Gen-76
83   83 Block1        1 2022  183   5      3      4     4   Check-4
84   84 Block1        1 2022  184   5      4      0    40    Gen-40
85   85 Block1        1 2022  185   5      5      0    28    Gen-28
86   86 Block1        1 2022  186   5      6      0    62    Gen-62
87   87 Block1        1 2022  187   5      7      0    26    Gen-26
88   88 Block1        1 2022  188   5      8      0    38    Gen-38
89   89 Block1        1 2022  189   5      9      0   103   Gen-103
90   90 Block1        1 2022  190   5     10      0     5     Gen-5
91   91 Block1        1 2022  191   5     11      3     3   Check-3
92   92 Block1        1 2022  192   5     12      0    21    Gen-21
93   93 Block1        1 2022  193   5     13      0    77    Gen-77
94   94 Block1        1 2022  194   5     14      0    64    Gen-64
95   95 Block1        1 2022  195   5     15      0    17    Gen-17
96   96 Block1        1 2022  196   5     16      0    71    Gen-71
97   97 Block1        1 2022  197   5     17      0    84    Gen-84
98   98 Block1        1 2022  198   5     18      0    34    Gen-34
99   99 Block1        1 2022  199   5     19      4     4   Check-4
100 100 Block1        1 2022  200   5     20      0    97    Gen-97
101 101 Block1        1 2022  201   6     20      0    78    Gen-78
102 102 Block1        1 2022  202   6     19      0    74    Gen-74
103 103 Block1        1 2022  203   6     18      0     8     Gen-8
104 104 Block1        1 2022  204   6     17      0    60    Gen-60
105 105 Block1        1 2022  205   6     16      3     3   Check-3
106 106 Block1        1 2022  206   6     15      0     7     Gen-7
107 107 Block1        1 2022  207   6     14      0   101   Gen-101
108 108 Block1        1 2022  208   6     13      0    88    Gen-88
109 109 Block1        1 2022  209   6     12      0    10    Gen-10
110 110 Block1        1 2022  210   6     11      0    47    Gen-47
111 111 Block1        1 2022  211   6     10      0    42    Gen-42
112 112 Block1        1 2022  212   6      9      0    89    Gen-89
113 113 Block1        1 2022  213   6      8      4     4   Check-4
114 114 Block1        1 2022  214   6      7      0    49    Gen-49
115 115 Block1        1 2022  215   6      6      0    30    Gen-30
116 116 Block2        1 2022  216   6      5      0   162   Gen-162
117 117 Block2        1 2022  217   6      4      0   140   Gen-140
118 118 Block2        1 2022  218   6      3      0   156   Gen-156
119 119 Block2        1 2022  219   6      2      0   131   Gen-131
120 120 Block2        1 2022  220   6      1      0   176   Gen-176
121 121 Block2        1 2022  221   7      1      0   196   Gen-196
122 122 Block2        1 2022  222   7      2      0   109   Gen-109
123 123 Block2        1 2022  223   7      3      0   112   Gen-112
124 124 Block2        1 2022  224   7      4      0   108   Gen-108
125 125 Block2        1 2022  225   7      5      4     4   Check-4
126 126 Block2        1 2022  226   7      6      0   141   Gen-141
127 127 Block2        1 2022  227   7      7      0   164   Gen-164
128 128 Block2        1 2022  228   7      8      0   165   Gen-165
129 129 Block2        1 2022  229   7      9      0   177   Gen-177
130 130 Block2        1 2022  230   7     10      0   149   Gen-149
131 131 Block2        1 2022  231   7     11      0   135   Gen-135
132 132 Block2        1 2022  232   7     12      0   159   Gen-159
133 133 Block2        1 2022  233   7     13      4     4   Check-4
134 134 Block2        1 2022  234   7     14      0   145   Gen-145
135 135 Block2        1 2022  235   7     15      0   138   Gen-138
136 136 Block2        1 2022  236   7     16      0   168   Gen-168
137 137 Block2        1 2022  237   7     17      0   181   Gen-181
138 138 Block2        1 2022  238   7     18      0   200   Gen-200
139 139 Block2        1 2022  239   7     19      0   121   Gen-121
140 140 Block2        1 2022  240   7     20      0   133   Gen-133
141 141 Block2        1 2022  241   8     20      0   115   Gen-115
142 142 Block2        1 2022  242   8     19      0   137   Gen-137
143 143 Block2        1 2022  243   8     18      1     1   Check-1
144 144 Block2        1 2022  244   8     17      0   178   Gen-178
145 145 Block2        1 2022  245   8     16      0   157   Gen-157
146 146 Block2        1 2022  246   8     15      0   195   Gen-195
147 147 Block2        1 2022  247   8     14      0   120   Gen-120
148 148 Block2        1 2022  248   8     13      0   119   Gen-119
149 149 Block2        1 2022  249   8     12      0   136   Gen-136
150 150 Block2        1 2022  250   8     11      0   125   Gen-125
151 151 Block2        1 2022  251   8     10      3     3   Check-3
152 152 Block2        1 2022  252   8      9      0   142   Gen-142
153 153 Block2        1 2022  253   8      8      0   191   Gen-191
154 154 Block2        1 2022  254   8      7      0   203   Gen-203
155 155 Block2        1 2022  255   8      6      0   167   Gen-167
156 156 Block2        1 2022  256   8      5      0   172   Gen-172
157 157 Block2        1 2022  257   8      4      0   192   Gen-192
158 158 Block2        1 2022  258   8      3      0   201   Gen-201
159 159 Block2        1 2022  259   8      2      4     4   Check-4
160 160 Block2        1 2022  260   8      1      0   126   Gen-126
161 161 Block2        1 2022  261   9      1      0   146   Gen-146
162 162 Block2        1 2022  262   9      2      0   106   Gen-106
163 163 Block2        1 2022  263   9      3      0   160   Gen-160
164 164 Block2        1 2022  264   9      4      0   163   Gen-163
165 165 Block2        1 2022  265   9      5      0   154   Gen-154
166 166 Block2        1 2022  266   9      6      0   152   Gen-152
167 167 Block2        1 2022  267   9      7      2     2   Check-2
168 168 Block2        1 2022  268   9      8      0   114   Gen-114
169 169 Block2        1 2022  269   9      9      0   139   Gen-139
170 170 Block2        1 2022  270   9     10      0   185   Gen-185
171 171 Block2        1 2022  271   9     11      0   147   Gen-147
172 172 Block2        1 2022  272   9     12      0   189   Gen-189
173 173 Block2        1 2022  273   9     13      0   197   Gen-197
174 174 Block2        1 2022  274   9     14      0   188   Gen-188
175 175 Block2        1 2022  275   9     15      2     2   Check-2
176 176 Block2        1 2022  276   9     16      0   151   Gen-151
177 177 Block2        1 2022  277   9     17      0   194   Gen-194
178 178 Block2        1 2022  278   9     18      0   132   Gen-132
179 179 Block2        1 2022  279   9     19      0   105   Gen-105
180 180 Block2        1 2022  280   9     20      0   187   Gen-187
181 181 Block2        1 2022  281  10     20      1     1   Check-1
182 182 Block2        1 2022  282  10     19      0   129   Gen-129
183 183 Block2        1 2022  283  10     18      0   107   Gen-107
184 184 Block2        1 2022  284  10     17      0   190   Gen-190
185 185 Block2        1 2022  285  10     16      0   124   Gen-124
186 186 Block2        1 2022  286  10     15      0   148   Gen-148
187 187 Block2        1 2022  287  10     14      0   182   Gen-182
188 188 Block2        1 2022  288  10     13      0   116   Gen-116
189 189 Block2        1 2022  289  10     12      3     3   Check-3
190 190 Block2        1 2022  290  10     11      0   127   Gen-127
191 191 Block2        1 2022  291  10     10      0   143   Gen-143
192 192 Block2        1 2022  292  10      9      0   179   Gen-179
193 193 Block2        1 2022  293  10      8      0   117   Gen-117
194 194 Block2        1 2022  294  10      7      0   199   Gen-199
195 195 Block2        1 2022  295  10      6      0   144   Gen-144
196 196 Block2        1 2022  296  10      5      0   113   Gen-113
197 197 Block2        1 2022  297  10      4      2     2   Check-2
198 198 Block2        1 2022  298  10      3      0   150   Gen-150
199 199 Block2        1 2022  299  10      2      0   198   Gen-198
200 200 Block2        1 2022  300  10      1      0   183   Gen-183
201 201 Block2        1 2022  301  11      1      3     3   Check-3
202 202 Block2        1 2022  302  11      2      0   122   Gen-122
203 203 Block2        1 2022  303  11      3      0   170   Gen-170
204 204 Block2        1 2022  304  11      4      0   204   Gen-204
205 205 Block2        1 2022  305  11      5      0   166   Gen-166
206 206 Block2        1 2022  306  11      6      0   158   Gen-158
207 207 Block2        1 2022  307  11      7      0   174   Gen-174
208 208 Block2        1 2022  308  11      8      0   155   Gen-155
209 209 Block2        1 2022  309  11      9      4     4   Check-4
210 210 Block2        1 2022  310  11     10      0   130   Gen-130
211 211 Block2        1 2022  311  11     11      0   184   Gen-184
212 212 Block2        1 2022  312  11     12      0   111   Gen-111
213 213 Block2        1 2022  313  11     13      0   186   Gen-186
214 214 Block2        1 2022  314  11     14      0   175   Gen-175
215 215 Block2        1 2022  315  11     15      0   169   Gen-169
216 216 Block2        1 2022  316  11     16      0   171   Gen-171
217 217 Block2        1 2022  317  11     17      1     1   Check-1
218 218 Block2        1 2022  318  11     18      0   128   Gen-128
219 219 Block2        1 2022  319  11     19      0   118   Gen-118
220 220 Block2        1 2022  320  11     20      0   110   Gen-110
221 221 Block2        1 2022  321  12     20      0   134   Gen-134
222 222 Block2        1 2022  322  12     19      0   161   Gen-161
223 223 Block2        1 2022  323  12     18      0   202   Gen-202
224 224 Block2        1 2022  324  12     17      0   173   Gen-173
225 225 Block2        1 2022  325  12     16      0   123   Gen-123
226 226 Block2        1 2022  326  12     15      0   193   Gen-193
227 227 Block2        1 2022  327  12     14      2     2   Check-2
228 228 Block2        1 2022  328  12     13      0   153   Gen-153
229 229 Block2        1 2022  329  12     12      0   180   Gen-180
230 230 Block3        1 2022  330  12     11      0   243   Gen-243
231 231 Block3        1 2022  331  12     10      0   207   Gen-207
232 232 Block3        1 2022  332  12      9      0   228   Gen-228
233 233 Block3        1 2022  333  12      8      0   283   Gen-283
234 234 Block3        1 2022  334  12      7      0   224   Gen-224
235 235 Block3        1 2022  335  12      6      1     1   Check-1
236 236 Block3        1 2022  336  12      5      0   248   Gen-248
237 237 Block3        1 2022  337  12      4      0   267   Gen-267
238 238 Block3        1 2022  338  12      3      0   264   Gen-264
239 239 Block3        1 2022  339  12      2      0   259   Gen-259
240 240 Block3        1 2022  340  12      1      0   244   Gen-244
241 241 Block3        1 2022  341  13      1      0   270   Gen-270
242 242 Block3        1 2022  342  13      2      0   272   Gen-272
243 243 Block3        1 2022  343  13      3      4     4   Check-4
244 244 Block3        1 2022  344  13      4      0   279   Gen-279
245 245 Block3        1 2022  345  13      5      0   215   Gen-215
246 246 Block3        1 2022  346  13      6      0   242   Gen-242
247 247 Block3        1 2022  347  13      7      0   216   Gen-216
248 248 Block3        1 2022  348  13      8      0   274   Gen-274
249 249 Block3        1 2022  349  13      9      0   237   Gen-237
250 250 Block3        1 2022  350  13     10      0   254   Gen-254
251 251 Block3        1 2022  351  13     11      2     2   Check-2
252 252 Block3        1 2022  352  13     12      0   253   Gen-253
253 253 Block3        1 2022  353  13     13      0   251   Gen-251
254 254 Block3        1 2022  354  13     14      0   284   Gen-284
255 255 Block3        1 2022  355  13     15      0   265   Gen-265
256 256 Block3        1 2022  356  13     16      0   235   Gen-235
257 257 Block3        1 2022  357  13     17      0   222   Gen-222
258 258 Block3        1 2022  358  13     18      0   257   Gen-257
259 259 Block3        1 2022  359  13     19      4     4   Check-4
260 260 Block3        1 2022  360  13     20      0   252   Gen-252
261 261 Block3        1 2022  361  14     20      0   221   Gen-221
262 262 Block3        1 2022  362  14     19      0   276   Gen-276
263 263 Block3        1 2022  363  14     18      0   281   Gen-281
264 264 Block3        1 2022  364  14     17      0   217   Gen-217
265 265 Block3        1 2022  365  14     16      3     3   Check-3
266 266 Block3        1 2022  366  14     15      0   211   Gen-211
267 267 Block3        1 2022  367  14     14      0   229   Gen-229
268 268 Block3        1 2022  368  14     13      0   219   Gen-219
269 269 Block3        1 2022  369  14     12      0   213   Gen-213
270 270 Block3        1 2022  370  14     11      0   261   Gen-261
271 271 Block3        1 2022  371  14     10      0   240   Gen-240
272 272 Block3        1 2022  372  14      9      0   278   Gen-278
273 273 Block3        1 2022  373  14      8      4     4   Check-4
274 274 Block3        1 2022  374  14      7      0   260   Gen-260
275 275 Block3        1 2022  375  14      6      0   209   Gen-209
276 276 Block3        1 2022  376  14      5      0   208   Gen-208
277 277 Block3        1 2022  377  14      4      0   230   Gen-230
278 278 Block3        1 2022  378  14      3      0   250   Gen-250
279 279 Block3        1 2022  379  14      2      0   206   Gen-206
280 280 Block3        1 2022  380  14      1      0   273   Gen-273
281 281 Block3        1 2022  381  15      1      0   269   Gen-269
282 282 Block3        1 2022  382  15      2      0   271   Gen-271
283 283 Block3        1 2022  383  15      3      0   246   Gen-246
284 284 Block3        1 2022  384  15      4      0   226   Gen-226
285 285 Block3        1 2022  385  15      5      1     1   Check-1
286 286 Block3        1 2022  386  15      6      0   280   Gen-280
287 287 Block3        1 2022  387  15      7      0   212   Gen-212
288 288 Block3        1 2022  388  15      8      0   205   Gen-205
289 289 Block3        1 2022  389  15      9      0   263   Gen-263
290 290 Block3        1 2022  390  15     10      0   268   Gen-268
291 291 Block3        1 2022  391  15     11      0   266   Gen-266
292 292 Block3        1 2022  392  15     12      0   247   Gen-247
293 293 Block3        1 2022  393  15     13      1     1   Check-1
294 294 Block3        1 2022  394  15     14      0   225   Gen-225
295 295 Block3        1 2022  395  15     15      0   236   Gen-236
296 296 Block3        1 2022  396  15     16      0   262   Gen-262
297 297 Block3        1 2022  397  15     17      0   249   Gen-249
298 298 Block3        1 2022  398  15     18      0   245   Gen-245
299 299 Block3        1 2022  399  15     19      0   227   Gen-227
300 300 Block3        1 2022  400  15     20      0   210   Gen-210
301 301 Block3        1 2022  401  16     20      0   233   Gen-233
302 302 Block3        1 2022  402  16     19      0   275   Gen-275
303 303 Block3        1 2022  403  16     18      3     3   Check-3
304 304 Block3        1 2022  404  16     17      0   258   Gen-258
305 305 Block3        1 2022  405  16     16      0   255   Gen-255
306 306 Block3        1 2022  406  16     15      0   234   Gen-234
307 307 Block3        1 2022  407  16     14      0   282   Gen-282
308 308 Block3        1 2022  408  16     13      0   223   Gen-223
309 309 Block3        1 2022  409  16     12      0   232   Gen-232
310 310 Block3        1 2022  410  16     11      0   238   Gen-238
311 311 Block3        1 2022  411  16     10      1     1   Check-1
312 312 Block3        1 2022  412  16      9      0   256   Gen-256
313 313 Block3        1 2022  413  16      8      0   239   Gen-239
314 314 Block3        1 2022  414  16      7      0   277   Gen-277
315 315 Block3        1 2022  415  16      6      0   241   Gen-241
316 316 Block3        1 2022  416  16      5      0   231   Gen-231
317 317 Block3        1 2022  417  16      4      0   220   Gen-220
318 318 Block3        1 2022  418  16      3      0   218   Gen-218
319 319 Block3        1 2022  419  16      2      2     2   Check-2
320 320 Block3        1 2022  420  16      1      0   214   Gen-214

$layout
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" style="display: block; margin: auto;" />

The main difference between using the FielDHub Shiny app and using the
standalone function `diagonal_arrangement()` is that the standalone
function will allocate filler only if it is necessary, while in R Shiny,
filler plots are generated automatically. In cases where users include
fillers, either between or after experiments, the Shiny app is
preferable for filling and visualizing all field plots.

### Partially Replicated Design Example

Partially replicated designs are commonly employed in early generation
field trials. This type of design is characterized by replication of a
portion of the entries, with the remaining entries only appearing once
in the experiment. As an example, considered a field trial with 288
plots containing 75 entries appearing two times each, and 138 entries
only appearing once. This field trials is arranged in a field of 16 rows
by 18 columns.

![](pREPExample.png)

In the figure above, green plots contain replicated entries, and yellow
plots contain entries that only appear once.

Instead of using the Shiny FielDHub app, users can use the standalone
FielDHub function `partially_replicated()`. The partially replicated
layout described above can be produced through scripting as follows. As
noted in the previous example, to obtain identical results between the
script and the Shiny app, users need to use the same seed number, which,
in this case, is 77.

``` r
pREP <- partially_replicated(
  nrows = 16, 
  ncols = 18,  
  repGens = c(138,75),
  repUnits = c(1,2),
  planter = "serpentine", 
  plotNumber = 1,
  exptName = "ExptA",
  locationNames = "FARGO",
  seed = 77
)
```

Users can access returned values from `partially_replicated()` as
follows,

``` r
pREP$infoDesign
$rows
[1] 16

$columns
[1] 18

$treatments_with_reps
[1] 75

$treatments_with_no_reps
[1] 138

$locations
[1] 1

$planter
[1] "serpentine"

$seed
[1] 77

$id_design
[1] 13
```

``` r
 head(pREP$fieldBook, 12)
   ID  EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
1   1 ExptA    FARGO 2022    1   1      1      0    80       G80
2   2 ExptA    FARGO 2022    2   1      2     49    49       G49
3   3 ExptA    FARGO 2022    3   1      3     15    15       G15
4   4 ExptA    FARGO 2022    4   1      4     44    44       G44
5   5 ExptA    FARGO 2022    5   1      5      0   185      G185
6   6 ExptA    FARGO 2022    6   1      6      9     9        G9
7   7 ExptA    FARGO 2022    7   1      7      0   133      G133
8   8 ExptA    FARGO 2022    8   1      8     58    58       G58
9   9 ExptA    FARGO 2022    9   1      9      4     4        G4
10 10 ExptA    FARGO 2022   10   1     10      0   113      G113
11 11 ExptA    FARGO 2022   11   1     11      0   190      G190
12 12 ExptA    FARGO 2022   12   1     12      0   148      G148
```

Users can plot the layout design from `partially_replicated()` using the
function `plot()` as follows,

``` r
plot(pREP)
$field_book
     ID  EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT
1     1 ExptA    FARGO 2022    1   1      1      0    80       G80
2     2 ExptA    FARGO 2022    2   1      2     49    49       G49
3     3 ExptA    FARGO 2022    3   1      3     15    15       G15
4     4 ExptA    FARGO 2022    4   1      4     44    44       G44
5     5 ExptA    FARGO 2022    5   1      5      0   185      G185
6     6 ExptA    FARGO 2022    6   1      6      9     9        G9
7     7 ExptA    FARGO 2022    7   1      7      0   133      G133
8     8 ExptA    FARGO 2022    8   1      8     58    58       G58
9     9 ExptA    FARGO 2022    9   1      9      4     4        G4
10   10 ExptA    FARGO 2022   10   1     10      0   113      G113
11   11 ExptA    FARGO 2022   11   1     11      0   190      G190
12   12 ExptA    FARGO 2022   12   1     12      0   148      G148
13   13 ExptA    FARGO 2022   13   1     13     39    39       G39
14   14 ExptA    FARGO 2022   14   1     14     17    17       G17
15   15 ExptA    FARGO 2022   15   1     15     20    20       G20
16   16 ExptA    FARGO 2022   16   1     16      0   207      G207
17   17 ExptA    FARGO 2022   17   1     17      0   202      G202
18   18 ExptA    FARGO 2022   18   1     18     42    42       G42
19   19 ExptA    FARGO 2022   19   2     18      0   111      G111
20   20 ExptA    FARGO 2022   20   2     17      0   201      G201
21   21 ExptA    FARGO 2022   21   2     16     50    50       G50
22   22 ExptA    FARGO 2022   22   2     15     52    52       G52
23   23 ExptA    FARGO 2022   23   2     14      0   197      G197
24   24 ExptA    FARGO 2022   24   2     13     48    48       G48
25   25 ExptA    FARGO 2022   25   2     12     61    61       G61
26   26 ExptA    FARGO 2022   26   2     11      0   178      G178
27   27 ExptA    FARGO 2022   27   2     10      0   193      G193
28   28 ExptA    FARGO 2022   28   2      9      0   180      G180
29   29 ExptA    FARGO 2022   29   2      8      0   184      G184
30   30 ExptA    FARGO 2022   30   2      7     26    26       G26
31   31 ExptA    FARGO 2022   31   2      6      0   208      G208
32   32 ExptA    FARGO 2022   32   2      5     52    52       G52
33   33 ExptA    FARGO 2022   33   2      4     49    49       G49
34   34 ExptA    FARGO 2022   34   2      3      0   206      G206
35   35 ExptA    FARGO 2022   35   2      2     68    68       G68
36   36 ExptA    FARGO 2022   36   2      1     16    16       G16
37   37 ExptA    FARGO 2022   37   3      1     69    69       G69
38   38 ExptA    FARGO 2022   38   3      2     28    28       G28
39   39 ExptA    FARGO 2022   39   3      3      0   196      G196
40   40 ExptA    FARGO 2022   40   3      4      0   154      G154
41   41 ExptA    FARGO 2022   41   3      5      0   146      G146
42   42 ExptA    FARGO 2022   42   3      6     75    75       G75
43   43 ExptA    FARGO 2022   43   3      7      0   189      G189
44   44 ExptA    FARGO 2022   44   3      8     36    36       G36
45   45 ExptA    FARGO 2022   45   3      9     54    54       G54
46   46 ExptA    FARGO 2022   46   3     10     72    72       G72
47   47 ExptA    FARGO 2022   47   3     11     31    31       G31
48   48 ExptA    FARGO 2022   48   3     12     33    33       G33
49   49 ExptA    FARGO 2022   49   3     13     53    53       G53
50   50 ExptA    FARGO 2022   50   3     14     67    67       G67
51   51 ExptA    FARGO 2022   51   3     15      0   175      G175
52   52 ExptA    FARGO 2022   52   3     16     27    27       G27
53   53 ExptA    FARGO 2022   53   3     17      2     2        G2
54   54 ExptA    FARGO 2022   54   3     18      0   130      G130
55   55 ExptA    FARGO 2022   55   4     18      0   203      G203
56   56 ExptA    FARGO 2022   56   4     17     46    46       G46
57   57 ExptA    FARGO 2022   57   4     16      0   212      G212
58   58 ExptA    FARGO 2022   58   4     15     72    72       G72
59   59 ExptA    FARGO 2022   59   4     14     12    12       G12
60   60 ExptA    FARGO 2022   60   4     13      0   140      G140
61   61 ExptA    FARGO 2022   61   4     12      0   105      G105
62   62 ExptA    FARGO 2022   62   4     11      0   112      G112
63   63 ExptA    FARGO 2022   63   4     10      0   165      G165
64   64 ExptA    FARGO 2022   64   4      9     13    13       G13
65   65 ExptA    FARGO 2022   65   4      8     39    39       G39
66   66 ExptA    FARGO 2022   66   4      7     14    14       G14
67   67 ExptA    FARGO 2022   67   4      6      0   164      G164
68   68 ExptA    FARGO 2022   68   4      5     60    60       G60
69   69 ExptA    FARGO 2022   69   4      4      0   135      G135
70   70 ExptA    FARGO 2022   70   4      3      0   125      G125
71   71 ExptA    FARGO 2022   71   4      2      0   129      G129
72   72 ExptA    FARGO 2022   72   4      1     73    73       G73
73   73 ExptA    FARGO 2022   73   5      1      0    77       G77
74   74 ExptA    FARGO 2022   74   5      2      0   205      G205
75   75 ExptA    FARGO 2022   75   5      3     37    37       G37
76   76 ExptA    FARGO 2022   76   5      4     62    62       G62
77   77 ExptA    FARGO 2022   77   5      5      0   106      G106
78   78 ExptA    FARGO 2022   78   5      6     29    29       G29
79   79 ExptA    FARGO 2022   79   5      7      0   138      G138
80   80 ExptA    FARGO 2022   80   5      8     53    53       G53
81   81 ExptA    FARGO 2022   81   5      9      0   118      G118
82   82 ExptA    FARGO 2022   82   5     10     35    35       G35
83   83 ExptA    FARGO 2022   83   5     11      0   132      G132
84   84 ExptA    FARGO 2022   84   5     12     54    54       G54
85   85 ExptA    FARGO 2022   85   5     13      0   166      G166
86   86 ExptA    FARGO 2022   86   5     14      0    81       G81
87   87 ExptA    FARGO 2022   87   5     15     47    47       G47
88   88 ExptA    FARGO 2022   88   5     16     41    41       G41
89   89 ExptA    FARGO 2022   89   5     17      0   109      G109
90   90 ExptA    FARGO 2022   90   5     18      0    76       G76
91   91 ExptA    FARGO 2022   91   6     18     45    45       G45
92   92 ExptA    FARGO 2022   92   6     17      0   100      G100
93   93 ExptA    FARGO 2022   93   6     16      0   159      G159
94   94 ExptA    FARGO 2022   94   6     15      0   117      G117
95   95 ExptA    FARGO 2022   95   6     14      0   115      G115
96   96 ExptA    FARGO 2022   96   6     13      0   126      G126
97   97 ExptA    FARGO 2022   97   6     12     55    55       G55
98   98 ExptA    FARGO 2022   98   6     11     74    74       G74
99   99 ExptA    FARGO 2022   99   6     10      0   194      G194
100 100 ExptA    FARGO 2022  100   6      9      0    98       G98
101 101 ExptA    FARGO 2022  101   6      8     64    64       G64
102 102 ExptA    FARGO 2022  102   6      7      0    91       G91
103 103 ExptA    FARGO 2022  103   6      6      0   143      G143
104 104 ExptA    FARGO 2022  104   6      5     31    31       G31
105 105 ExptA    FARGO 2022  105   6      4      0   177      G177
106 106 ExptA    FARGO 2022  106   6      3     43    43       G43
107 107 ExptA    FARGO 2022  107   6      2      8     8        G8
108 108 ExptA    FARGO 2022  108   6      1     24    24       G24
109 109 ExptA    FARGO 2022  109   7      1      0    97       G97
110 110 ExptA    FARGO 2022  110   7      2      0    82       G82
111 111 ExptA    FARGO 2022  111   7      3     36    36       G36
112 112 ExptA    FARGO 2022  112   7      4     29    29       G29
113 113 ExptA    FARGO 2022  113   7      5     56    56       G56
114 114 ExptA    FARGO 2022  114   7      6     35    35       G35
115 115 ExptA    FARGO 2022  115   7      7     50    50       G50
116 116 ExptA    FARGO 2022  116   7      8      6     6        G6
117 117 ExptA    FARGO 2022  117   7      9     75    75       G75
118 118 ExptA    FARGO 2022  118   7     10      0    85       G85
119 119 ExptA    FARGO 2022  119   7     11     58    58       G58
120 120 ExptA    FARGO 2022  120   7     12     17    17       G17
121 121 ExptA    FARGO 2022  121   7     13     66    66       G66
122 122 ExptA    FARGO 2022  122   7     14      0   172      G172
123 123 ExptA    FARGO 2022  123   7     15      0   145      G145
124 124 ExptA    FARGO 2022  124   7     16     63    63       G63
125 125 ExptA    FARGO 2022  125   7     17     44    44       G44
126 126 ExptA    FARGO 2022  126   7     18      3     3        G3
127 127 ExptA    FARGO 2022  127   8     18      0   209      G209
128 128 ExptA    FARGO 2022  128   8     17      4     4        G4
129 129 ExptA    FARGO 2022  129   8     16      0   162      G162
130 130 ExptA    FARGO 2022  130   8     15     40    40       G40
131 131 ExptA    FARGO 2022  131   8     14      0    83       G83
132 132 ExptA    FARGO 2022  132   8     13      1     1        G1
133 133 ExptA    FARGO 2022  133   8     12      0   163      G163
134 134 ExptA    FARGO 2022  134   8     11     38    38       G38
135 135 ExptA    FARGO 2022  135   8     10      0   147      G147
136 136 ExptA    FARGO 2022  136   8      9      0   195      G195
137 137 ExptA    FARGO 2022  137   8      8      0   110      G110
138 138 ExptA    FARGO 2022  138   8      7     13    13       G13
139 139 ExptA    FARGO 2022  139   8      6     69    69       G69
140 140 ExptA    FARGO 2022  140   8      5      0   157      G157
141 141 ExptA    FARGO 2022  141   8      4     20    20       G20
142 142 ExptA    FARGO 2022  142   8      3     57    57       G57
143 143 ExptA    FARGO 2022  143   8      2      0   173      G173
144 144 ExptA    FARGO 2022  144   8      1      1     1        G1
145 145 ExptA    FARGO 2022  145   9      1     70    70       G70
146 146 ExptA    FARGO 2022  146   9      2     38    38       G38
147 147 ExptA    FARGO 2022  147   9      3     70    70       G70
148 148 ExptA    FARGO 2022  148   9      4     12    12       G12
149 149 ExptA    FARGO 2022  149   9      5      0    88       G88
150 150 ExptA    FARGO 2022  150   9      6      0    99       G99
151 151 ExptA    FARGO 2022  151   9      7      0   179      G179
152 152 ExptA    FARGO 2022  152   9      8      0   210      G210
153 153 ExptA    FARGO 2022  153   9      9     37    37       G37
154 154 ExptA    FARGO 2022  154   9     10     22    22       G22
155 155 ExptA    FARGO 2022  155   9     11      0   155      G155
156 156 ExptA    FARGO 2022  156   9     12      0   142      G142
157 157 ExptA    FARGO 2022  157   9     13      0   116      G116
158 158 ExptA    FARGO 2022  158   9     14      0   161      G161
159 159 ExptA    FARGO 2022  159   9     15      0   137      G137
160 160 ExptA    FARGO 2022  160   9     16     10    10       G10
161 161 ExptA    FARGO 2022  161   9     17     19    19       G19
162 162 ExptA    FARGO 2022  162   9     18     30    30       G30
163 163 ExptA    FARGO 2022  163  10     18     59    59       G59
164 164 ExptA    FARGO 2022  164  10     17      0   139      G139
165 165 ExptA    FARGO 2022  165  10     16     40    40       G40
166 166 ExptA    FARGO 2022  166  10     15     18    18       G18
167 167 ExptA    FARGO 2022  167  10     14      0   107      G107
168 168 ExptA    FARGO 2022  168  10     13     32    32       G32
169 169 ExptA    FARGO 2022  169  10     12      0   122      G122
170 170 ExptA    FARGO 2022  170  10     11     25    25       G25
171 171 ExptA    FARGO 2022  171  10     10     74    74       G74
172 172 ExptA    FARGO 2022  172  10      9      6     6        G6
173 173 ExptA    FARGO 2022  173  10      8     33    33       G33
174 174 ExptA    FARGO 2022  174  10      7     73    73       G73
175 175 ExptA    FARGO 2022  175  10      6      0   167      G167
176 176 ExptA    FARGO 2022  176  10      5      0   213      G213
177 177 ExptA    FARGO 2022  177  10      4      0   134      G134
178 178 ExptA    FARGO 2022  178  10      3      0   104      G104
179 179 ExptA    FARGO 2022  179  10      2      0   101      G101
180 180 ExptA    FARGO 2022  180  10      1      0   144      G144
181 181 ExptA    FARGO 2022  181  11      1     19    19       G19
182 182 ExptA    FARGO 2022  182  11      2      7     7        G7
183 183 ExptA    FARGO 2022  183  11      3      0    87       G87
184 184 ExptA    FARGO 2022  184  11      4      7     7        G7
185 185 ExptA    FARGO 2022  185  11      5      0    93       G93
186 186 ExptA    FARGO 2022  186  11      6      0    86       G86
187 187 ExptA    FARGO 2022  187  11      7     26    26       G26
188 188 ExptA    FARGO 2022  188  11      8     22    22       G22
189 189 ExptA    FARGO 2022  189  11      9      0   151      G151
190 190 ExptA    FARGO 2022  190  11     10     67    67       G67
191 191 ExptA    FARGO 2022  191  11     11     30    30       G30
192 192 ExptA    FARGO 2022  192  11     12      5     5        G5
193 193 ExptA    FARGO 2022  193  11     13      0    92       G92
194 194 ExptA    FARGO 2022  194  11     14     48    48       G48
195 195 ExptA    FARGO 2022  195  11     15     51    51       G51
196 196 ExptA    FARGO 2022  196  11     16      0   150      G150
197 197 ExptA    FARGO 2022  197  11     17     59    59       G59
198 198 ExptA    FARGO 2022  198  11     18     41    41       G41
199 199 ExptA    FARGO 2022  199  12     18      0   114      G114
200 200 ExptA    FARGO 2022  200  12     17     27    27       G27
201 201 ExptA    FARGO 2022  201  12     16      0   170      G170
202 202 ExptA    FARGO 2022  202  12     15      0   128      G128
203 203 ExptA    FARGO 2022  203  12     14     42    42       G42
204 204 ExptA    FARGO 2022  204  12     13     51    51       G51
205 205 ExptA    FARGO 2022  205  12     12     57    57       G57
206 206 ExptA    FARGO 2022  206  12     11      0   156      G156
207 207 ExptA    FARGO 2022  207  12     10     46    46       G46
208 208 ExptA    FARGO 2022  208  12      9      0    90       G90
209 209 ExptA    FARGO 2022  209  12      8      0    84       G84
210 210 ExptA    FARGO 2022  210  12      7     28    28       G28
211 211 ExptA    FARGO 2022  211  12      6      0   169      G169
212 212 ExptA    FARGO 2022  212  12      5     56    56       G56
213 213 ExptA    FARGO 2022  213  12      4      0   174      G174
214 214 ExptA    FARGO 2022  214  12      3      8     8        G8
215 215 ExptA    FARGO 2022  215  12      2     15    15       G15
216 216 ExptA    FARGO 2022  216  12      1      0   211      G211
217 217 ExptA    FARGO 2022  217  13      1      0   187      G187
218 218 ExptA    FARGO 2022  218  13      2     66    66       G66
219 219 ExptA    FARGO 2022  219  13      3      0   153      G153
220 220 ExptA    FARGO 2022  220  13      4     24    24       G24
221 221 ExptA    FARGO 2022  221  13      5     21    21       G21
222 222 ExptA    FARGO 2022  222  13      6     23    23       G23
223 223 ExptA    FARGO 2022  223  13      7     65    65       G65
224 224 ExptA    FARGO 2022  224  13      8      0   199      G199
225 225 ExptA    FARGO 2022  225  13      9     34    34       G34
226 226 ExptA    FARGO 2022  226  13     10     64    64       G64
227 227 ExptA    FARGO 2022  227  13     11     11    11       G11
228 228 ExptA    FARGO 2022  228  13     12      0   191      G191
229 229 ExptA    FARGO 2022  229  13     13      0    94       G94
230 230 ExptA    FARGO 2022  230  13     14      0    96       G96
231 231 ExptA    FARGO 2022  231  13     15      0   131      G131
232 232 ExptA    FARGO 2022  232  13     16      0   181      G181
233 233 ExptA    FARGO 2022  233  13     17      0   186      G186
234 234 ExptA    FARGO 2022  234  13     18      0   103      G103
235 235 ExptA    FARGO 2022  235  14     18     10    10       G10
236 236 ExptA    FARGO 2022  236  14     17      0   152      G152
237 237 ExptA    FARGO 2022  237  14     16      0   168      G168
238 238 ExptA    FARGO 2022  238  14     15      0   160      G160
239 239 ExptA    FARGO 2022  239  14     14     47    47       G47
240 240 ExptA    FARGO 2022  240  14     13     23    23       G23
241 241 ExptA    FARGO 2022  241  14     12      0    89       G89
242 242 ExptA    FARGO 2022  242  14     11      0   149      G149
243 243 ExptA    FARGO 2022  243  14     10     43    43       G43
244 244 ExptA    FARGO 2022  244  14      9      0    78       G78
245 245 ExptA    FARGO 2022  245  14      8      0   183      G183
246 246 ExptA    FARGO 2022  246  14      7      0   136      G136
247 247 ExptA    FARGO 2022  247  14      6      5     5        G5
248 248 ExptA    FARGO 2022  248  14      5      3     3        G3
249 249 ExptA    FARGO 2022  249  14      4     16    16       G16
250 250 ExptA    FARGO 2022  250  14      3      0   200      G200
251 251 ExptA    FARGO 2022  251  14      2      0   108      G108
252 252 ExptA    FARGO 2022  252  14      1     68    68       G68
253 253 ExptA    FARGO 2022  253  15      1     18    18       G18
254 254 ExptA    FARGO 2022  254  15      2      0   121      G121
255 255 ExptA    FARGO 2022  255  15      3      9     9        G9
256 256 ExptA    FARGO 2022  256  15      4      0   123      G123
257 257 ExptA    FARGO 2022  257  15      5      0   171      G171
258 258 ExptA    FARGO 2022  258  15      6     60    60       G60
259 259 ExptA    FARGO 2022  259  15      7     11    11       G11
260 260 ExptA    FARGO 2022  260  15      8      0   204      G204
261 261 ExptA    FARGO 2022  261  15      9      2     2        G2
262 262 ExptA    FARGO 2022  262  15     10      0   182      G182
263 263 ExptA    FARGO 2022  263  15     11      0   119      G119
264 264 ExptA    FARGO 2022  264  15     12     65    65       G65
265 265 ExptA    FARGO 2022  265  15     13      0   198      G198
266 266 ExptA    FARGO 2022  266  15     14     63    63       G63
267 267 ExptA    FARGO 2022  267  15     15      0   188      G188
268 268 ExptA    FARGO 2022  268  15     16     34    34       G34
269 269 ExptA    FARGO 2022  269  15     17      0   124      G124
270 270 ExptA    FARGO 2022  270  15     18     55    55       G55
271 271 ExptA    FARGO 2022  271  16     18     71    71       G71
272 272 ExptA    FARGO 2022  272  16     17     14    14       G14
273 273 ExptA    FARGO 2022  273  16     16     25    25       G25
274 274 ExptA    FARGO 2022  274  16     15     21    21       G21
275 275 ExptA    FARGO 2022  275  16     14     32    32       G32
276 276 ExptA    FARGO 2022  276  16     13      0   141      G141
277 277 ExptA    FARGO 2022  277  16     12      0   158      G158
278 278 ExptA    FARGO 2022  278  16     11     61    61       G61
279 279 ExptA    FARGO 2022  279  16     10      0    79       G79
280 280 ExptA    FARGO 2022  280  16      9      0    95       G95
281 281 ExptA    FARGO 2022  281  16      8      0   120      G120
282 282 ExptA    FARGO 2022  282  16      7      0   102      G102
283 283 ExptA    FARGO 2022  283  16      6     62    62       G62
284 284 ExptA    FARGO 2022  284  16      5     71    71       G71
285 285 ExptA    FARGO 2022  285  16      4      0   127      G127
286 286 ExptA    FARGO 2022  286  16      3      0   192      G192
287 287 ExptA    FARGO 2022  287  16      2     45    45       G45
288 288 ExptA    FARGO 2022  288  16      1      0   176      G176

$layout
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" style="display: block; margin: auto;" />

To see more examples, please go to
<https://didiermurillof.github.io/FielDHub/reference/index.html>.
