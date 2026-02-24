# Split-Plot Design

This vignette shows how to generate a **Split-Plot Design** using both
the FielDHub Shiny App and the scripting function
[`split_plot()`](https://didiermurillof.github.io/FielDHub/reference/split_plot.md)
from the `FielDHub` package.

### 1. Using the FielDHub Shiny App

To launch the app you need to run either

``` r
FielDHub::run_app()
```

or

``` r
library(FielDHub)
run_app()
```

Once the app is running, go to **Other Designs** \> **Split-Plot
Design**

Then, follow the following steps where we show how to generate this kind
of design by an example with 3 whole plots, 2 sub-plots and 3 reps. We
will run this experiment in just one location.

## Inputs

1.  **Import entries’ list?** Choose whether to import a list with entry
    numbers and names for treatments.
    - If the selection is `No`, that means the app is going to generate
      synthetic data for entries and names of the treatment/genotypes
      based on the user inputs.

    - If the selection is `Yes`, the entries list must fulfill a
      specific format and must be a `.csv` file. The file must have the
      single column `TREATMENT`, containing a list of unique names that
      identify each treatment/genotype. Duplicate values are not
      allowed, all entries must be unique. In the following, we show an
      example of the entries list format. This example has an entry list
      with 5 whole-plots and 3 sub-plots.

| WHOLEPLOT | SUBPLOT |
|:----------|:--------|
| A         | 1       |
| B         | 2       |
| C         | 3       |
| D         |         |
| E         |         |

2.  Choose whether to use the split-plot design in a RCBD or CRD with
    the **Select SPD Type** box.

3.  Set the number of whole-plots in the design with the **Whole-plots**
    box. Set it to `5`.

4.  Set the number of sub-plots contained with the **Sub-plots Within
    Whole-plots** box. Set it to `3`.

5.  Select the number of replications of these treatments with the
    **Input \# of Full Reps** box. Set it to `4`.

6.  Enter the number of locations in **Input \# of Locations**. We will
    run this experiment over a single location, so set it to `1`.

7.  Select `serpentine` or `cartesian` in the **Plot Order Layout**. For
    this example we will use the default `serpentine` layout.

8.  Enter the starting plot number in the **Starting Plot Number** box.
    If the experiment has multiple locations, you must enter a comma
    separated list of numbers the length of the number of locations for
    the input to be valid. For this case, set it to `101`.

9.  Enter a name for the location of the experiment in the **Input
    Location** box. If there are multiple locations, each name must be
    in a comma separated list. Set it to `"FARGO"`.

10. To ensure that randomizations are consistent across sessions, we can
    set a random seed in the box labeled **random seed**. In this
    example, we will set it to `1237`.

11. Once we have entered the information for our experiment on the left
    side panel, click the **Run!** button to run the design.

### Outputs

After you run a split-plot design in FielDHub, there are several ways to
display the information contained in the field book.

#### Field Layout

When you first click the run button on a split-plot design, FielDHub
displays the Field Layout tab, which shows the entries and their
arrangement in the field. In the box below the display, you can change
the layout of the field. You can also display a heatmap over the field
by changing **Type of Plot** to `Heatmap`. To view a heatmap, you must
first simulate an experiment over the described field with the
**Simulate!** button. A pop-up window will appear where you can enter
what variable you want to simulate along with minimum and maximum
values.

#### Field Book

The **Field Book** displays all the information on the experimental
design in a table format. It contains the specific plot number and the
row and column address of each entry, as well as the corresponding
treatment on that plot. This table is searchable, and we can filter the
data in relevant columns. If we have simulated data for a heatmap, an
additional column for that variable appears in the field book.

### 2. Using the `FielDHub` function: `split_plot()`

You can run the same design with a function in the FielDHub package,
[`split_plot()`](https://didiermurillof.github.io/FielDHub/reference/split_plot.md).

First, you need to load the `FielDHub` package by typing

``` r
library(FielDHub)
```

Then, you can enter the information describing the above design like
this:

``` r
spd <- split_plot(
  wp = 5,
  sp = 3,
  reps = 4, 
  type = 2, 
  plotNumber = 101, 
  locationNames = "FARGO",
  l = 1,
  seed = 1240
) 
```

##### Details on the inputs entered in `split_plot()` above

The description for the inputs that we used to generate the design,

- `wp = 5` is the number of whole-plots.
- `sp = 3` is the number of sub-plots.
- `reps = 4` is the number of reps
- `type = 2` CRD or RCBD, 1 or 2 respectively
- `l = 1` is the number of locations.
- `plotNumber = 101` is the starting plot number.
- `locationNames = "FARGO"` is an optional name for each location.
- `seed = 1240` is the random seed to replicate identical
  randomizations.

#### Print `spd` object

``` r
print(spd)
```

    Split Plot Design 

    Information on the design parameters: 
    List of 7
     $ WholePlots    : int [1:5] 1 2 3 4 5
     $ SubPlots      : int [1:3] 1 2 3
     $ locationNumber: num 1
     $ locationNames : chr "FARGO"
     $ plotNumbers   : num 101
     $ typeDesign    : chr "RCBD"
     $ seed          : num 1240

     10 First observations of the data frame with the split_plot field book: 
       ID LOCATION PLOT REP WHOLE_PLOT SUB_PLOT TRT_COMB
    1   1    FARGO  101   1          2        2      2|2
    2   2    FARGO  101   1          2        1      2|1
    3   3    FARGO  101   1          2        3      2|3
    4   4    FARGO  102   1          4        3      4|3
    5   5    FARGO  102   1          4        2      4|2
    6   6    FARGO  102   1          4        1      4|1
    7   7    FARGO  103   1          1        1      1|1
    8   8    FARGO  103   1          1        3      1|3
    9   9    FARGO  103   1          1        2      1|2
    10 10    FARGO  104   1          3        3      3|3

#### Access to `spd` object

The
[`split_plot()`](https://didiermurillof.github.io/FielDHub/reference/split_plot.md)
function returns a list consisting of all the information displayed in
the output tabs in the FielDHub app: design information, plot layout,
plot numbering, entries list, and field book. These are accessible by
the `$` operator, i.e. `spd$layoutRandom` or `spd$fieldBook`.

`spd$fieldBook` is a list containing information about every plot in the
field, with information about the location of the plot and the treatment
in each plot. As seen in the output below, the field book has columns
for `ID`, `LOCATION`, `PLOT`, `REP`, `WHOLE_PLOT`, `SUB_PLOT`, and
`TRT_COMB`.

``` r
field_book <- spd$fieldBook
head(spd$fieldBook, 10)
```

       ID LOCATION PLOT REP WHOLE_PLOT SUB_PLOT TRT_COMB
    1   1    FARGO  101   1          2        2      2|2
    2   2    FARGO  101   1          2        1      2|1
    3   3    FARGO  101   1          2        3      2|3
    4   4    FARGO  102   1          4        3      4|3
    5   5    FARGO  102   1          4        2      4|2
    6   6    FARGO  102   1          4        1      4|1
    7   7    FARGO  103   1          1        1      1|1
    8   8    FARGO  103   1          1        3      1|3
    9   9    FARGO  103   1          1        2      1|2
    10 10    FARGO  104   1          3        3      3|3

#### Plot the field layout

For plotting the layout in function of the coordinates `ROW` and
`COLUMN`, you can use the the generic function
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) as follows,

``` r
plot(spd)
```

![](split_plot_files/figure-html/unnamed-chunk-12-1.png)

  
  
  
