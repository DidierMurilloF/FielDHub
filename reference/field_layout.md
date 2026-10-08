# Field layout of a design

Returns the field book of a design with the `ROW` and `COLUMN` of every
plot, for the layout, planter and stacking chosen. This is the field
book that [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws
and that the app exports.

The field books of
[`CRD()`](https://didiermurillof.github.io/FielDHub/reference/CRD.md),
[`RCBD()`](https://didiermurillof.github.io/FielDHub/reference/RCBD.md),
[`latin_square()`](https://didiermurillof.github.io/FielDHub/reference/latin_square.md),
[`full_factorial()`](https://didiermurillof.github.io/FielDHub/reference/full_factorial.md),
[`split_plot()`](https://didiermurillof.github.io/FielDHub/reference/split_plot.md),
[`split_split_plot()`](https://didiermurillof.github.io/FielDHub/reference/split_split_plot.md),
[`strip_plot()`](https://didiermurillof.github.io/FielDHub/reference/strip_plot.md),
[`incomplete_blocks()`](https://didiermurillof.github.io/FielDHub/reference/incomplete_blocks.md),
[`row_column()`](https://didiermurillof.github.io/FielDHub/reference/row_column.md)
and the lattice designs have no field coordinates: their plots can be
arranged in several ways, and the layouts that renumber the plots along
the planting path also change `PLOT`. The other designs place their
plots when they are built; their field book already has `ROW` and
`COLUMN`, and it is returned as it is.

## Usage

``` r
field_layout(x, layout = 1, planter = "serpentine", stacked = "vertical")
```

## Arguments

- x:

  A design created by a FielDHub function.

- layout:

  Layout option, a whole number. The options depend on the design, the
  planter and the stacking; an unavailable option is an error that lists
  the available ones.

- planter:

  Order in which the plots are planted: `"serpentine"` (by default) or
  `"cartesian"`.

- stacked:

  How the reps are arranged: `"vertical"` (by default), `"horizontal"`,
  or `"grid_panel"` for designs in incomplete blocks and split plots in
  complete blocks with more than two reps.

## Value

A data frame: the field book of every location with the columns `ID`,
`LOCATION`, `PLOT`, `ROW` and `COLUMN` first.

## Author

Didier Murillo \[aut\]

## Examples

``` r
rcbd <- RCBD(t = 6, reps = 3, plotNumber = 101, seed = 1)
head(field_layout(rcbd))
#>   ID LOCATION PLOT ROW COLUMN REP TREATMENT
#> 1  1     loc1  101   1      1   1        T1
#> 2  2     loc1  102   1      2   1        T4
#> 3  3     loc1  103   1      3   1        T3
#> 4  4     loc1  104   1      4   1        T6
#> 5  5     loc1  105   1      5   1        T2
#> 6  6     loc1  106   1      6   1        T5
# Blocks side by side, plots planted row by row
head(field_layout(rcbd, stacked = "horizontal", planter = "cartesian"))
#>   ID LOCATION PLOT ROW COLUMN REP TREATMENT
#> 1  1     loc1  101   1      1   1        T1
#> 2  2     loc1  102   2      1   1        T4
#> 3  3     loc1  103   3      1   1        T3
#> 4  4     loc1  104   4      1   1        T6
#> 5  5     loc1  105   5      1   1        T2
#> 6  6     loc1  106   6      1   1        T5
```
