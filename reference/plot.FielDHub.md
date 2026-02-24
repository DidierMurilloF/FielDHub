# Plot a `FielDHub` object

Draw a field layout plot for a `FielDHub` object.

## Usage

``` r
# S3 method for class 'FielDHub'
plot(x, ...)
```

## Arguments

- x:

  a object inheriting from class `FielDHub`

- ...:

  further arguments passed to utility function `plot_layout()`.

  - `layout` a integer. Options available depend on the type of design
    and its characteristics

  - `l` a integer to specify the location to plot.

  - `planter` it can be `serpentine` or `cartesian`.

  - `stacked` it can be `vertical` or `horizontal` stacked layout.

## Value

- a plot object inheriting from class `fieldLayout`

- `field_book` a data frame with the fieldbook that includes the
  coordinates ROW and COLUMN.

## Author

Didier Murillo \[aut\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Plot a RCBD design with 24 treatments and 3 reps.
s <- RCBD(t = 24, reps = 3, plotNumber = 101, seed = 12)
plot(s)
} # }
```
