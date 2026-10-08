# Randomized cyclic Latin rectangle

Arrange treatments in fewer complete rows than a Latin square, with no
treatment repeated in any column. Each row is a complete replicate.

## Usage

``` r
latin_rectangle(
  t,
  rows = 3,
  l = 1,
  plotNumber = 101,
  planter = "serpentine",
  seed = NULL,
  locationNames = NULL
)
```

## Arguments

- t:

  Number of treatments (at least two), or distinct character labels.

- rows:

  Number of complete rows, from two through the treatment count.

- l:

  Number of independently randomized locations.

- plotNumber:

  First plot number: one positive whole number, or one per location.
  Plot numbers restart at the supplied value for each location.

- planter:

  Plot-number traversal, `"serpentine"` or `"cartesian"`.

- seed:

  Randomization seed; `NULL` draws and records one automatic seed.

- locationNames:

  Distinct nonblank location labels, one per location; `NULL` generates
  `LOC1`, `LOC2`, and so on. Labels are preserved.

## Value

A schema-1 `FielDHub` result with `infoDesign`, a field book containing
ID, LOCATION, PLOT, ROW, COLUMN, REP and TREATMENT, and replay metadata.
REP is the complete row block. Use
[`reproduce_design()`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md)
for replay and
[`field_layout()`](https://didiermurillof.github.io/FielDHub/reference/field_layout.md)
for the recorded coordinates.

## Details

The construction takes the first `rows` consecutive shifts of the cyclic
Latin square, then independently permutes rows, columns and treatment
labels at each location. It samples this cyclic construction family, not
uniformly from all Latin rectangles, and does not optimize efficiency.
It uses `O(l * rows * t)` construction work with no search.

Rows are complete blocks; columns are incomplete when `rows < t`. This
is not necessarily a balanced incomplete-block or Youden design.
Consecutive cyclic shifts keep the additive row/column/treatment model
connected. Its residual degrees of freedom at one location are
`(rows - 2) * (t - 1)`: two-row rectangles are saturated and cannot
independently estimate residual variation under that model. Interactions
are not separately estimable. Choose replication and analysis with the
intended scientific use in mind.

Field coordinates and plot numbering are fixed at construction. The
`planter` and `stacked` arguments to
[`field_layout()`](https://didiermurillof.github.io/FielDHub/reference/field_layout.md)
do not rearrange a saved rectangle. Supplied seeds preserve the caller's
RNG; an automatic seed consumes exactly one recorded draw.

## References

Peter G. Doyle, *The number of Latin rectangles*.
<https://math.dartmouth.edu/~doyle/docs/latin/latin.pdf>.

## Examples

``` r
x <- latin_rectangle(t = 5, rows = 3, seed = 27)
x$fieldBook
#>    ID LOCATION PLOT ROW COLUMN REP TREATMENT
#> 1   1     LOC1  101   1      1   1        T1
#> 2   2     LOC1  102   1      2   1        T3
#> 3   3     LOC1  103   1      3   1        T4
#> 4   4     LOC1  104   1      4   1        T5
#> 5   5     LOC1  105   1      5   1        T2
#> 6   6     LOC1  106   2      5   2        T1
#> 7   7     LOC1  107   2      4   2        T4
#> 8   8     LOC1  108   2      3   2        T3
#> 9   9     LOC1  109   2      2   2        T2
#> 10 10     LOC1  110   2      1   2        T5
#> 11 11     LOC1  111   3      1   3        T4
#> 12 12     LOC1  112   3      2   3        T1
#> 13 13     LOC1  113   3      3   3        T2
#> 14 14     LOC1  114   3      4   3        T3
#> 15 15     LOC1  115   3      5   3        T5
identical(reproduce_design(x), x)
#> [1] TRUE
```
