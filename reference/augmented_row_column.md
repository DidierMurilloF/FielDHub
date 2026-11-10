# Augmented row-column design

Arrange unreplicated test entries and `k` replicated checks in a
complete rectangular field, so that every check appears exactly once in
every field line along `check_direction`, and search the check positions
for an efficient design under fixed row and column effects.

## Usage

``` r
augmented_row_column(
  nrows,
  ncols,
  checks,
  check_direction,
  entries = NULL,
  objective = arc_design_defaults()$objective,
  method = "staged",
  budget = NULL,
  time_limit = NULL,
  l = 1,
  plotNumber = 101,
  planter = "serpentine",
  locationNames = NULL,
  exptName = NULL,
  year = NULL,
  seed = NULL,
  data = NULL
)
```

## Arguments

- nrows:

  Number of field rows, a whole number.

- ncols:

  Number of field columns, a whole number.

- checks:

  The checks: a whole-number count `k >= 2`, which gives the labels
  `CH1`, ..., `CHk`, or a character vector of at least two distinct,
  nonmissing, nonblank labels, used in that order. With `data`, it must
  be the count of checks.

- check_direction:

  `"columns"` or `"rows"`: the physical direction along which every
  check appears once per line. Required, with no default; it is never
  inferred from the field shape. With `"columns"`, each check appears
  once in every field column, so `v = nrows` and `s = ncols`; with
  `"rows"`, once in every field row, so `v = ncols` and `s = nrows`. The
  design needs `s <= v`.

- entries:

  The test entries, excluding checks. `NULL` (default) fills every test
  plot, `nrows * ncols - k * s` of them, with the labels `G(k+1)`, ...,
  `G(k+n)`. A whole-number count must equal that number of test plots
  and gives the same labels. A character vector of distinct, nonmissing,
  nonblank labels must have that length. Must be `NULL` when `data` is
  given.

- objective:

  The criterion the search optimizes: `"V_tt"` (minimize the mean
  variance of test-versus-test comparisons), `"V_tc"` (minimize the mean
  variance of test-versus-check comparisons) or `"E_aug"` (maximize the
  harmonic-mean efficiency of all treatment comparisons). The default is
  `"V_tt"`. Pass `objective` explicitly when a result must be
  reproducible across package versions; reproduction code always states
  it.

- method:

  Search method. Only `"staged"` (staged annealing with recolouring
  restarts) is available.

- budget:

  Objective-function calls allowed per location, a positive whole
  number. `NULL` (default) uses 60,000 calls when `v * s <= 600` and
  100,000 above. Pass `budget` explicitly when a result must be
  reproducible across package versions. The recorded parameters keep the
  effective value.

- time_limit:

  Optional per-location search cutoff, in seconds of wall-clock time, or
  `NULL` (default) for none. See "Search, budget and time limit" below.

- l:

  Number of locations, each an independent run of the same search.

- plotNumber:

  First plot number: one positive whole number, used at every location,
  or one per location.

- planter:

  Plot-number path, `"serpentine"` or `"cartesian"`.

- locationNames:

  Distinct nonblank location labels, one per location; `NULL` gives `1`,
  ..., `l`.

- exptName:

  Experiment name recorded in the `EXPT` column; `NULL` gives `"Expt1"`.

- year:

  Year recorded in the `YEAR` column; `NULL` gives the current calendar
  year. The effective year is recorded, so a later replay reproduces it.

- seed:

  Randomization seed; `NULL` draws and records one automatic seed.

- data:

  Optional entry table with exactly the columns `ENTRY` and `NAME`, as
  in
  [`diagonal_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/diagonal_arrangement.md):
  the first `checks` rows are the checks and the remaining rows the
  tests. `ENTRY` holds distinct positive whole numbers and `NAME`
  distinct, nonblank text labels. It must have `k` rows plus one row per
  test plot.

## Value

A schema-1 `FielDHub` result with class `fieldhub_augmented_row_column`
and the elements:

- `infoDesign`: `nrows`, `ncols`, `check_direction`, the mathematical
  `v`, `s` and `k`, the check labels `checks`, the test count `entries`,
  `locations`, `locationNames`, `residual_df`, `objective`, `method`,
  the effective `budget`, `seed` and `id_design`.

- `fieldBook`: `ID`, `EXPT`, `LOCATION`, `YEAR`, `PLOT`, `ROW`,
  `COLUMN`, `CHECKS`, `ENTRY`, `TREATMENT` and `CHECK_DIRECTION`. `ROW`
  and `COLUMN` are physical field coordinates; `CHECKS` is the check's
  `ENTRY` on check plots and 0 on test plots; `TREATMENT` is the label,
  unchanged.

- `statistics`: one row per location with `LOCATION`, `E_aug`, `V_tt`,
  `V_tc`, `V_cc`, `max_tt`, `max_tc`, `residual_df`, `authority` and
  `lambda_min_rel`, from the final scoring of the returned placement.

- `diagnostics`: per location, the location and its seed, the search
  status and phase statuses, objective and proxy calls, proposals,
  failed proposals, disconnected candidates and `time_limit_hit`.

- `contraction`: per location, the `k x s` matrix whose entry `(h, j)`
  is the cross-line holding check `h` in field line `j` (field column
  `j` for `"columns"`, field row `j` for `"rows"`), with rows in
  check-label order.

- `timing`: per location, search, verification and total elapsed
  seconds, kept apart from the deterministic content.

- `metadata`: the seed, RNG settings, package version and the complete
  effective `parameters`.

## Details

**Construction.** The design is a complete `nrows x ncols` field. Every
test entry occupies one plot and every check occupies `s` plots: exactly
one in each of the `s` field lines along `check_direction`. Along the
`v` cross-lines, no check is repeated and every cross-line holds between
`floor(k * s / v)` and `ceiling(k * s / v)` check plots, at least one.
This is the restricted contraction construction of Piepho and Williams
(2026); square fields (`v = s`) include the square-array designs of
Bailey and Haines (2025). Field coordinates and plot numbers are fixed
at construction.

**Model and objectives.** Under the additive model with fixed row,
column and entry effects and independent errors, the residual degrees of
freedom are `k * s - k - v - s + 2`. Fields with fewer than 1 are
rejected. Fields with fewer than 10 raise a warning: only adding a check
or changing the number of rows or columns raises them. `E_aug`, `V_tt`
and `V_tc` are reported for every location, whichever is optimized.

**Search, budget and time limit.** FielDHub searches the check positions
with staged annealing; the search is FielDHub's own method, not part of
the published construction. Each location runs one search with its own
seed (the recorded seed plus the location index minus one) and at most
`budget` objective calls. `time_limit`, when given, bounds each
location's search phase only: final verification, randomization and
field-book assembly always run to completion, and with `l` locations the
call can take about `l * time_limit` seconds plus that work. A location
stopped by the time limit records `time_limit_hit = TRUE` in its
diagnostics. A cutoff reached before the search has produced any design
raises a `fieldhub_arc_search_error`, as does a search that finds no
connected start; there is no silent fallback to another method.

**Randomization.** After the search, the field lines and cross-lines are
permuted at random, the check labels receive one random permutation, and
the tests are assigned to the test plots at random. None of these steps
changes an objective. The returned `contraction` and `statistics`
describe this final, randomized placement.

## Reproducibility

`reproduce_design(x)` and
`do.call(augmented_row_column, x$metadata$parameters)` replay the design
from its recorded parameters. Replay is exact (identical field book,
contraction, statistics, diagnostics and parameters; only `timing` and
the recorded package version are excluded) for evaluation-budget runs
that did not hit `time_limit`, within the same software and numerical
environment: the same FielDHub and R versions, BLAS/LAPACK library and
thread count, and platform. A `time_limit` cutoff makes replay
timing-dependent. In a different environment, replay gives a valid
design with comparable statistics, which may be a different design.

Standalone reproduction code is available for the standard entry
template at any size, and for any upload up to 200 rows. An upload whose
`ENTRY` values are exactly `1, ..., k + n` in row order is recorded as
its check and test labels, with `data = NULL`; other uploads are
recorded as uploaded, and above 200 rows the reproduction code falls
back to the saved result (RDS).

## References

Piepho, H.-P., and Williams, E. R. (2026). Rectangular augmented
row-column designs generated from contractions. *Biometrical Journal*,
68(5), e70182.
[doi:10.1002/bimj.70182](https://doi.org/10.1002/bimj.70182)

Bailey, R. A., and Haines, L. M. (2025). Square array designs for
unreplicated test treatments with replicated controls. *Journal of
Agricultural, Biological and Environmental Statistics*.
[doi:10.1007/s13253-025-00699-1](https://doi.org/10.1007/s13253-025-00699-1)

## See also

[`diagonal_arrangement`](https://didiermurillof.github.io/FielDHub/reference/diagonal_arrangement.md),
[`reproduce_design`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md),
[`design_arguments`](https://didiermurillof.github.io/FielDHub/reference/design_arguments.md)

## Examples

``` r
# 4 checks and 48 tests in a 10 x 8 field; every check appears once in
# each field column. A small budget keeps the example fast.
x <- augmented_row_column(nrows = 10, ncols = 8, checks = 4,
                          check_direction = "columns", budget = 2000,
                          seed = 2026)
x$infoDesign$residual_df
#> [1] 12
x$statistics
#>   LOCATION     E_aug     V_tt     V_tc      V_cc   max_tt   max_tc residual_df
#> 1        1 0.5884851 3.502748 1.872188 0.2767563 4.120002 2.037172          12
#>     authority lambda_min_rel
#> 1 full_layout      0.1722068
head(x$fieldBook)
#>   ID  EXPT LOCATION YEAR PLOT ROW COLUMN CHECKS ENTRY TREATMENT CHECK_DIRECTION
#> 1  1 Expt1        1 2026  101   1      1      0     7        G7         columns
#> 2  2 Expt1        1 2026  102   1      2      3     3       CH3         columns
#> 3  3 Expt1        1 2026  103   1      3      1     1       CH1         columns
#> 4  4 Expt1        1 2026  104   1      4      4     4       CH4         columns
#> 5  5 Expt1        1 2026  105   1      5      0    30       G30         columns
#> 6  6 Expt1        1 2026  106   1      6      0     8        G8         columns
# Check plots per field column: each check once in every column
checks <- x$fieldBook[x$fieldBook$CHECKS > 0, ]
table(checks$TREATMENT, checks$COLUMN)
#>      
#>       1 2 3 4 5 6 7 8
#>   CH1 1 1 1 1 1 1 1 1
#>   CH2 1 1 1 1 1 1 1 1
#>   CH3 1 1 1 1 1 1 1 1
#>   CH4 1 1 1 1 1 1 1 1

# The same field turned through 90 degrees, with labelled checks
y <- augmented_row_column(nrows = 8, ncols = 10,
                          checks = c("Check A", "Check B", "Check C",
                                     "Check D"),
                          check_direction = "rows", objective = "E_aug",
                          budget = 2000, seed = 7)
y$contraction
#> $`1`
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]   10    7    5    9    6    1    2    8
#> [2,]    8    3    9    6    4    2    5    7
#> [3,]    2   10    1    7    5    8    3    4
#> [4,]    9    1    4    3   10    6    8    2
#> 

# Replay from the recorded parameters
identical(reproduce_design(x)$fieldBook, x$fieldBook)
#> [1] TRUE
```
