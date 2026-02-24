# Generate the sparse or p-rep allocation to multiple locations.

Generate the sparse or p-rep allocation to multiple locations.

## Usage

``` r
do_optim(
  design = "sparse",
  lines,
  l,
  copies_per_entry,
  add_checks = FALSE,
  checks = NULL,
  rep_checks = NULL,
  force_balance = TRUE,
  seed,
  data = NULL
)
```

## Arguments

- design:

  Type of experimental design. It can be `prep` or `sparse`

- lines:

  Number of genotypes, experimental lines or treatments.

- l:

  Number of locations or sites. By default `l = 1`.

- copies_per_entry:

  Number of copies per plant. When design is `sparse` then
  `copies_per_entry` should be less than `l`

- add_checks:

  Option to add checks. Optional if `design = "prep"`

- checks:

  Number of genotypes checks.

- rep_checks:

  Replication for each check.

- force_balance:

  Get balanced unbalanced locations. By default `force_balance = TRUE`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- data:

  (optional) Data frame with 2 columns: `ENTRY | NAME `. ENTRY must be
  numeric.

## Value

A list with three elements.

- `list_locs` is a list with each location list of entries.

- `allocation` is a matrix with the allocation of treatments.

- `size_locations` is a data frame with one column for each location and
  one row with the size of the location.

## References

Edmondson, R.N. Multi-level Block Designs for Comparative Experiments.
JABES 25, 500–522 (2020). https://doi.org/10.1007/s13253-020-00416-0

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\]

## Examples

``` r
sparse_example <- do_optim(
   design = "sparse",
   lines = 120, 
   l = 4, 
   copies_per_entry = 3, 
   add_checks = TRUE, 
   checks = 4,
   seed = 15
)
```
