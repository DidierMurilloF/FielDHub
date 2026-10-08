# Shared design arguments and compatibility

Use named arguments when writing FielDHub scripts. The common names
below describe the same concepts across the design families that support
them. Each function's help page defines its supported inputs and
constraints.

## Common arguments

- `t`:

  Treatments in classical designs: a count or, where supported, a vector
  of labels. Check-based designs use `lines` for the number of
  experimental entries excluding checks.

- `reps`:

  Full replicates per location in classical, incomplete-block, lattice,
  row-column and strip-plot designs. In
  [`latin_square()`](https://didiermurillof.github.io/FielDHub/reference/latin_square.md),
  this is the number of independent squares. Partial and multi-location
  replication uses the design-specific allocation arguments instead.

- `l`:

  Number of locations.
  [`CRD()`](https://didiermurillof.github.io/FielDHub/reference/CRD.md)
  and
  [`latin_square()`](https://didiermurillof.github.io/FielDHub/reference/latin_square.md)
  generate one location and have no `l` argument.

- `locationNames`:

  Location labels, in location order.
  [`CRD()`](https://didiermurillof.github.io/FielDHub/reference/CRD.md)
  takes one location name.

- `plotNumber`:

  Starting plot number, or one start per location.

- `seed`:

  Seed recorded in the result to reproduce randomization. When `seed` is
  `NULL`, one integer is drawn from the current random-number stream and
  recorded; the design's own randomization does not change the caller's
  stream.

- `k`:

  Number of plots per incomplete block.

- `checks`:

  A check count, entry identifiers, or labels; see "Dimensions, factors
  and checks" below.

- `rep_checks`:

  Check replication: how many times each check is repeated.
  [`RCBD()`](https://didiermurillof.github.io/FielDHub/reference/RCBD.md)
  takes one count per check label supplied in `checks` (recycled from a
  scalar).
  [`optimized_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/optimized_arrangement.md)
  takes either a total check count or one count per check label (this is
  `amountChecks` in 1.5.x scripts; see "Migrating existing scripts").
  Multi-location allocations
  ([`multi_location_prep()`](https://didiermurillof.github.io/FielDHub/reference/multi_location_prep.md),
  `do_optim(design = "prep")`) take the same one-count-per-check form.
  [`sparse_allocation()`](https://didiermurillof.github.io/FielDHub/reference/sparse_allocation.md)
  and `do_optim(design = "sparse")` have no `rep_checks`: every check is
  replicated once per location by construction.

## Dimensions, factors and checks

`nrows` and `ncols` describe field dimensions in spatial designs. In
[`row_column()`](https://didiermurillof.github.io/FielDHub/reference/row_column.md),
`nrows` is the number of rows per replicate;
[`field_layout()`](https://didiermurillof.github.io/FielDHub/reference/field_layout.md)
controls how replicates are placed in the field. Factorial, split-plot
and strip-plot designs keep their factor-specific arguments so whole
plots, subplots and crossed strips remain distinct. The meaning of
`checks` is design-specific: a count, entry identifiers, or labels.
Consult the function's help before transferring a check vector between
design families.

Partial and multi-location replication does not share one argument name
across families; each keeps the allocation arguments that describe its
own replication scheme:
[`partially_replicated()`](https://didiermurillof.github.io/FielDHub/reference/partially_replicated.md)'s
`repGens` (how many entries get each replication level) and `repUnits`
(those replication levels), and
[`RCBD_augmented()`](https://didiermurillof.github.io/FielDHub/reference/RCBD_augmented.md)'s
`b` (number of augmented blocks) and `repsExpt` (replicates of the whole
experiment). These are design-specific allocation arguments, not aliases
of `reps` or `rep_checks`.

## Boolean controls

Logical switches such as `continuous`, `factorLabels`, `spread_reps`,
`allow_fillers`, and `randomizeH` require one nonmissing `TRUE` or
`FALSE`. Numeric switches (`0`/`1`), strings, vectors, and arrays are
not supported. Invalid switches signal a `fieldhub_input_error` before
randomization, with `argument`, `value`, and `options` fields
identifying the correction.

## Migrating existing scripts

`CRD(locationName = ...)` remains supported; use
`CRD(locationNames = ...)` in new code. In
[`incomplete_blocks()`](https://didiermurillof.github.io/FielDHub/reference/incomplete_blocks.md),
[`alpha_lattice()`](https://didiermurillof.github.io/FielDHub/reference/alpha_lattice.md),
[`square_lattice()`](https://didiermurillof.github.io/FielDHub/reference/square_lattice.md),
[`rectangular_lattice()`](https://didiermurillof.github.io/FielDHub/reference/rectangular_lattice.md)
and
[`row_column()`](https://didiermurillof.github.io/FielDHub/reference/row_column.md),
replace the argument name `r` with `reps`. In
[`strip_plot()`](https://didiermurillof.github.io/FielDHub/reference/strip_plot.md),
replace `b` with `reps`. In
[`optimized_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/optimized_arrangement.md),
replace `amountChecks` with `rep_checks`. `RCBD_augmented(b = ...)`
still denotes blocks and is unchanged.

Old names and positional calls retain their meaning and signal a warning
of class `fieldhub_deprecated_warning`. Supplying both names for one
argument is an error, even if their values agree. These aliases do not
change seeded designs or the field names of saved results.

## See also

[`CRD`](https://didiermurillof.github.io/FielDHub/reference/CRD.md),
[`incomplete_blocks`](https://didiermurillof.github.io/FielDHub/reference/incomplete_blocks.md),
[`row_column`](https://didiermurillof.github.io/FielDHub/reference/row_column.md),
[`strip_plot`](https://didiermurillof.github.io/FielDHub/reference/strip_plot.md),
[`field_layout`](https://didiermurillof.github.io/FielDHub/reference/field_layout.md)
