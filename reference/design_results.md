# Design results and reproducibility metadata

FielDHub uses a versioned result contract. Schema 1 retains the
established elements and storage types of each design family; validation
checks those elements without coercing valid field books or removing
user columns. Field, family, allocation and optimization results share
the same construction boundary, with validation appropriate to each
result type.

## Field designs

Field designs inherit from `FielDHub`, with a first class such as
`fieldhub_rcbd` identifying the engine. Test inheritance with
`inherits(x, "FielDHub")`, not equality with `class(x)`. Their
`fieldBook` contains finite numeric `ID` and `PLOT` vectors and
nonmissing atomic `LOCATION` identifiers. Integer and double storage,
factors, and the existing character or numeric location conventions are
preserved rather than normalized.

Additional required columns depend on the design:

- CRD and RCBD: `REP`, `TREATMENT`. RCBD with repeated checks also
  requires `ENTRY` and `CHECKS`.

- Latin square: `SQUARE`, `ROW`, `COLUMN`, `TREATMENT`.

- Factorial: `REP`, `TRT_COMB`, and one `FACTOR_<name>` column per
  recorded factor.

- Split plot: `REP`, `WHOLE_PLOT`, `SUB_PLOT`, `TRT_COMB`; split-split
  plot also requires `SUB_SUB_PLOT`.

- Strip plot: `REP`, `HSTRIP`, `VSTRIP`, `TRT_COMB`.

- Incomplete blocks and lattices: `REP`, `IBLOCK`, `UNIT`, `ENTRY`,
  `TREATMENT`.

- Row-column: `REP`, `ROW`, `COLUMN`, `ENTRY`, `TREATMENT`.

- Diagonal, optimized, sparse, augmented and partially replicated:
  `EXPT`, `YEAR`, `ROW`, `COLUMN`, `CHECKS`, `ENTRY`, `TREATMENT`.
  Augmented RCBD adds `BLOCK`; partially replicated designs add `REP`.

Required extension columns are atomic vectors, not matrix or list
columns. Numeric values must be finite. Missing `CHECKS` values remain
supported, as do missing `REP` values on partially replicated filler
plots with `ENTRY = 0`. Other required extension values cannot be
missing. Extra columns are permitted. Use
[`field_layout()`](https://didiermurillof.github.io/FielDHub/reference/field_layout.md)
to obtain the final coordinates and plot numbers for a chosen planting
path and layout.

## Family splits and allocation plans

[`split_families()`](https://didiermurillof.github.io/FielDHub/reference/split_families.md)
returns `rowsEachlist` with location counts and `data_locations` with
`ENTRY`, `NAME`, `FAMILY` and `LOCATION`. Counts must agree with the
entry table, including locations with zero entries.

[`do_optim()`](https://didiermurillof.github.io/FielDHub/reference/do_optim.md)
retains the `Sparse` or `MultiPrep` class. Its allocation counts,
location sizes and entry lists are validated; these plans are not field
books and do not yet specify plot coordinates.

## Standalone optimization

[`swap_pairs()`](https://didiermurillof.github.io/FielDHub/reference/swap_pairs.md)
retains its matrices, distances and stopping diagnostics, with the
classes `fieldhub_pair_swap` and `fieldhub_optimization`. Its shared
metadata includes the input matrix and every optimization control. The
validator checks field geometry, entry counts and retained search steps.
These results can be saved and replayed with
[`reproduce_design()`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md).

## Reproducibility

`metadata` records `design`, `schema_version`, `seed`, `rng_kind`,
`package_version` and, in newly generated results, `parameters`.
Parameters use effective values after validation, including defaults,
the resolved year and seed, and uploaded data. Automatic seeds are
integers; explicit real-valued seeds retain the legacy behavior of R's
[`set.seed()`](https://rdrr.io/r/base/Random.html), which truncates to
an integer.

Use [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) to retain the exact
result and its metadata, then
[`reproduce_design()`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md)
to reconstruct it. Exact replay of optimization may require the same
dependency versions and platform. Layout selections and simulated
responses added later by the app are not part of the core design
parameters.

Older schema-1 metadata without parameters still validates, but cannot
be replayed automatically. Objects saved by FielDHub 1.5.0 without
metadata remain supported by the print, summary and plotting
compatibility methods; they are not silently rewritten to the new
schema.

## See also

[`reproduce_design`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md),
[`field_layout`](https://didiermurillof.github.io/FielDHub/reference/field_layout.md),
[`design_arguments`](https://didiermurillof.github.io/FielDHub/reference/design_arguments.md)
