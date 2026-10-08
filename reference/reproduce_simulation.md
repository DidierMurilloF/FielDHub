# Reconstruct recorded simulated responses

Replays a classic truncated-normal or spatial AR1-by-AR1 simulation
using its saved input field book, parameters, seed, and RNG settings.

## Usage

``` r
reproduce_simulation(x)
```

## Arguments

- x:

  A simulation record containing `input_field_book`, `field_book`, and
  `metadata`. Metadata must contain `model`, `schema_version`, `seed`,
  `rng_kind`, `package_version`, and the named `parameters` used by that
  model.

## Value

The reconstructed simulation record, invisibly.

## Details

This reconstructs the responses, not the experimental design or its
layout: it starts from the exact saved input field book. Use
[`reproduce_design()`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md)
separately to reconstruct the experimental design. The caller's RNG
settings and `.Random.seed` are restored on exit. Recorded arguments are
passed as values rather than evaluated as R code. A changed FielDHub
version signals a `fieldhub_reproduction_warning`; reproduction across
software versions or platforms is not guaranteed.

Spatial `min_value` and `max_value` determine the model's center and
genetic-effect scale, not hard response bounds. Spatial records retain
unrounded per-location simulation values separately from the field book,
whose responses are rounded to two decimal places. Classic records use a
truncated-normal model, with rounding also applied to the field book.

## See also

[`reproduce_design()`](https://didiermurillof.github.io/FielDHub/reference/reproduce_design.md)
