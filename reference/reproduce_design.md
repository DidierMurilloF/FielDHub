# Reconstruct a design from its recorded inputs

Calls the design engine using the input parameters and random-number
settings recorded in a result's metadata.

## Usage

``` r
reproduce_design(x)
```

## Arguments

- x:

  A FielDHub design, allocation plan, or pair-swap optimization result
  with recorded parameters.

## Value

A newly generated design or allocation plan, invisibly.

## Details

The caller's RNG settings and `.Random.seed` are restored on exit,
including when reconstruction fails. Arguments are passed as values, so
language objects in the parameter list are not evaluated as code.

Reproduction requires the same software versions and platform for
algorithms whose results depend on numerical optimization. A different
FielDHub version signals a `fieldhub_reproduction_warning`; dependency
or platform differences are not checked. Older results without recorded
parameters cannot be reconstructed automatically, but remain usable for
printing and plotting.

## Examples

``` r
x <- RCBD(t = 5, reps = 3, seed = 38)
identical(reproduce_design(x), x)
#> [1] TRUE
```
