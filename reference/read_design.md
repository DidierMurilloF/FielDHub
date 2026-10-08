# Read a validated recorded design

Read a validated recorded design

## Usage

``` r
read_design(file, format = "rds")
```

## Arguments

- file:

  Local filename. Only read serialized files from trusted sources.

- format:

  Explicit format key from
  [`design_formats()`](https://didiermurillof.github.io/FielDHub/reference/design_formats.md);
  currently `"rds"` is the lossless import format.

## Value

The complete saved result, unchanged. File/format input mistakes raise
`fieldhub_input_error`; corrupt or incompatible contents raise
`fieldhub_import_error`, with the original condition in `parent`.

## Details

Imports validate the result schema, recorded parameters and known
engine. They do not randomize, reconstruct a design, or execute an R
script. Unknown schema versions are rejected, not silently coerced.
Pre-metadata legacy objects remain usable through
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) but do not contain
the recorded-input contract required by this importer.

## Examples

``` r
file <- tempfile(fileext = ".rds")
x <- RCBD(t = 5, reps = 3, seed = 27)
write_design(x, file)
identical(read_design(file), x)
#> [1] TRUE
unlink(file)
```
