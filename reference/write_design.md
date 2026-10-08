# Write a recorded design or standalone replay script

Write a recorded design or standalone replay script

## Usage

``` r
write_design(x, file, format = "rds", overwrite = FALSE)
```

## Arguments

- x:

  A validated design, allocation or optimization result with recorded
  parameters. App-selected layouts and simulations are separate
  artifacts.

- file:

  Local output filename; its parent directory must already exist.

- format:

  Explicit exporter key from
  [`design_formats()`](https://didiermurillof.github.io/FielDHub/reference/design_formats.md):
  `"rds"` saves the exact result and metadata; `"r"` saves a standalone
  script assigning the reconstructed result to `design`.

- overwrite:

  One logical value. Existing files are protected by default.

## Value

`file`, invisibly. Unsupported formats and overwrite mistakes raise
`fieldhub_input_error`; write failures raise `fieldhub_export_error`.

## Details

Serialization completes in a temporary file before the destination is
copied. This protects an existing file from serialization failures, but
is not an atomic filesystem transaction. Disk/copy failures still
require normal backups. The temporary file is removed on exit.

R scripts embed only portable data and never evaluate recorded arguments
during export. Large uploads or nonportable values require the RDS
format. Reconstruction can depend on matching software versions and
platform; the RDS format preserves the actual saved result independently
of replay.

## Examples

``` r
file <- tempfile(fileext = ".rds")
write_design(CRD(t = 4, reps = 2, seed = 27), file)
read_design(file)$metadata
#> $design
#> [1] "crd"
#> 
#> $schema_version
#> [1] 1
#> 
#> $seed
#> [1] 27
#> 
#> $rng_kind
#> [1] "Mersenne-Twister" "Inversion"        "Rejection"       
#> 
#> $package_version
#> [1] "1.6.0"
#> 
#> $parameters
#> $parameters$t
#> [1] 4
#> 
#> $parameters$reps
#> [1] 2
#> 
#> $parameters$plotNumber
#> [1] 101
#> 
#> $parameters$seed
#> [1] 27
#> 
#> $parameters$data
#> NULL
#> 
#> $parameters$locationNames
#> [1] 1
#> 
#> 
unlink(file)
```
