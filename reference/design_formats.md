# Supported recorded-design file formats

Lists the version and read/write capabilities of the built-in design
format registry. Formats are selected explicitly, not guessed from
filenames. CSV tables and app workflow ZIPs are separate exports;
neither is silently interpreted as a complete recorded design by these
functions.

## Usage

``` r
design_formats()
```

## Value

A data frame with format, extension, media_type, format_version,
readable and writable columns.

## Examples

``` r
design_formats()
#>   format extension               media_type format_version readable writable
#> 1    rds       rds application/octet-stream              1     TRUE     TRUE
#> 2      r         R               text/plain              1    FALSE     TRUE
```
