# FielDHub 1.7.6 submission notes (draft)

This update from CRAN version 1.5.0 introduces a compiled block-design engine,
updates the Shiny app, and expands design layout and trait simulation. NEWS
documents changes to seeded outputs and defaults. Version 1.7.6 draws spatial
correlations per simulation draw and location when requested. Explicit fixed
correlations preserve the corresponding 1.7.5 simulated responses.

## Check results

- `R CMD check --no-manual --as-cran` on macOS arm64 with R 4.6.2 reported
  0 errors, 0 warnings, and 0 notes. The check included online CRAN incoming
  feasibility, examples, installed-package tests, and vignette rebuilds.
- The complete installed-package suite, with non-CRAN, slow-search, and
  exhaustive tests enabled, reported 4,185 passes, 0 failures, and 0 warnings.
  One test requires `RNGkind()` support for `binom.kind`, which is unavailable
  in R 4.6.2.
- Reverse-dependency checks passed for agricolaeplotr 1.0.0, beautils 0.3.0, and
  modeldiagramR 0.2.1 against both the candidate and CRAN version 1.5.0.
- The documentation site built and verified 134 public page paths. Changed-line
  correctness lint, launcher and release-tool checks, and the static deployment
  check passed. Core R line coverage was 90.88%, up from 90.85% for 1.7.5.
- All 66 benchmark comparisons against the 1.7.5 development baseline met the
  release thresholds. Smaller factor-pair and spatial workloads also returned
  equivalent results against CRAN version 1.5.0.

## Maintainer checks before submission

Confirm the standard and extended platform checks on the final commit and run
the locked Linux deployment image. Complete the independent scientific review
and the historical licensing review described in RELEASING.md.
