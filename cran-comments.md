# FielDHub 1.6.0 — draft submission notes

This minor release modernizes the Shiny app, simplifies required dependencies,
and adds shared validation and reproducibility contracts. NEWS documents
changed defaults, rejected invalid inputs and corrections to seeded results.

## Local validation

- R 4.5.3 on aarch64-apple-darwin20 (macOS 27.0).
- `R CMD check --no-manual --as-cran`: completed with 0 errors, 0 warnings
  and 0 NOTES, including examples, installed-package tests and vignette rebuilds.
- Full plain-R suite, including NOT_CRAN and golden checks: 17,664 assertions
  across 1,006 test cases, with no failures, errors, warnings or skips.
- JavaScript loading tests: 15 passed.
- Source build with vignettes, changed-line correctness lint and release-tool
  checks passed. Website home, reference and NEWS pages were generated locally.
- Help and About Us were checked in Chrome at four viewport sizes; the displayed
  app version is 1.6.0, with no email links or separate copyright label.
- The locked Linux Docker image built successfully. Its non-root runtime and
  seeded replay checks passed with network access disabled. An isolated worker
  produced the exact same seeded design, and the container served the 1.6.0 app.

## Before submission

These are draft notes, not a claim of CRAN acceptance or a submission request.
Confirm the remote platform/check matrix, documentation and coverage gates.
Complete the independent scientific review, exact-revision performance review
and historical licensing review described in RELEASING.md before publication.
Reverse-dependency checks have not been performed for this candidate; do not
reuse the previous release's platform or reverse-dependency claims.
