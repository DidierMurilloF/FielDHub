# Augmented row-column fixtures

`import_fixtures.R` regenerates `tests/testthat/fixtures/augmented-row-column/`
(design CSVs and `manifest.csv`). The directory is build-ignored (`^data-raw$`),
and the package gains no dependency: readxl is used by this script only.

## Sources and licences

| Input | Source | Licence |
| --- | --- | --- |
| Table 1 contractions (21), Example 2 contraction | Piepho & Williams (2026), Biometrical Journal 68(5):e70182, Supporting File 3 | CC-BY |
| Example 1 and 2 contractions and printed layouts | Same article, Section 2, Examples 1 and 2 | CC-BY |
| Bailey-Haines cyclic squares (t = 12, k = 3) | Bailey & Haines (2025), JABES, Table 1 | CC-BY |
| Restricted 12 x 8 pair, cyclic 5 x 5 (k = 4) | Independent specification review, 2026-10-10 | project-internal |
| Column-binary square pair, zero-df design | Mathematical audit report | project-internal |
| Recoloring counterexample | Implementation-plan audit | project-internal |
| False-QR design (v = 54, 5 x 14) | M1 spike 2b | project-internal |

Tracked inputs (copied once from the research files; the importer never reads
`research/`) are in `inputs/`, each with a provenance comment on its first line.

## External input

- Supplement archive: Supporting File 3, `bimj70182-sup-0003-Data.zip`, from
  https://onlinelibrary.wiley.com/doi/10.1002/bimj70182
- SHA-256: `ee6f945f4deff3f32878a846c8e400c2c50e0ba0a4a99b8940b1b26f83adfcda`
  (the script aborts on mismatch; it hashes with `digest`, else `sha256sum`,
  else `openssl`).
- Environment variable: `FIELDHUB_ARC_SUPPLEMENT_ZIP`, the path to the archive.

The archive holds 22 `con(...).xlsx` files (21 Table 1 rows plus Example 2).
File names have inconsistent parameter order and one file labels the long
dimension `row!`, so each file is oriented as k x s (k < s) and matched to its
Table 1 row by (v, s, k) taken from its contents.

## Rerun

From the repository root:

```
FIELDHUB_ARC_SUPPLEMENT_ZIP=/path/to/zip Rscript data-raw/augmented-row-column/import_fixtures.R
```

The script uses its own expansion routine and the test oracle in
`tests/testthat/helper-arc-oracle.R`; it stops on any failed assertion
(published-value agreement, binarity, connectivity, df checks). Output is
deterministic: rows sorted by file name, LF line endings, no timestamps,
15 significant digits.

## Clean-clone reproduction

```
git clone --branch research/augmented-row-column <repo> /tmp/arc-check
cd /tmp/arc-check
FIELDHUB_ARC_SUPPLEMENT_ZIP=/path/to/zip Rscript data-raw/augmented-row-column/import_fixtures.R
git status --short tests/testthat/fixtures/augmented-row-column   # expect no changes
git diff --exit-code tests/testthat/fixtures/augmented-row-column
```

Any difference means the committed fixtures are stale or the inputs changed.
