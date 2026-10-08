# Adding designs and formats

## Add an engine through the registry

[`latin_rectangle()`](https://didiermurillof.github.io/FielDHub/reference/latin_rectangle.md)
is the worked example: its engine, declaration and tests were added
after the generic registry infrastructure, without adding a branch to
replay, schema, printing, layout or drawing dispatchers.

1.  Write independent scientific tests first. Specify counts and labels
    from the requested inputs, not from the result’s own summary. Check
    geometry, replications, model rank/efficiency where relevant, and
    corrupted fixtures. State the construction family, sampling
    limitations and a finite search budget. A fixed-seed snapshot alone
    is not scientific validation.
2.  Add an exported `R/fct_<engine>.R` function. Validate types, counts,
    products, labels and options before allocating large data or calling
    `resolve_seed()`. Call `local_design_seed()` once the effective seed
    is known. Do not detach packages, print status, change options or
    call Shiny. Use classed conditions.
3.  Return through `new_fieldhub_design()`, with `infoDesign$id_design`
    and its effective seed, common field-book keys and a complete named
    parameter list. Include choices that change output and supplied data
    needed for replay.
4.  Add one declaration in `R/result_design_registry.R`: stable
    snake-case key, public engine name, required columns (or a pure
    column-resolver function), and optional title/layout/render
    handlers. A fixed-coordinate book with a `TREATMENT` column can use
    `field_book_layouts` and `draw_registered_layout`. Declare
    specialized handlers only if the generic contract does not fit.
    Never add metadata-directed
    [`eval()`](https://rdrr.io/r/base/eval.html) or dispatch to
    arbitrary saved names.
5.  Regenerate Rd/NAMESPACE with `devtools::document()`. Add fixed-seed
    calls to `tests/testthat/helper-catalogue.R` and the result-class
    expectations. Test explicit/automatic seeds, errors without RNG
    leakage, replay, layouts, labels containing spaces/punctuation,
    locations and format round-trips. Review and accept only the new
    snapshots; existing outputs must stay fixed.

The Latin rectangle test also checks the additive row/column/treatment
model rank independently with
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) and
[`qr()`](https://rdrr.io/r/base/qr.html). Its two-row case is saturated,
so the help explicitly warns that residual variation cannot be estimated
from that model. It is not advertised as a uniformly sampled Latin
rectangle, an optimized design, or a balanced incomplete-block design.

## Expose the design in the app

The scientific engine does not depend on the app. To add a page, supply
these declarations/adapters; do not copy a server module:

| File | Addition |
|----|----|
| `R/app_registry.R` | Label, navigation group, engine, module/spec key and a unique server order |
| `R/app_design_specs.R` | Shared controls, upload shape, values and the existing classic/spatial workflow family |
| `R/app_design_args.R` | Plain `design_args_<key>(values, data)` mapping to public API arguments; use exact `[[` lookup and numeric normalization |
| `R/app_classic_workflow_registry.R` or spatial equivalent | Output identifiers, layout choices, table/heatmap columns, simulation and export settings |

The shared `mod_design_ui()` and `mod_design_server()` do not change.
Tests must compare raw-control and uploaded-data paths against direct
API calls, construct the UI without starting a server, and verify that
every registry entry uses the generic module. Do not commit Shiny server
or browser tests. Manually verify a real session, invalid-input
recovery, simulation and downloads before release. Help topics are
derived from the app registry; do not maintain a second list.

## Add an import/export format

Add a reviewed entry to `fieldhub_format_registry()` in
`R/io_design_formats.R`: extension, media type, positive format version,
reader and/or writer. A missing reader means write-only. Public
[`design_formats()`](https://didiermurillof.github.io/FielDHub/reference/design_formats.md),
[`read_design()`](https://didiermurillof.github.io/FielDHub/reference/read_design.md)
and
[`write_design()`](https://didiermurillof.github.io/FielDHub/reference/write_design.md)
dispatch from these declarations and need no new branches.

Readers must return a complete recorded result that passes the common
schema and known-engine validator. Parse data, never execute source.
Writers receive a validated result and a temporary filename, not the
user’s final destination. Version incompatible format changes and define
whether old versions are read, migrated or rejected. Do not call a bare
CSV table lossless if labels, classes, parameters or RNG settings cannot
be recovered from it.

Add round-trip checks across every engine,
malformed/truncated/unknown-version fixtures, path and overwrite guards,
and assertions that imports do not consume randomness or reconstruct the
experiment. Preserve existing destinations on serialization failure.
Optional new-format dependencies belong in Suggests with an actionable
dependency condition; no automatic installation.

## Review checklist

Run the scoped tests first, then the complete plain-R suite and macOS
goldens, full package check with vignettes, changed-line lint and
unchanged core coverage gate. Use the release benchmark comparison when
scientific/engine work changes. Update both NEWS files, reference
validation, architecture guidance and examples. Check minimal-dependency
installation so script users do not require Shiny. Record actual local
and CI evidence separately; consult `RELEASING.md` before publishing or
assigning a release tag.
