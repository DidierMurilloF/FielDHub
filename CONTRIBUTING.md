# Contributing to FielDHub

<!-- This CONTRIBUTING.md is adapted from https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c -->

First of all, thanks for considering contributing to FielDHub! 👍 It's people like you that make it rewarding for us - the project maintainers - to work on FielDHub. 😊

FielDHub is an open-source project, maintained by people who care.

[repo]: https://github.com/DidierMurilloF/FielDHub
[issues]: https://github.com/DidierMurilloF/FielDHub/issues
[new_issue]: https://github.com/DidierMurilloF/FielDHub/issues/new
[website]: https://DidierMurilloF.github.io/FielDHub
[citation]: https://DidierMurilloF.github.io/FielDHub/authors.html
[email]: mailto:didier.murilloflorez@ndsu.edu

## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## How you can contribute

There are several ways you can contribute to this project. If you want to know more about why and how to contribute to open source projects like this one, see this [Open Source Guide](https://opensource.guide/how-to-contribute/).

### Share the love ❤️

Think FielDHub is useful? Let others discover it, by telling them in person, via Twitter or a blog post.

Using FielDHub for a paper you are writing? Consider [citing it][citation].

### Ask a question ⁉️

Using FielDHub and got stuck? Browse the [documentation][website] to see if you can find a solution. Still stuck? Post your question as an [issue on GitHub][new_issue]. While we cannot offer user support, we'll try to do our best to address it, as questions often lead to better documentation or the discovery of bugs.

Want to ask a question in private? Contact the package maintainer by [email][email].

### Propose an idea 💡

Have an idea for a new FielDHub feature? Take a look at the [documentation][website] and [issue list][issues] to see if it isn't included or suggested yet. If not, suggest your idea as an [issue on GitHub][new_issue]. While we can't promise to implement your idea, it helps to:

* Explain in detail how it would work.
* Keep the scope as narrow as possible.

See below if you want to contribute code for your idea as well.

### Report a bug 🐛

Using FielDHub and discovered a bug? That's annoying! Don't let others have the same experience and report it as an [issue on GitHub][new_issue] so we can fix it. A good bug report makes it easier for us to do so, so please include:

* Your operating system name and version (e.g. Mac OS 10.13.6).
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

### Improve the documentation 📖

Noticed a typo on the website? Think a function could use a better example? Good documentation makes all the difference, so your help to improve it is very welcome!

#### The website

[This website][website] is generated with [`pkgdown`](http://pkgdown.r-lib.org/). That means we don't have to write any html: content is pulled together from documentation in the code, vignettes, [Markdown](https://guides.github.com/features/mastering-markdown/) files, the package `DESCRIPTION` and `_pkgdown.yml` settings. If you know your way around `pkgdown`, you can [propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to improve documentation. If not, [report an issue][new_issue] and we can point you in the right direction.

#### Function documentation

Functions are described as comments near their code and translated to documentation using [`roxygen2`](https://klutometis.github.io/roxygen/). If you want to improve a function description:

1. Go to `R/` directory in the [code repository][repo].
2. Look for the file that holds the function: an exported function `f()` is
   in `R/fct_f.R` (see [Source layout](#source-layout) for the other files).
3. [Propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to update the function documentation in the roxygen comments (starting with `#'`).

### Contribute code 📝

Care to fix bugs or implement new functionality for FielDHub? Awesome! 👏 Have a look at the [issue list][issues] and leave a comment on the things you want to work on. See also the development guidelines below.

## Development guidelines

From the repository root, run `source("dev/run_dev.R")` to load the source
package and start the app. This path does not need `golem-config.yml`, detach
packages, clear your workspace, or regenerate documentation. Stop the app
normally before reloading changed source. Installed-package users run
`FielDHub::run_app()` after installing the optional app dependencies in README.
The root `app.R` returns the app object for source-based Shiny deployment.

Regenerate documentation separately with `devtools::document()` when changing
roxygen comments. `Rscript --vanilla tools/check-launchers.R .` checks both
entry-script contracts without starting a server or changing the R session.

We try to follow the [GitHub flow](https://guides.github.com/introduction/flow/) for development.

1. Fork [this repo][repo] and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2. If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream master`.
3. Open the RStudio project file (`.Rproj`).
4. Make your changes:
    * Add a plain-R regression test that fails before fixing a defect, then
      implement the fix. Keep scientific logic out of Shiny modules; do not add
      Shiny server or browser tests.
    * Run the full tests with `NOT_CRAN=true FIELDHUB_GOLDEN=true`. Golden
      snapshots are recorded on macOS. Review deliberate scientific output
      changes and announce affected inputs in NEWS; never accept snapshots
      blindly.
    * For output-preserving refactors, compare complete fixed-seed results with
      the previous implementation using `identical()`. Preserve supplied labels,
      field-book column types, RNG state, and process options.
    * Document your code (see function documentation above).
    * Run `R CMD check --as-cran`, build the UI without starting a session, and
      inspect the completed check log. A development-version NOTE is expected;
      other findings require investigation. Correctness lint and core coverage
      are also checked in CI.
5. Commit and push your changes.
6. Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request).

Maintainers follow the [release checklist](RELEASING.md), including the full R
and platform matrix, reconstruction checks, deployment validation, and reviewed
performance benchmarks. Use a minor release for deliberate API or behavior
changes; keep migration guidance alongside the change.
The [deployment guide](deployment/README.md) documents the locked Docker build
and distinguishes source checks from verified image execution.

Scientific changes also need independent count, geometry, or numerical checks.
Use the plain helpers in `tests/testthat/helper-invariants.R`, specify expected
levels from the inputs (including missing groups), and include corrupted
fixtures to prove the checks detect failures. See the
[scientific validation guide](vignettes/scientific_validation.Rmd) for the
family-by-family checks, reference calculations, and computational limits.

## Source layout

Each file in `R/` is named `<prefix>_<topic>.R`. The prefix says what the
file is responsible for; the topic says what it holds (for example
`engine_diagonal_checks.R`, `layout_planting_path.R`, `result_methods.R`).

| Prefix | Responsibility |
|---|---|
| `fct_` | exported design engines (one per exported function) |
| `engine_` | internal randomization/allocation/optimization helpers |
| `layout_` | field coordinates, planting paths, layout options |
| `render_` | drawing only (desplot/ggplot/heatmap data) |
| `io_` | uploads, exports, archives |
| `validate_` | input validators and parsers |
| `result_` | result constructor, schema, S3 methods, reproduction |
| `sim_` | response simulation |
| `api_` | cross-cutting public documentation |
| `app_`, `mod_` | Shiny layer only |

- A `fct_` file is named after its exported function (`fct_RCBD.R` holds
  `RCBD()`). Helpers used only by that function may sit beside it.
- Exception: the allocation and pair-swap result builders
  (`new_fieldhub_allocation()`, `validate_fieldhub_allocation()`,
  `new_fieldhub_optimization()`, `validate_fieldhub_optimization()`) live in
  `fct_do_optim.R` and `fct_swap_pairs.R`, not in `result_` files. Each
  validator reads the arguments of its engine (`formals(do_optim)`,
  `formals(swap_pairs)`), so a `result_` file would depend on the `fct_` file
  that depends on it. `fct_reproduce_design.R` calls both validators.
- `api_` files hold roxygen documentation only: `api_vocabulary.R` (argument
  names) and `api_result_contract.R` (the structure of every result, shared
  by all designs, so it is `api_` rather than `result_`).
- `app_`/`mod_` files hold the Shiny wiring. Plain helpers the app uses
  (reading an input, preparing a simulation or an export) live in core
  prefixes, where they are unit-tested and count toward core coverage. Core
  files (every other prefix) never refer to `app_`/`mod_` code and make no
  `shiny`, `DT`, `bslib`, `shinyjs` or `shinyalert` calls.
- Dependencies between files go one way: no file may depend, directly or
  through other files, on a file that depends on it. When two files call
  each other, move the mutually dependent functions into one file, or into a
  lower layer that both use.
- R loads these files in alphabetical order, so top-level code (such as
  `x <- f()`) may only use objects defined above it in the same file.

Two files are exceptions:

- `globals.R` declares the column names used in non-standard evaluation.
- `run_app.R` holds `run_app()`, the exported launcher of the Shiny app.

Tests live in `tests/testthat/test_<topic>.R` and are plain R: never Shiny
server or browser tests. `test_source_layout.R` checks this layout (prefixes,
one `fct_` file per exported function, no core reference to the Shiny
layer, no dependency cycle between core files). Structural checks that must
also run under `R CMD check`, where `R/` is not installed, inspect the
namespace with `core_functions()` and `app_functions()` from
`helper-source.R`. The core coverage gate in CI
(`inst/ci/compare-core-coverage.R`) measures every `R/` file except `app_*`,
`mod_*` and `run_app.R`.

## Attribution

This Contributing is adapted from [CONTRIBUTING](https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c).
