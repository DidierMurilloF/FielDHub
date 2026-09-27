# Releasing FielDHub

Release decisions, repository settings, publishing, and tags belong to the
maintainers. A clean local check is not evidence that remote CI has passed.

## Prepare the candidate

1. Choose the release scope and version. Scientific output changes, classed
   errors, deprecations, or changed defaults must be explicit in NEWS. Do not
   release the modernization's behavior changes as an undocumented patch.
2. Review `DESCRIPTION`, both NEWS files, generated documentation, and the
   migration examples. Keep a development version between releases.
3. Give a second reviewer the scientific changes, old/new comparisons,
   deliberate snapshot changes, and any remaining limitations. Agree on a
   release owner and a backup reviewer; do not infer approval from green CI.

## Required evidence

- Run the complete plain-R tests, including the tests skipped on CRAN and
  macOS golden snapshots:

  ```sh
  NOT_CRAN=true FIELDHUB_GOLDEN=true Rscript -e 'devtools::test(stop_on_failure=TRUE)'
  Rscript -e 'pkgload::load_all(); invisible(FielDHub:::app_ui(NULL))'
  Rscript -e 'rcmdcheck::rcmdcheck(args=c("--no-manual", "--as-cran"), error_on="warning")'
  ```

- Read the completed `00check.log`, including `* DONE`. Development builds may
  have the large-version-components NOTE; remove that cause for a release.
  Investigate every other NOTE, warning, or error. Network-unavailable checks
  must be rerun with network access before submission.
- Confirm the remote check matrix passes on macOS, Linux, Windows, R-devel,
  oldrel, and the declared minimum R version. Keep the required check names
  `macos-latest (release)` and `ubuntu-latest (release)` stable.
- Confirm pkgdown, changed-line correctness lint, and the core coverage gate
  pass. Review legacy-object compatibility and design/simulation reconstruction
  tests. No Shiny server or browser tests are required: exercise business logic
  as plain R functions and construct the UI without starting a session.
- Build the deployment image and verify its non-root runtime and locked
  dependencies. A package check does not validate deployment.

## Record performance and quality

Install the exact candidate into a separate library. From the repository root,
run the scripts against it, choosing output files that do not already exist:

```sh
Rscript tools/check-release-tools.R .
Rscript tools/benchmark-dimensions.R dimensions.csv /path/to/candidate-library
Rscript tools/benchmark-spatial.R spatial.csv /path/to/candidate-library
Rscript tools/benchmark-exports.R exports.csv /path/to/candidate-library
Rscript tools/benchmark-optimizers.R optimizers.csv /path/to/candidate-library
```

The manually dispatchable `release-benchmarks` workflow runs the same four
benchmarks and retains CSV files and session information for 90 days. Copy the
reviewed results into the permanent release assets before they expire.

Compare with the previous release on the same hardware, R version, and package
versions, with no competing workloads. Record and explain regressions before
publishing; repeat noisy measurements. Do not treat shared-runner timings as
precise cross-release comparisons. Review optimizer quality and stop reasons,
not just speed. `largest_R_allocation_bytes` measures the largest R allocation,
not peak memory, and is `NA` when profiling is unavailable.

## Publish and follow up

1. The maintainer checks required branch protection and approves the merge.
2. Tag the exact reviewed release commit and publish the source and benchmark
   evidence. Verify historical tags against the corresponding CRAN sources.
3. Submit to CRAN and resolve incoming checks; do not equate submission with
   acceptance. Announce affected inputs for any corrected scientific outputs.
4. Publish the documentation, verify package installation and downloadable
   archives, and open the next development version.

Use opt-in issue reports for feedback. Do not add usage tracking or collect
field books, treatment labels, or other experiment data without consent.
