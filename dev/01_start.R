# Load the existing package from the repository root.
# Package metadata and authorship are maintained in DESCRIPTION.
# No scaffolding or configuration generation is needed.
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# To launch the development app, run dev/run_dev.R.
# See CONTRIBUTING.md for the development workflow.
