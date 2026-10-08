# Run selected steps from the repository root, with development tools installed.
# Design pages use the native Shiny module in R/app_design_module.R and the
# catalogue in R/app_registry.R. See vignettes/extending_fieldhub.Rmd before
# adding a design; do not regenerate modules or dependency lists.

# Regenerate documentation after changing public function documentation.
devtools::document()

# Run the package test suite.
devtools::test()

# Optional documentation build:
# devtools::build_vignettes()
