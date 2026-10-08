# Validate the package before deployment. Run from the repository root.
devtools::check()

# The existing Dockerfile starts the installed package using native Shiny.
# See deployment/README.md for the pinned build and runtime checks.
# Refresh deployment/renv.lock only from deliberately prepared, tested versions:
# Rscript tools/lock-deployment.R .
# Rscript tools/check-deployment.R .
# docker build --tag fieldhub-local .

# For an installed package outside Docker:
# shiny::runApp(FielDHub::run_app(launch.browser = FALSE), host = "127.0.0.1", port = 3838)
