# Build-time and CI contract check; constructs the app without starting a server.
stopifnot(identical(system2("id", "-u", stdout = TRUE), "10001"))
lock <- renv::lockfile_read("renv.lock")
stopifnot(identical(as.character(getRversion()), lock$R$Version))
for (record in lock$Packages) {
  installed <- utils::packageDescription(record$Package, fields = "Version")
  if (!identical(installed, record$Version)) stop("Dependency drift: ", record$Package)
}
# Load optional namespaces before measuring app-construction side effects.
FielDHub:::app_check_dependencies()
options_before <- options()
app <- FielDHub::run_app(launch.browser = FALSE)
stopifnot(inherits(app, "shiny.appobj"), identical(options(), options_before))
design <- FielDHub::RCBD(t = 5, reps = 3, seed = 38)
stopifnot(identical(FielDHub::reproduce_design(design), design))
cat("Non-root runtime, locked versions, app construction and design replay: OK\n")
