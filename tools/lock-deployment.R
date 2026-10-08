# Regenerate the deployment lock from a deliberately prepared R library.
# Does not initialize renv in the source tree or change the developer library.
arguments <- commandArgs(TRUE)
if (length(arguments) != 1L) stop("Usage: Rscript tools/lock-deployment.R <repo>")
root <- normalizePath(arguments[1L], mustWork = TRUE)
description <- read.dcf(file.path(root, "DESCRIPTION"))
imports <- trimws(sub(" *\\(.*", "", strsplit(description[1L, "Imports"], ",")[[1L]]))
environment <- new.env(parent = baseenv())
sys.source(file.path(root, "R/app_dependencies.R"), envir = environment)
packages <- unique(c(imports, environment$app_dependencies(), "mirai", "renv"))
if (!requireNamespace("renv", quietly = TRUE)) stop("Install renv to refresh the deployment lock.")
project <- tempfile("fieldhub-deployment-lock-")
dir.create(project)
lockfile <- file.path(root, "deployment", "renv.lock")
dir.create(dirname(lockfile), showWarnings = FALSE)
tryCatch({
  renv::snapshot(project = project, lockfile = lockfile, packages = packages,
    repos = c(CRAN = "https://cloud.r-project.org"), prompt = FALSE)
  lock <- renv::lockfile_read(lockfile)
  lock$Packages <- lapply(lock$Packages, function(package) {
    package[intersect(c("Package", "Version", "Source", "Repository", "Hash"), names(package))]
  })
  invisible(renv::lockfile_write(lock, lockfile))
}, finally = unlink(project, recursive = TRUE))
cat("Review the lockfile diff and build the image before accepting updated versions.\n")
