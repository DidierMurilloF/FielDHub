# Static deployment contracts; no daemon, package installation, or app server.
arguments <- commandArgs(TRUE)
if (length(arguments) != 1L) stop("Pass the repository path.")
root <- normalizePath(arguments[1L], mustWork = TRUE)
lock <- jsonlite::read_json(file.path(root, "deployment", "renv.lock"))
docker <- readLines(file.path(root, "Dockerfile"))
stopifnot(any(grepl("^ARG BASE_IMAGE=rocker/r-ver:4\\.5\\.3@sha256:[a-f0-9]{64}$", docker)))
stopifnot(identical(lock$R$Version, "4.5.3"), identical(lock$Packages$renv$Version, "1.1.4"))
# fs 2.1.0 in the locked dependency set requires system libuv headers.
stopifnot(any(grepl("libuv1-dev", docker, fixed = TRUE)))
stopifnot(any(grepl("cmake xz-utils", docker, fixed = TRUE)))
environment <- new.env(parent = baseenv())
sys.source(file.path(root, "R/app_dependencies.R"), envir = environment)
description <- read.dcf(file.path(root, "DESCRIPTION"))
imports <- trimws(sub(" *\\(.*", "", strsplit(description[1L, "Imports"], ",")[[1L]]))
stopifnot(all(c(imports, environment$app_dependencies(), "mirai", "renv") %in% names(lock$Packages)))
stopifnot(!any(c("golem", "shinyalert", "shinycssloaders", "plotly", "kableExtra", "zip") %in%
                names(lock$Packages)))
for (name in names(lock$Packages)) {
  package <- lock$Packages[[name]]
  stopifnot(identical(package$Package, name), nzchar(package$Version),
            identical(package$Source, "Repository"), identical(package$Repository, "CRAN"))
}
stopifnot(any(docker == "USER 10001:10001"), any(docker == "RUN Rscript --vanilla check-runtime.R"))
stopifnot(any(grepl("shiny::runApp(FielDHub::run_app(launch.browser=FALSE)", docker, fixed = TRUE)))
stopifnot(!any(grepl("install.packages|remotes::|latest|sudo", docker) & !grepl("^#", docker)))
ignore <- readLines(file.path(root, ".dockerignore"))
stopifnot(all(c(".git", ".Renviron", ".Rprofile", ".superpowers", "ROADMAP.md") %in% ignore))
workflow <- yaml::read_yaml(file.path(root, ".github/workflows/deployment.yaml"))
steps <- workflow$jobs$image$steps
stopifnot(any(vapply(steps, function(x) !is.null(x$run) && grepl("docker build", x$run, fixed = TRUE), logical(1))))
cat("Pinned image, dependency lock, non-root runtime and deployment CI contracts: OK\n")
