# Plain script-entry contract check. No packages are detached, no documentation
# is regenerated, and no Shiny server is started by this check.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) stop("Usage: Rscript tools/check-launchers.R <repo>")
root <- normalizePath(arguments[[1L]], mustWork = TRUE)
check_launcher <- function(file, serve) {
  calls <- list()
  app <- structure(list(), class = "launcher_sentinel")
  record <- function(name, value = NULL) function(...) {
    calls[[length(calls) + 1L]] <<- list(name = name, args = list(...))
    value
  }
  environment <- new.env(parent = baseenv())
  environment[["::"]] <- function(package, name) {
    key <- paste(as.character(substitute(package)), as.character(substitute(name)), sep = "::")
    switch(key,
      "pkgload::load_all" = record("load"),
      "FielDHub::run_app" = record("app", app),
      "shiny::runApp" = record("serve"),
      stop("Unexpected launcher dependency: ", key))
  }
  # Fail before evaluation if a script mutates options or the search path.
  code <- parse(file.path(root, file))
  names <- all.names(code, functions = TRUE)
  stopifnot(!any(c("options", "detach_all_attached", "document_and_reload", "rm", "setwd") %in% names))
  result <- eval(code, environment)
  expected <- if (serve) c("load", "app", "serve") else c("load", "app")
  stopifnot(identical(vapply(calls, `[[`, character(1), "name"), expected))
  stopifnot(identical(calls[[1L]]$args,
    list(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)))
  stopifnot(length(calls[[2L]]$args) == 0L)
  if (serve) stopifnot(identical(calls[[3L]]$args, list(app))) else stopifnot(identical(result, app))
}
check_launcher("dev/run_dev.R", serve = TRUE)
check_launcher("app.R", serve = FALSE)
cat("Development and deployment launcher contracts: OK\n")
