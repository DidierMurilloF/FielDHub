# Shared helpers for static/structural tests that inspect package function
# bodies through the namespace (asNamespace("FielDHub")) instead of parsing
# R/ source files. R/ is not available under R CMD check, so these work
# there too. See .superpowers/sdd/2026-09-27-m1-m3-completion/constraints.md
# ruling R2.

#' Functions defined in the FielDHub namespace, restricted to core code
#'
#' @return A named list of functions whose environment is the FielDHub
#'   namespace, excluding Shiny app/module/golem entry points
#'   (`app_*`, `golem_*`, `mod_*`, `run_app`).
core_functions <- function() {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions[!grepl("^(app_|golem_|mod_)|^run_app$", names(functions))]
}

#' Shiny app/module functions defined in the FielDHub namespace
#'
#' @return A named list of functions whose environment is the FielDHub
#'   namespace, restricted to `app_*`/`mod_*` entry points.
app_functions <- function() {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions[grepl("^(app_|mod_)", names(functions))]
}
