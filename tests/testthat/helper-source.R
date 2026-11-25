# Helpers for the hard-rule guards in test_package_hygiene.R (source layout,
# dependency hygiene). They inspect package functions through the namespace
# (asNamespace("FielDHub")) instead of parsing R/ source files, because R/ is
# not available under R CMD check.

#' Functions defined in the FielDHub namespace, restricted to core code
#'
#' @return A named list of functions whose environment is the FielDHub
#'   namespace, excluding Shiny app/module entry points
#'   (`app_*`, `mod_*`, `run_app`).
core_functions <- function() {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions[!grepl("^(app_|mod_)|^run_app$", names(functions))]
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

#' Every symbol `expr` refers to, except the field names of `x$name` and
#' `x@name`
#'
#' Like `all.names(expr)`, but an input/output ID such as
#' `output$sparse_allocation` does not count as a reference to the
#' same-named package function.
#'
#' @return A character vector (with repeats).
#' @noRd
fieldhub_symbol_refs <- function(expr) {
  refs <- character()
  walk <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (nzchar(name)) refs <<- c(refs, name)
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    field_access <- is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("$", "@")
    n <- length(e)
    for (i in seq_len(n)) {
      if (field_access && i == 3L) next
      if (is.call(e[[i]]) || is.symbol(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  refs
}

