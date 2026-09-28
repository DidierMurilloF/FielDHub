#' Record effective arguments after design generation
#'
#' Called directly from an engine, after randomization, so recording does not
#' move evaluation of input expressions ahead of the scientific calculation.
#' Overrides preserve values that an engine expands internally, such as the
#' starting plot numbers. Seed is always the resolved seed, not NULL.
#' @noRd
record_design_parameters <- function(frame, overrides = list(), exclude = character()) {
  defaults <- formals(sys.function(-1L))
  arguments <- setdiff(names(defaults), exclude)
  # Some engines have optional arguments with no default, which remain
  # genuinely missing when unused. Do not force those promises.
  available <- vapply(arguments, function(name) {
    name %in% names(overrides) || !identical(defaults[[name]], quote(expr = )) ||
      !eval(call("missing", as.name(name)), envir = frame)
  }, logical(1))
  arguments <- arguments[available]
  setNames(lapply(arguments, function(name) {
    if (name %in% names(overrides)) overrides[[name]] else {
      get(name, envir = frame, inherits = FALSE)
    }
  }), arguments)
}

#' Validate optional reproduction arguments without rejecting older metadata
#' @noRd
recorded_parameter_problems <- function(metadata) {
  parameters <- metadata$parameters
  if (is.null(parameters)) return(character())
  if (!is.list(parameters) || is.data.frame(parameters) || is.null(names(parameters)) ||
      anyNA(names(parameters)) || any(!nzchar(names(parameters))) ||
      anyDuplicated(names(parameters)) > 0L) {
    return("has invalid recorded input parameters")
  }
  if (!identical(parameters$seed, metadata$seed)) {
    return("has recorded input parameters with a different seed")
  }
  character()
}
