#' Reconstruct a design from its recorded inputs
#'
#' @description Calls the design engine using the input parameters and
#' random-number settings recorded in a result's metadata.
#'
#' @param x A FielDHub design, allocation plan, or pair-swap optimization result
#' with recorded parameters.
#'
#' @details The caller's RNG settings and \code{.Random.seed} are restored
#' on exit, including when reconstruction fails. Arguments are passed as
#' values, so language objects in the parameter list are not evaluated as code.
#'
#' Reproduction requires the same software versions and platform for algorithms
#' whose results depend on numerical optimization. A different FielDHub version
#' signals a \code{fieldhub_reproduction_warning}; dependency or platform
#' differences are not checked. Older results without recorded parameters cannot
#' be reconstructed automatically, but remain usable for printing and plotting.
#'
#' @return A newly generated design or allocation plan, invisibly.
#' @examples
#' x <- RCBD(t = 5, reps = 3, seed = 38)
#' identical(reproduce_design(x), x)
#' @export
reproduce_design <- function(x) {
  engine <- reproduction_engine(x)
  meta <- x$metadata
  current_version <- as.character(utils::packageVersion("FielDHub"))
  if (!identical(meta$package_version, current_version)) {
    fieldhub_warn(
      "This result was generated with FielDHub ", meta$package_version,
      "; the installed version is ", current_version, ". Its output may differ.",
      class = "fieldhub_reproduction_warning",
      data = list(recorded_version = meta$package_version, installed_version = current_version)
    )
  }
  local_rng_state()
  previous_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous_kind)), add = TRUE, after = FALSE)
  tryCatch(
    do.call(RNGkind, as.list(meta$rng_kind)),
    error = function(e) fieldhub_abort(
      "Cannot restore the recorded RNG settings: ", conditionMessage(e),
      data = list(parent = e)
    )
  )
  parameters <- translate_legacy_parameters(x$metadata$design, meta$parameters)
  invisible(do.call(engine, parameters, quote = TRUE))
}

#' Rename a recorded result's legacy parameter names to their current ones
#'
#' @description A result saved by an older FielDHub version can have its
#' parameters recorded under an argument name that has since gained a
#' vocabulary alias (AD-08), such as `optimized_arrangement()`'s former
#' `amountChecks` (now `rep_checks`). Replaying it by calling the current
#' engine with that legacy name would still work (the alias itself still
#' accepts it), but would raise a `fieldhub_deprecated_warning` about an
#' argument the replaying caller never typed. Rename it before replay so
#' reproduce_design() is silent for both old- and new-shape recordings.
#'
#' @param design Recorded `metadata$design` identifier.
#' @param parameters Recorded `metadata$parameters` list.
#' @return `parameters`, with any known legacy names renamed to current ones.
#' @noRd
translate_legacy_parameters <- function(design, parameters) {
  legacy_names <- list(optimized_arrangement = c(amountChecks = "rep_checks"))
  renames <- legacy_names[[design]]
  if (is.null(renames)) return(parameters)
  for (old in names(renames)) {
    new <- renames[[old]]
    if (old %in% names(parameters) && !(new %in% names(parameters))) {
      names(parameters)[names(parameters) == old] <- new
    }
  }
  parameters
}

#' Resolve a recorded engine without evaluating arbitrary function names
#' @noRd
reproduction_engine <- function(x) {
  if (!is.list(x)) fieldhub_abort("'x' must be a FielDHub design, allocation plan, or optimization result.")
  if (!is.list(x$metadata) || is.null(x$metadata$parameters)) {
    fieldhub_abort("This result has no recorded parameters; reconstruct it from its original inputs.")
  }
  if (inherits(x, "FielDHub")) {
    validate_fieldhub_design(x)
  } else if (inherits(x, "Sparse") || inherits(x, "MultiPrep")) {
    validate_fieldhub_allocation(x)
  } else if (inherits(x, "fieldhub_optimization")) {
    validate_fieldhub_optimization(x)
  } else {
    fieldhub_abort("'x' must be a FielDHub design, allocation plan, or optimization result.")
  }
  engines <- fieldhub_engine_registry()
  name <- unname(engines[x$metadata$design])
  if (length(name) != 1L || is.na(name)) {
    fieldhub_abort("The recorded design engine is not supported: ", x$metadata$design, ".")
  }
  get(name, envir = asNamespace("FielDHub"), inherits = FALSE)
}

#' Core design identifiers and their public engine names
#' @noRd
fieldhub_engine_registry <- function() {
  vapply(fieldhub_design_registry(), `[[`, character(1), "engine")
}
