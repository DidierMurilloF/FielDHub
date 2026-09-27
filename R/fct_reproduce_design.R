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
  invisible(do.call(engine, meta$parameters, quote = TRUE))
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
  c(crd = "CRD", rcbd = "RCBD", latin_square = "latin_square",
    full_factorial = "full_factorial", split_plot = "split_plot",
    split_split_plot = "split_split_plot", strip_plot = "strip_plot",
    incomplete_blocks = "incomplete_blocks", row_column = "row_column",
    square_lattice = "square_lattice", rectangular_lattice = "rectangular_lattice",
    alpha_lattice = "alpha_lattice", partially_replicated = "partially_replicated",
    rcbd_augmented = "RCBD_augmented", diagonal_arrangement = "diagonal_arrangement",
    optimized_arrangement = "optimized_arrangement", split_families = "split_families",
    sparse_allocation = "sparse_allocation", multi_location_prep = "multi_location_prep",
    allocation_sparse = "do_optim", allocation_prep = "do_optim", pair_swap = "swap_pairs")
}
