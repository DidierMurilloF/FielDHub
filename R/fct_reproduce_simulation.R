#' Reconstruct recorded simulated responses
#'
#' @description Replays a classic truncated-normal or spatial AR1-by-AR1
#' simulation using its saved input field book, parameters, seed, and RNG settings.
#'
#' @param x A simulation record containing \code{input_field_book},
#' \code{field_book}, and \code{metadata}. Metadata must contain \code{model},
#' \code{schema_version}, \code{seed}, \code{rng_kind}, \code{package_version},
#' and the named \code{parameters} used by that model.
#'
#' @details This reconstructs the responses, not the experimental design or its
#' layout: it starts from the exact saved input field book. Use
#' \code{reproduce_design()} separately to reconstruct the experimental design.
#' The caller's RNG settings and \code{.Random.seed} are restored on exit.
#' Recorded arguments are passed as values rather than evaluated as R code.
#' A changed FielDHub version signals a \code{fieldhub_reproduction_warning};
#' reproduction across software versions or platforms is not guaranteed.
#'
#' Spatial \code{min_value} and \code{max_value} determine the model's center
#' and genetic-effect scale, not hard response bounds. Spatial records retain
#' unrounded per-location simulation values separately from the field book,
#' whose responses are rounded to two decimal places. Classic records use
#' a truncated-normal model, with rounding also applied to the field book.
#'
#' @return The reconstructed simulation record, invisibly.
#' @seealso \code{reproduce_design()}
#' @export
reproduce_simulation <- function(x) {
  engine <- simulation_reproduction_engine(x)
  meta <- x$metadata
  current_version <- as.character(utils::packageVersion("FielDHub"))
  if (!identical(meta$package_version, current_version)) {
    fieldhub_warn(
      "This simulation was generated with FielDHub ", meta$package_version,
      "; the installed version is ", current_version, ". Its output may differ.",
      class = "fieldhub_reproduction_warning",
      data = list(recorded_version = meta$package_version, installed_version = current_version)
    )
  }
  local_rng_state()
  previous_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous_kind)), add = TRUE, after = FALSE)
  tryCatch(
    restore_recorded_rng(meta$rng_kind),
    error = function(e) fieldhub_abort(
      "Cannot restore the recorded simulation RNG settings: ", conditionMessage(e),
      data = list(parent = e)
    )
  )
  invisible(do.call(engine, c(list(field_book = x$input_field_book), meta$parameters),
                    quote = TRUE))
}

#' Validate the record and select a known simulation service
#' @noRd
simulation_reproduction_engine <- function(x) {
  if (!is.list(x) || is.data.frame(x) || !is.data.frame(x$input_field_book) ||
      !is.data.frame(x$field_book) || !is.list(x$metadata)) {
    fieldhub_abort("A simulation record must contain input_field_book, field_book, and metadata.")
  }
  meta <- x$metadata
  problems <- fieldhub_metadata_problems(meta)
  if (length(problems) > 0L || is.null(meta$parameters)) {
    fieldhub_abort("The simulation has invalid or missing recorded parameters or RNG metadata.")
  }
  if (!is.character(meta$model) || length(meta$model) != 1L || is.na(meta$model) ||
      !meta$model %in% c("truncated_normal", "ar1xar1")) {
    fieldhub_abort("The recorded simulation model is not supported.")
  }
  engine <- if (meta$model == "truncated_normal") {
    simulate_classic_field_book
  } else simulate_spatial_field_book
  expected <- setdiff(names(formals(engine)), "field_book")
  if (!setequal(names(meta$parameters), expected)) {
    fieldhub_abort("The recorded parameters do not match the simulation model.")
  }
  engine
}
