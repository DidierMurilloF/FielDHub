#' Run one public design engine without exposing conditions to a worker caller
#' @noRd
run_design_job <- function(engine, args, rng_kind = RNGkind()) {
  warnings <- list()
  tryCatch({
    local_rng_state()
    previous_kind <- RNGkind()
    on.exit(do.call(RNGkind, as.list(previous_kind)), add = TRUE, after = FALSE)
    engines <- unique(unname(fieldhub_engine_registry()))
    if (!is.character(engine) || !is.null(dim(engine)) || length(engine) != 1L ||
        is.na(engine) || !engine %in% engines) {
      fieldhub_abort("Choose a supported public design engine.")
    }
    if (!is.list(args) || is.data.frame(args) ||
        (length(args) && (is.null(names(args)) || anyNA(names(args)) ||
          any(!nzchar(names(args))) || anyDuplicated(names(args)) > 0L))) {
      fieldhub_abort("Design job arguments must be a list with distinct names.")
    }
    if (!identical(rng_kind, previous_kind)) do.call(RNGkind, as.list(rng_kind))
    captured <- capture_fieldhub_warnings(
      do.call(getExportedValue("FielDHub", engine), args, quote = TRUE),
      on_error = function(condition, recorded) warnings <<- recorded
    )
    list(ok = TRUE, value = captured$value, warnings = captured$warnings)
  }, error = function(condition) {
    list(ok = FALSE, condition = condition, warnings = warnings)
  })
}

#' Select the worker backend only when a dedicated pool is available
#' @noRd
design_task_backend <- function(mirai_available, daemons_running) {
  if (isTRUE(mirai_available) && isTRUE(daemons_running)) "mirai" else "sync"
}

#' Validate the optional app worker count without changing its representation
#' @noRd
design_worker_count <- function(workers) {
  validate_iteration_budget(workers, "workers", minimum = 0)
  invisible(workers)
}

#' Find the public name of a design spec's engine for a worker request
#' @noRd
design_engine_name <- function(engine) {
  engines <- unique(unname(fieldhub_engine_registry()))
  matches <- vapply(engines, function(name) {
    identical(engine, getExportedValue("FielDHub", name))
  }, logical(1))
  if (sum(matches) != 1L) fieldhub_abort("The design spec needs a supported public engine.")
  engines[matches]
}
