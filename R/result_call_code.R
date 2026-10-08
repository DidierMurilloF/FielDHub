#' Explain values that cannot be embedded as portable, inert R data
#' @noRd
call_value_problem <- function(value) {
  if (is.null(value)) return(NULL)
  if (!typeof(value) %in% c("logical", "integer", "double", "complex", "character", "raw", "list")) {
    return("recorded arguments contain values that are not portable data")
  }
  supported <- c("data.frame", "factor", "ordered", "Date", "POSIXct", "POSIXt", "difftime", "AsIs")
  if (any(!attr(value, "class", exact = TRUE) %in% supported)) {
    return("recorded arguments contain unsupported data classes")
  }
  if (is.data.frame(value) && nrow(value) > 200L) {
    return("an uploaded table has more than 200 rows")
  }
  children <- c(if (is.list(value)) unclass(value) else list(), attributes(value))
  for (child in children) {
    problem <- call_value_problem(child)
    if (!is.null(problem)) return(problem)
  }
  NULL
}

#' Recognize constant defaults without evaluating dynamic default expressions
#' @noRd
call_constant_default <- function(value) {
  if (is.null(value) || is.atomic(value)) return(TRUE)
  if (!is.call(value) || !is.symbol(value[[1L]]) ||
      !as.character(value[[1L]]) %in% c("c", "list", "+", "-")) return(FALSE)
  all(vapply(as.list(value)[-1L], call_constant_default, logical(1)))
}

#' Arguments whose explicit presence an engine distinguishes from omission
#' @noRd
call_presence_arguments <- function(expr) {
  if (missing(expr) || (!is.call(expr) && !is.pairlist(expr))) return(character())
  parts <- as.list(expr)
  if (is.call(expr) && identical(parts[[1L]], as.name("missing")) &&
      length(parts) == 2L && is.symbol(parts[[2L]])) return(as.character(parts[[2L]]))
  unique(unlist(lapply(parts, call_presence_arguments), use.names = FALSE))
}

#' Self-contained R source for a recorded design or allocation
#'
#' Constant defaults are omitted, but the effective seed is always retained.
#' A local scope restores the caller's RNG state after using the recorded RNG
#' settings. Values are deparsed as data, never evaluated to generate source.
#' An empty character vector with a reason attribute denotes an RDS fallback:
#' NULL itself cannot carry attributes in R.
#' @noRd
design_call_code <- function(x, width = 80L) {
  validate_iteration_budget(width, "width", minimum = 20)
  if (width > 500L) fieldhub_abort("`width` must not exceed 500.")
  unavailable <- function(reason) structure(character(), reason = reason)
  engine <- tryCatch(reproduction_engine(x), fieldhub_error = identity)
  if (inherits(engine, "fieldhub_error")) return(unavailable(conditionMessage(engine)))
  parameters <- translate_legacy_parameters(x$metadata$design, x$metadata$parameters)
  defaults <- formals(engine)
  preserve <- c("seed", call_presence_arguments(body(engine)))
  if (!all(names(parameters) %in% names(defaults))) {
    return(unavailable("recorded arguments are not supported by the installed engine"))
  }
  allocation_code <- character()
  values <- list()
  for (name in names(parameters)) {
    value <- parameters[[name]]
    if (inherits(value, "Sparse") || inherits(value, "MultiPrep")) {
      nested <- design_call_code(value, width)
      if (!length(nested)) {
        return(unavailable(paste("the supplied allocation cannot be rebuilt:", attr(nested, "reason"))))
      }
      variable <- paste0("alloc", if (length(allocation_code)) length(allocation_code) else "")
      nested[1L] <- paste0(variable, " <- ", nested[1L])
      allocation_code <- c(allocation_code, nested)
      values[[name]] <- variable
      next
    }
    problem <- call_value_problem(value)
    if (!is.null(problem)) return(unavailable(paste0("`", name, "`: ", problem)))
    if (!name %in% preserve && !identical(defaults[[name]], quote(expr = )) &&
        call_constant_default(defaults[[name]]) &&
        identical(value, eval(defaults[[name]], envir = baseenv()))) next
    values[[name]] <- deparse(value, width.cutoff = width,
      control = c("keepInteger", "showAttributes", "quoteExpressions", "digits17"))
  }
  arguments <- unlist(lapply(names(values), function(name) {
    lines <- values[[name]]
    lines[1L] <- paste0(deparse(as.name(name), backtick = TRUE), " = ", lines[1L])
    lines[length(lines)] <- paste0(lines[length(lines)], ",")
    paste0("  ", lines)
  }), use.names = FALSE)
  arguments[length(arguments)] <- sub(",$", "", arguments[length(arguments)])
  public_name <- unname(fieldhub_engine_registry()[x$metadata$design])
  rng <- paste(deparse(x$metadata$rng_kind), collapse = " ")
  c(
    "local({",
    "  .rng_kind <- RNGkind()",
    "  .had_seed <- exists(\".Random.seed\", globalenv(), inherits = FALSE)",
    "  .rng_seed <- if (.had_seed) get(\".Random.seed\", globalenv())",
    "  on.exit({",
    "    do.call(RNGkind, as.list(.rng_kind))",
    "    if (.had_seed) assign(\".Random.seed\", .rng_seed, globalenv())",
    "    else if (exists(\".Random.seed\", globalenv(), inherits = FALSE))",
    "      rm(\".Random.seed\", envir = globalenv())",
    "  }, add = TRUE)",
    paste0("  .recorded_kind <- as.list(unname(", rng, "))"),
    "  if (length(.recorded_kind) == 4L && !\"binom.kind\" %in% names(formals(RNGkind)))",
    "    stop(\"The recorded binomial RNG requires R >= 4.7.\")",
    "  if (length(.recorded_kind) == 3L && \"binom.kind\" %in% names(formals(RNGkind)))",
    "    .recorded_kind[[4L]] <- \"Buggy BTPE\"",
    "  do.call(RNGkind, .recorded_kind)",
    if (length(allocation_code)) paste0("  ", allocation_code),
    paste0("  FielDHub::", public_name, "("),
    paste0("  ", arguments),
    "  )",
    "})"
  )
}

#' Standalone section shared by the app panel and workflow archive
#' @noRd
design_call_section <- function(x) {
  code <- design_call_code(x)
  if (!length(code)) {
    return(c(paste0("# Standalone call unavailable: ", encodeString(attr(code, "reason")), "."),
             "# Use the saved RDS and reconstruction instructions below.", ""))
  }
  code[1L] <- paste0("design <- ", code[1L])
  c("# Standalone call: no input files are needed for this section.",
    "# Use the recorded software versions to reproduce numerical optimization.",
    code, "", "# Optional: load the exact saved result and its full workflow context.")
}
