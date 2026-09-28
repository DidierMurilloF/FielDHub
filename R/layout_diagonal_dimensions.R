#' Search margins of the diagonal field-size candidates
#'
#' @description Candidate fields have `minimum` to `maximum` more plots
#' than the entries they hold (10% to 20% by default). The one source for
#' `field_dimensions()`, `diagonal_dimension_choices()` and the sizes
#' `diagonal_arrangement()` names in its "dimensions do not match" error.
#' @noRd
.diagonal_size_margins <- c(minimum = 0.10, maximum = 0.20)

#' Smallest and largest candidate field size for a number of entries
#'
#' @param lines_within_loc Number of experimental entries, excluding checks.
#' @param minimum_extra Lower search margin relative to the number of entries.
#' @return `c(smallest, largest)`, whole numbers of plots.
#' @noRd
field_size_range <- function(lines_within_loc,
                             minimum_extra = .diagonal_size_margins[["minimum"]]) {
    c(floor(lines_within_loc + lines_within_loc * minimum_extra),
      ceiling(lines_within_loc + lines_within_loc * .diagonal_size_margins[["maximum"]]))
}

#' Candidate field dimensions for diagonal checks
#'
#' @param lines_within_loc Number of experimental entries, excluding checks.
#' @param minimum_extra Lower search margin relative to the number of entries.
#' @return Lists of rectangular dimension labels, in candidate-size order.
#' @noRd
field_dimensions <- function(lines_within_loc,
                             minimum_extra = .diagonal_size_margins[["minimum"]]) {
    cached_field_dimensions(lines_within_loc, minimum_extra)
}

#' Compute uncached candidates in their established order
#' @noRd
find_field_dimensions <- function(lines_within_loc, minimum_extra) {
    range <- field_size_range(lines_within_loc, minimum_extra)
    t <- range[1]:range[2]
    non_primes <- t[!is_prime(t)]
    choices_list <- list()
    i <- 1
    for (n in non_primes) {
        choices_list[[i]] <- factor_subsets(n, diagonal = TRUE)$labels
        i <- i + 1
    }
    return(choices_list)
}

#' Bounded least-recently-used cache for deterministic dimension queries
#'
#' Only successful, warning-free results are retained. Exact argument values,
#' types and attributes distinguish queries. The byte bound covers retained
#' keys and values, not temporary allocations during the search itself.
#' @noRd
new_dimension_cache <- function(compute, max_entries = 64L, max_bytes = 4 * 1024^2) {
  valid_bound <- function(x) {
    is.numeric(x) && !is.complex(x) && length(x) == 1L &&
      is.finite(x) && x >= 1 && x %% 1 == 0
  }
  if (!is.function(compute) || !valid_bound(max_entries) || !valid_bound(max_bytes)) {
    fieldhub_abort("A dimension cache needs a function and positive whole-number bounds.")
  }
  entries <- list()
  sizes <- numeric()
  function(...) {
    key <- list(...)
    hits <- which(vapply(entries, function(entry) identical(entry$key, key), logical(1)))
    if (length(hits) > 0L) {
      i <- hits[1]
      result <- entries[[i]]$value
      order <- c(setdiff(seq_along(entries), i), i)
      entries <<- entries[order]
      sizes <<- sizes[order]
      return(result)
    }
    warned <- FALSE
    result <- withCallingHandlers(compute(...), warning = function(e) warned <<- TRUE)
    if (warned) return(result)
    entry <- list(key = key, value = result)
    bytes <- as.numeric(utils::object.size(entry))
    if (bytes > max_bytes) return(result)
    while (length(entries) >= max_entries || sum(sizes) + bytes > max_bytes) {
      entries <<- entries[-1L]
      sizes <<- sizes[-1L]
    }
    entries[[length(entries) + 1L]] <<- entry
    sizes <<- c(sizes, bytes)
    result
  }
}

#' Cached candidate searches shared by the core and app
#' @noRd
cached_field_dimensions <- new_dimension_cache(function(lines_within_loc, minimum_extra) {
  find_field_dimensions(lines_within_loc, minimum_extra)
})

#' Query diagonal check options without advancing the random-number stream
#'
#' @param ... Arguments passed to the legacy check-placement helper.
#' @return The option tables and check maps from `available_percent()`.
#' @noRd
diagonal_check_options <- function(...) {
    local_rng_state()
    available_percent(...)
}

#' Feasible dimensions for single, multiple and sparse diagonal arrangements
#'
#' @param lines Number of experimental plots in this location, excluding checks.
#' @param checks Numeric entry identifiers of the checks.
#' @param kindExpt Either `SUDC` or `DBUDC`.
#' @param stacked Either `By Row` or `By Column` for multiple arrangements.
#' @param planter Either `serpentine` or `cartesian`.
#' @param data Entry list with checks first and a `BLOCK` column for `DBUDC`.
#' @param minimum_extra Lower search margin; the upper margin is 20 percent.
#' @return Character dimension labels, ordered by increasing difference between
#'   rows and columns. An empty vector means no feasible option was found.
#' @noRd
diagonal_dimension_choices <- function(lines, checks, kindExpt = "SUDC",
                                       stacked = "By Row", planter = "serpentine",
                                       data = NULL,
                                       minimum_extra = .diagonal_size_margins[["minimum"]]) {
    invalid_lines <- !is.numeric(lines) || length(lines) != 1L ||
        is.na(lines) || !is.finite(lines) || lines < 1 || lines %% 1 != 0 ||
        lines > floor(.Machine$integer.max / 1.20)
    if (invalid_lines) {
        fieldhub_abort("`lines` must be one positive whole number in the supported range.")
    }
    invalid_checks <- !is.numeric(checks) || length(checks) == 0L ||
        anyNA(checks) || any(!is.finite(checks)) || any(checks < 1) ||
        any(checks %% 1 != 0) || anyDuplicated(checks) > 0
    if (invalid_checks) {
        fieldhub_abort("`checks` must contain distinct positive whole-number identifiers.")
    }
    choices <- list(kindExpt = c("SUDC", "DBUDC"),
                    stacked = c("By Row", "By Column"),
                    planter = c("serpentine", "cartesian"))
    values <- list(kindExpt = kindExpt, stacked = stacked, planter = planter)
    for (name in names(choices)) {
        value <- values[[name]]
        if (!is.character(value) || length(value) != 1L || is.na(value) ||
            !value %in% choices[[name]]) {
            fieldhub_abort("`", name, "` must be one of: ",
                           paste(choices[[name]], collapse = ", "), ".")
        }
    }
    if (!is.numeric(minimum_extra) || length(minimum_extra) != 1L ||
        is.na(minimum_extra) || !is.finite(minimum_extra) ||
        minimum_extra < 0 || minimum_extra > .diagonal_size_margins[["maximum"]]) {
        fieldhub_abort("`minimum_extra` must be one number between 0 and ",
                       sprintf("%.2f", .diagonal_size_margins[["maximum"]]), ".")
    }
    if (kindExpt == "DBUDC") {
        if (!is.data.frame(data) || !all(c("ENTRY", "BLOCK") %in% names(data)) ||
            nrow(data) != lines + length(checks)) {
            fieldhub_abort("`data` must contain the checks followed by all lines, with ENTRY and BLOCK columns.")
        }
        block <- suppressWarnings(as.numeric(as.character(data$BLOCK[-seq_along(checks)])))
        if (anyNA(block) || any(!is.finite(block)) || any(block < 1) ||
            any(block %% 1 != 0) ||
            !identical(sort(unique(block)), as.numeric(seq_len(length(unique(block)))))) {
            fieldhub_abort("`data$BLOCK` must number the experimental blocks consecutively from one.")
        }
    }

    candidates <- unlist(field_dimensions(lines, minimum_extra), use.names = FALSE)
    if (length(candidates) == 0L) return(character())
    dims <- do.call(rbind, strsplit(candidates, " x ", fixed = TRUE))
    storage.mode(dims) <- "integer"
    feasible <- vapply(seq_along(candidates), function(i) {
        options <- diagonal_check_options(
            n_rows = dims[i, 1], n_cols = dims[i, 2], checks = checks,
            Option_NCD = TRUE, kindExpt = kindExpt, stacked = stacked,
            planter_mov1 = planter, data = data,
            dim_data = lines + length(checks), dim_data_1 = lines
        )
        !is.null(options$dt)
    }, logical(1))
    candidates <- candidates[feasible]
    dims <- dims[feasible, , drop = FALSE]
    candidates[order(abs(dims[, 1] - dims[, 2]))]
}
