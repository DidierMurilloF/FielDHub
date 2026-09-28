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
