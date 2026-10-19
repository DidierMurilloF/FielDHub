#' Retain complete successful design jobs within one session
#'
#' Keys contain the engine, all effective arguments and the RNG kind. Results
#' without a resolved seed are not reusable. Bounds cover retained keys and jobs.
#' @noRd
new_design_job_cache <- function(max_entries = 64L, max_bytes = 64 * 1024^2) {
  validate_iteration_budget(max_entries, "max_entries", minimum = 1)
  validate_iteration_budget(max_bytes, "max_bytes", minimum = 1)
  entries <- list()
  sizes <- numeric()
  get <- function(key) {
    hits <- which(vapply(entries, function(entry) identical(entry$key, key), logical(1)))
    if (!length(hits)) return(NULL)
    index <- hits[1L]
    value <- entries[[index]]$job
    order <- c(setdiff(seq_along(entries), index), index)
    entries <<- entries[order]
    sizes <<- sizes[order]
    value
  }
  put <- function(key, job) {
    if (!isTRUE(job$ok) || is.null(key$args$seed)) return(FALSE)
    entry <- list(key = key, job = job)
    size <- as.numeric(utils::object.size(entry))
    if (size > max_bytes) return(FALSE)
    keep <- !vapply(entries, function(old) identical(old$key, key), logical(1))
    entries <<- entries[keep]
    sizes <<- sizes[keep]
    while (length(entries) >= max_entries || sum(sizes) + size > max_bytes) {
      entries <<- entries[-1L]
      sizes <<- sizes[-1L]
    }
    entries[[length(entries) + 1L]] <<- entry
    sizes <<- c(sizes, size)
    TRUE
  }
  list(get = get, put = put)
}

#' Schedule the current selection before uncached alternatives
#'
#' Only one job is in flight. Selecting an option replaces the pending queue;
#' a completed older job may populate the cache but cannot replace the selected
#' result. This state is plain R so races and invalidation can be tested directly.
#' @noRd
new_design_option_queue <- function(cache = new_design_job_cache()) {
  selection <- NULL
  current <- NULL
  pending <- list()
  active <- NULL
  serial <- 0
  select <- function(key, alternatives = list()) {
    selection <<- key
    current <<- cache$get(key)
    pending <<- alternatives
    invisible(NULL)
  }
  clear <- function() {
    selection <<- NULL
    current <<- NULL
    pending <<- list()
    invisible(NULL)
  }
  next_job <- function(allow_prefetch = TRUE) {
    if (!is.null(active) || is.null(selection)) return(NULL)
    key <- NULL
    if (is.null(current)) {
      key <- selection
    } else if (allow_prefetch) {
      while (length(pending)) {
        candidate <- pending[[1L]]
        pending <<- pending[-1L]
        if (!identical(candidate, selection) && is.null(cache$get(candidate))) {
          key <- candidate
          break
        }
      }
    }
    if (is.null(key)) return(NULL)
    serial <<- serial + 1
    active <<- list(id = serial, key = key)
    active
  }
  finish <- function(token, job) {
    if (is.null(active) || !identical(active, token)) return(FALSE)
    cache$put(token$key, job)
    if (identical(token$key, selection)) current <<- job
    active <<- NULL
    TRUE
  }
  list(select = select, clear = clear, next_job = next_job, finish = finish,
       result = function() current)
}

#' Replace only the requested percentage for each valid diagonal option
#' @noRd
diagonal_option_requests <- function(args, percentages) {
  lapply(unique(as.numeric(percentages)), function(percent) {
    args$checksPercent <- percent
    args
  })
}
