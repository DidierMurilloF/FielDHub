#' Compare counts of same-check pairs, shortest distances first
#' @noRd
diagonal_distance_improves <- function(candidate, current) {
  different <- which(candidate != current)
  length(different) > 0L && candidate[different[1L]] < current[different[1L]]
}

#' Exchange check identities while preserving all other field positions
#'
#' Distances between check slots are fixed. A candidate changes only pairs
#' incident to the two exchanged slots; counting those pairs gives the exact
#' change in the full distance profile. Fewer pairs at the shortest differing
#' distance is a strict improvement, even before the minimum distance rises.
#' Enumeration is deterministic and bounded by swaps and candidate evaluations.
#' @noRd
optimize_diagonal_checks <- function(layout, checks, dist_method = "euclidean",
                                      max_swaps = 100L, max_evaluations = 100000L) {
  swap_distance_function(dist_method)
  validate_iteration_budget(max_swaps, "max_swaps", minimum = 0)
  validate_iteration_budget(max_evaluations, "max_evaluations", minimum = 0)
  slots <- matrix(layout %in% as.character(checks), nrow(layout), ncol(layout))
  labels <- as.numeric(layout[slots])
  frequency <- table(labels)
  diagnostics <- list(swaps = 0L, evaluations = 0L, stop_reason = "no_exchange",
                      before_min = NA_real_, after_min = NA_real_, dist_method = dist_method)
  if (length(frequency) == 0L || all(frequency == 1L)) {
    diagnostics$stop_reason <- "no_repeated_checks"
    return(list(layout = layout, diagnostics = diagnostics))
  }
  positions <- which(slots, arr.ind = TRUE)
  distances <- as.matrix(stats::dist(positions, method = dist_method))
  pairs <- which(upper.tri(distances), arr.ind = TRUE)
  colnames(pairs) <- c("first", "second")
  levels <- sort(unique(distances[pairs]))
  bins <- matrix(match(distances, levels), nrow(distances), ncol(distances))
  same <- labels[pairs[, "first"]] == labels[pairs[, "second"]]
  counts <- tabulate(bins[pairs[same, , drop = FALSE]], nbins = length(levels))
  diagnostics$before_min <- levels[which(counts > 0L)[1L]]
  diagnostics$after_min <- diagnostics$before_min
  if (length(frequency) < 2L) return(list(layout = layout, diagnostics = diagnostics))

  repeat {
    if (diagnostics$evaluations >= max_evaluations) {
      diagnostics$stop_reason <- "evaluation_limit"
      break
    }
    if (diagnostics$swaps >= max_swaps) {
      diagnostics$stop_reason <- "swap_limit"
      break
    }
    groups <- split(seq_along(labels), labels)
    best_delta <- integer(length(levels))
    best_pair <- integer()
    for (i in seq_len(nrow(pairs))) {
      first <- pairs[i, "first"]
      second <- pairs[i, "second"]
      if (labels[first] == labels[second]) next
      if (diagnostics$evaluations >= max_evaluations) break
      diagnostics$evaluations <- diagnostics$evaluations + 1L
      first_group <- setdiff(groups[[as.character(labels[first])]], first)
      second_group <- setdiff(groups[[as.character(labels[second])]], second)
      old <- c(bins[first, first_group], bins[second, second_group])
      new <- c(bins[second, first_group], bins[first, second_group])
      delta <- tabulate(new, nbins = length(levels)) - tabulate(old, nbins = length(levels))
      if (diagonal_distance_improves(delta, best_delta)) {
        best_delta <- delta
        best_pair <- c(first, second)
      }
    }
    if (!length(best_pair)) {
      diagnostics$stop_reason <- if (diagnostics$evaluations >= max_evaluations) {
        "evaluation_limit"
      } else "local_optimum"
      break
    }
    labels[best_pair] <- rev(labels[best_pair])
    counts <- counts + best_delta
    diagnostics$swaps <- diagnostics$swaps + 1L
  }
  result <- layout
  result[slots] <- labels
  diagnostics$after_min <- levels[which(counts > 0L)[1L]]
  list(layout = result, diagnostics = diagnostics)
}

#' Synchronize check identities in a field book after check-only exchanges
#' @noRd
diagonal_optimized_field_book <- function(book, layout, entries) {
  rows <- book$CHECKS != 0
  coordinates <- cbind(nrow(layout) + 1L - book$ROW[rows], book$COLUMN[rows])
  ids <- as.numeric(layout[coordinates])
  book$ENTRY[rows] <- if (is.integer(book$ENTRY)) as.integer(ids) else ids
  book$CHECKS[rows] <- if (is.integer(book$CHECKS)) as.integer(ids) else ids
  book$TREATMENT[rows] <- as.character(entries$NAME[match(ids, entries$ENTRY)])
  book
}
