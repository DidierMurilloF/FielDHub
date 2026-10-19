#' Same-check spacing measured independently of the optimization score
#'
#' Coordinates use equally scaled grid steps. A neighbor is another occurrence
#' of the same check within the declared radius, including its boundary.
#' @noRd
diagonal_spacing_metrics <- function(layout, checks, entries,
                                     dist_method = "euclidean", radius = 5) {
  rows <- lapply(checks, function(check) {
    positions <- which(layout == as.character(check), arr.ind = TRUE)
    count <- nrow(positions)
    distances <- as.matrix(stats::dist(positions, method = dist_method))
    pairs <- distances[upper.tri(distances)]
    neighbors <- distances <= radius + 1e-12
    diag(neighbors) <- FALSE
    diag(distances) <- Inf
    data.frame(
      ENTRY = check,
      TREATMENT = as.character(entries$NAME[match(check, entries$ENTRY)]),
      occurrences = count,
      min_distance = if (length(pairs)) min(pairs) else NA_real_,
      mean_nearest_distance = if (count > 1L) mean(apply(distances, 1L, min)) else NA_real_,
      mean_pairwise_distance = if (length(pairs)) mean(pairs) else NA_real_,
      neighboring_occurrences = sum(rowSums(neighbors) > 0L),
      close_pairs = sum(pairs <= radius + 1e-12),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Aggregate distances equally across checks, neighbors across occurrences
#' @noRd
diagonal_spacing_totals <- function(metrics) {
  available_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  c(min_distance = if (all(is.na(metrics$min_distance))) NA_real_ else
      min(metrics$min_distance, na.rm = TRUE),
    mean_nearest_distance = available_mean(metrics$mean_nearest_distance),
    mean_pairwise_distance = available_mean(metrics$mean_pairwise_distance),
    neighbor_percent = 100 * sum(metrics$neighboring_occurrences) / sum(metrics$occurrences))
}
