#' Print recorded spacing without regenerating a design or rerunning search
#' @noRd
print_diagonal_spacing <- function(x, detailed = FALSE, location = NULL) {
  reports <- x$check_spacing
  if (is.null(reports)) return(invisible(NULL))
  number <- function(values, percentage = FALSE) {
    ifelse(is.na(values), "NA", if (percentage) sprintf("%.1f%%", values) else sprintf("%.3f", values))
  }
  metric_labels <- c("Minimum same-check distance", "Mean nearest same-check distance",
    "Mean pairwise same-check distance", "Occurrences with a same-check neighbor")
  for (site in if (is.null(location)) seq_along(reports) else location) {
    report <- reports[[site]]
    before <- diagonal_spacing_totals(report$before)
    optimized <- !is.null(report$after)
    cat("\nCheck spacing \u2014 Location ", site, "\n", sep = "")
    cat("Optimization:", if (optimized) "ON" else "OFF", "\n")
    cat("Distance: ", if (report$dist_method == "euclidean") "Euclidean" else "Manhattan",
      ", in grid steps\n", sep = "")
    cat("Same-check neighbor radius:", report$neighbor_radius, "grid steps\n\n")
    formatted <- function(totals) c(number(totals[1:3]), number(totals[4], TRUE))
    table <- data.frame(Metric = metric_labels, stringsAsFactors = FALSE)
    if (optimized) {
      table$Before <- formatted(before)
      table$After <- formatted(diagonal_spacing_totals(report$after))
    } else table$Value <- formatted(before)
    print(table, row.names = FALSE, right = TRUE)
    if (detailed) {
      cat("\nPer-check spacing:\n")
      metrics <- report$before
      details <- data.frame(Check = metrics$TREATMENT, Reps = metrics$occurrences,
        stringsAsFactors = FALSE)
      add_columns <- function(data, prefix) {
        details[[paste0(prefix, "minimum")]] <<- number(data$min_distance)
        details[[paste0(prefix, "nearest")]] <<- number(data$mean_nearest_distance)
        details[[paste0(prefix, "pairwise")]] <<- number(data$mean_pairwise_distance)
        details[[paste0(prefix, "neighbors")]] <<- number(
          100 * data$neighboring_occurrences / data$occurrences, TRUE)
      }
      add_columns(metrics, if (optimized) "Before " else "")
      if (optimized) add_columns(report$after, "After ")
      print(details, row.names = FALSE, right = TRUE)
    }
    if (optimized) {
      diagnostics <- x$infoDesign$check_optimization[[site]]
      cat("\nSearch:", gsub("_", " ", diagnostics$stop_reason), "\n")
      cat("Accepted swaps:", diagnostics$swaps, "\n")
      cat("Candidate evaluations:", format(diagnostics$evaluations, big.mark = ",", scientific = FALSE), "\n")
    }
  }
  cat("\nDistance averages give equal weight to checks with repeated occurrences.\n",
    "Minimum distance is the smallest across checks.\n",
    "Neighbor percentage includes all check occurrences.\n",
    "NA means that a same-check distance cannot be calculated.\n",
    "Spacing metrics do not measure statistical precision.\n", sep = "")
  invisible(NULL)
}
