pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && length(args) != 4L) stop("Supply case, rows, columns, lines.")
geometry <- if (length(args)) as.integer(args[2:4]) else c(20L, 20L, 350L)
out <- if (length(args)) file.path("research/diagonal-spacing-metric-comparison", args[[1]]) else
  "research/diagonal-spacing-metric-comparison"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(!file.exists(file.path(out, "results.csv")))

# Independent evaluation from physical field-book coordinates. Both searches
# are assessed in BOTH units; distances with different units are never ranked.
evaluate <- function(book, checks, method) {
  distances <- lapply(checks, function(check) {
    positions <- as.matrix(book[book$ENTRY == check, c("ROW", "COLUMN")])
    d <- as.matrix(stats::dist(positions, method = method))
    pairs <- d[upper.tri(d)]
    near3 <- d <= 3 + 1e-12
    near5 <- d <= 5 + 1e-12
    diag(near3) <- diag(near5) <- FALSE
    diag(d) <- Inf
    c(minimum = min(pairs), nearest = mean(apply(d, 1, min)), pairwise = mean(pairs),
      pairs3 = sum(pairs <= 3 + 1e-12), pairs5 = sum(pairs <= 5 + 1e-12),
      neighbors5 = sum(rowSums(near5) > 0), occurrences = nrow(positions))
  })
  values <- do.call(rbind, distances)
  c(minimum = min(values[, "minimum"]), nearest = mean(values[, "nearest"]),
    worst_nearest = min(values[, "nearest"]), pairwise = mean(values[, "pairwise"]),
    pairs3 = sum(values[, "pairs3"]), pairs5 = sum(values[, "pairs5"]),
    clustered_percent5 = 100 * sum(values[, "neighbors5"]) / sum(values[, "occurrences"]))
}
rows <- list()
frozen <- list()
for (seed in 1:100) {
  original <- diagonal_arrangement(nrows = geometry[[1]], ncols = geometry[[2]], lines = geometry[[3]],
    checks = 4, seed = seed, year = 2026, spread_checks = FALSE)
  checks <- original$infoDesign$entry_checks[[1]]
  books <- list(original = original$fieldBook)
  layouts <- list(original = original$layoutRandom[[1]])
  runs <- list()
  methods <- if (seed %% 2L) c("euclidean", "manhattan") else c("manhattan", "euclidean")
  for (method in methods) {
    start <- proc.time()[["elapsed"]]
    result <- optimize_diagonal_checks(layouts$original, checks, dist_method = method,
      max_swaps = 100L, max_evaluations = 100000L)
    elapsed <- proc.time()[["elapsed"]] - start
    books[[method]] <- diagonal_optimized_field_book(original$fieldBook, result$layout,
      original$data_entry[[1]])
    layouts[[method]] <- result$layout
    nonchecks <- !original$fieldBook$ENTRY %in% checks
    stopifnot(identical(books[[method]][nonchecks, ], original$fieldBook[nonchecks, ]),
      identical(table(books[[method]]$ENTRY), table(original$fieldBook$ENTRY)),
      identical(result$layout %in% checks, layouts$original %in% checks))
    runs[[method]] <- c(result$diagnostics, elapsed_seconds = elapsed)
  }
  for (variant in names(books)) {
    for (measurement in c("euclidean", "manhattan")) {
      score <- evaluate(books[[variant]], checks, measurement)
      diagnostic <- if (variant == "original") list(swaps = 0L, evaluations = 0L,
        stop_reason = "not_run", elapsed_seconds = 0) else runs[[variant]]
      rows[[length(rows) + 1L]] <- data.frame(seed = seed, optimizer = variant,
        measurement = measurement, as.list(score), swaps = diagnostic$swaps,
        evaluations = diagnostic$evaluations, stop_reason = diagnostic$stop_reason,
        elapsed_seconds = diagnostic$elapsed_seconds)
    }
  }
  frozen[[as.character(seed)]] <- list(original = original, layouts = layouts, diagnostics = runs)
  if (seed %% 10 == 0) { cat("Completed", seed, "of 100 paired seeds\n"); flush.console() }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(out, "results.csv"), row.names = FALSE)
saveRDS(frozen, file.path(out, "frozen-layouts.rds"), version = 3)
metrics <- c("minimum", "nearest", "worst_nearest", "pairwise", "pairs3", "pairs5", "clustered_percent5")
summary <- do.call(rbind, lapply(split(results, list(results$optimizer, results$measurement)), function(x) {
  data.frame(optimizer = x$optimizer[[1]], measurement = x$measurement[[1]],
    as.list(colMeans(x[metrics])), median_seconds = median(x$elapsed_seconds),
    mean_evaluations = mean(x$evaluations))
}))
write.csv(summary, file.path(out, "summary.csv"), row.names = FALSE)
comparisons <- list()
for (measurement in c("euclidean", "manhattan")) {
  a <- results[results$optimizer == "euclidean" & results$measurement == measurement, ]
  b <- results[results$optimizer == "manhattan" & results$measurement == measurement, ]
  stopifnot(identical(a$seed, b$seed))
  for (metric in metrics) {
    delta <- b[[metric]] - a[[metric]]
    direction <- if (metric %in% c("pairs3", "pairs5", "clustered_percent5")) -1 else 1
    gain <- direction * delta
    comparisons[[length(comparisons) + 1L]] <- data.frame(measurement = measurement,
      metric = metric, manhattan_wins = sum(gain > 1e-10), ties = sum(abs(gain) <= 1e-10),
      euclidean_wins = sum(gain < -1e-10), mean_manhattan_minus_euclidean = mean(delta),
      median_manhattan_minus_euclidean = median(delta))
  }
}
write.csv(do.call(rbind, comparisons), file.path(out, "paired-comparison.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
print(summary, row.names = FALSE, digits = 5)
print(do.call(rbind, comparisons), row.names = FALSE, digits = 5)

profile <- function(layout, checks, method) sort(unlist(lapply(checks, function(check) {
  as.numeric(stats::dist(which(layout == check, arr.ind = TRUE), method = method))
})))
profiles <- list()
for (seed in names(frozen)) {
  item <- frozen[[seed]]
  checks <- item$original$infoDesign$entry_checks[[1]]
  for (measurement in c("euclidean", "manhattan")) {
    a <- profile(item$layouts$euclidean, checks, measurement)
    b <- profile(item$layouts$manhattan, checks, measurement)
    differing <- which(abs(a - b) > 1e-10)
    winner <- if (!length(differing)) "tie" else if (a[differing[[1]]] > b[differing[[1]]]) {
      "euclidean"
    } else "manhattan"
    profiles[[length(profiles) + 1L]] <- data.frame(seed = as.integer(seed),
      measurement = measurement, winner = winner)
  }
}
write.csv(do.call(rbind, profiles), file.path(out, "distance-profile-comparison.csv"), row.names = FALSE)
