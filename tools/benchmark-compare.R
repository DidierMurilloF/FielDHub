# Plain benchmark contracts shared by the release CLI and its fixture checks.
benchmark_contracts <- function() list(
  dimensions = list(keys = "plots", invariant = c("factor_pairs", "repetitions")),
  spatial = list(keys = c("rows", "columns"),
    invariant = c("plots", "correlation_x", "correlation_y", "nugget", "repetitions")),
  exports = list(keys = c("rows", "columns"), invariant = c("plots", "csv_bytes")),
  optimizers = list(keys = c("rows", "columns", "distance_method"),
    invariant = c("plots", "replicated_entries", "input_seed", "optimizer_seed", "iterations_per_threshold")))

benchmark_keys <- function(x, columns) {
  if (anyNA(x[columns])) stop("Missing benchmark case key.")
  keys <- do.call(paste, c(x[columns], sep = ":"))
  if (anyDuplicated(keys)) stop("Duplicate benchmark case key.")
  keys
}

compare_benchmark_tables <- function(baseline, candidate, family) {
  contract <- benchmark_contracts()[[family]]
  if (is.null(contract)) stop("Unknown benchmark family: ", family)
  metrics <- c("median_seconds", "largest_R_allocation_bytes")
  diagnostics <- if (family == "optimizers") {
    c("retained_min_distance", "stop_reason", "iterations", "thresholds_attempted")
  } else character()
  required <- c("package_version", "r_version", "platform", contract$keys,
                contract$invariant, metrics, diagnostics)
  for (x in list(baseline, candidate)) {
    if (!is.data.frame(x) || !nrow(x) || !all(required %in% names(x))) {
      stop("Incomplete benchmark table: ", family)
    }
    for (column in c("package_version", "r_version", "platform")) {
      if (anyNA(x[[column]]) || length(unique(x[[column]])) != 1L) {
        stop("Inconsistent benchmark environment: ", column)
      }
    }
    for (column in metrics) {
      value <- x[[column]]
      if (!is.numeric(value) && !(is.logical(value) && all(is.na(value)))) {
        stop("Non-numeric benchmark metric: ", column)
      }
      if (any(!is.finite(value) & !is.na(value)) || any(value < 0, na.rm = TRUE) ||
          (column == "median_seconds" && anyNA(value))) stop("Invalid benchmark metric: ", column)
    }
    if (family == "optimizers") {
      for (column in setdiff(diagnostics, "stop_reason")) {
        if (!is.numeric(x[[column]]) || any(!is.finite(x[[column]])) || any(x[[column]] < 0)) {
          stop("Invalid optimizer diagnostic: ", column)
        }
      }
      if (anyNA(x$stop_reason) || any(!nzchar(x$stop_reason))) stop("Missing optimizer stop reason.")
    }
  }
  for (column in c("r_version", "platform")) {
    if (!identical(baseline[[column]][1L], candidate[[column]][1L])) {
      stop("Benchmark environments differ: ", column)
    }
  }
  before <- benchmark_keys(baseline, contract$keys)
  after <- benchmark_keys(candidate, contract$keys)
  if (!setequal(before, after)) stop("Benchmark cases differ: ", family)
  candidate <- candidate[match(before, after), , drop = FALSE]
  rows <- list()
  add <- function(i, metric, status, reason) {
    b <- baseline[[metric]][i]
    c <- candidate[[metric]][i]
    rows[[length(rows) + 1L]] <<- data.frame(
      case = paste(family, before[i], sep = ":"), metric = metric,
      baseline = if (is.na(b)) "unavailable" else as.character(b),
      candidate = if (is.na(c)) "unavailable" else as.character(c),
      ratio = if (is.numeric(b) && is.numeric(c) && !is.na(b) && b > 0) c / b else NA_real_,
      status = status, reason = reason, stringsAsFactors = FALSE)
  }
  for (i in seq_len(nrow(baseline))) {
    for (metric in contract$invariant) {
      b <- baseline[[metric]][i]
      c <- candidate[[metric]][i]
      if (is.na(b) || is.na(c) || !isTRUE(all.equal(b, c, check.attributes = FALSE))) {
        stop("Benchmark workload/output changed: ", family, ":", before[i], ":", metric)
      }
    }
    for (metric in metrics) {
      b <- baseline[[metric]][i]
      c <- candidate[[metric]][i]
      missing <- is.na(b) || is.na(c)
      regressed <- !missing && c > max(b * 1.25, b + if (metric == "median_seconds") 0.01 else 0)
      add(i, metric, if (missing || regressed) "review" else "pass",
          if (missing) "Allocation profiling unavailable; supply alternative evidence." else
            if (regressed) "Increase exceeds 25% (and 10 ms for runtime)." else "Within comparison threshold.")
    }
    for (metric in diagnostics) {
      b <- baseline[[metric]][i]
      c <- candidate[[metric]][i]
      changed <- if (metric == "retained_min_distance") c < b - 1e-10 else !isTRUE(all.equal(b, c))
      add(i, metric, if (changed) "review" else "pass",
          if (changed) "Optimizer quality or stopping behavior changed." else "Quality/diagnostic preserved.")
    }
  }
  do.call(rbind, rows)
}

compare_benchmark_directories <- function(baseline, candidate) {
  manifests <- lapply(c(baseline, candidate), function(path) {
    x <- read.dcf(file.path(path, "manifest.dcf"))
    required <- c("Revision", "PackageVersion", "RVersion", "RBuild", "Platform", "Host", "BLAS", "Dependencies", "BenchmarkCode")
    if (nrow(x) != 1L || !all(required %in% colnames(x)) || anyNA(x[, required]) ||
        any(!nzchar(x[, required])) ||
        !grepl("^[0-9a-f]{40}$", x[1L, "Revision"])) stop("Incomplete benchmark manifest.")
    x
  })
  for (column in c("RVersion", "RBuild", "Platform", "Host", "BLAS", "Dependencies", "BenchmarkCode")) {
    if (!identical(manifests[[1L]][1L, column], manifests[[2L]][1L, column])) {
      stop("Benchmark manifests differ: ", column)
    }
  }
  rows <- lapply(names(benchmark_contracts()), function(family) {
    tables <- lapply(c(baseline, candidate), function(path) {
      utils::read.csv(file.path(path, paste0(family, ".csv")), stringsAsFactors = FALSE)
    })
    result <- compare_benchmark_tables(tables[[1L]], tables[[2L]], family)
    mapping <- c(package_version = "PackageVersion", r_version = "RVersion", platform = "Platform")
    for (i in seq_along(tables)) for (column in names(mapping)) {
      if (!identical(as.character(tables[[i]][[column]][1L]), unname(manifests[[i]][1L, mapping[[column]]]))) {
        stop("Benchmark table disagrees with its manifest: ", family, ":", column)
      }
    }
    result
  })
  result <- do.call(rbind, rows)
  result$baseline_revision <- manifests[[1L]][1L, "Revision"]
  result$candidate_revision <- manifests[[2L]][1L, "Revision"]
  result
}

review_benchmark_exceptions <- function(report, exceptions) {
  columns <- c("case", "metric", "baseline", "candidate", "baseline_revision", "candidate_revision", "reason")
  if (!is.data.frame(exceptions) || !all(columns %in% names(exceptions)) || anyNA(exceptions[columns]) ||
      any(!nzchar(trimws(exceptions$reason)))) stop("Incomplete benchmark review explanations.")
  key_columns <- setdiff(columns, "reason")
  keys <- benchmark_keys(report, key_columns)
  matches <- match(benchmark_keys(exceptions, key_columns), keys)
  if (anyNA(matches) || any(report$status[matches] != "review")) stop("Stale or unnecessary benchmark exception.")
  report$status[matches] <- "reviewed"
  report$reason[matches] <- exceptions$reason
  report
}
