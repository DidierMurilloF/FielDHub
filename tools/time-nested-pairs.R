# Paired timing for the speed rule (spec 7): for each shape of the golden grid
# and each seed, one R process times blocksdesign's optimizer call and the
# candidate's resolvable block engine back to back, so core type and load
# cancel within the pair:
#   Rscript tools/time-nested-pairs.R pairs.csv [package-library] [shard=i/k]
# The candidate must be an installed build (R CMD INSTALL . --preclean), never
# devtools::load_all(), which compiles the C++ without optimisation. Seeds are
# 1..20 per shape (1..5 for t >= 1000). One row (caller, t, k, r, seed,
# bd_seconds, engine_seconds) is appended per pair as it finishes; rerunning
# with the same path resumes. Optional shard=i/k runs only shard i of k (shapes
# spread by longest-processing-time on the baseline's recorded time when
# tools/nested-goldens-baseline.csv.gz is present); concatenate the shard CSVs
# and pass them to tools/check-nested-goldens.R, which applies nested_speed().
arguments <- commandArgs(trailingOnly = TRUE)
shard_arg <- grep("^shard=", arguments, value = TRUE)
arguments <- setdiff(arguments, shard_arg)
if (length(arguments) < 1L || length(arguments) > 2L || length(shard_arg) > 1L) {
  stop("Usage: Rscript tools/time-nested-pairs.R pairs.csv [package-library] [shard=i/k]")
}
path <- arguments[1]
if (length(arguments) == 2L) .libPaths(c(normalizePath(arguments[2], mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
if (file.exists(file.path(find.package("FielDHub"), "DESCRIPTION")) &&
    dir.exists(file.path(find.package("FielDHub"), "src"))) {
  stop("FielDHub is loaded from a source tree; time an installed build instead.")
}
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

grid <- nested_golden_grid("full")
grid$seeds <- ifelse(grid$t >= 1000, 5L, pmin(grid$seeds, 20L))
if (length(shard_arg) == 1L) {
  ik <- as.integer(strsplit(sub("^shard=", "", shard_arg), "/", fixed = TRUE)[[1]])
  if (length(ik) != 2L || anyNA(ik) || ik[2] < 1L || ik[1] < 1L || ik[1] > ik[2]) {
    stop("shard= must be i/k with 1 <= i <= k.")
  }
  cost <- (grid$t * grid$r)^2 * grid$seeds
  baseline_path <- file.path(script_dir, "nested-goldens-baseline.csv.gz")
  if (file.exists(baseline_path)) {
    base <- utils::read.csv(baseline_path, stringsAsFactors = FALSE)
    med <- stats::aggregate(seconds ~ caller + t + k + r, base, stats::median)
    known <- match(nested_shape_key(grid), nested_shape_key(med))
    cost <- ifelse(is.na(known), cost, med$seconds[known] * grid$seeds)
  }
  load <- numeric(ik[2])
  owner <- integer(nrow(grid))
  for (i in order(-cost, seq_along(cost))) {
    owner[i] <- which.min(load)
    load[owner[i]] <- load[owner[i]] + cost[i]
  }
  grid <- grid[owner == ik[1], , drop = FALSE]
}

engine <- utils::getFromNamespace("blocks_engine", "FielDHub")
pair <- function(t, k, r, seed) {
  bd <- system.time(blocksdesign::blocks(treatments = t, replicates = r,
                                         blocks = list(r, t / k), seed = seed))[["elapsed"]]
  set.seed(seed)
  en <- system.time(suppressWarnings(engine(t, r, t / k, resolvable = TRUE)))[["elapsed"]]
  c(bd, en)
}
invisible(pair(12, 3, 2, 1))  # warm up both code paths before the first timed pair

done <- if (file.exists(path)) utils::read.csv(path) else NULL
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  for (seed in seq_len(g$seeds)) {
    if (!is.null(done) && any(done$caller == g$caller & done$t == g$t & done$k == g$k &
                              done$r == g$r & done$seed == seed)) next
    s <- pair(g$t, g$k, g$r, seed)
    row <- data.frame(caller = g$caller, t = g$t, k = g$k, r = g$r, seed = seed,
                      bd_seconds = s[1], engine_seconds = s[2])
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
cat(sprintf("%s: %d rows\n", path, if (file.exists(path)) nrow(utils::read.csv(path)) else 0L))
