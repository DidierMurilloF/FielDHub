# Paired timing of the twostage row step (spec 6.8 and the speed rule of spec
# 7): for each shape of the extended row-column grid (protocol rc) and each
# seed, one R process builds the column design exactly as
# row_column(method = "twostage") does, then times the pre-change R row step
# (tools/reference-improve-efficiency.R) and the package's engine row step on
# that same design and seed, back to back (in alternating order), so core type
# and load cancel within the pair:
#   Rscript tools/time-row-step-pairs.R pairs.csv [package-library] [seeds=10] [shard=i/k]
# The package must be an installed build (R CMD INSTALL . --preclean), never
# devtools::load_all(), which compiles the C++ without optimisation. One row
# (caller, t, k, r, iterations, seed, bd_seconds = reference R, engine_seconds
# = engine, and the row A-efficiency of each) is appended per pair; rerunning
# with the same path resumes. Shards are spread by longest-processing-time on
# the rc baseline's recorded time. tools/check-nested-goldens.R reads the
# concatenated CSVs as its pairs.csv argument (bd_seconds column).
arguments <- commandArgs(trailingOnly = TRUE)
shard_arg <- grep("^shard=", arguments, value = TRUE)
seeds_arg <- grep("^seeds=", arguments, value = TRUE)
arguments <- setdiff(arguments, c(shard_arg, seeds_arg))
if (length(arguments) < 1L || length(arguments) > 2L || length(shard_arg) > 1L || length(seeds_arg) > 1L) {
  stop("Usage: Rscript tools/time-row-step-pairs.R pairs.csv [package-library] [seeds=10] [shard=i/k]")
}
path <- arguments[1]
seeds <- if (length(seeds_arg) == 1L) as.integer(sub("^seeds=", "", seeds_arg)) else 10L
if (length(arguments) == 2L) .libPaths(c(normalizePath(arguments[2], mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
if (dir.exists(file.path(find.package("FielDHub"), "src"))) {
  stop("FielDHub is loaded from a source tree; time an installed build instead.")
}
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
ns <- asNamespace("FielDHub")
reference <- new.env(parent = ns)
sys.source(file.path(script_dir, "reference-improve-efficiency.R"), envir = reference)

grid <- nested_golden_grid("rc")
grid$seeds <- seeds
if (length(shard_arg) == 1L) {
  ik <- as.integer(strsplit(sub("^shard=", "", shard_arg), "/", fixed = TRUE)[[1]])
  if (length(ik) != 2L || anyNA(ik) || ik[2] < 1L || ik[1] < 1L || ik[1] > ik[2]) {
    stop("shard= must be i/k with 1 <= i <= k.")
  }
  cost <- (grid$t * grid$r)^2
  baseline_path <- file.path(script_dir, "nested-goldens-rc-baseline.csv.gz")
  if (file.exists(baseline_path)) {
    base <- utils::read.csv(baseline_path, stringsAsFactors = FALSE)
    base$iterations[is.na(base$iterations)] <- -1L
    med <- stats::aggregate(seconds ~ t + k + r + iterations, base, stats::median)
    it <- ifelse(is.na(grid$iterations), -1L, grid$iterations)
    known <- match(paste(grid$t, grid$k, grid$r, it), paste(med$t, med$k, med$r, med$iterations))
    cost <- ifelse(is.na(known), cost, med$seconds[known])
  }
  cost <- cost * grid$seeds
  load <- numeric(ik[2])
  owner <- integer(nrow(grid))
  for (i in order(-cost, seq_along(cost))) {
    owner[i] <- which.min(load)
    load[owner[i]] <- load[owner[i]] + cost[i]
  }
  grid <- grid[owner == ik[1], , drop = FALSE]
}

# The column design of row_column(t, nrows, reps, method = "twostage", seed),
# location 1, as that function builds it before its row step
column_design <- function(t, nrows, reps, seed) {
  ncols <- t / nrows
  set.seed(ns$offset_design_seed(seed, 1L))
  mydes <- suppressWarnings(ns$blocks_engine(t, reps, ncols, resolvable = TRUE))
  mydes <- ns$rerandomize_ibd(ibd_design = mydes)
  design <- mydes$Design_new
  level_3 <- rep(rep(paste0("B", 1:nrows), times = ncols), times = reps)
  design$Level_3 <- paste(design$Level_1, level_3, sep = ".")
  design$Level_3 <- factor(design$Level_3, levels = unique(design$Level_3))
  design[c("Level_1", "Level_2", "Level_3", "plots", "treatments")]
}
pair <- function(t, nrows, reps, seed, iterations, engine_first) {
  design <- column_design(t, nrows, reps, seed)
  row_seed <- ns$offset_design_seed(seed, 1L)
  run <- function(fun) {
    out <- NULL
    s <- system.time(out <- suppressWarnings(fun(design, iterations, seed = row_seed)))[["elapsed"]]
    c(s, out$best_a_efficiency)
  }
  if (engine_first) {
    en <- run(ns$improve_efficiency)
    bd <- run(reference$improve_efficiency)
  } else {
    bd <- run(reference$improve_efficiency)
    en <- run(ns$improve_efficiency)
  }
  c(bd, en)
}
invisible(pair(12, 3, 2, 1, 50, TRUE))  # warm up both code paths before the first timed pair

done <- if (file.exists(path)) utils::read.csv(path) else NULL
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  iterations <- if (is.na(g$iterations)) 1000L else g$iterations
  for (seed in seq_len(g$seeds)) {
    if (!is.null(done) && any(done$t == g$t & done$k == g$k & done$r == g$r & done$seed == seed &
                              ((is.na(done$iterations) & is.na(g$iterations)) |
                                 (!is.na(done$iterations) & !is.na(g$iterations) &
                                    done$iterations == g$iterations)))) next
    s <- pair(g$t, g$k, g$r, seed, iterations, engine_first = seed %% 2L == 0L)
    row <- data.frame(caller = g$caller, t = g$t, k = g$k, r = g$r, iterations = g$iterations,
                      seed = seed, bd_seconds = s[1], engine_seconds = s[3],
                      reference_row_A = s[2], engine_row_A = s[4])
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
cat(sprintf("%s: %d rows\n", path, if (file.exists(path)) nrow(utils::read.csv(path)) else 0L))
