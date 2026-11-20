# Paired timing of the onestage build for the speed rule (phase 2 spec 7):
#
#   Rscript tools/time-onestage-pairs.R <pairs.csv> <library> [grid=os|quick] [seeds=10]
#           [shard=i/k]
#
# Per shape, `iterations` and seed, in one process, it times the pre-change
# builder (reference$build_row_column_onestage(), blocksdesign::design(),
# sourced from tools/reference-onestage.R into an environment whose parent is
# the FielDHub namespace) and the candidate's FielDHub:::build_row_column_onestage()
# with the same arguments, back to back, alternating which goes first (odd
# seeds the reference first), after one warm-up pair. Errors (a fallback
# signals onestage_infeasible) are caught and the time up to them recorded.
# One row is appended per pair as it finishes (rerunning with the same path
# resumes):
#   caller,t,k,r,iterations,seed,bd_seconds,engine_seconds
# `caller` is onestage_nested or onestage_latinized, `k` is nrows.
#
# <library> holds the candidate (R CMD INSTALL . --preclean); blocksdesign
# 4.9 is taken from the libraries after it in .libPaths(). The script stops if
# FielDHub is loaded from a source tree. grid=os: the shapes and iterations
# of nested_golden_grid("os"); grid=quick: 24/6/2, 48/6/3, 100/10/4 and 9/3/2,
# both codings, iterations 20 and 200. Seeds 1..seeds per row (default 10),
# 1..5 at t = 500. shard=i/k runs shard i of k (rows spread by
# longest-processing-time on (t r)^2 x seeds). Judge the result with
# nested_speed() (tests/testthat/helper-nested-goldens.R).
arguments <- commandArgs(trailingOnly = TRUE)
option <- function(name, default) {
  hit <- grep(paste0("^", name, "="), arguments, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[1])
}
positional <- arguments[!grepl("=", arguments, fixed = TRUE)]
if (length(positional) != 2L) {
  stop("Usage: Rscript tools/time-onestage-pairs.R <pairs.csv> <library> [grid=os|quick] ",
       "[seeds=10] [shard=i/k]")
}
path <- positional[1]
.libPaths(c(normalizePath(positional[2], mustWork = TRUE), .libPaths()))
grid_name <- option("grid", "os")
if (!grid_name %in% c("os", "quick")) stop("grid= must be os or quick.")
seeds <- as.integer(option("seeds", "10"))
shard <- option("shard", "1/1")
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
if (dir.exists(file.path(find.package("FielDHub"), "src"))) {
  stop("FielDHub is loaded from a source tree; time an installed build instead.")
}
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
reference <- new.env(parent = asNamespace("FielDHub"))
sys.source(file.path(script_dir, "reference-onestage.R"), envir = reference)
candidate <- utils::getFromNamespace("build_row_column_onestage", "FielDHub")

grid <- if (grid_name == "quick") {
  shapes <- data.frame(t = c(24L, 48L, 100L, 9L), k = c(6L, 6L, 10L, 3L), r = c(2L, 3L, 4L, 2L))
  merge(merge(shapes, data.frame(caller = nested_onestage_callers)),
        data.frame(iterations = c(20L, 200L)))
} else {
  nested_golden_grid("os")[c("caller", "t", "k", "r", "iterations")]
}
grid$seeds <- ifelse(grid$t == 500, 5L, seeds)
ik <- as.integer(strsplit(shard, "/", fixed = TRUE)[[1]])
if (length(ik) != 2L || anyNA(ik) || ik[1] < 1L || ik[1] > ik[2]) stop("shard= must be i/k.")
cost <- (as.double(grid$t) * grid$r)^2 * grid$seeds * grid$iterations
load <- numeric(ik[2])
owner <- integer(nrow(grid))
for (i in order(-cost, seq_along(cost))) {
  owner[i] <- which.min(load)
  load[owner[i]] <- load[owner[i]] + cost[i]
}
grid <- grid[owner == ik[1], , drop = FALSE]

timed <- function(builder, args) {
  system.time(tryCatch(suppressWarnings(do.call(builder, args)), error = function(e) NULL))[["elapsed"]]
}
pair <- function(t, k, r, latinize, iterations, seed) {
  args <- list(nt = t, nrows = k, ncols = t %/% k, reps = r, latinize = latinize,
               searches = iterations, seed = seed)
  if (seed %% 2L == 1L) {
    bd <- timed(reference$build_row_column_onestage, args)
    en <- timed(candidate, args)
  } else {
    en <- timed(candidate, args)
    bd <- timed(reference$build_row_column_onestage, args)
  }
  c(bd, en)
}
invisible(pair(12L, 3L, 2L, FALSE, 20L, 1L))  # warm up both code paths
invisible(pair(12L, 3L, 2L, TRUE, 20L, 2L))

done <- if (file.exists(path)) utils::read.csv(path) else NULL
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  for (seed in seq_len(g$seeds)) {
    if (!is.null(done) && any(done$caller == g$caller & done$t == g$t & done$k == g$k &
                              done$r == g$r & done$iterations == g$iterations &
                              done$seed == seed)) next
    s <- pair(g$t, g$k, g$r, g$caller == "onestage_latinized", g$iterations, seed)
    row <- data.frame(caller = g$caller, t = g$t, k = g$k, r = g$r, iterations = g$iterations,
                      seed = seed, bd_seconds = s[1], engine_seconds = s[2])
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
cat(sprintf("%s: %d rows\n", path, if (file.exists(path)) nrow(utils::read.csv(path)) else 0L))
