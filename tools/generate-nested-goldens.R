# Records the efficiencies of the block-design callers through their public
# functions (spec 7). Run it against the code to be measured:
#   Rscript tools/generate-nested-goldens.R results.csv <full|ci|rc|os> [package-library] [seed_offset=0] [shard=i/k]
# Seeds are seed_offset + 1:seeds, so seed_offset=1000 gives the fresh seeds
# of the engine comparison. One row per shape and seed is appended as it
# finishes; rerunning with the same path resumes and skips the rows already
# present. <results>.meta.rds records the versions and the platform.
# Optional shard=i/k runs only shard i of k: shapes are spread over shards by
# longest-processing-time on the estimated cost (t r)^2 x seeds (protocol os:
# times (1 + iterations / 20)). Run each shard
# on its own core with its own results path, then concatenate the CSVs.
# Protocol os is the onestage row-column grid (phase 2 spec 7; callers
# onestage_nested and onestage_latinized, iterations 200 and 20); its rows carry
# the clash, bound and optimiser_seconds columns.
# Protocol rc is the extended row_column twostage grid (spec 6.8); its rows carry
# the iterations column. The committed baseline is the concatenated shard CSVs of a full run, gzipped, as
# tools/nested-goldens-baseline.csv.gz; compare a candidate run with it using
# tools/check-nested-goldens.R.
arguments <- commandArgs(trailingOnly = TRUE)
shard_arg <- grep("^shard=", arguments, value = TRUE)
offset_arg <- grep("^seed_offset=", arguments, value = TRUE)
arguments <- setdiff(arguments, c(shard_arg, offset_arg))
if (length(arguments) < 2L || length(arguments) > 3L || length(shard_arg) > 1L || length(offset_arg) > 1L) {
  stop("Usage: Rscript tools/generate-nested-goldens.R results.csv <full|ci|rc|os> [package-library] [seed_offset=0] [shard=i/k]")
}
path <- arguments[1]
protocol <- arguments[2]
if (!protocol %in% c("full", "ci", "rc", "os")) stop("<protocol> must be full, ci, rc or os.")
seed_offset <- if (length(offset_arg) == 1L) as.integer(sub("^seed_offset=", "", offset_arg)) else 0L
if (is.na(seed_offset)) stop("seed_offset= must be a whole number.")
if (length(arguments) == 3L) .libPaths(c(normalizePath(arguments[3], mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

grid <- nested_golden_grid(protocol)
if (length(shard_arg) == 1L) {
  ik <- as.integer(strsplit(sub("^shard=", "", shard_arg), "/", fixed = TRUE)[[1]])
  if (length(ik) != 2L || anyNA(ik) || ik[2] < 1L || ik[1] < 1L || ik[1] > ik[2]) {
    stop("shard= must be i/k with 1 <= i <= k.")
  }
  cost <- (grid$t * grid$r)^2 * grid$seeds
  # onestage rows: the search time grows with the number of searches
  if (protocol == "os") cost <- cost * (1 + grid$iterations / 20)
  load <- numeric(ik[2])
  owner <- integer(nrow(grid))
  for (i in order(-cost, seq_along(cost))) {
    owner[i] <- which.min(load)
    load[owner[i]] <- load[owner[i]] + cost[i]
  }
  grid <- grid[owner == ik[1], , drop = FALSE]
}
saveRDS(list(FielDHub = as.character(utils::packageVersion("FielDHub")),
             blocksdesign = tryCatch(as.character(utils::packageVersion("blocksdesign")),
                                     error = function(e) NA_character_),
             R = R.version.string, platform = R.version$platform,
             protocol = protocol, seed_offset = seed_offset),
        paste0(path, ".meta.rds"))
done <- if (file.exists(path)) utils::read.csv(path) else NULL
done_iterations <- if (is.null(done)) NULL else if ("iterations" %in% names(done)) done$iterations else rep(NA_integer_, nrow(done))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  for (seed in seed_offset + seq_len(g$seeds)) {
    if (!is.null(done) && any(done$caller == g$caller & done$t == g$t & done$k == g$k &
                              done$r == g$r & done$seed == seed &
                              ((is.na(done_iterations) & is.na(g$iterations)) |
                                 (!is.na(done_iterations) & !is.na(g$iterations) &
                                    done_iterations == g$iterations)))) next
    iterations <- if (is.na(g$iterations)) NULL else g$iterations
    row <- nested_golden_record(g$caller, g$t, g$k, g$r, seed, iterations)
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
cat(sprintf("%s: %d rows\n", path, nrow(utils::read.csv(path))))
