# M1 benchmark: the internal C++ block engine against blocksdesign on the
# do_optim() call shape, blocks(treatments = v, replicates = s, blocks = L).
# Run against the installed candidate, sequentially on one pinned core:
#   taskset -c 2 Rscript tools/benchmark-blocks-engine.R results.csv <protocol> [package-library]
# <protocol> is "A" (owner decision 2026-11-05: 50 seeds for v <= 300, 10 for
# v = 600, 3 for v >= 1000), "smoke" (v <= 100, 1 seed) or a whole number of
# seeds for every case.
# Rows are appended as they finish; rerunning with the same path resumes and
# skips (case, seed) rows already present. A summary is written to <results>.md.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 2L || length(arguments) > 3L) {
  stop("Usage: Rscript tools/benchmark-blocks-engine.R results.csv <A|smoke|seeds> [package-library]")
}
path <- arguments[1]
protocol <- arguments[2]
seeds_for <- switch(protocol,
  A = function(v) if (v <= 300) 50L else if (v == 600) 10L else 3L,
  smoke = function(v) if (v <= 100) 1L else 0L,
  {
    fixed <- suppressWarnings(as.integer(protocol))
    if (is.na(fixed) || fixed < 1L) stop("<protocol> must be A, smoke or a positive whole number.")
    function(v) fixed
  })
if (length(arguments) == 3L) .libPaths(c(normalizePath(arguments[3], mustWork = TRUE), .libPaths()))
library(FielDHub)
library(blocksdesign)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

grid <- do.call(rbind, lapply(c(20, 50, 100, 300, 600, 1000, 2000), function(v) {
  do.call(rbind, lapply(3:6, function(L) {
    s <- c(seq.int(2, L - 1), seq.int(L + 1, 2 * L - 1))
    data.frame(v = v, L = L, s = s[s >= 2])
  }))
}))
done <- if (file.exists(path)) utils::read.csv(path) else NULL
timed <- function(expr) {
  gc()
  t <- system.time(value <- expr)[["elapsed"]]
  list(value = value, seconds = t)
}
for (i in seq_len(nrow(grid))) {
  for (seed in seq_len(seeds_for(grid$v[i]))) {
    g <- grid[i, ]
    if (!is.null(done) && any(done$v == g$v & done$L == g$L & done$s == g$s & done$seed == seed)) next
    set.seed(seed)
    e <- timed(FielDHub:::blocks_engine(g$v, g$s, g$L))
    counts <- table(e$value$Design$treatments, e$value$Design$Level_1)
    k <- colSums(counts)
    n <- g$v * g$s
    margins_ok <- all(rowSums(counts) == g$s) && all(sort(k) == sort(n %/% g$L + (seq_len(g$L) <= n %% g$L)))
    info <- diag(rowSums(counts)) - unclass(counts) %*% diag(1 / k, g$L) %*% t(unclass(counts))
    connected <- sum(eigen(info, symmetric = TRUE, only.values = TRUE)$values > 1e-9) == g$v - 1
    b <- timed(blocks(treatments = g$v, replicates = g$s, blocks = g$L, searches = 20, seed = seed))
    row <- data.frame(v = g$v, L = g$L, s = g$s, seed = seed,
                      engine_d = e$value$Blocks_model[["D-Efficiency"]], engine_s = e$seconds,
                      engine_stop = e$value$search$stop_reason, margins_ok = margins_ok,
                      connected = connected,
                      bd_d = b$value$Blocks_model[["D-Efficiency"]], bd_s = b$seconds)
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
res <- utils::read.csv(path)
key <- interaction(res$v, res$L, res$s, drop = TRUE)
summary <- do.call(rbind, lapply(split(res, key), function(x) data.frame(
  v = x$v[1], L = x$L[1], s = x$s[1], seeds = nrow(x),
  engine_d = stats::median(x$engine_d), bd_d = stats::median(x$bd_d),
  d_diff = stats::median(x$engine_d) - stats::median(x$bd_d),
  engine_s = stats::median(x$engine_s), bd_s = stats::median(x$bd_s),
  speedup = stats::median(x$bd_s) / stats::median(x$engine_s))))
summary <- summary[order(summary$v, summary$L, summary$s), ]
integrity <- c(sum(res$margins_ok), sum(res$connected), nrow(res))
summary$quality_pass <- summary$d_diff >= -1e-4
summary$speed_pass <- summary$v < 100 | summary$speedup >= 10
header <- c("# M1 engine benchmark",
            sprintf("FielDHub %s, blocksdesign %s, %s, %s; protocol %s", utils::packageVersion("FielDHub"),
                    utils::packageVersion("blocksdesign"), R.version.string, Sys.info()[["machine"]],
                    protocol),
            sprintf("Gates M1-1/M1-2 (exact margins, connected): %d and %d of %d runs.",
                    integrity[1], integrity[2], integrity[3]),
            sprintf("Quality gate (median D >= blocksdesign - 1e-4): %d/%d cases pass.",
                    sum(summary$quality_pass), nrow(summary)),
            sprintf("Speed gate (v >= 100, median time <= 1/10): %d/%d cases pass.",
                    sum(summary$speed_pass[summary$v >= 100]), sum(summary$v >= 100)), "")
table <- c(paste("|", paste(names(summary), collapse = " | "), "|"),
           paste("|", paste(rep("---", ncol(summary)), collapse = " | "), "|"),
           apply(format(summary, digits = 7), 1, function(r) paste("|", paste(trimws(r), collapse = " | "), "|")))
writeLines(c(header, table), paste0(path, ".md"))
cat(header, sep = "\n")
