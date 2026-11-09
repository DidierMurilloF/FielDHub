# M1.5 gates for the pair-swap optimizer (spec-m15 sections 4.4 and 5.6).
#   speed: Gate A2, master 720449e against the candidate, both from clean installs, on
#          the 5 fields of spec 1 (built as in spec 1: 20% of the plots' worth of
#          entries duplicated). It times swap_pairs() itself, the optimizer that
#          partially_replicated() calls, so field-book assembly is not counted.
#   rule:  Gates B2-B5 on the installed candidate
# Run pinned to one P-core with its sibling idle:
#   taskset -c 2 Rscript tools/benchmark-swap-pairs.R <master-library> <candidate-library> <speed|rule> <seeds> <out.csv>
args <- commandArgs(TRUE)
fields <- list(c(10, 20, 40), c(16, 25, 80), c(20, 50, 200), c(30, 60, 360), c(40, 75, 600))
make_field <- function(nr, nc, m, seed) {
  set.seed(seed)
  matrix(as.numeric(sample(c(rep(seq_len(m), each = 2L), seq.int(m + 1L, nr * nc - m)))), nr)
}
# 10 entries with 6 copies (checks), 60 with 2, the rest single, on 12 x 20 (Gate B4)
make_checks_field <- function(seed) {
  set.seed(seed)
  matrix(as.numeric(sample(c(rep(1:10, each = 6L), rep(11:70, each = 2L), 71:130))), 12)
}
shared_total <- function(X, scope) {
  v <- as.vector(X)
  k <- table(v[!is.na(v)])
  covered <- as.numeric(names(k)[if (scope == "all") k >= 2 else k == 2])
  sum(vapply(covered, function(g) FielDHub:::.shared_pairs(which(v == g), nrow(X)), integer(1)))
}

if (identical(args[1], "--worker")) {
  # --worker <master|candidate> <library> <seeds> <out.csv>: time swap_pairs() on the 5 fields
  .libPaths(c(normalizePath(args[3]), .libPaths()))
  library(FielDHub)
  invisible(swap_pairs(make_field(8, 10, 10, 1), seed = 1))  # warm-up (byte compilation)
  for (f in fields) for (s in eval(parse(text = args[4]))) {
    X <- make_field(f[1], f[2], f[3], s)
    gc()
    t <- system.time(x <- swap_pairs(X, seed = s))[["elapsed"]]
    utils::write.table(data.frame(side = args[2], field = paste0(f[1], "x", f[2]), seed = s, seconds = t,
                                  min_distance = x$min_distance, layout = paste(x$optim_design, collapse = ",")),
                       args[5], sep = ",", row.names = FALSE, col.names = !file.exists(args[5]),
                       append = file.exists(args[5]))
  }
  quit(save = "no")
}

if (length(args) != 5L || !args[3] %in% c("speed", "rule")) {
  stop("Usage: benchmark-swap-pairs.R <master-library> <candidate-library> <speed|rule> <seeds> <out.csv>")
}
master <- normalizePath(args[1]); lib <- normalizePath(args[2]); mode <- args[3]; seeds <- args[4]; out <- args[5]
if (file.exists(out)) file.remove(out)
self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))

if (mode == "speed") {
  for (side in c("master", "candidate")) {
    status <- system2("Rscript", c("--vanilla", self, "--worker", side,
                                   if (side == "master") master else lib, shQuote(seeds), out))
    if (status != 0L) stop("the ", side, " worker failed")
  }
  d <- utils::read.csv(out)
  old <- d[d$side == "master", ]; new <- d[d$side == "candidate", ]
  stopifnot(identical(old$layout, new$layout))  # same layouts, same seeds (Gate A1 in passing)
  tab <- data.frame(field = unique(old$field),
                    master_s = tapply(old$seconds, old$field, stats::median)[unique(old$field)],
                    new_s = tapply(new$seconds, new$field, stats::median)[unique(old$field)])
  tab$speedup <- tab$master_s / tab$new_s
  print(tab, row.names = FALSE, digits = 3)
  cat(sprintf("A2: 40x75 median master/new >= 10 (got %.1f); every field faster: %s\n",
              tab$speedup[tab$field == "40x75"], all(tab$speedup > 1)))
  quit(save = "no")
}

.libPaths(c(lib, .libPaths()))
library(FielDHub)
rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(...)
for (f in fields) for (s in eval(parse(text = seeds))) {
  X <- make_field(f[1], f[2], f[3], s)
  for (pen in c(0, Inf, 0.5, 2, 10)) {
    t <- system.time(x <- swap_pairs(X, seed = s, rowcol_penalty = pen))[["elapsed"]]
    add(field = paste0(f[1], "x", f[2]), seed = s, penalty = pen, seconds = t, min_distance = x$min_distance,
        shared = shared_total(x$optim_design, "pairs"))
  }
}
for (s in eval(parse(text = seeds))) {
  X <- make_checks_field(s)
  off <- swap_pairs(X, seed = s)
  on <- swap_pairs(X, seed = s, rowcol_penalty = Inf, rowcol_scope = "all")
  add(field = "checks12x20", seed = s, penalty = -1, seconds = NA, min_distance = off$min_distance,
      shared = shared_total(off$optim_design, "all"))
  add(field = "checks12x20", seed = s, penalty = Inf, seconds = NA, min_distance = on$min_distance,
      shared = shared_total(on$optim_design, "all"))
}
d <- do.call(rbind, rows)
utils::write.csv(d, out, row.names = FALSE)
main <- d[d$field != "checks12x20", ]
on <- main[main$penalty == Inf, ]; off <- main[main$penalty == 0, ]
med <- function(x, by) tapply(x, by, stats::median)
dist_ok <- med(on$min_distance, on$field) >= med(off$min_distance, off$field) - 1
cat(sprintf("B2: S = 0 in %d/%d runs; median min distance on >= off - 1 on %d/%d fields\n",
            sum(on$shared == 0), nrow(on), sum(dist_ok), length(dist_ok)))
cat("B3: trade-off (medians)\n")
print(stats::aggregate(cbind(shared, min_distance) ~ field + penalty, main, stats::median), row.names = FALSE)
b4 <- d[d$field == "checks12x20", ]
cat(sprintf("B4: S(all scope, on) <= S(off) in %d/%d seeds\n",
            sum(b4$shared[b4$penalty == Inf] <= b4$shared[b4$penalty == -1]), sum(b4$penalty == Inf)))
big <- main[main$field == "40x75", ]
cat(sprintf("B5: 40x75 time on/off <= 3 (got %.2f)\n",
            stats::median(big$seconds[big$penalty == Inf]) / stats::median(big$seconds[big$penalty == 0])))
